{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

-- | A compact fence-pointer index for uniformly distributed keys.
--
-- TODO: add utility functions for clash probability calculations
--
-- TODO: (de-)serialisation
--
module Database.LSMTree.Internal.Run.Index.Compact (
    -- $compact
    CompactIndex (..)
    -- * Invariants and bounds
  , rangeFinderPrecisionBounds
  , suggestRangeFinderPrecision
    -- * Queries
  , PageNo (..)
  , SearchResult (..)
  , PageSpan (..)
  , toPageSpan
  , search
  , countClashes
  , hasClashes
  , sizeInPages
    -- * Construction
    -- $construction-invariants
  , Append (..)
  , fromList
  , fromListSingles
    -- ** Incremental
    -- $incremental
  , MCompactIndex
  , new
  , append
  , unsafeEnd
  , appendSingle
  , appendMulti
    -- *** Chunks
  , Chunk (..)
  , FinalChunk (..)
  , fromChunks
  ) where

import           Control.Exception (assert)
import           Control.Monad (forM_, when)
import           Control.Monad.ST
import           Data.Bit hiding (flipBit)
import           Data.Foldable (toList)
import           Data.Map.Range (Bound (..), Clusive (Exclusive, Inclusive))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, fromJust)
import           Data.STRef.Strict
import qualified Data.Vector.Algorithms.Search as VA
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Database.LSMTree.Internal.Serialise

{- $compact

  A fence-pointer index is a mapping of disk pages, identified by some number
  @i@, to min-max information for keys on that page.

  Fence-pointer indexes can be constructed incrementally (e.g., using 'new',
  'append', 'unsafeEnd', and 'fromChunks') or in one go (e.g., using
  'fromList'). Regardless of the construction method however, some invariants
  must be upheld: see [construction invariants](#construction-invariants).

  Given a serialised target key @k@, an index can be 'search'ed to find a disk
  page @i@ that /might/ contain @k@. Fence-pointer indices offer no guarantee of
  whether the page contains the key, but the indices do guarentee that no page
  other than page @i@ could store @k@.

  === Intro

  A run is a file that stores a collection of key-value pairs. The data is
  sorted by key, and the data is divided into disk pages. A fence pointer index
  is a mapping of each disk page to min\/max information for keys on that page.
  As such, the index stores the keys that mark the /boundaries/ for each page. A
  lookup of a key in a run first searches the fence pointer index to find the
  page that contains the relevant key range, before doing a single I/O to
  retrieve the page. The guarantee of an index search is this:

  GUARANTEE: /if/ the key is somewhere in the run, then it can only be in the
  page that is returned by the index search.

  This module presents a /compact/ implementation of a fence pointer index.
  However, we first consider a simple implementation of a fence pointer index.
  After that, we show how we can save on the memory size of the index
  representation if the keys are uniformly distributed, like hashes. Let's start
  with some basic definitions:

  > type Run k v = -- elided
  > type Page k v = -- elided
  > minKey :: Page k v -> k
  > maxKey :: Page k v - k
  > type PageNo = Int

  The first implementation one may come up with is a vector containing the
  minimum key and maximum key on each page. As such, the index stores the
  key-interval @[minKey p, maxKey p]@ for each page @p@. @search1@ searches the
  vector for the key-interval that contains the search key, if it exists, and
  returns the corresponding vector index as a page number.

  > type Index1 k = V.Vector (k, k)
  >
  > mkIndex1 :: Run k v -> Index1 k
  > mkIndex1 = V.fromList . fmap (\p -> (minKey p, maxKey p))
  >
  > search1 :: k -> Index1 k -> Maybe pageNo
  > search1 = -- elided

  We can reduce the memory size of `Index1` by half if we store only the minimum
  keys on each page. As such, the index now stores the key-interval @[minKey p,
  minKey p')@ for each page @p@ and successor page @p'@. GUARANTEE is still
  guaranteed, because the old intervals are strictly contained in the new ones.
  The cost of this is that @search2@ could return a @Just@ even if @search1@
  would return a @Nothing@, but we prefer to lose some precision in order to
  achieve the memory size reduction. @search2@ searches the vector for the
  largest key smaller or equal to the given one, if it exists, and returns the
  corresponding vector index as a page number.

  > type Index2 k = V.Vector k
  >
  > mkIndex2 :: Run k v -> Index2 k
  > mkIndex2 = V.fromList . fmap minKey
  >
  > search2 :: k -> Index k -> pageNo
  > search2 = -- elided

  Now on to creating a more compact representation, which relies on a property
  of the keys that are used: the keys must be uniformly distributed values, like
  hashes.

  === Compact representation

  As mentioned before, we can save on the memory size of the index
  representation if the keys are uniformly distributed, like hashes. From now
  on, we will just assume that our keys are hashes, which can be viewed as
  strings of bits.

  The intuition behind the compact index is this: often, we don't need to look
  at full bit-strings to compare independently generated hashes against
  eachother. The probability of the @n@ most significant bits of two
  independently generated hashes matching is @(1/(2^n))@, and if we pick @n@
  large enough then we can expect a very small number of collisions. More
  generally, the expected number of @n@-bit hashes that have to be generated
  before a collision is observed is @2^(n/2)@, see [the birthday
  problem](https://en.wikipedia.org/wiki/Birthday_problem#Probability_of_a_shared_birthday_(collision).
  Or, phrased differently, if we store only the @n@ most significant bits of
  independently generated hashes in our index, then we can store up to @2^(n/2)@
  of thoses hashes before the expected number of collisions becomes one. Still,
  we need a way to break ties if there are collisions, because the probability
  of collisions is non-zero.

  ==== Clashes

  In our previous incarnations of the "simple" index, we relied on the minimum
  keys to define the boundaries between pages, or more precisely, the pages'
  key-intervals. If we look only at a subset of the most significant bits, that
  is no longer true in the presence of collisions. Let's illustrate this using
  an example of two contiguous pages @pi@ and @pj@:

  > minKey pi == "00000000"
  > maxKey pi == "10000000"
  > minKey pj == "10000001"
  > maxKey pj == "11111111"

  Say we store only the 4 most significant bits (left-to-right) in our compact
  index. This means that those 4 bits of @maxKey pi@ and @minKey pj@ collide.
  The problem is that the interval @[minKey pi, maxKey pi]@ strictly contains
  the interval @[minKey pi, minKey pj)@! The first 4 bits of @minKey pj@ do not
  actually define the boundary between @pi@ and @pj@, and so there would be no
  way to answer if a search key with first 4 bits "1000" could be found in @pi@
  or @pj@. We call such a situation a /clash/,

  The solution in these cases is to: (i) record that a clash occurred between
  pages @pi@ and @pj@, and (ii) store the full key @maxKey pi@ separately. The
  record of clashes can be implemented as simply as a single bit per page: with
  a @True@ bit meaning a clash with the previous page. Note therefore that the
  first page's bit is always going to be @False@. We store the full keys using
  a 'Map'. It is ok that this is not a compact representation, because we
  expect to store full keys for only a very small number of pages.

  The example below shows a simplified view of the compact index implementation
  so far. As an example, we store the @32@ most significant bits of each minimum
  key in the @primary@ index, the record of clashes is called @clashes@, and the
  @IntMap@ is named the @tieBreaker@ map. @search3@ can at any point during the
  search, consult @clashes@ and @tieBreaker@ to /break ties/.

  > --              (primary         , clashes      , tieBreaker)
  > type Index3 k = (VU.Vector Word32, VU.Vector Bit, IntMap k  )
  >
  > mkIndex3 :: Run k v -> Index3
  > mkIndex3 = -- elided
  >
  > search3 :: k -> Index3 -> pageNo
  > search3 = -- elided

  Let's compare the memory size of @Index2@ with @Index3@. Say we have \(n\)
  pages, and keys that are \(k\) bits each, then the memory size of @Index2@ is
  \(O(n~k)\) bits. Alternatively, for the same \(n\) pages, if we store only the
  \(c\) most significant bits, then the memory size of @Index3@ is
  \(O\left(n~c + n + k E\left[\text{collision}~n~c\right]\right)\) bits, where
  the last summand is the expected number of collisions for \(n\) independently
  generated hashes of bit-size \(c\). Precisely, the expected number of
  collisions is \(\frac{n^2 - n}{2 \times 2^c}\), so we can simplify the memory
  size to \(O\left(n~c + n + k \frac{n^2}{2^c}\right)\).

  So, @Index3@ is not strictly an improvement over @Index2@ if we look at memory
  complexity alone. However, /in practice/ @Index3@ is an improvement over
  @Index2@ if we can pick an \(c\) that is (i) much smaller than \(k\) and (ii)
  keeps \( \text{E}\left[\text{collision}~n~c\right] \) small (e.g. close to 1).
  For example, storing the first 48 bits of 2.5 million SHA256 hashes reduces
  memory size from \(256 n\) bits to \(48 n + n\) bits, because the expected
  number of collisions is smaller than 1.

  ==== Range-finder

  We extend this scheme by noting that the first few most significant bits of
  the keys are repeated many times in adjacent keys. For example, out of a
  sequence of sorted uniformly distributed keys, approximately a quarter will
  start with bits 00, a quarter with 01, et cetera. In principle these bits
  could be shared and not repeated.

  This is the same principle exploited by a (bitwise) trie. While a trie uses
  this idea recursively, we use a simple single level approach, with one top
  level \"range finder\" array. The idea is that we choose to share the first
  \(r\) bits in a dense array of size \(2^r\). Each index \(i\) in this array
  corresponds to a \(r\)-bit prefix of keys with value \(i\). Each entry
  specifies the interval of the primary index that contains keys that share the
  bit prefix \(i\). Thus we can take a search key, extract its first \(r\) bits
  and use this to index into the range finder array, yielding the interval of
  the primary index that we then need to search.

  This idea can thus reduce the range of the primary index that must be
  searched, improving search performance, but more interestingly some versions
  of this idea can extend the number of key prefix bits that can be stored, at
  the cost of a very modest increase in memory use.

  There are several plausible variation on this idea. The one we use is to
  ensure that entries in the primary array are perfectly partitioned in their
  first \(r\) bits, thus ensuring that the intervals mentioned above have no
  overlap (and no gaps). Ensuring this requires that the keys in each page all
  share the same \(r\)-bit key prefix. This must be guaranteed during page
  construction, and could come at the cost of some underfull pages. The
  advantage of this choice is that the range finder array can be represented
  with a single integer for each entry. Each one represents the offset in the
  primary index of the beginning of the span of key (prefixes) that share the
  same \(r\)-bit prefix. Pairs of entries thus give the interval in the primary
  index (as an inclusive lower bound and exclusive upper bound). Consequently,
  the array is of size \(2^r + 1\), with a final entry for the end of the last
  interval.

  Furthermore, this allows the primary index to omit those first \(r\) bits and
  instead store the 32 bits of the key after dropping the initial \(r\) bits.
  This allows representing several more bits, at a relatively low additional
  memory cost. For example, storing an additional 8 bits costs \(4 \times 2^8 =
  1024\) bytes. Of course this scales poorly so we do not allow more than 16
  bits, for a maximum memory cost of 256Kb. This representation therefore can
  store between 32 and 48 bit key prefixes, which is good for up to around 16M
  pages. At 40 entries per page this would be over 650M keys. To go any bigger
  one would need to move to 64bit entries in the primary index, which doubles
  the memory cost, but that would allow 64--80 bit key prefixes.

  Note that dropping the initial \(r\) bits from the entries in the primary
  index means that the index overall is not monotone, it is only monotone
  within each interval. This is ok of course because the binary search must
  always be performed within these intervals.

  Note also that it is not always better to use more range finder bits, since
  more bits means smaller sequences that share the same bit prefix. Shorter
  runs increases the relative wastage due to underfull pages. Using the
  number of bits needed to get the expected number of collisions to be 1 also
  happens to give sensible partition sizes -- as well as keeping the tie
  breaker map small -- so this is what we use in 'suggestRangeFinderPrecision'.

  === Representing clashes and larger-than-page entries

  A complicating factor is the need to represent larger-than-page entries in
  the compact index. This need arises from the fact that we can have single
  key/value pairs that need more than one disk page to represent.

  One strategy would be to read the first page, discover that it is a
  multi-page entry and then read the subsequent pages. This would however
  double the disk I\/O latency and complicate the I\/O handling logic (which is
  non-trivial due to it being asynchronous).

  The strategy we use is to have the index be able to return the range of pages
  that need to be read, and then all pages can be read in one batch. This means
  the index must return not just individual page numbers but intervals, and
  with a representation capable of doing so. This is not implausible since we
  have an entry in the primary index for every disk page, and so we have
  entries both for the first and subsequent pages of a larger-than-page entry.
  The natural thing to do is have each of these subsequent primary index
  entries contain the same key prefix value. This means a binary search will
  find the /last/ entry in a run of equal prefix values.

  What leads to complexity is that we will /also/ get runs of equal values if
  we have clashes between pages (as discussed above). So in the general case
  we may have a run of equal values made up of a mixture of clashes and
  larger-than-page entries.

  So the general situation is that after a binary search we have found the
  end of what may turn out to be a run of clashes and larger-than-page values
  and we must disambigutate and return the appropriate single page (for the
  ordinary case) or an interval of pages (for the LTP case).

  To disambigutate we make use of the clash bits, and we make the choice to
  say that /all/ the entries for a LTP have their clash bit set, irrespective
  of whether the LTP is in fact involved in a clash. This may seem
  counter-intuitive but it leads to a simpler mathematical definition (below).

  The search algorithm involves searching backwards in the clash bits to find
  the beginning of the run of entries that are involved. To establish which
  entry within the run is the right one to return, we can consult the tie
  breaker map by looking for the biggest entry that is less than or equal to
  the full search key. This may then point to an index within the run of
  clashing entries, in which case this is the right entry, but it may also
  point to an earlier and thus irrelevant entry, in which case the first entry
  in the run is the right one.

  Note that this second case also covers the case of a single non-clashing LTP.

  Finally, to determine if the selected entry is an LTP and if so what interval
  of pages to return, we make use of a second bit vector of LTP \"overflow\"
  pages. This bit vector has 1 values for LTP overflow pages (i.e. the 2nd and
  subsequent pages) and 0 otherwise. We can then simply search forwards to find
  the length of the LTP (or 1 if it is not an LTP).

  === A semi-formal description of the compact index #rep-descr#

  * \(n\) is the number of pages
  * \(ps = \{p_i \mid 0 \leq i < n \}\) is a sorted set of pages
  * \(p^{min}_i\) is the full minimum key on a page \(p_i \in ps\).
  * \(p^{max}_i\) is the full maximum key on a page \(p_i \in ps\).
  * \(r \in \left[0, 16\right] \) is the range-finder bit-precision
  * \(\texttt{topBits16}(r, k)\) extracts the \(r\) most significant bits from
    \(k\). We call these \(r\) bits the range-finder bits.
  * \(\texttt{sliceBits32}(m, k)\) extracts the 32 most significant bits from \(k\)
    /after/ \(r\). We call these 32 bits the primary bits.
  * \(c \in \left[32, 48\right] \) is the overall key prefix length represented
     by the compact index.
  * Since we always use 32 primary bits then \(c = r+32\).
  * We choose \(r\) such that \(c = 2~log_2~n\), which keeps the expected
    number of collisions low.
  * \(i \in \left[0, n \right)\), unless stated otherwise
  * \(j \in \left[0, 2^m\right)\), unless stated otherwise
  * Pages must be partitioned: \(\forall p_i \in ps. \texttt{topBits16}(m, p^{min}_i) ~\texttt{==}~ \texttt{topBits16}(m,p^{max}_i) \)

  \[
  \begin{align*}
    RF     :&~ \texttt{Array Word16 PageNo} \\
    RF[j]   =&~ \min~ \{ i \mid j \leq \texttt{topBits16}(m, p^{min}_i) \}  \\
    RF[2^m] =&~ n \\
    \\
    P    :&~ \texttt{Array PageNo Word32} \\
    P[i] =&~ \texttt{sliceBits32}(m, p^{min}_i) \\
    \\
    C    :&~ \texttt{Array PageNo Bit} \\
    C[0] =&~ \texttt{false} \\
    C[i] =&~ \texttt{sliceBits32}(m, p^{max}_{i-1}) ~\texttt{==}~ \texttt{sliceBits32}(m, p^{min}_i) \\
    \\
    TB            :&~ \texttt{Map Key PageNo} \\
    TB(p^{min}_i) =&~
      \begin{cases}
        p^{min}_i        &, \text{if}~ C[i] \land \neg LTP[i] \\
        \text{undefined} &, \text{otherwise} \\
      \end{cases} \\
    \\
    LTP    :&~ \texttt{Array PageNo Bit} \\
    LTP[0] =&~ \texttt{false} \\
    LTP[i] =&~ p^{min}_{i-1} ~\texttt{==}~ p^{min}_i \\
  \end{align*}
  \]

  === An informal description of the search algorithm #search-descr#

  The easiest way to think about the search algorithm is that we start with the
  full interval of page numbers, and shrink it until we get to an interval that
  contains only a single page (or in case of a larger-than-page value, multiple
  pages that have the same minimum key). Assume @k@ is our search key.

  * Use the range-finder bits of our @k@ to index into \(RF\), which should
    provide us with the bounds for a sub-vector of \(P\) to search. In
    particular, all pages in this sub-vector have minimum\/maximum keys that
    start with the same range-finder bits as @k@.
  * Search \(P\) for the vector index @i@ that maps to the largest prim-bits
    value that is smaller or equal to the primary bits of @k@.
  * Check \(C\) if the page corresponding to @i@ is part of a
    clash, if it is not, then we are done!
  * If there is a clash, we go into clash recovery mode. This means we have to
    resolve the ambiguous page boundary using \(TB\). Note, however, that if
    there are multiple clashes in a row, there could be multiple ambiguous page
    boundaries that have to be resolved. We can achieve this using a three-step
    process:

      * Search \(TB\) for the vector index @j1@ that maps to the largest full
        key that is smaller than or equal to @k@.
      * Do a linear search backwards through \(C\) starting from @i@ to find the
        first page @j2@ that is not part of a clash.
      * Take the maximum of @j1@ or @j2@. Consider the two cases where either
        @j1@ or @j2@ is largest (@j1@ can not be equal to @j2@):

          * @j1@ is largest: we resolved ambiguous page boundaries "to the left"
            (toward lower vector indexes) until we resolved an ambiguous page
            boundary "to the right" (toward the current vector index).
          * @j2@ is largest: we resolved ambiguous page boundaries only "to the
            left", and ended up on a page that doesn't have a clash.

  * For larger-than-page values, the steps up until now would only provide us
    with the page where the larger-than-page value starts. We use \(LTP\) to do
    a linear search to find the page where the larger-than-page value ends.

  Convince yourself that clash recovery works without any larger-than-page
  values, and then consider the case where the index does contain
  larger-than-page values. Hints:

  \[
    \begin{align*}
    LTP[i] &~ \implies C[i] \\
    LTP[i] &~ \implies TB(p^{min}_i) = \text{undefined} \\
    \end{align*}
  \]
-}

-- | A compact fence-pointer index for uniformly distributed keys.
--
-- Compact indexes save space by storing bit-prefixes of keys.
--
-- See [a semi-formal description of the compact index](#rep-descr) for more
-- details about the representation.
data CompactIndex = CompactIndex {
    -- | \(RF\): A vector that partitions 'ciPrimary' into sub-vectors containing elements
    -- that all share the same range-finder bits.
    ciRangeFinder          :: !(VU.Vector Word32)
    -- | \(m\): Determines the size of 'ciRangeFinder' as @2 ^ 'ciRangeFinderPrecision'
    -- + 1@.
  , ciRangeFinderPrecision :: !Int
    -- | \(P\): Maps a page @i@ to the 32-bit slice of primary bits of its minimum key.
  , ciPrimary              :: !(VU.Vector Word32)
    -- | \(C\): A clash on page @i@ means that the primary bits of the minimum key on
    -- that page aren't sufficient to decide whether a search for a key should
    -- continue left or right of the page.
  , ciClashes              :: !(VU.Vector Bit)
    -- | \(TB\): Maps a full minimum key to the page @i@ that contains it, but only if
    -- there is a clash on page @i@.
  , ciTieBreaker           :: !(Map SerialisedKey Int)
    -- | \(LTP\): Record of larger-than-page values. Given a span of pages for
    -- the larger-than-page value, the first page will map to 'False', and the
    -- remainder of the pages will be set to 'True'. Regular pages default to
    -- 'False'.
  , ciLargerThanPage       :: !(VU.Vector Bit)
  }

{-------------------------------------------------------------------------------
  Invariants
-------------------------------------------------------------------------------}

-- | Inclusive bounds for range-finder bit-precision.
rangeFinderPrecisionBounds :: (Int, Int)
rangeFinderPrecisionBounds = (0, 16)

-- | Given the number of expected pages in an index, suggest a range-finder
-- bit-precision.
--
-- https://en.wikipedia.org/wiki/Birthday_problem#Probability_of_a_shared_birthday_(collision)
suggestRangeFinderPrecision :: Int -> Int
suggestRangeFinderPrecision maxPages =
    -- The calculation (before clamping) gives us the /total/ number of bits we
    -- expect to use for prefixes
    clamp $ ceiling @Double $ 2 * logBase 2 (fromIntegral maxPages)
  where
    (lb, ub)              = rangeFinderPrecisionBounds
    -- Clamp ensures that we use only bits above @32@ as range finder bits. That
    -- is, we clamp @x@ to the range @[32 .. 48]@.
    clamp x | x < (lb+32) = lb
            | x > (ub+32) = ub
            | otherwise   = x - 32

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | A 0-based number identifying a disk page.
newtype PageNo = PageNo Int
  deriving stock (Show, Eq, Ord)
  deriving newtype Num

data SearchResult =
    NoResult
  | SinglePage PageNo
  -- | @'Multipage' s e@: the value is larger than a page, starts on page s, and
  -- ends on page e.
  | MultiPage PageNo PageNo
  deriving stock (Show, Eq)

-- | A span of pages, representing an inclusive interval of page numbers.
data PageSpan = PageSpan {
    pageSpanStart :: PageNo
  , pageSpanEnd   :: PageNo
  }

toPageSpan :: SearchResult -> Maybe PageSpan
toPageSpan NoResult        = Nothing
toPageSpan (SinglePage i)  = Just (PageSpan i i)
toPageSpan (MultiPage i j) = assert (i < j) $ Just (PageSpan i j)

-- | Given a search key, find the number of the disk page that /might/ contain
-- this key.
--
-- Note: there is no guarantee that the disk page contains the key. However, it
-- is guaranteed that /no other/ disk page contain the key.
--
-- See [an informal description of the search algorithm](#search-descr) for more
-- details about the search algorithm.
--
-- TODO: optimisation ideas: we should be able to get it all unboxed,
-- non-allocating and without unnecessary bounds checking.
--
-- One way to think of the search algorithm is that it starts with the full page
-- number interval, and shrinks it to a minimal interval that contains the
-- search key. The code below is annotated with @Pre:@ and @Post:@ comments that
-- describe the interval at that point.
search :: SerialisedKey -> CompactIndex -> SearchResult
search k CompactIndex{..} = -- Pre: @[0, V.length ciPrimary)@
    let !rfbits    = fromIntegral $ keyTopBits16 ciRangeFinderPrecision k
        !lb        = fromIntegral $ ciRangeFinder VU.! rfbits
        !ub        = fromIntegral $ ciRangeFinder VU.! (rfbits + 1)
        -- Post: @[lb, ub)@
        !primbits  = keySliceBits32 ciRangeFinderPrecision k
    in
      case unsafeSearchLEBounds primbits ciPrimary lb ub of
        Nothing -> NoResult -- Post: @[lb, lb)@ (empty).
        Just !i ->         -- Post: @[lb, i]@.
          if unBit $ ciClashes VU.! i then
            -- Post: @[lb, i]@, now in clash recovery mode.
            let i1  = fromJust $ bitIndexFromToRev (BoundInclusive lb) (BoundInclusive i) (Bit False) ciClashes
                i2  = maybe 0 snd $ Map.lookupLE k ciTieBreaker
                !i3 = max i1 i2 -- Post: the intersection of @[i1, i]@ and @[i2, i].
                !i4 = bitLongestPrefixFromTo (BoundExclusive i3) (BoundInclusive i) (Bit True) ciLargerThanPage
                      -- Post: [i3, i4]
            in  if i3 == i4 then SinglePage (PageNo i3) else MultiPage (PageNo i3) (PageNo i4)
                -- Post: @[i3, i4]@ if a larger-than-page value that starts at
                -- @i3@ and ends at @i4@, @[i3, i3]@ otherwise for a "normal"
                -- page.
          else  SinglePage (PageNo i) -- Post: @[i, i]@


countClashes :: CompactIndex -> Int
countClashes = Map.size . ciTieBreaker

hasClashes :: CompactIndex -> Bool
hasClashes = not . Map.null . ciTieBreaker

sizeInPages :: CompactIndex -> Int
sizeInPages = VU.length . ciPrimary

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

{- $construction-invariants #construction-invariants#

  Constructing a compact index can go wrong, unless the following conditions are
  met:

  * /Sorted/: pages must be appended in sorted order according to the keys they
    contain.

  * /Partitioned/: pages must be partitioned. That is, the range-finder bits for
    all keys within a page must match.
-}

-- | One-shot construction.
fromList :: Int -> Int -> [Append] -> CompactIndex
fromList rfprec maxcsize apps = runST $ do
    mci <- new rfprec maxcsize
    cs <- mapM (`append` mci) apps
    (c, fc) <- unsafeEnd mci
    pure (fromChunks (concat cs ++ toList c) fc)

-- | One-shot construction using only 'appendSingle'.
fromListSingles :: Int -> Int -> [(SerialisedKey, SerialisedKey)] -> CompactIndex
fromListSingles rfprec maxcsize apps = runST $ do
    mci <- new rfprec maxcsize
    cs <- mapM (`appendSingle` mci) apps
    (c, fc) <- unsafeEnd mci
    pure (fromChunks (catMaybes cs ++ toList c) fc)

{- $incremental #incremental#

  Incremental construction of a compact index yields chunks incrementally. These
  chunks can be converted to a 'CompactIndex' once incremental construction is
  finalised.

  Incremental construction is an 'ST' computation that can be started using
  'new', returning an 'MCompactIndex' structure that accumulates internal state.
  'append'ing new pages to the 'MCompactIndex' /might/ yield 'Chunk's.
  Incremental construction can be finalised with 'unsafeEnd', which yields both
  a 'Chunk' (possibly) and 'FinalChunk'. Use 'fromChunks' on the 'Chunk's and
  the 'FinalChunk' to construct a 'CompactIndex'.
-}

-- | A mutable version of 'CompactIndex'. See [incremental
-- construction](#incremental).
data MCompactIndex s = MCompactIndex {
    -- * Core index structure
    mciRangeFinder          :: !(VU.MVector s Word32)
  , mciRangeFinderPrecision :: !Int
    -- | Accumulates a chunk of 'ciPrimary'.
  , mciPrimary              :: !(STRef s (VU.MVector s Word32))
    -- | Accumulates a chunk of 'ciClashes'
  , mciClashes              :: !(STRef s (VU.MVector s Bit))
  , mciTieBreaker           :: !(STRef s (Map SerialisedKey Int))
    -- | Accumulates a chunk of 'ciLargerThanPage'
  , mciLargerThanPage       :: !(STRef s (VU.MVector s Bit))
    -- * Aux information required for incremental construction

    -- | Maximum size of a chunk
  , mciMaxChunkSize         :: !Int
    -- | The number of the current disk page we are constructing the index for.
  , mciCurrentPageNumber    :: !(STRef s Int)
    -- | The range-finder bits of the page-minimum key that we saw last.
    --
    --  This should be 'SNothing' if we haven't seen any keys/pages yet.
  , mciLastMinRfbits        :: !(STRef s (SMaybe Word16))
    -- | The primary bits of the page-maximum key that we saw last.
    --
    -- This should be 'SNothing' if we haven't seen any keys/pages yet.
  , mciLastMaxPrimbits      :: !(STRef s (SMaybe Word32))
    -- | The ful minimum key of the page that we saw last.
    --
    -- This should be 'SNothing' if we haven't seen any keys/pages yet.
  , mciLastMinKey           :: !(STRef s (SMaybe SerialisedKey))
  }

-- | @'new' rfprec maxcsize@ creates a new mutable index with a range-finder
-- bit-precision of @rfprec, and with a maximum chunk size of @maxcsize@.
--
-- PRECONDITION: maxcsize > 0
--
-- PRECONDITION: @rfprec@ should be within the bounds defined by
-- @rangeFinderPrecisionBounds@.
--
-- Note: after initialisation, both @rfprec@ and @maxcsize@ can no longer be changed.
new :: Int -> Int -> ST s (MCompactIndex s)
new rfprec maxcsize = MCompactIndex
    -- Core index structure
    <$> VUM.new (2 ^ rfprec + 1)
    <*> pure rfprec
    <*> (VUM.new maxcsize >>= newSTRef)
    <*> (VUM.new maxcsize >>= newSTRef)
    <*> newSTRef Map.empty
    <*> (VUM.new maxcsize >>= newSTRef)
    -- Aux information required for incremental construction
    <*> pure maxcsize
    <*> newSTRef 0
    <*> newSTRef SNothing
    <*> newSTRef SNothing
    <*> newSTRef SNothing

-- | Min\/max key-info for pages
data Append =
    -- | One or more keys are in this page, and their values fit within a single
    -- page.
    AppendSinglePage SerialisedKey SerialisedKey
    -- | There is only one key in this page, and it's value does not fit within
    -- a single page.
  | AppendMultiPage SerialisedKey Word32 -- ^ Number of overflow pages

-- | Append a new page entry to a mutable compact index.
--
-- INVARIANTS: see [construction invariants](#construction-invariants).
append :: Append -> MCompactIndex s -> ST s [Chunk]
append (AppendSinglePage kmin kmax) mci = toList <$> appendSingle (kmin, kmax) mci
append (AppendMultiPage k n) mci        = appendMulti (k, n) mci

-- | Append a single page to a mutable compact index.
--
-- INVARIANTS: see [construction invariants](#construction-invariants).
appendSingle :: forall s. (SerialisedKey, SerialisedKey) -> MCompactIndex s -> ST s (Maybe Chunk)
appendSingle (minKey, maxKey) mci@MCompactIndex{..} = do
    pageNr <- readSTRef mciCurrentPageNumber
    let ix = pageNr `mod` mciMaxChunkSize
    goAppend pageNr ix
    writeSTRef mciCurrentPageNumber $! pageNr + 1
    yield mci
  where
    minRfbits :: Word16
    minRfbits = keyTopBits16 mciRangeFinderPrecision minKey

    minPrimbits, maxPrimbits :: Word32
    minPrimbits = keySliceBits32 mciRangeFinderPrecision minKey
    maxPrimbits = keySliceBits32 mciRangeFinderPrecision maxKey

    -- | Meat of the function
    goAppend ::
         Int -- ^ Current /global/ page number
      -> Int -- ^ Current /local/ page number (inside the current chunk)
      -> ST s ()
    goAppend pageNr ix = do
        fillRangeFinder
        writePrimary
        writeClashesAndLTP
      where
        -- | Fill range-finder vector
        fillRangeFinder :: ST s ()
        fillRangeFinder = do
            lastMinRfbits <- readSTRef mciLastMinRfbits
            let lb = smaybe NoBound (\i -> Bound (fromIntegral i) Exclusive) lastMinRfbits
                ub = Bound (fromIntegral minRfbits) Inclusive
                x  = fromIntegral pageNr
            writeRange mciRangeFinder lb ub x
            writeSTRef mciLastMinRfbits $! SJust minRfbits

        -- | Set value in primary vector
        writePrimary :: ST s ()
        writePrimary =
            readSTRef mciPrimary >>= \vum -> VUM.write vum ix minPrimbits

        -- | Set value in clash vector, tie-breaker map and larger-than-page
        -- vector
        writeClashesAndLTP :: ST s ()
        writeClashesAndLTP = do
            lastMaxPrimbits <- readSTRef mciLastMaxPrimbits
            let clash = lastMaxPrimbits == SJust minPrimbits
            writeSTRef mciLastMaxPrimbits $! SJust maxPrimbits

            lastMinKey <- readSTRef mciLastMinKey
            let ltp = SJust minKey == lastMinKey
            writeSTRef mciLastMinKey $! SJust minKey

            readSTRef mciClashes >>= \vum -> VUM.write vum ix (Bit clash)
            readSTRef mciLargerThanPage >>= \vum -> VUM.write vum ix (Bit ltp)
            when (clash && not ltp) $ modifySTRef' mciTieBreaker (Map.insert minKey pageNr)

-- | Append multiple pages to the index. The minimum keys and maximum keys for
-- all these pages are set to the same key.
--
-- @appendMulti (k, n)@ is equivalent to @replicateM (n + 1) (appendSingle (k,
-- k))@, but the former should be faster faster.
--
-- INVARIANTS: see [construction invariants](#construction-invariants).
appendMulti :: forall s. (SerialisedKey, Word32) -> MCompactIndex s -> ST s [Chunk]
appendMulti (k, n0) mci@MCompactIndex{..} =
    maybe id (:) <$> appendSingle (k, k) mci <*> overflows (fromIntegral n0)
  where
    minPrimbits :: Word32
    minPrimbits = keySliceBits32 mciRangeFinderPrecision k

    -- | Fill primary, clash and LTP vectors for a larger-than-page value. Yields
    -- chunks if necessary
    overflows :: Int -> ST s [Chunk]
    overflows n
      | n <= 0 = pure []
      | otherwise = do
          pageNr <- readSTRef mciCurrentPageNumber
          let ix = pageNr `mod` mciMaxChunkSize -- will be 0 in recursive calls
              remInChunk = min n (mciMaxChunkSize - ix)
          readSTRef mciPrimary >>= \vum ->
            writeRange vum (BoundInclusive ix) (BoundExclusive $ ix + remInChunk) minPrimbits
          readSTRef mciClashes >>= \vum ->
            writeRange vum (BoundInclusive ix) (BoundExclusive $ ix + remInChunk) (Bit True)
          readSTRef mciLargerThanPage >>= \vum ->
            writeRange vum (BoundInclusive ix) (BoundExclusive $ ix + remInChunk) (Bit True)
          writeSTRef mciCurrentPageNumber $! pageNr + remInChunk
          res <- yield mci
          maybe id (:) res <$> overflows (n - remInChunk)

-- | Yield a chunk and start a new one if the current chunk is already full.
--
-- TODO(optimisation): yield will eagerly allocate new mutable vectors, but
-- maybe that should be done lazily.
--
-- INVARIANTS: see [construction invariants](#construction-invariants).
yield :: MCompactIndex s -> ST s (Maybe Chunk)
yield MCompactIndex{..} = do
    pageNr <- readSTRef mciCurrentPageNumber
    if pageNr `mod` mciMaxChunkSize == 0 then do -- The current chunk is full
      cPrimary <- VU.unsafeFreeze =<< readSTRef mciPrimary
      (writeSTRef mciPrimary $!) =<< VUM.new mciMaxChunkSize
      cClashes <- VU.unsafeFreeze =<< readSTRef mciClashes
      (writeSTRef mciClashes $!) =<< VUM.new mciMaxChunkSize
      cLargerThanPage <- VU.unsafeFreeze =<< readSTRef mciLargerThanPage
      (writeSTRef mciLargerThanPage $!) =<< VUM.new mciMaxChunkSize
      pure $ Just (Chunk{..})
    else -- the current chunk is not yet full
      pure Nothing

-- | Finalise incremental construction, yielding final chunks.
--
-- This function is unsafe, so do /not/ modify the 'MCompactIndex' after using
-- 'unsafeEnd'.
--
-- INVARIANTS: see [construction invariants](#construction-invariants).
unsafeEnd :: MCompactIndex s -> ST s (Maybe Chunk, FinalChunk)
unsafeEnd mci@MCompactIndex{..} = do
    pageNr <- readSTRef mciCurrentPageNumber
    let ix = pageNr `mod` mciMaxChunkSize

    -- Only slice out a chunk if there are entries in the chunk
    mchunk <- if ix == 0 then pure Nothing else do
      cPrimary        <- VU.unsafeFreeze . VUM.slice 0 ix =<< readSTRef mciPrimary
      cClashes        <- VU.unsafeFreeze . VUM.slice 0 ix =<< readSTRef mciClashes
      cLargerThanPage <- VU.unsafeFreeze . VUM.slice 0 ix =<< readSTRef mciLargerThanPage
      pure $ Just Chunk{..}

    -- We are not guaranteed to have seen all possible range-finder bit
    -- combinations, so we have to fill in the remainder of the rangerfinder
    -- vector.
    fillRangeFinderToEnd mci
    fcRangeFinder <- VU.unsafeFreeze mciRangeFinder
    let fcRangeFinderPrecision = mciRangeFinderPrecision
    fcTieBreaker <- readSTRef mciTieBreaker

    pure (mchunk, FinalChunk {..})

-- | Fill the remainder of the range-finder vector.
fillRangeFinderToEnd :: MCompactIndex s -> ST s ()
fillRangeFinderToEnd MCompactIndex{..} = do
    pageNr <- readSTRef mciCurrentPageNumber
    lastMinRfbits <- readSTRef mciLastMinRfbits
    let lb = smaybe NoBound (BoundExclusive . fromIntegral) lastMinRfbits
        ub = NoBound
        x  = fromIntegral pageNr
    writeRange mciRangeFinder lb ub x
    writeSTRef mciLastMinRfbits $! SJust $ 2 ^ mciRangeFinderPrecision

data Chunk = Chunk {
    cPrimary        :: !(VU.Vector Word32)
  , cClashes        :: !(VU.Vector Bit)
  , cLargerThanPage :: !(VU.Vector Bit)
  }
  deriving stock (Show, Eq)

data FinalChunk = FinalChunk {
    fcRangeFinder          :: !(VU.Vector Word32)
  , fcRangeFinderPrecision :: !Int
  , fcTieBreaker           :: !(Map SerialisedKey Int)
  }
  deriving stock (Show, Eq)

-- | Feed in 'Chunk's in the same order that they were yielded from incremental
-- construction.
fromChunks :: [Chunk] -> FinalChunk -> CompactIndex
fromChunks cs FinalChunk{..} = CompactIndex {
      ciRangeFinder          = fcRangeFinder
    , ciRangeFinderPrecision = fcRangeFinderPrecision
    , ciPrimary              = VU.concat $ fmap cPrimary cs
    , ciClashes              = VU.concat $ fmap cClashes cs
    , ciTieBreaker           = fcTieBreaker
    , ciLargerThanPage       = VU.concat $ fmap cLargerThanPage cs
    }

{-------------------------------------------------------------------------------
  Strict 'Maybe'
-------------------------------------------------------------------------------}

data SMaybe a = SNothing | SJust !a
  deriving stock Eq

smaybe :: b -> (a -> b) -> SMaybe a -> b
smaybe snothing sjust = \case
    SNothing -> snothing
    SJust x  -> sjust x

{-------------------------------------------------------------------------------
 Vector extras
-------------------------------------------------------------------------------}

writeRange :: VU.Unbox a => VU.MVector s a -> Bound Int -> Bound Int -> a -> ST s ()
writeRange v lb ub x = forM_ [lb' .. ub'] $ \j -> VUM.write v j x
  where
    lb' = vectorLowerBound lb
    ub' = mvectorUpperBound v ub

-- | Find the largest element that is smaller or equal to to the given one
-- within the vector interval @[lb, ub)@, and return its vector index.
--
-- Note: this function uses 'unsafeThaw', so all considerations for using
-- 'unsafeThaw' apply to using 'unsafeSearchLEBounds' too.
--
-- PRECONDITION: the vector is sorted in ascending order within the interval
-- @[lb, ub)@.
unsafeSearchLEBounds ::
     (VG.Vector v e, Ord e)
  => e -> v e -> Int -> Int -> Maybe Int -- TODO: return -1?
unsafeSearchLEBounds e vec lb ub = runST $ do
    -- Vector search algorithms work on mutable vectors only.
    vec' <- VG.unsafeThaw vec
    -- @i@ is the first index where @e@ is strictly smaller than the element at
    -- @i@.
    i <- VA.gallopingSearchLeftPBounds (> e) vec' lb ub
    -- The last (and therefore largest) element that is lesser-equal @e@ is
    -- @i-1@. However, if @i==lb@, then the interval @[lb, ub)@ doesn't contain
    -- any elements that are lesser-equal @e@.
    pure $ if i == lb then Nothing else Just (i - 1)

-- | Return the index of the last bit in the vector with the specified value, if
-- any.
--
-- TODO(optimise): optimise by implementing this function similarly to how
-- 'bitIndex' is implemented internally. Another alternative I tried is using
-- the @vector-rotvec@ package and 'V.elemIndex', but 'V.elemIndex' is up to 64x
-- slower than bitIndex.
bitIndexFromToRev :: Bound Int -> Bound Int -> Bit -> VU.Vector Bit -> Maybe Int
bitIndexFromToRev lb ub b v = reverseIx <$> bitIndex b (VU.reverse $ VU.slice lb' (ub' - lb' + 1) v)
  where
    reverseIx x = ub' - x
    lb' = vectorLowerBound lb
    ub' = vectorUpperBound v ub

-- | Like 'bitIndex', but only searches the vector within the give lower and
-- upper bound. Returns the index into the original vector, not the slice.
bitIndexFromTo :: Bound Int -> Bound Int -> Bit -> VU.Vector Bit -> Maybe Int
bitIndexFromTo lb ub b v = shiftIx <$> bitIndex b (VU.slice lb' (ub' - lb' + 1) v)
  where
    shiftIx = (lb'+)
    lb' = vectorLowerBound lb
    ub' = vectorUpperBound v ub

-- | Find the longest prefix of the vector that consists of only bits matching
-- the given value. The return value is the index of the last bit in the prefix.
bitLongestPrefixFromTo :: Bound Int -> Bound Int -> Bit -> Vector Bit -> Int
bitLongestPrefixFromTo lb ub b v = maybe ub' (subtract 1) $ bitIndexFromTo lb ub (toggle b) v
  where
    toggle (Bit x) = Bit (not x)
    ub' = vectorUpperBound v ub

vectorLowerBound :: Bound Int -> Int
vectorLowerBound = \case
    NoBound          -> 0
    BoundExclusive i -> i + 1
    BoundInclusive i -> i

mvectorUpperBound :: VGM.MVector v a => v s a -> Bound Int -> Int
mvectorUpperBound v = \case
    NoBound          -> VGM.length v - 1
    BoundExclusive i -> i - 1
    BoundInclusive i -> i

vectorUpperBound :: VG.Vector v a => v a -> Bound Int -> Int
vectorUpperBound v = \case
    NoBound          -> VG.length v - 1
    BoundExclusive i -> i - 1
    BoundInclusive i -> i
