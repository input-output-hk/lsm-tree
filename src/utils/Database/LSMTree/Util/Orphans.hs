{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Database.LSMTree.Util.Orphans () where

import           Control.DeepSeq (NFData (..))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Primitive.ByteArray (ByteArray (..))
import           Data.WideWord.Word256 (Word256 (..))
import           Data.Word (Word64)
import           Database.LSMTree.Internal.Run.BloomFilter (Hashable (..))
import           Database.LSMTree.Internal.Run.Index.Compact (Append (..),
                     CompactIndex (..), SearchResult (..))
import           Database.LSMTree.Internal.Serialise (Serialise (..),
                     SerialisedKey (..))
import           GHC.Generics (Generic)
import           System.Random (Uniform)

deriving instance Generic SerialisedKey
deriving instance NFData SerialisedKey

deriving instance Generic SearchResult
deriving instance NFData SearchResult

deriving instance Generic Append
deriving instance NFData Append

deriving instance Generic CompactIndex
deriving instance NFData CompactIndex

{-------------------------------------------------------------------------------
  Word256
-------------------------------------------------------------------------------}

deriving anyclass instance Uniform Word256

instance Hashable Word256 where
  hashIO32 (Word256 a b c d) = hashIO32 (a, b, c, d)

instance Serialise Word256 where
  serialise (Word256{word256hi, word256m1, word256m0, word256lo}) =
      fromByteString $ B.toLazyByteString $ mconcat [
          B.word64BE word256hi
        , B.word64BE word256m1
        , B.word64BE word256m0
        , B.word64BE word256lo
        ]
    where
      fromByteString :: LBS.ByteString -> SerialisedKey
      fromByteString =
            (\(SBS.SBS ba) -> SerialisedKey (ByteArray ba))
          . SBS.toShort
          . LBS.toStrict

{-------------------------------------------------------------------------------
  Word64
-------------------------------------------------------------------------------}

instance Serialise Word64 where
  serialise x =
      fromByteString $ B.toLazyByteString $ B.word64BE x
    where
      fromByteString :: LBS.ByteString -> SerialisedKey
      fromByteString =
            (\(SBS.SBS ba) -> SerialisedKey (ByteArray ba))
          . SBS.toShort
          . LBS.toStrict
