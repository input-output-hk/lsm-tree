{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
{-# OPTIONS_GHC -Wno-orphans #-}

module Database.LSMTree.Internal (
    -- * Exceptions
    LSMTreeError (..)
    -- * Session
  , Session (..)
  , SessionState (..)
  , SessionEnv (..)
    -- ** Implementation of public API
  , withSession
  , openSession
  , closeSession
    -- * Table handle
  , TableHandle (..)
  , TableHandleState (..)
  , TableHandleEnv (..)
  , TableContent (..)
  , Level (..)
  , withOpenTable
    -- ** Implementation of public API
  , withTable
  , new
  , close
  , lookups
  , toNormalLookupResult
  , updates
    -- * Snapshots
  , SnapshotLabel
  , snapshot
  , open
  , deleteSnapshot
  , listSnapshots
    -- * configuration
  , TableConfig (..)
  , resolveMupsert
  , ResolveMupsert (..)
  , SizeRatio (..)
  , MergePolicy (..)
  , WriteBufferAlloc (..)
  , NumEntries (..)
  , BloomFilterAlloc (..)
  ) where

import           Control.Concurrent.Class.MonadMVar.Strict
import           Control.Concurrent.ReadWriteVar (RWVar)
import qualified Control.Concurrent.ReadWriteVar as RW
import           Control.Monad (unless, void, when)
import           Control.Monad.Class.MonadThrow
import           Data.Bifunctor (Bifunctor (..))
import           Data.BloomFilter (Bloom)
import qualified Data.ByteString.Char8 as BSC
import           Data.Either (fromRight)
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Vector as V
import           Data.Word (Word32, Word64)
import           Database.LSMTree.Internal.Assertions (assert, assertNoThunks)
import           Database.LSMTree.Internal.BlobRef
import           Database.LSMTree.Internal.Entry (Entry (..), NumEntries (..),
                     combineMaybe)
import           Database.LSMTree.Internal.IndexCompact (IndexCompact)
import           Database.LSMTree.Internal.Lookup (lookupsIO)
import           Database.LSMTree.Internal.Managed
import qualified Database.LSMTree.Internal.Normal as Normal
import           Database.LSMTree.Internal.Paths (RunFsPaths (..),
                     SessionRoot (..), SnapshotName)
import qualified Database.LSMTree.Internal.Paths as Paths
import           Database.LSMTree.Internal.Run (Run)
import qualified Database.LSMTree.Internal.Run as Run
import           Database.LSMTree.Internal.Serialise (SerialisedBlob,
                     SerialisedKey, SerialisedValue)
import           Database.LSMTree.Internal.TempRegistry
import qualified Database.LSMTree.Internal.Vector as V
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer)
import qualified Database.LSMTree.Internal.WriteBuffer as WB
import qualified GHC.IO.Handle as GHC
import           NoThunks.Class
import qualified System.FS.API as FS
import           System.FS.API (FsErrorPath, FsPath, Handle, HasFS)
import qualified System.FS.API.Lazy as FS
import qualified System.FS.API.Strict as FS
import qualified System.FS.BlockIO.API as HasBlockIO
import           System.FS.BlockIO.API (HasBlockIO)
import qualified System.IO as System

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- TODO: give this a nicer Show instance.
data LSMTreeError =
    SessionDirDoesNotExist FsErrorPath
  | SessionDirLocked FsErrorPath
  | SessionDirMalformed FsErrorPath
    -- | All operations on a closed session or tables within a closed session
    -- will throw this exception. There is one exception (pun intended) to this
    -- rule: the idempotent operation 'Database.LSMTree.Common.closeSession'.
  | ErrSessionClosed
    -- | All operations on a closed table will throw this exception. There is
    -- one exception (pun intended) to this rule: the idempotent operation
    -- 'Database.LSMTree.Common.close'.
  | ErrTableClosed
  | ErrExpectedNormalTable
  | ErrSnapshotExists SnapshotName
  | ErrSnapshotNotExists SnapshotName
  | ErrSnapshotWrongType SnapshotName
  deriving (Show, Exception)

{-------------------------------------------------------------------------------
  Session
-------------------------------------------------------------------------------}

-- | A session provides context that is shared across multiple table handles.
--
-- For more information, see 'Database.LSMTree.Common.Session'.
newtype Session m h = Session {
      -- | The primary purpose of this 'RWVar' is to ensure consistent views of
      -- the open-/closedness of a session when multiple threads require access
      -- to the session's fields (see 'withOpenSession'). We use more
      -- fine-grained synchronisation for various mutable parts of an open
      -- session.
      --
      -- TODO: RWVars only work in IO, not in IOSim.
      --
      -- TODO: how fair is an RWVar?
      sessionState :: RWVar (SessionState m h)
    }

data SessionState m h =
    SessionOpen !(SessionEnv m h)
  | SessionClosed

data SessionEnv m h = SessionEnv {
    -- | The path to the directory in which this sesion is live. This is a path
    -- relative to root of the 'HasFS' instance.
    sessionRoot        :: !SessionRoot
  , sessionHasFS       :: !(HasFS m h)
  , sessionHasBlockIO  :: !(HasBlockIO m h)
    -- TODO: add file locking to HasFS, because this one only works in IO.
  , sessionLockFile    :: !GHC.Handle
    -- | A session-wide shared, atomic counter that is used to produce unique
    -- names, for example: run names, table IDs.
  , sessionUniqCounter :: !(UniqCounter m)
    -- | Open tables are tracked here so they can be closed once the session is
    -- closed. Tables also become untracked when they are closed manually.
    --
    -- NOTE: table are assigned unique identifiers using 'sessionUniqCounter' to
    -- ensure that modifications to the set of known tables are independent.
    -- Each identifier is added only once in 'new', and is deleted only once in
    -- 'close'. A table only make modifications for their own identifier. This
    -- means that modifications can be made concurrently in any order without
    -- further restrictions.
  , sessionOpenTables  :: !(StrictMVar m (Map Word64 (TableHandle m h)))
  }

-- | 'withOpenSession' ensures that the session stays open for the duration of the
-- provided continuation.
--
-- NOTE: any operation except 'sessionClose' can use this function.
{-# INLINE withOpenSession #-}
withOpenSession ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> (SessionEnv m h -> m a)
  -> m a
withOpenSession sesh action = RW.with (sessionState sesh) $ \case
    SessionClosed -> throwIO ErrSessionClosed
    SessionOpen seshEnv -> action seshEnv

-- | An atomic counter for producing unique 'Word64' values.
newtype UniqCounter m = UniqCounter (StrictMVar m Word64)

{-# INLINE newUniqCounter #-}
newUniqCounter :: MonadMVar m => Word64 -> m (UniqCounter m)
newUniqCounter x = UniqCounter <$> newMVar x

{-# INLINE incrUniqCounter #-}
-- | Return the current state of the atomic counter, and then increment the
-- counter.
incrUniqCounter :: MonadMVar m => UniqCounter m -> m Word64
incrUniqCounter (UniqCounter uniqVar) = modifyMVar uniqVar (\x -> pure ((x+1), x))

--
-- Implementation of public API
--

{-# SPECIALISE withSession :: HasFS IO h -> HasBlockIO IO h -> FsPath -> (Session IO h -> IO a) -> IO a #-}
-- | See 'Database.LSMTree.Common.withSession'.
withSession ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => HasFS m h
  -> HasBlockIO m h
  -> FsPath
  -> (Session m h -> m a)
  -> m a
withSession hfs hbio dir = bracket (openSession hfs hbio dir) closeSession

{-# SPECIALISE openSession :: HasFS IO h -> HasBlockIO IO h -> FsPath -> IO (Session IO h) #-}
-- | See 'Database.LSMTree.Common.openSession'.
openSession ::
     forall m h. m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => HasFS m h
  -> HasBlockIO m h -- TODO: could we prevent the user from having to pass this in?
  -> FsPath -- ^ Path to the session directory
  -> m (Session m h)
openSession hfs hbio dir = do
    dirExists <- FS.doesDirectoryExist hfs dir
    unless dirExists $
      throwIO (SessionDirDoesNotExist (FS.mkFsErrorPath hfs dir))
    -- List directory contents /before/ trying to acquire a file lock, so that
    -- that the lock file does not show up in the listed contents.
    dirContents <- FS.listDirectory hfs dir
    -- TODO: unsafeToFilePath will error if not in IO. Once we enable running
    -- this code in IOSim, we should also make sure we have added file locking
    -- to the HasFS API.
    lf <- FS.unsafeToFilePath hfs lockFilePath
    -- Try to acquire the session file lock as soon as possible to reduce the
    -- risk of race conditions.
    --
    -- The lock is only released when an exception is raised, otherwise the lock
    -- is included in the returned Session.
    bracketOnError
      (acquireLock lf)
      (traverse_ System.hClose) -- also releases the lock
      $ \case
          Nothing -> throwIO (SessionDirLocked (FS.mkFsErrorPath hfs lockFilePath))
          Just sessionFileLock ->
            if Set.null dirContents then newSession sessionFileLock
                                    else restoreSession sessionFileLock
  where
    root             = Paths.SessionRoot dir
    lockFilePath     = Paths.lockFile root
    activeDirPath    = Paths.activeDir root
    snapshotsDirPath = Paths.snapshotsDir root

    acquireLock path = fromRight Nothing <$>
        try @m @SomeException
          (do lockFile <- System.openFile path System.WriteMode
              success <- GHC.hTryLock lockFile GHC.ExclusiveLock
              pure $ if success then Just lockFile else Nothing
          )

    mkSession lockFile x = do
        counterVar <- newUniqCounter x
        openTablesVar <- newMVar Map.empty
        sessionVar <- RW.new $ SessionOpen $ SessionEnv {
            sessionRoot = root
          , sessionHasFS = hfs
          , sessionHasBlockIO = hbio
          , sessionLockFile = lockFile
          , sessionUniqCounter = counterVar
          , sessionOpenTables = openTablesVar
          }
        pure $! Session sessionVar

    newSession sessionFileLock = do
        FS.createDirectory hfs activeDirPath
        FS.createDirectory hfs snapshotsDirPath
        mkSession sessionFileLock 0

    restoreSession sessionFileLock = do
        -- If the layouts are wrong, we throw an exception, and the lock file
        -- is automatically released by bracketOnError.
        checkTopLevelDirLayout
        checkActiveDirLayout
        checkSnapshotsDirLayout
        -- TODO: remove once we have proper snapshotting. Before that, we must
        -- prevent name clashes with runs that are still present in the active
        -- directory by starting the unique counter at a strictly higher number
        -- than the name of any run in the active directory.
        --
        -- This is a crude number to start the counter at, but it should do the
        -- trick until we have proper snapshots.
        x <- Set.size <$> FS.listDirectory hfs activeDirPath
        mkSession sessionFileLock (fromIntegral x)

    -- Check that the active directory and snapshots directory exist. We assume
    -- the lock file already exists at this point.
    --
    -- This does /not/ check that /only/ the expected files and directories
    -- exist. This means that unexpected files in the top-level directory are
    -- ignored for the layout check.
    checkTopLevelDirLayout = do
      FS.doesDirectoryExist hfs activeDirPath >>= \b ->
        unless b $ throwIO (SessionDirMalformed (FS.mkFsErrorPath hfs activeDirPath))
      FS.doesDirectoryExist hfs snapshotsDirPath >>= \b ->
        unless b $ throwIO (SessionDirMalformed (FS.mkFsErrorPath hfs snapshotsDirPath))

    -- Nothing to check: runs are verified when loading a table handle, not when
    -- a session is restored.
    --
    -- TODO: when we implement proper snapshotting, the files in the active
    -- directory should be ignored and cleaned up.
    checkActiveDirLayout = pure ()

    -- Nothing to check: snapshots are verified when they are loaded, not when a
    -- session is restored.
    checkSnapshotsDirLayout = pure ()

{-# SPECIALISE closeSession :: Session IO h -> IO () #-}
-- | See 'Database.LSMTree.Common.closeSession'.
--
-- A session's global resources will only be released once it is sure that no
-- tables are open anymore.
closeSession ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> m ()
closeSession Session{sessionState} =
    RW.modify_ sessionState $ \case
      SessionClosed -> pure SessionClosed
      SessionOpen seshEnv -> do
        -- Since we have a write lock on the session state, we know that no
        -- tables will be added while we are closing the session, and that we
        -- are the only thread currently closing the session.
        --
        -- We technically don't have to overwrite this with an empty Map, but
        -- why not
        tables <- modifyMVar (sessionOpenTables seshEnv) (\m -> pure (Map.empty, m))
        -- Close tables first, so that we know none are open when we release
        -- session-wide resources.
        --
        -- If any table from this set has been closed already by a different
        -- thread, the idempotent 'close' will act like a no-op, and so we are
        -- not in trouble.
        mapM_ close tables
        HasBlockIO.close (sessionHasBlockIO seshEnv)
        System.hClose (sessionLockFile seshEnv)
        pure SessionClosed

{-------------------------------------------------------------------------------
  Table handle
-------------------------------------------------------------------------------}

-- | A handle to an on-disk key\/value table.
--
-- For more information, see 'Database.LSMTree.Normal.TableHandle'.
data TableHandle m h = TableHandle {
      tableConfig      :: !TableConfig
      -- | The primary purpose of this 'RWVar' is to ensure consistent views of
      -- the open-/closedness of a table when multiple threads require access to
      -- the table's fields (see 'withOpenTable'). We use more fine-grained
      -- synchronisation for various mutable parts of an open table.
      --
      -- TODO: RWVars only work in IO, not in IOSim.
      --
      -- TODO: how fair is an RWVar?
    , tableHandleState :: !(RWVar (TableHandleState m h))
    }

-- | A table handle may assume that its corresponding session is still open as
-- long as the table handle is open. A session's global resources, and therefore
-- resources that are inherited by the table, will only be released once the
-- session is sure that no tables are open anymore.
data TableHandleState m h =
    TableHandleOpen !(TableHandleEnv m h)
  | TableHandleClosed

data TableHandleEnv m h = TableHandleEnv {
    -- === Session
    -- | Inherited from session
    tableSessionRoot         :: !SessionRoot
    -- | Inherited from session
  , tableHasFS               :: !(HasFS m h)
    -- | Inherited from session
  , tableHasBlockIO          :: !(HasBlockIO m h)
    -- | Inherited from session
  , tablesSessionUniqCounter :: !(UniqCounter m)
    -- | Open tables are tracked in the corresponding session, so when a table
    -- is closed it should become untracked (forgotten).
  , tableSessionUntrackTable :: !(m ())
    -- === Table-specific
    -- | All of the state being in a single `StrictMVar` is a relatively simple
    -- solution, but there could be more concurrency. For example, while inserts
    -- are in progress, lookups could still look at the old state without
    -- waiting for the MVar.
    -- TODO: switch to more fine-grained synchronisation approach
  , tableContent             :: !(StrictMVar m (TableContent h))
  }

data TableContent h = TableContent {
    tableWriteBuffer :: !WriteBuffer
    -- | A hierarchy of levels. The vector indexes double as level numbers.
  , tableLevels      :: !(Levels (Handle h))
    -- | Cache of flattened 'levels'.
    --
    -- INVARIANT: when 'level's is modified, this cache should be updated as
    -- well, for example using 'mkLevelsCache'.
  , tableCache       :: !(LevelsCache (Handle h))
  }

-- | 'withOpenTable' ensures that the table stays open for the duration of the
-- provided continuation.
--
-- NOTE: any operation except 'close' can use this function.
{-# INLINE withOpenTable #-}
withOpenTable ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => TableHandle m h
  -> (TableHandleEnv m h -> m a)
  -> m a
withOpenTable th action = RW.with (tableHandleState th) $ \case
    TableHandleClosed -> throwIO ErrTableClosed
    TableHandleOpen thEnv -> action thEnv

type Levels h = V.Vector (Level h)

-- | Runs in order from newer to older
data Level h = Level {
    incomingRuns :: !(MergingRun h)
  , residentRuns :: !(V.Vector (Run h))
  }

-- TODO: proper instance
deriving via OnlyCheckWhnfNamed "Level" (Level h) instance NoThunks (Level h)

-- | Merges are stepped to completion immediately, so there is no representation
-- for ongoing merges (yet)
--
-- TODO: this should also represent ongoing merges once we implement scheduling.
newtype MergingRun h = SingleRun (Run h)

{-# SPECIALISE closeLevels :: HasFS IO h -> Levels (Handle h) -> IO () #-}
closeLevels ::
     m ~ IO
  => HasFS m h
  -> Levels (Handle h)
  -> m ()
closeLevels hfs levels = V.mapM_ (closeLevel hfs) levels

{-# SPECIALISE closeLevel :: HasFS IO h -> Level (Handle h) -> IO () #-}
closeLevel ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => HasFS m h
  -> Level (Handle h)
  -> m ()
closeLevel hfs (Level mr rs) = do
  case mr of SingleRun r -> Run.removeReference hfs r
  V.mapM_ (Run.removeReference hfs) rs

-- | Flattend cache of the runs that referenced by a table handle.
--
-- This cache includes a vector of runs, but also vectors of the runs broken
-- down into components, like bloom filters, fence pointer indexes and file
-- handles. This allows for quick access in the lookup code. Recomputing this
-- cache should be relatively rare.
--
-- Use 'mkLevelsCache' to ensure that there are no mismatches between the vector
-- of runs and the vectors of run components.
data LevelsCache h = LevelsCache_ {
    cachedRuns      :: !(V.Vector (Run h))
  , cachedFilters   :: !(V.Vector (Bloom SerialisedKey))
  , cachedIndexes   :: !(V.Vector IndexCompact)
  , cachedKOpsFiles :: !(V.Vector h)
  }

-- | Flatten the argument 'Level's into a single vector of runs, and use that to
-- populate the 'LevelsCache'.
mkLevelsCache :: Levels h -> LevelsCache h
mkLevelsCache lvls = LevelsCache_ {
      cachedRuns      = rs
    , cachedFilters   = V.map Run.runFilter rs
    , cachedIndexes   = V.map Run.runIndex rs
    , cachedKOpsFiles = V.map Run.runKOpsFile rs
    }
  where rs = V.concatMap (\(Level (SingleRun r) rs') -> V.cons r rs') lvls

--
-- Implementation of public API
--

{-# SPECIALISE withTable :: Session IO h -> TableConfig -> (TableHandle IO h -> IO a) -> IO a #-}
-- | See 'Database.LSMTree.Normal.withTable'.
withTable ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> TableConfig
  -> (TableHandle m h -> m a)
  -> m a
withTable sesh conf = bracket (new sesh conf) close

{-# SPECIALISE new :: Session IO h -> TableConfig -> IO (TableHandle IO h) #-}
-- | See 'Database.LSMTree.Normal.new'.
new ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> TableConfig
  -> m (TableHandle m h)
new sesh conf = withOpenSession sesh $ \seshEnv -> newWithLevels seshEnv conf V.empty

{-# SPECIALISE newWithLevels :: SessionEnv IO h -> TableConfig -> Levels (Handle h) -> IO (TableHandle IO h) #-}
newWithLevels ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => SessionEnv m h
  -> TableConfig
  -> Levels (Handle h)
  -> m (TableHandle m h)
newWithLevels seshEnv conf !levels = do
    assertNoThunks levels $ pure ()
    -- The session is kept open until we've updated the session's set of tracked
    -- tables. If 'closeSession' is called by another thread while this code
    -- block is being executed, that thread will block until it reads the
    -- /updated/ set of tracked tables.
    contentVar <- newMVar $ TableContent WB.empty levels (mkLevelsCache levels)
    tableId <- incrUniqCounter (sessionUniqCounter seshEnv)
    -- Action to untrack the current table
    let forget = modifyMVar_ (sessionOpenTables seshEnv) $ pure . Map.delete tableId
    tableVar <- RW.new $ TableHandleOpen $ TableHandleEnv {
          tableSessionRoot = sessionRoot seshEnv
        , tableHasFS = sessionHasFS seshEnv
        , tableHasBlockIO = sessionHasBlockIO seshEnv
        , tablesSessionUniqCounter = sessionUniqCounter seshEnv
        , tableSessionUntrackTable = forget
        , tableContent = contentVar
        }
    let !th = TableHandle conf tableVar
    -- Track the current table
    modifyMVar_ (sessionOpenTables seshEnv) $ pure . Map.insert tableId th
    pure $! th

{-# SPECIALISE close :: TableHandle IO h -> IO () #-}
-- | See 'Database.LSMTree.Normal.close'.
close ::
     m ~ IO  -- TODO: replace by @io-classes@ constraints for IO simulation.
  => TableHandle m h
  -> m ()
close th = RW.modify_ (tableHandleState th) $ \case
    TableHandleClosed -> pure TableHandleClosed
    TableHandleOpen thEnv -> do
      -- Since we have a write lock on the table state, we know that we are the
      -- only thread currently closing the table. We can safely make the session
      -- forget about this table.
      tableSessionUntrackTable thEnv
      lvls <- tableLevels <$> readMVar (tableContent thEnv)
      closeLevels (tableHasFS thEnv) lvls
      pure TableHandleClosed

{-# SPECIALISE lookups :: V.Vector SerialisedKey -> TableHandle IO h -> (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h)))) -> lookupResult) -> IO (V.Vector lookupResult) #-}
-- | See 'Database.LSMTree.Normal.lookups'.
lookups ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => V.Vector SerialisedKey
  -> TableHandle m h
  -> (Maybe (Entry SerialisedValue (BlobRef (Run (Handle h)))) -> lookupResult)
     -- ^ How to map from an entry to a lookup result. Use
     -- 'toNormalLookupResult' or 'toMonoidalLookupResult'.
  -> m (V.Vector lookupResult)
lookups ks th fromEntry = withOpenTable th $ \thEnv -> do
    let resolve = resolveMupsert (tableConfig th)
    tableContent <- readMVar (tableContent thEnv)
    let !wb = tableWriteBuffer tableContent
    let !cache = tableCache tableContent
    ioRes <-
      lookupsIO
        (tableHasBlockIO thEnv)
        resolve
        (cachedRuns cache)
        (cachedFilters cache)
        (cachedIndexes cache)
        (cachedKOpsFiles cache)
        ks
    pure $! V.zipWithStrict
              (\k1 e2 -> fromEntry $ combineMaybe resolve (WB.lookup wb k1) e2)
              ks ioRes

toNormalLookupResult :: Maybe (Entry v b) -> Normal.LookupResult v b
toNormalLookupResult = \case
    Just e -> case e of
      Insert v            -> Normal.Found v
      InsertWithBlob v br -> Normal.FoundWithBlob v br
      Mupdate _           -> error "toNormalLookupResult: Mupdate unexpected"
      Delete              -> Normal.NotFound
    Nothing -> Normal.NotFound

{-# SPECIALISE updates :: V.Vector (SerialisedKey, Entry SerialisedValue SerialisedBlob) -> TableHandle IO h -> IO () #-}
-- | See 'Database.LSMTree.Normal.updates'.
--
-- Does not enforce that mupsert and blobs should not occur in the same table.
updates ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => V.Vector (SerialisedKey, Entry SerialisedValue SerialisedBlob)
  -> TableHandle m h
  -> m ()
updates es th = withOpenTable th $ \thEnv -> do
    let hfs = tableHasFS thEnv
    modifyWithTempRegistry_
      (takeMVar (tableContent thEnv))
      (putMVar (tableContent thEnv)) $
        updatesWithInterleavedFlushes
            (tableConfig th)
            hfs
            (tableSessionRoot thEnv)
            (tablesSessionUniqCounter thEnv)
            es

{-# SPECIALISE updatesWithInterleavedFlushes :: TableConfig -> HasFS IO h -> SessionRoot -> UniqCounter IO -> V.Vector (SerialisedKey, Entry SerialisedValue SerialisedBlob) -> TempRegistry IO -> TableContent h -> IO (TableContent h) #-}
-- | A single batch of updates can fill up the write buffer multiple times. We
-- flush the write buffer each time it fills up before trying to fill it up
-- again.
--
-- TODO: in practice the size of a batch will be much smaller than the maximum
-- size of the write buffer, so we should optimise for the case that small
-- batches are inserted. Ideas:
--
-- * we can allow a range of sizes to flush to disk rather than just the max size
--
-- * could do a map bulk merge rather than sequential insert, on the prefix of
--   the batch that's guaranteed to fit
--
-- * or flush the existing buffer if we would expect the next batch to cause the
--   buffer to become too large
--
-- TODO: we could also optimise for the case where the write buffer is small
-- compared to the size of the batch, but it is less critical. In particular, in
-- case the write buffer is empty, or if it fills up multiple times for a single
-- batch of updates, we might be able to skip adding entries to the write buffer
-- for a large part. When the write buffer is empty, we can sort and deduplicate
-- the vector of updates directly, slice it up into properly sized sub-vectors,
-- and write those to disk. Of course, any remainder that did not fit into a
-- whole run should then end up in a fresh write buffer.
updatesWithInterleavedFlushes ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => TableConfig
  -> HasFS m h
  -> SessionRoot
  -> UniqCounter m
  -> V.Vector (SerialisedKey, Entry SerialisedValue SerialisedBlob)
  -> TempRegistry m
  -> TableContent h
  -> m (TableContent h)
updatesWithInterleavedFlushes conf hfs root uniqC es reg tc = do
    let wb = tableWriteBuffer tc
        (wb', es') = WB.addEntriesUpToN resolve es maxn wb
    -- never exceed the write buffer capacity
    assert (unNumEntries (WB.numEntries wb') <= maxn) $ pure ()
    let tc' = setWriteBuffer wb' tc
    -- If the new write buffer has not reached capacity yet, then it must be the
    -- cases that we have performed all the updates.
    if unNumEntries (WB.numEntries wb') < maxn then do
      assert (V.null es') $ pure ()
      pure $! tc'
    -- If the write buffer did reach capacity, then we flush.
    else do
      assert (unNumEntries (WB.numEntries wb') == maxn) $ pure ()
      tc'' <- flushWriteBuffer hfs root uniqC reg tc'
      -- In the fortunate case where we have already performed all the updates,
      -- return,
      if V.null es' then
        pure $! tc''
      -- otherwise, keep going
      else
        updatesWithInterleavedFlushes conf hfs root uniqC es' reg tc''
  where
    AllocNumEntries (NumEntries maxn) = confWriteBufferAlloc conf
    resolve = resolveMupsert conf

    setWriteBuffer :: WriteBuffer -> TableContent h -> TableContent h
    setWriteBuffer wbToSet tc0 = TableContent {
          tableWriteBuffer = wbToSet
        , tableLevels = tableLevels tc0
        , tableCache = tableCache tc0
        }

{-# SPECIALISE flushWriteBuffer :: HasFS IO h -> SessionRoot -> UniqCounter IO -> TempRegistry IO -> TableContent h -> IO (TableContent h) #-}
-- | Flush the write buffer to disk, regardless of whether it is full or not.
--
-- The returned table content contains an updated set of levels, where the write
-- buffer is inserted into level 1.
--
-- TODO: merge runs when level becomes full. This is currently a placeholder
-- implementation that is sufficient to pass the tests, but simply writes small
-- runs to disk without merging.
flushWriteBuffer ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => HasFS m h
  -> SessionRoot
  -> UniqCounter m
  -> TempRegistry m
  -> TableContent h
  -> m (TableContent h)
flushWriteBuffer hfs root uniqC reg tc
  | WB.null (tableWriteBuffer tc) = pure tc
  | otherwise = do
    n <- incrUniqCounter uniqC
    r <- allocateTemp reg
            (Run.fromWriteBuffer hfs
              (Paths.runPath root n)
              (tableWriteBuffer tc))
            (Run.removeReference hfs)
    let levels' = addRunToLevels r (tableLevels tc)
    pure $! TableContent {
        tableWriteBuffer = WB.empty
      , tableLevels = levels'
      , tableCache = mkLevelsCache levels'
      }
  where
    addRunToLevels :: Run h -> Levels h -> Levels h
    addRunToLevels r levels = case V.uncons levels of
        Nothing -> V.singleton $ Level (SingleRun r) V.empty
        Just (Level (SingleRun r') runs, ls) -> V.cons (Level (SingleRun r) (V.cons r' runs)) ls

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

type SnapshotLabel = String

{-# SPECIALISE snapshot :: SnapshotName -> String -> TableHandle IO h -> IO Int #-}
-- |  See 'Database.LSMTree.Normal.snapshot''.
snapshot ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => SnapshotName
  -> SnapshotLabel
  -> TableHandle m h
  -> m Int
snapshot snap label th = do
    withOpenTable th $ \thEnv -> do
      -- For the temporary implementation it is okay to just flush the buffer
      -- before taking the snapshot.
      let hfs = tableHasFS thEnv
      content <- modifyWithTempRegistry
                    (takeMVar (tableContent thEnv))
                    (putMVar (tableContent thEnv)) $ \reg content -> do
        r <- flushWriteBuffer
              hfs
              (tableSessionRoot thEnv)
              (tablesSessionUniqCounter thEnv)
              reg
              content
        pure (r, r)
      -- At this point, we've flushed the write buffer but we haven't created the
      -- snapshot file yet. If an asynchronous exception happens beyond this
      -- point, we'll take that loss, as the inner state of the table is still
      -- consistent.
      let runNumbers = V.map (\(Level (SingleRun mr) rs) ->
                                ( runNumber (Run.runRunFsPaths mr)
                                , V.map (runNumber . Run.runRunFsPaths) rs)) $
                          tableLevels content
          snapPath = Paths.snapshot (tableSessionRoot thEnv) snap
      FS.doesFileExist (tableHasFS thEnv) snapPath >>= \b ->
              when b $ throwIO (ErrSnapshotExists snap)
      FS.withFile
        (tableHasFS thEnv)
        snapPath
        (FS.WriteMode FS.MustBeNew) $ \h ->
          void $ FS.hPutAllStrict (tableHasFS thEnv) h
                      (BSC.pack $ show (label, runNumbers, tableConfig th))
      pure $! V.sum (V.map (\((_ :: Word64), rs) -> 1 + V.length rs) runNumbers)

{-# SPECIALISE open :: Session IO h -> String -> SnapshotName -> IO (TableHandle IO h) #-}
-- |  See 'Database.LSMTree.Normal.open'.
open ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> SnapshotLabel -- ^ Expected label
  -> SnapshotName
  -> m (TableHandle m h)
open sesh label snap = do
    withOpenSession sesh $ \seshEnv -> do
      let hfs = sessionHasFS seshEnv
          snapPath = Paths.snapshot (sessionRoot seshEnv) snap
      FS.doesFileExist hfs snapPath >>= \b ->
        unless b $ throwIO (ErrSnapshotNotExists snap)
      bs <- FS.withFile
              hfs
              snapPath
              FS.ReadMode $ \h ->
                FS.hGetAll (sessionHasFS seshEnv) h
      let (label', runNumbers, conf) = read . BSC.unpack . BSC.toStrict $ bs
      unless (label == label') $ throwIO (ErrSnapshotWrongType snap)
      let runPaths = V.map (bimap (RunFsPaths (Paths.activeDir $ sessionRoot seshEnv))
                                  (V.map (RunFsPaths (Paths.activeDir $ sessionRoot seshEnv))))
                           runNumbers
      with (openLevels hfs runPaths) (newWithLevels seshEnv conf)

{-# SPECIALISE openLevels :: HasFS IO h -> V.Vector (RunFsPaths, V.Vector RunFsPaths) -> Managed IO (Levels (FS.Handle h)) #-}
-- | Open multiple levels.
--
-- If an error occurs when opening multiple runs in sequence, then we have to
-- make sure that all runs that have been succesfully opened already are closed
-- again. The 'Managed' monad allows us to fold 'bracketOnError's over an @m@
-- action.
--
-- TODO: 'Managed' is actually not properly exception-safe, because an async
-- exception can be raised just after a 'bracketOnError's, but still inside the
-- 'bracketOnError' surrounding it. We don't just need folding of
-- 'bracketOnError', we need to release all runs inside the same mask! We should
-- use something like 'TempRegistry'.
openLevels ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => HasFS m h
  -> V.Vector (RunFsPaths, V.Vector RunFsPaths)
  -> Managed m (Levels (Handle h))
openLevels hfs levels =
    flip V.mapMStrict levels $ \(mrPath, rsPaths) -> do
      mr <- Managed $ bracketOnError (Run.openFromDisk hfs mrPath) (Run.removeReference hfs)
      rs <- flip V.mapMStrict rsPaths $ \run ->
        Managed $ bracketOnError
                    (Run.openFromDisk hfs run)
                    (Run.removeReference hfs)
      pure $! Level (SingleRun mr) rs

{-# SPECIALISE deleteSnapshot :: Session IO h -> SnapshotName -> IO () #-}
-- |  See 'Database.LSMTree.Common.deleteSnapshot'.
deleteSnapshot ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> SnapshotName
  -> m ()
deleteSnapshot sesh snap =
    withOpenSession sesh $ \seshEnv -> do
      let hfs = sessionHasFS seshEnv
          snapPath = Paths.snapshot (sessionRoot seshEnv) snap
      FS.doesFileExist hfs snapPath >>= \b ->
        unless b $ throwIO (ErrSnapshotNotExists snap)
      FS.removeFile hfs snapPath

{-# SPECIALISE listSnapshots :: Session IO h -> IO [SnapshotName] #-}
-- |  See 'Database.LSMTree.Common.listSnapshots'.
listSnapshots ::
     m ~ IO -- TODO: replace by @io-classes@ constraints for IO simulation.
  => Session m h
  -> m [SnapshotName]
listSnapshots sesh =
    withOpenSession sesh $ \seshEnv -> do
      let hfs = sessionHasFS seshEnv
          root = sessionRoot seshEnv
      contents <- FS.listDirectory hfs (Paths.snapshotsDir (sessionRoot seshEnv))
      snaps <- mapM (checkSnapshot hfs root) $ Set.toList contents
      pure $ catMaybes snaps
  where
    checkSnapshot hfs root s =
      case Paths.mkSnapshotName s of
        Nothing   -> pure Nothing
        Just snap -> do
          -- check that it is a file
          b <- FS.doesFileExist hfs (Paths.snapshot root snap)
          if b then pure $ Just snap
               else pure $ Nothing

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Table configuration parameters, including LSM tree tuning parameters.
--
-- Some config options are fixed (for now):
--
-- * Merge policy: Tiering
--
-- * Size ratio: 4
data TableConfig = TableConfig {
    confMergePolicy      :: !MergePolicy
    -- Size ratio between the capacities of adjacent levels.
  , confSizeRatio        :: !SizeRatio
    -- | Total number of bytes that the write buffer can use.
    --
    -- The maximum is 4GiB, which should be more than enough for realistic
    -- applications.
  , confWriteBufferAlloc :: !WriteBufferAlloc
  , confBloomFilterAlloc :: !BloomFilterAlloc
    -- | Function for resolving 'Mupsert' values. This should be 'Nothing' for
    -- normal tables and 'Just' for monoidal tables.
  , confResolveMupsert   :: !(Maybe ResolveMupsert)
  }
  deriving Show

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
deriving instance Read TableConfig

resolveMupsert :: TableConfig -> SerialisedValue -> SerialisedValue -> SerialisedValue
resolveMupsert conf = maybe const unResolveMupsert (confResolveMupsert conf)

data MergePolicy =
    -- | Use tiering on intermediate levels, and levelling on the last level.
    -- This makes it easier for delete operations to disappear on the last
    -- level.
    MergePolicyLazyLevelling
{- TODO: disabled for now. Would we ever want to provide pure tiering?
  | MergePolicyTiering
-}
{- TODO: disabled for now
  | MergePolicyLevelling
-}
  deriving (Show, Eq)

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
deriving instance Read MergePolicy

data SizeRatio = Four
  deriving (Show, Eq)

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
deriving instance Read SizeRatio

-- | Allocation method for the write buffer.
data WriteBufferAlloc =
    -- | Total number of key\/value pairs that can be present in the write
    -- buffer before flushing the write buffer to disk.
    --
    -- NOTE: if the sizes of values vary greatly, this can lead to wonky runs on
    -- disk, and therefore unpredictable performance.
    AllocNumEntries !NumEntries
{- TODO: disabled for now
  | -- | Total number of bytes that the write buffer can use.
    --
    -- The maximum is 4GiB, which should be more than enough for realistic
    -- applications.
    AllocTotalBytes !Word32
-}
  deriving (Show, Eq)

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
deriving instance Read WriteBufferAlloc
-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
deriving instance Read NumEntries

-- | Allocation method for bloom filters.
--
-- NOTE: a __physical__ database entry is a key\/operation pair that exists in a
-- file, i.e., a run. Multiple physical entries that have the same key
-- constitute a __logical__ database entry.
data BloomFilterAlloc =
    -- | Allocate a fixed number of bits per physical entry in each bloom
    -- filter.
    AllocFixed
      Word32 -- ^ Bits per physical entry.
  | -- | Allocate as many bits as required per physical entry to get the requested
    -- false-positive rate. Do this for each bloom filter.
    AllocRequestFPR
      Double -- ^ Requested FPR.
{- TODO: disabled for now
  | -- | Allocate bits amongst all bloom filters according to the Monkey algorithm.
    --
    -- TODO: add more configuration paramters that the Monkey algorithm might
    -- need once we implement it.
    AllocMonkey
      Word32 -- ^ Total number of bytes that bloom filters can use collectively.
             --
             -- The maximum is 4GiB, which should be more than enough for
             -- realistic applications.
 -}
  deriving (Show, Eq)

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
deriving instance Read BloomFilterAlloc

newtype ResolveMupsert = ResolveMupsert {
    unResolveMupsert :: SerialisedValue -> SerialisedValue -> SerialisedValue
  }

instance Show ResolveMupsert where show _ = "ResolveMupsert function can not be printed"

-- | TODO: this should be removed once we have proper snapshotting with proper
-- persistence of the config to disk.
instance Read ResolveMupsert where readsPrec = error "ResolveMupsert function can not be read"
