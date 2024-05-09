{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeFamilies   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Database.LSMTree.Extras.Orphans (
    byteSwapWord256
  , indexWord8ArrayAsWord256
  , indexWord8ArrayAsWord128
  ) where

import           Control.DeepSeq
import qualified Data.Primitive as P
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.WideWord.Word128 (Word128 (..), byteSwapWord128)
import           Data.WideWord.Word256 (Word256 (..))
import           Database.LSMTree.Internal.Primitive (indexWord8ArrayAsWord64)
import qualified Database.LSMTree.Internal.RawBytes as RB
import           Database.LSMTree.Internal.Serialise (SerialisedBlob (..),
                     SerialisedKey (..), SerialisedValue (..))
import           Database.LSMTree.Internal.Serialise.Class
import           Database.LSMTree.Internal.Vector
import           Database.LSMTree.Internal.WriteBuffer (WriteBuffer (..))
import           GHC.Generics
import           GHC.Word
import qualified System.FS.API as FS
import qualified System.FS.IO.Internal.Handle as FS
import           System.Posix.Types (COff (..))
import           System.Random (Uniform)
import           Test.QuickCheck

{-------------------------------------------------------------------------------
  Word256
-------------------------------------------------------------------------------}

deriving anyclass instance Uniform Word256

instance SerialiseKey Word256 where
  serialiseKey w256 =
    RB.RawBytes $ mkPrimVector 0 32 $ P.runByteArray $ do
      ba <- P.newByteArray 32
      P.writeByteArray ba 0 $ byteSwapWord256 w256
      return ba
  deserialiseKey (RawBytes (PV.Vector off len ba)) =
    requireBytesExactly "Word256" 32 len $
      byteSwapWord256 $ indexWord8ArrayAsWord256 ba off

instance SerialiseValue Word256 where
  serialiseValue w256 =
    RB.RawBytes $ mkPrimVector 0 32 $ P.runByteArray $ do
      ba <- P.newByteArray 32
      P.writeByteArray ba 0 w256
      return ba
  deserialiseValue (RawBytes (PV.Vector off len ba)) =
    requireBytesExactly "Word256" 32 len $
      indexWord8ArrayAsWord256 ba off
  deserialiseValueN = deserialiseValue . mconcat -- TODO: optimise

instance Arbitrary Word256 where
  arbitrary = Word256 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink w256 = [ w256'
                | let i256 = toInteger w256
                , i256' <- shrink i256
                , toInteger (minBound :: Word256) <= i256'
                , toInteger (maxBound :: Word256) >= i256'
                , let w256' = fromIntegral i256'
                ]

{-# INLINE byteSwapWord256 #-}
byteSwapWord256 :: Word256 -> Word256
byteSwapWord256 (Word256 a3 a2 a1 a0) =
    Word256 (byteSwap64 a0) (byteSwap64 a1) (byteSwap64 a2) (byteSwap64 a3)

{-# INLINE indexWord8ArrayAsWord256 #-}
indexWord8ArrayAsWord256 :: P.ByteArray -> Int -> Word256
indexWord8ArrayAsWord256 !ba !off =
    Word256 (indexWord8ArrayAsWord64 ba (off + 24))
            (indexWord8ArrayAsWord64 ba (off + 16))
            (indexWord8ArrayAsWord64 ba (off + 8))
            (indexWord8ArrayAsWord64 ba off)

newtype instance VUM.MVector s Word256 = MV_Word256 (PV.MVector s Word256)
newtype instance VU.Vector     Word256 = V_Word256  (PV.Vector    Word256)

deriving via VU.UnboxViaPrim Word256 instance VGM.MVector VU.MVector Word256
deriving via VU.UnboxViaPrim Word256 instance VG.Vector   VU.Vector  Word256

instance VUM.Unbox Word256

{-------------------------------------------------------------------------------
  Word128
-------------------------------------------------------------------------------}

deriving anyclass instance Uniform Word128

instance SerialiseKey Word128 where
  serialiseKey w128 =
    RB.RawBytes $ mkPrimVector 0 16 $ P.runByteArray $ do
      ba <- P.newByteArray 16
      P.writeByteArray ba 0 $ byteSwapWord128 w128
      return ba
  deserialiseKey (RawBytes (PV.Vector off len ba)) =
    requireBytesExactly "Word128" 16 len $
      byteSwapWord128 $ indexWord8ArrayAsWord128 ba off

instance SerialiseValue Word128 where
  serialiseValue w128 =
    RB.RawBytes $ mkPrimVector 0 16 $ P.runByteArray $ do
      ba <- P.newByteArray 16
      P.writeByteArray ba 0 w128
      return ba
  deserialiseValue (RawBytes (PV.Vector off len ba)) =
    requireBytesExactly "Word128" 16 len $
      indexWord8ArrayAsWord128 ba off
  deserialiseValueN = deserialiseValue . mconcat -- TODO: optimise

instance Arbitrary Word128 where
  arbitrary = Word128 <$> arbitrary <*> arbitrary
  shrink w128 = [ w128'
                | let i128 = toInteger w128
                , i128' <- shrink i128
                , toInteger (minBound :: Word128) <= i128'
                , toInteger (maxBound :: Word128) >= i128'
                , let w128' = fromIntegral i128'
                ]

{-# INLINE indexWord8ArrayAsWord128 #-}
indexWord8ArrayAsWord128 :: P.ByteArray -> Int -> Word128
indexWord8ArrayAsWord128 !ba !off =
    Word128 (indexWord8ArrayAsWord64 ba (off + 8))
            (indexWord8ArrayAsWord64 ba off)

newtype instance VUM.MVector s Word128 = MV_Word128 (PV.MVector s Word128)
newtype instance VU.Vector     Word128 = V_Word128  (PV.Vector    Word128)

deriving via VU.UnboxViaPrim Word128 instance VGM.MVector VU.MVector Word128
deriving via VU.UnboxViaPrim Word128 instance VG.Vector   VU.Vector  Word128

instance VUM.Unbox Word128

{-------------------------------------------------------------------------------
  NFData
-------------------------------------------------------------------------------}

deriving stock instance Generic (FS.HandleOS h)
deriving anyclass instance NFData (FS.HandleOS h)
deriving newtype instance NFData FS.BufferOffset
deriving newtype instance NFData COff
deriving anyclass instance NFData FS.FsPath
deriving instance NFData h => NFData (FS.Handle h)
instance NFData (FS.HasFS m h) where
  rnf x = x `seq` ()

{-------------------------------------------------------------------------------
  RawBytes
-------------------------------------------------------------------------------}

instance SerialiseKey RawBytes where
  serialiseKey = id
  deserialiseKey = id

instance SerialiseValue RawBytes where
  serialiseValue = id
  deserialiseValue = id
  deserialiseValueN = mconcat

{-------------------------------------------------------------------------------
  SerialisedKey/Value/Blob
-------------------------------------------------------------------------------}

deriving newtype instance SerialiseKey SerialisedKey

deriving newtype instance SerialiseValue SerialisedValue

deriving newtype instance SerialiseValue SerialisedBlob

deriving stock instance Eq WriteBuffer
