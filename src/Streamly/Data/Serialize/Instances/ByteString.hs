{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Streamly.Data.Serialize.Instances.ByteString () where

import Streamly.Internal.Data.Serialize (Serialize(..))
import Data.ByteString.Internal as Strict
import Data.ByteString.Lazy as Lazy
import qualified Streamly.Internal.Data.Unbox as Unbox
import GHC.Exts
import Foreign.ForeignPtr
import GHC.Base (IO(..))
import Control.Monad (foldM_)

--------------------------------------------------------------------------------
-- Strict ByteString
--------------------------------------------------------------------------------
instance Serialize Strict.ByteString where
    {-# INLINE size #-}
    size i (Strict.PS _ _ l) = i + l + size 0 l

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, lenBytes) <- deserialize off arr end :: IO (Int, Int)
        fp <- Strict.mallocByteString lenBytes
        let arrS# = Unbox.getMutableByteArray# arr
            !(I# srcStartBytes#)    = off1
            !(I# lenBytes#)         = lenBytes
        withForeignPtr fp $ \(Ptr addr#) -> IO $ \s# -> (# copyMutableByteArrayToAddr#
                                                        arrS# srcStartBytes# addr# lenBytes# s#
                                                        , () #)
        return (off1 + lenBytes, Strict.PS fp 0 lenBytes)

    {-# INLINE serialize #-}
    serialize off arr (Strict.PS fp _ lenBytes) = do
        off1 <- serialize off arr lenBytes
        let arrD# = Unbox.getMutableByteArray# arr
            !(I# dstStartBytes#) = off1
            !(I# lenBytes#) = lenBytes
        withForeignPtr fp $ \(Ptr addr#) -> IO $ \s# -> (# copyAddrToByteArray#
                                    addr# arrD# dstStartBytes# lenBytes# s#
                                , () #)
        pure (off1 + lenBytes)


--------------------------------------------------------------------------------
-- Lazy ByteString
--------------------------------------------------------------------------------
instance Serialize Lazy.ByteString where
    {-# INLINE size #-}
    size i lazyByteString = let len = fromIntegral $ Lazy.length lazyByteString in  i + len + size 0 len

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, lenBytes) <- deserialize off arr end :: IO (Int, Int)
        fp <- Strict.mallocByteString lenBytes
        let arrS# = Unbox.getMutableByteArray# arr
            !(I# srcStartBytes#)    = off1
            !(I# lenBytes#)         = lenBytes
        withForeignPtr fp $ \(Ptr addr#) -> IO $ \s# -> (# copyMutableByteArrayToAddr#
                                                        arrS# srcStartBytes# addr# lenBytes# s#
                                                        , () #)
        return (off1 + lenBytes, Lazy.fromStrict $ Strict.PS fp 0 lenBytes)

    {-# INLINE serialize #-}
    serialize off arr lazyByteString = do
        let lenBytes = fromIntegral $  Lazy.length lazyByteString
        off1 <- serialize off arr lenBytes
        let arrD# = Unbox.getMutableByteArray# arr
            strictByteStringArr = Lazy.toChunks lazyByteString
        foldM_ (\newdstStartBytes (Strict.PS fp _ len) -> do
            let !(I# len#) = len
                !(I# newdstStartBytes#) = newdstStartBytes
            withForeignPtr fp $ \(Ptr addr#) -> IO $ \s# -> (# copyAddrToByteArray#
                                    addr# arrD# newdstStartBytes# len# s#
                                , () #)
            return (newdstStartBytes + len)
            ) off1 strictByteStringArr
        pure (off1 + lenBytes)

