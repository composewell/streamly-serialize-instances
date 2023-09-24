{-# LANGUAGE UnboxedTuples #-}

module Streamly.Data.Serialize.Instances.ByteString () where

import Data.Int (Int64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.Base (IO(..))
import Control.Monad (foldM_)
import Streamly.Internal.Data.Serialize (Serialize(..))

import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Streamly.Internal.Data.Unbox as Unbox

import GHC.Exts

--------------------------------------------------------------------------------
-- Strict ByteString
--------------------------------------------------------------------------------

instance Serialize Strict.ByteString where
    {-# INLINE size #-}
    size i (Strict.PS _ _ l) = i + l + 8 -- 8 is the length of Int64

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, len) <- deserialize off arr end :: IO (Int, Int64)
        let lenBytes = fromIntegral len
        bs <- Strict.create lenBytes $ \(Ptr addr#) ->
            let arrS# = Unbox.getMutableByteArray# arr
                !(I# srcStartBytes#)    = off1
                !(I# lenBytes#)         = lenBytes
            in IO $ \s# -> (# copyMutableByteArrayToAddr#
                    arrS# srcStartBytes# addr# lenBytes# s#
                    , () #)
        return (off1 + lenBytes, bs)

    {-# INLINE serialize #-}
    serialize off arr (Strict.PS fp srcOffset lenBytes) = do
        off1 <- serialize off arr (fromIntegral lenBytes :: Int64)
        let arrD# = Unbox.getMutableByteArray# arr
            !(I# dstStartBytes#) = off1
            !(I# lenBytes#) = lenBytes
        withForeignPtr fp $ \srcPtr -> 
            let !(Ptr addr#) = srcPtr `plusPtr` srcOffset
                in IO $ \s# -> (# copyAddrToByteArray#
                        addr# arrD# dstStartBytes# lenBytes# s#
                    , () #)
        pure (off1 + lenBytes)


--------------------------------------------------------------------------------
-- Lazy ByteString
--------------------------------------------------------------------------------

instance Serialize Lazy.ByteString where
    {-# INLINE size #-}
    size i lazyByteString = let len = fromIntegral $ Lazy.length lazyByteString in  i + len + 8 -- 8 is the length of Int64

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, len) <- deserialize off arr end :: IO (Int, Int64)
        let lenBytes = fromIntegral len
        bs <- Strict.create lenBytes $ \(Ptr addr#) ->
            let arrS# = Unbox.getMutableByteArray# arr
                !(I# srcStartBytes#)    = off1
                !(I# lenBytes#)         = lenBytes 
            in IO $ \s# -> (# copyMutableByteArrayToAddr#
                    arrS# srcStartBytes# addr# lenBytes# s#
                    , () #)
        return (off1 + lenBytes, Lazy.fromStrict bs)

    {-# INLINE serialize #-}
    serialize off arr lazyByteString = do
        let l = Lazy.length lazyByteString
            lenBytes = fromIntegral l
        off1 <- serialize off arr l
        let arrD# = Unbox.getMutableByteArray# arr
            strictByteStringArr = Lazy.toChunks lazyByteString
        foldM_ (\newdstStartBytes (Strict.PS fp srcOffset len) -> do
            let !(I# len#) = len
                !(I# newdstStartBytes#) = newdstStartBytes
            withForeignPtr fp $ \srcPtr -> 
                let !(Ptr addr#) = srcPtr `plusPtr` srcOffset
                    in IO $ \s# -> (# copyAddrToByteArray#
                        addr# arrD# newdstStartBytes# len# s#
                    , () #)
            return (newdstStartBytes + len)
            ) off1 strictByteStringArr
        pure (off1 + lenBytes)
