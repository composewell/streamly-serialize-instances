{-# LANGUAGE UnboxedTuples #-}

module Streamly.Data.Serialize.Instances.ByteString () where

import Data.Int (Int64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.Base (IO(..))
import Streamly.Internal.Data.MutByteArray (Serialize(..))

import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Streamly.Internal.Data.MutByteArray as MBA

import GHC.Exts

--------------------------------------------------------------------------------
-- Strict ByteString
--------------------------------------------------------------------------------

instance Serialize Strict.ByteString where

    {-# INLINE addSizeTo #-}
    addSizeTo i (Strict.PS _ _ l) = i + l + 8 -- 8 is the length of Int64

    {-# INLINE deserializeAt #-}
    deserializeAt off arr end = do
        (off1, len) <- deserializeAt off arr end :: IO (Int, Int64)
        let lenBytes = fromIntegral len
        bs <- Strict.create lenBytes $ \(Ptr addr#) ->
            let arrS# = MBA.getMutableByteArray# arr
                !(I# srcStartBytes#)    = off1
                !(I# lenBytes#)         = lenBytes
            in IO $ \s# -> (# copyMutableByteArrayToAddr#
                    arrS# srcStartBytes# addr# lenBytes# s#
                    , () #)
        return (off1 + lenBytes, bs)

    {-# INLINE serializeAt #-}
    serializeAt off arr (Strict.PS fp srcOffset lenBytes) = do
        off1 <- serializeAt off arr (fromIntegral lenBytes :: Int64)
        let arrD# = MBA.getMutableByteArray# arr
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

$(MBA.deriveSerialize [d|instance Serialize Lazy.ByteString|])
