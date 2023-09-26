{-# LANGUAGE UnboxedTuples #-}

module Streamly.Data.Serialize.Instances.ByteString () where

import Data.Int (Int64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.Base (IO(..))
import Streamly.Internal.Data.Serialize (Serialize(..))

import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Serialize.TH as Serialize

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

$(Serialize.deriveSerialize ''Lazy.ByteString)
