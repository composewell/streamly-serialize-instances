-- This is required as all the instances in this module are orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Streamly.Data.Serialize.Instances
-- Copyright   : (c) 2023 Composewell technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Data.Serialize.Instances.Text () where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Int (Int64)
import Streamly.Internal.Data.Serialize (Serialize(..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))

import qualified Data.Text.Internal as Strict (Text(..))
import qualified Streamly.Internal.Data.Unbox as Unbox

#if MIN_VERSION_text(2,0,0)

import qualified Data.Text.Array as TArr (Array(..))
#define T_ARR_CON TArr.ByteArray
#define LEN_TO_BYTES(l) l

#else

import qualified Data.Text.Array as TArr (Array(..))
#define T_ARR_CON TArr.Array
#define LEN_TO_BYTES(l) l * 2

#endif

import GHC.Exts

--------------------------------------------------------------------------------
-- Strict Text
--------------------------------------------------------------------------------

instance Serialize Strict.Text where
    size i (Strict.Text _ _ lenTArr) =
        -- 8 is the length of Int64
        i + LEN_TO_BYTES(lenTArr) + 8

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, lenTArr64) <- deserialize off arr end :: IO (Int, Int64)
        let lenTArr = fromIntegral lenTArr64 :: Int
            lenBytes = fromIntegral (LEN_TO_BYTES(lenTArr))

        -- Check the available length in input buffer
        if (off1 + lenBytes <= end)
        then do
            newArr <- Unbox.newBytes lenBytes
            -- XXX We can perform an unrolled word copy directly?
            Unbox.putSliceUnsafe arr off1 newArr 0 lenBytes
            pure
                ( off1 + lenBytes
                , Strict.Text
                      (T_ARR_CON
                          (unsafeCoerce# (Unbox.getMutableByteArray# newArr)))
                      0
                      lenTArr
                )
        else error $ "deserialize: Strict.Text: input buffer underflow: off1 = "
                ++ show off1 ++ " lenBytes = " ++ show lenBytes
                ++ " end = " ++ show end

    {-# INLINE serialize #-}
    serialize off arr (Strict.Text (T_ARR_CON barr#) offTArr lenTArr) = do
        off1 <- serialize off arr (fromIntegral lenTArr :: Int64)
        let lenBytes = LEN_TO_BYTES(lenTArr)
        Unbox.putSliceUnsafe
            (MutableByteArray (unsafeCoerce# barr#)) (LEN_TO_BYTES(offTArr))
            arr off1
            lenBytes
        pure (off1 + lenBytes)

--------------------------------------------------------------------------------
-- Lazy Text
--------------------------------------------------------------------------------
