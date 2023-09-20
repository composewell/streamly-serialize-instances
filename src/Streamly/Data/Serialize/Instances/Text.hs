-- |
-- Module      : Streamly.Data.Serialize.Instances
-- Copyright   : (c) 2023 Composewell technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

-- This is required as all the instances in this module are orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Streamly.Data.Serialize.Instances.Text () where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Serialize (Serialize(..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))

import qualified Data.Text.Internal as Strict (Text(..))
import qualified Data.Text.Array as TArr (Array(..))
import qualified Streamly.Internal.Data.Unbox as Unbox

import GHC.Exts

--------------------------------------------------------------------------------
-- Strict Text
--------------------------------------------------------------------------------

instance Serialize Strict.Text where
    size i (Strict.Text (TArr.Array _) _ len16) =
            i + len16 * 2 + 8 -- XXX sizeof Int

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, len16) <- deserialize off arr end :: IO (Int, Int)
        let lenBytes = len16 * 2

        -- Check the available length in input buffer
        if (off1 + lenBytes <= end)
        then do
            newArr <- Unbox.newBytes lenBytes
            -- XXX We can perform an unrolled word copy directly?
            Unbox.putSliceUnsafe arr off1 newArr 0 lenBytes
            pure
                ( off1 + lenBytes
                , Strict.Text
                      (TArr.Array
                          (unsafeCoerce# (Unbox.getMutableByteArray# newArr)))
                      0
                      len16
                )
        else error $ "deserialize: Strict.Text: input buffer underflow: off1 = "
                ++ show off1 ++ " lenBytes = " ++ show lenBytes
                ++ " end = " ++ show end

    {-# INLINE serialize #-}
    serialize off arr (Strict.Text (TArr.Array barr#) off16 len16) = do
        off1 <- serialize off arr (len16 :: Int)
        let lenBytes = len16 * 2
        Unbox.putSliceUnsafe
            (MutableByteArray (unsafeCoerce# barr#)) (off16 * 2)
            arr off1
            lenBytes
        pure (off1 + lenBytes)

--------------------------------------------------------------------------------
-- Lazy Text
--------------------------------------------------------------------------------
