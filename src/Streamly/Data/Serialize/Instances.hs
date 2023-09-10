{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- This is required as all the instances in this module are orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Streamly.Data.Serialize.Instances () where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Serialize (Serialize(..))
import Data.Text.Internal (Text(..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))
import Data.Maybe (fromJust)
import Data.Fixed (Fixed)

import qualified Data.Text.Array as TArr (Array(..))
import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Serialize.TH as Serialize

import Data.Time (Day, TimeOfDay, LocalTime, DiffTime, UTCTime)

import GHC.Exts

import Data.Scientific (Scientific)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

$(Serialize.deriveSerializeWith
      Serialize.defaultConfig
      [d|instance Serialize (Fixed a)|])
$(Serialize.deriveSerialize ''Day)
$(Serialize.deriveSerialize ''TimeOfDay)
$(Serialize.deriveSerialize ''LocalTime)
$(Serialize.deriveSerialize ''DiffTime)
$(Serialize.deriveSerialize ''UTCTime)

--------------------------------------------------------------------------------
-- Scientific
--------------------------------------------------------------------------------

$(Serialize.deriveSerialize ''Scientific)

--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------

instance Serialize Text where
    size i (Text (TArr.Array _) _ len16) =
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
                , Text
                      (TArr.Array
                          (unsafeCoerce# (Unbox.getMutableByteArray# newArr)))
                      0
                      len16
                )
        else error $ "deserialize: Text: input buffer underflow: off1 = "
                ++ show off1 ++ " lenBytes = " ++ show lenBytes
                ++ " end = " ++ show end

    {-# INLINE serialize #-}
    serialize off arr (Text (TArr.Array barr#) off16 len16) = do
        off1 <- serialize off arr (len16 :: Int)
        let lenBytes = len16 * 2
        Unbox.putSliceUnsafe
            (MutableByteArray (unsafeCoerce# barr#)) (off16 * 2)
            arr off1
            lenBytes
        pure (off1 + lenBytes)

--------------------------------------------------------------------------------
-- BSL.ByteString
--------------------------------------------------------------------------------

-- TODO: Serialize it independently

-- XXX Extremely inefficient serialization of BSL.ByteString
-- XXX Depends on list serialization
instance Serialize BSL.ByteString where
    {-# INLINE size #-}
    size i val = i + fromIntegral (BSL.length val) + 8
    -- size i val = size i (BSL.unpack val)

    {-# INLINE deserialize #-}
    deserialize off arr end = fmap BSL.pack <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (BSL.unpack val)

--------------------------------------------------------------------------------
-- Aeson.Value
--------------------------------------------------------------------------------

-- TODO: Serialize it independently

-- XXX Extremely inefficient serialization of Aeson.Value
instance Serialize Aeson.Value where
    {-# INLINE size #-}
    size i val = size i (Aeson.encode val)

    {-# INLINE deserialize #-}
    deserialize off arr end =
        fmap (fromJust . Aeson.decode) <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (Aeson.encode val)

--------------------------------------------------------------------------------
-- Vector.Vector
--------------------------------------------------------------------------------

-- TODO: Serialize it independently

-- XXX Extremely inefficient serialization of Vector
instance Serialize a => Serialize (Vector.Vector a) where
    {-# INLINE size #-}
    size i val = size i (Vector.toList val)

    {-# INLINE deserialize #-}
    deserialize off arr end =
        fmap Vector.fromList <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (Vector.toList val)
