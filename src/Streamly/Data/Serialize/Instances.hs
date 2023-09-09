{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}

-- XXX Fix the derivation in streamly and remove this.
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Streamly.Data.Serialize.Instances where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Serialize (Serialize(..))
import Data.Text.Internal (Text(..))
import Streamly.Internal.Data.Array (Array(..))
import Streamly.Internal.Data.Unbox (MutableByteArray(..))
import Data.Maybe (fromJust)

import qualified Data.Text.Array as TArr (Array(..))
import qualified Streamly.Internal.Data.Unbox as Unbox
import qualified Streamly.Internal.Data.Serialize.TH as Serialize
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Array as Array

-- XXX Only import the required
import Data.Time
-- import Data.Time.Calendar.OrdinalDate
-- import Data.Time.LocalTime
-- import Language.Haskell.TH
import Prelude
import GHC.Exts
import GHC.Integer.GMP.Internals
import Data.Fixed

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import qualified Data.Scientific as Scientific

--------------------------------------------------------------------------------
-- Common Instances
--------------------------------------------------------------------------------

instance Serialize (Array a) where
    {-# INLINE size #-}
    size i (Array {..}) = i + (arrEnd - arrStart) + 8

    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, byteLen) <- deserialize off arr end :: IO (Int, Int)
        let off2 = off1 + byteLen
        let slice = MutArray.MutArray arr off1 off2 off2
        newArr <- MutArray.clone slice
        pure (off2, Array.unsafeFreeze newArr)

    {-# INLINE serialize #-}
    serialize off arr (Array {..}) = do
        let arrLen = arrEnd - arrStart
        off1 <- serialize off arr arrLen
        Unbox.putSliceUnsafe arrContents arrStart arr off1 arrLen
        pure (off1 + arrLen)

unpackInt :: Int -> Int#
unpackInt (I# i#) = i#

data LiftedInteger
    = LIS Int
    | LIP (Array Word)
    | LIN (Array Word)

$(Serialize.deriveSerialize ''LiftedInteger)

liftInteger :: Integer -> LiftedInteger
liftInteger (S# x) = LIS (I# x)
liftInteger (Jp# (BN# x)) =
    LIP (Array (MutableByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))
liftInteger (Jn# (BN# x)) =
    LIN (Array (MutableByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))

unliftInteger :: LiftedInteger -> Integer
unliftInteger (LIS (I# x)) = S# x
unliftInteger (LIP (Array (MutableByteArray x) _ _)) =
    Jp# (BN# (unsafeCoerce# x))
unliftInteger (LIN (Array (MutableByteArray x) _ _)) =
    Jn# (BN# (unsafeCoerce# x))

$(Serialize.deriveSerialize ''E12)
$(Serialize.deriveSerialize ''Fixed)

instance Serialize Integer where
    size i a = size i (liftInteger a)

    {-# INLINE deserialize #-}
    deserialize off arr end = fmap unliftInteger <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (liftInteger val)

$(Serialize.deriveSerialize ''Maybe)
$(Serialize.deriveSerialize ''Day)
$(Serialize.deriveSerialize ''TimeOfDay)
$(Serialize.deriveSerialize ''LocalTime)
$(Serialize.deriveSerialize ''DiffTime)
$(Serialize.deriveSerialize ''UTCTime)

instance Serialize Text where
    size i (Text (TArr.Array _) _ len16) =
            i + len16 * 2 + 8 -- XXX sizeof Int

    -- XXX Need to check the bounds here
    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, len16) <- deserialize off arr end :: IO (Int, Int)
        let lenBytes = len16 * 2

        -- XXX Check the available length in input buffer
        if (off1 + lenBytes <= end)
        then do
            newArr <- Unbox.newBytes lenBytes
            -- XXX We can perform an unrolled word copy directly
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

-- XXX Extremely inefficient serialization of Aeson.Value
instance Serialize Aeson.Value where
    {-# INLINE size #-}
    size i val = size i (Aeson.encode val)

    {-# INLINE deserialize #-}
    deserialize off arr end =
        fmap (fromJust . Aeson.decode) <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (Aeson.encode val)

-- XXX Extremely inefficient serialization of Vector
instance Serialize a => Serialize (Vector.Vector a) where
    {-# INLINE size #-}
    size i val = size i (Vector.toList val)

    {-# INLINE deserialize #-}
    deserialize off arr end =
        fmap Vector.fromList <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (Vector.toList val)

data SciL = SciL Integer Int
$(Serialize.deriveSerialize ''SciL)

{-# INLINE sciLToScientific #-}
sciLToScientific :: SciL -> Scientific.Scientific
sciLToScientific (SciL coeff expo) = Scientific.scientific coeff expo

{-# INLINE scientificToSciL #-}
scientificToSciL :: Scientific.Scientific -> SciL
scientificToSciL val =
    SciL (Scientific.coefficient val) (Scientific.base10Exponent val)

-- XXX Can we bypass the transformation?
instance Serialize Scientific.Scientific where
    {-# INLINE size #-}
    size i val = size i (scientificToSciL val)

    {-# INLINE deserialize #-}
    deserialize off arr end =
        fmap (sciLToScientific) <$> deserialize off arr end

    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (Scientific.base10Exponent val)
