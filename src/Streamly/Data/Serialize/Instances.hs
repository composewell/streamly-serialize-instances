-- |
-- Module      : Streamly.Data.Serialize.Instances
-- Copyright   : (c) 2023 Composewell technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Data.Serialize.Instances () where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Fixed (Fixed)
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import Data.Time (Day, TimeOfDay, LocalTime, DiffTime, UTCTime)
import Streamly.Data.Serialize.Instances.Text ()
import Streamly.Internal.Data.Serialize (Serialize(..))
import Data.Proxy (Proxy (..))
import Streamly.Internal.Data.Unbox (MutableByteArray)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import qualified Streamly.Internal.Data.Serialize.TH as Serialize
import qualified Data.Vector.Mutable as MVector
import qualified Streamly.Internal.Data.Unbox as Unbox

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

instance Serialize a => Serialize (Vector.Vector a) where

    {-# INLINE size #-}
    size :: Int -> (Vector.Vector a) -> Int
    size acc = Vector.foldl' size (acc + Unbox.sizeOf (Proxy :: Proxy Int))

    {-# INLINE serialize #-}
    serialize :: Int -> MutableByteArray -> (Vector.Vector a) -> IO Int
    serialize off arr val = do
        let len = Vector.length val
        finalOffset <- 
            Vector.foldM'
                (\curOff v -> serialize curOff arr v)
                (off + Unbox.sizeOf (Proxy :: Proxy Int))
                val
        _ <- Unbox.pokeByteIndex off arr len
        pure finalOffset

    {-# INLINE deserialize #-}
    deserialize :: Int -> MutableByteArray -> Int -> IO (Int, Vector.Vector a)
    deserialize off arr s = do

        (off1, len) <- deserialize off arr s
        val <- MVector.new len
        (off2, val1) <- fillVector len 0 off1 val
        val2 <- Vector.freeze val1
        pure (off2, val2)
        where

        fillVector len acc off1 val
            | acc >= len = pure (off1, val)
            | otherwise = do
                (off2, v) <- deserialize off1 arr s
                MVector.write val acc v
                fillVector len (acc + 1) off2 val