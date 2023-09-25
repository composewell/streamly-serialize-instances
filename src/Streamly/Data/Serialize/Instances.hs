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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vector
import qualified Streamly.Internal.Data.Serialize.TH as Serialize
import Data.List (foldl')
import Data.Proxy
import qualified Data.Vector.Mutable as MVector
import qualified Streamly.Internal.Data.Unbox as Unbox
import Streamly.Internal.Data.Unbox (MutableByteArray)
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
    size acc = foldl' size (acc + Unbox.sizeOf (Proxy :: Proxy Int))

    {-# INLINE serialize #-}
    serialize :: Int -> MutableByteArray -> (Vector.Vector a) -> IO Int
    serialize off arr val = do
        let len = length val
        finalOffset <- Vector.foldM (\curOff v -> serialize curOff arr v) (off+ Unbox.sizeOf (Proxy :: Proxy Int)) val
        _ <- serialize off arr len
        pure finalOffset

    {-# INLINE deserialize #-}
    deserialize :: Int -> MutableByteArray -> Int -> IO (Int, Vector.Vector a)
    deserialize off arr s = do
        (off', len) <- deserialize off arr s
        val <- MVector.new len
        (off1, dVal) <- getDeserialized len 0 off' val
        dVal' <- Vector.freeze dVal
        pure (off1, dVal')
        where

            getDeserialized len acc off' val
                | acc >= len = pure (off', val)
                | otherwise = do
                    (off1, v) <- deserialize off' arr s
                    MVector.write val acc v
                    getDeserialized len (acc + 1) off1 val