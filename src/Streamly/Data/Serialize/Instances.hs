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
import Data.Int (Int64)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Time (Day, TimeOfDay, LocalTime, DiffTime, UTCTime)
import Streamly.Data.Serialize.Instances.Text ()
import Streamly.Data.Serialize.Instances.ByteString ()
import Streamly.Internal.Data.Serialize (Serialize(..))
import Streamly.Internal.Data.Unbox (MutableByteArray)

import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Streamly.Internal.Data.Serialize.TH as Serialize
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
-- Aeson.Value
--------------------------------------------------------------------------------

$(Serialize.deriveSerialize ''Map)
$(Serialize.deriveSerialize ''Aeson.Key)
$(Serialize.deriveSerialize ''Aeson.KeyMap)
$(Serialize.deriveSerialize ''Aeson.Value)

--------------------------------------------------------------------------------
-- Vector.Vector
--------------------------------------------------------------------------------

instance Serialize a => Serialize (Vector.Vector a) where

    {-# INLINE size #-}
    size :: Int -> (Vector.Vector a) -> Int
    size acc = Vector.foldl' size (acc + Unbox.sizeOf (Proxy :: Proxy Int64))

    {-# INLINE serialize #-}
    serialize :: Int -> MutableByteArray -> (Vector.Vector a) -> IO Int
    serialize off arr val = do
        let len = Vector.length val
        finalOffset <-
            Vector.foldM'
                (\curOff v -> serialize curOff arr v)
                (off + Unbox.sizeOf (Proxy :: Proxy Int64))
                val
        Unbox.pokeByteIndex off arr ((fromIntegral :: Int -> Int64) len)
        pure finalOffset

    {-# INLINE deserialize #-}
    deserialize :: Int -> MutableByteArray -> Int -> IO (Int, Vector.Vector a)
    deserialize off arr s = do

        (off1, len64) <- deserialize off arr s
        let len = (fromIntegral :: Int64 -> Int) len64
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
