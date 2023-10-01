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
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Time (Day, TimeOfDay, LocalTime, DiffTime, UTCTime)
import Streamly.Data.Serialize.Instances.Text ()
import Streamly.Data.Serialize.Instances.ByteString ()
import Streamly.Internal.Data.Serialize (Serialize(..))
import Streamly.Internal.Data.Unbox (MutableByteArray)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Aeson
#endif

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
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
-- Map
--------------------------------------------------------------------------------

-- Comparing 2 stategies for encoding a HashMap
--
-- 1. Direct binary serialization
-- 2. Converting the Map to a [] (using toList) and then encoding it using
--    binary serilization
--
-- Strategy 2 is more efficient than Strategy 1.
-- Check why this is the case.

-- $(Serialize.deriveSerialize ''Map)

instance (Ord k, Serialize k, Serialize v) => Serialize (Map k v) where
    {-# INLINE size #-}
    size acc val = size acc (Map.toList val)
    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (Map.toList val)
    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, kvList) <- deserialize off arr end
        pure (off1, Map.fromList kvList)

--------------------------------------------------------------------------------
-- HashMap
--------------------------------------------------------------------------------

-- Comparing 2 stategies for encoding a HashMap
--
-- 1. Direct binary serialization
-- 2. Converting the HashMap to a [] (using toList) and then encoding it using
--    binary serilization
--
-- Compared to Strategy 1, Strategy 2 is more efficient for encoding and less
-- efficient for decoding.
--
-- Encoding is costlier in Strategy 1 because HashMap duplicates a about 20% of
-- data in the ADT.
--
-- Decoding is costlier in Strategy 2 as the HashMap needs to be constructed
-- again.
--
-- Encoding is extremely slow in strategy 1. This might also be becuase of too
-- much nesting. We can revisit this to find the exact cause.

{-
$(Serialize.deriveSerialize ''HashMap.Leaf)
$(Serialize.deriveSerialize ''HashMap.HashMap)
-}

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HashMap.HashMap k v) where
    {-# INLINE size #-}
    size acc val = size acc (HashMap.toList val)
    {-# INLINE serialize #-}
    serialize off arr val = serialize off arr (HashMap.toList val)
    {-# INLINE deserialize #-}
    deserialize off arr end = do
        (off1, kvList) <- deserialize off arr end
        pure (off1, HashMap.fromList kvList)

--------------------------------------------------------------------------------
-- Aeson.Value
--------------------------------------------------------------------------------

#if MIN_VERSION_aeson(2,0,0)
$(Serialize.deriveSerialize ''Aeson.Key)
$(Serialize.deriveSerialize ''Aeson.KeyMap)
#endif

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
