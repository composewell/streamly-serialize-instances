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
import Streamly.Internal.Data.MutByteArray (Serialize(..), MutByteArray)

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Aeson
#endif

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import qualified Streamly.Internal.Data.MutByteArray as MBA

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance Serialize (Fixed a)|])
$(MBA.deriveSerialize [d|instance Serialize Day|])
$(MBA.deriveSerialize [d|instance Serialize TimeOfDay|])
$(MBA.deriveSerialize [d|instance Serialize LocalTime|])
$(MBA.deriveSerialize [d|instance Serialize DiffTime|])
$(MBA.deriveSerialize [d|instance Serialize UTCTime|])

--------------------------------------------------------------------------------
-- Scientific
--------------------------------------------------------------------------------

$(MBA.deriveSerialize [d|instance Serialize Scientific|])

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

-- $(MBA.deriveSerialize [d|instance Serialize Map|])

instance (Ord k, Serialize k, Serialize v) => Serialize (Map k v) where
    {-# INLINE addSizeTo #-}
    addSizeTo acc val = addSizeTo acc (Map.toList val)
    {-# INLINE serializeAt #-}
    serializeAt off arr val = serializeAt off arr (Map.toList val)
    {-# INLINE deserializeAt #-}
    deserializeAt off arr end = do
        (off1, kvList) <- deserializeAt off arr end
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
$(MBA.deriveSerialize [d|instance Serialize HashMap.Leaf|])
$(MBA.deriveSerialize [d|instance Serialize HashMap.HashMap|])
-}

instance (Eq k, Hashable k, Serialize k, Serialize v) =>
         Serialize (HashMap.HashMap k v) where
    {-# INLINE addSizeTo #-}
    addSizeTo acc val = addSizeTo acc (HashMap.toList val)
    {-# INLINE serializeAt #-}
    serializeAt off arr val = serializeAt off arr (HashMap.toList val)
    {-# INLINE deserializeAt #-}
    deserializeAt off arr end = do
        (off1, kvList) <- deserializeAt off arr end
        pure (off1, HashMap.fromList kvList)

--------------------------------------------------------------------------------
-- Aeson.Value
--------------------------------------------------------------------------------

#if MIN_VERSION_aeson(2,0,0)
$(MBA.deriveSerialize [d|instance Serialize Aeson.Key|])
$(MBA.deriveSerialize
      [d|instance Serialize v => Serialize (Aeson.KeyMap v)|])
#endif

$(MBA.deriveSerialize [d|instance Serialize Aeson.Value|])

--------------------------------------------------------------------------------
-- Vector.Vector
--------------------------------------------------------------------------------

instance Serialize a => Serialize (Vector.Vector a) where

    {-# INLINE addSizeTo #-}
    addSizeTo :: Int -> (Vector.Vector a) -> Int
    addSizeTo acc = Vector.foldl' addSizeTo (acc + MBA.sizeOf (Proxy :: Proxy Int64))

    {-# INLINE serializeAt #-}
    serializeAt :: Int -> MutByteArray -> (Vector.Vector a) -> IO Int
    serializeAt off arr val = do
        let len = Vector.length val
        finalOffset <-
            Vector.foldM'
                (\curOff v -> serializeAt curOff arr v)
                (off + MBA.sizeOf (Proxy :: Proxy Int64))
                val
        MBA.pokeAt off arr ((fromIntegral :: Int -> Int64) len)
        pure finalOffset

    {-# INLINE deserializeAt #-}
    deserializeAt :: Int -> MutByteArray -> Int -> IO (Int, Vector.Vector a)
    deserializeAt off arr s = do

        (off1, len64) <- deserializeAt off arr s
        let len = (fromIntegral :: Int64 -> Int) len64
        val <- MVector.new len
        (off2, val1) <- fillVector len 0 off1 val
        val2 <- Vector.freeze val1
        pure (off2, val2)

        where

        fillVector len acc off1 val
            | acc >= len = pure (off1, val)
            | otherwise = do
                (off2, v) <- deserializeAt off1 arr s
                MVector.write val acc v
                fillVector len (acc + 1) off2 val
