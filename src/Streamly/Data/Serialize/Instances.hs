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
import Streamly.Data.Serialize.Instances.ByteString ()
import Streamly.Internal.Data.Serialize (Serialize(..))

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Streamly.Internal.Data.Serialize.TH as Serialize

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
