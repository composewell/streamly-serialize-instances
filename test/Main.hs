-- |
-- Copyright   : (c) 2022 Composewell technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Main (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import System.Random (randomRIO)
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import Streamly.Data.Serialize.Instances ()
import Test.QuickCheck.Instances ()

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as MBA

import Data.Time (UTCTime)
import Data.Scientific (Scientific)

import qualified Data.Text as TextS
import qualified Data.Text.Lazy as TextL
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector

import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString

import qualified Data.HashMap.Strict as SHashMap

#if (!(MIN_VERSION_aeson(2,0,3)))
import Test.QuickCheck
#endif
import Test.Hspec.QuickCheck
import Test.Hspec as H

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

#if (!(MIN_VERSION_aeson(2,0,3)))

instance Arbitrary Aeson.Value where
    arbitrary = go 5

        where

        go :: Int -> Gen Aeson.Value
        go 0 = pure $ Aeson.object []
        go n = do
            obj <- go (n - 1)
            arr <- sequence $ map go [(n - 1),(n - 2) .. 0]
            (arbText :: TextS.Text) <- arbitrary
            pure $ Aeson.object
                [ TextS.pack "array"  Aeson..= arr
                , TextS.pack "string" Aeson..= arbText
                , TextS.pack "number" Aeson..= (23.45 :: Double)
                , TextS.pack "bool" Aeson..= (True :: Bool)
                , TextS.pack "object" Aeson..= obj
                ]

#endif

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

poke ::
       forall a. MBA.Serialize a
    => a
    -> IO (MutByteArray, Int, Int)
poke val = do
    let sz = MBA.addSizeTo 0 val

    let excessSize = 100
    randomOff <- randomRIO (10, excessSize)
    -- Use a proper slice to test instead of the array directly. This will catch
    -- any hardcoded 0 offsets
    let arrSize = sz + excessSize
        serStartOff = randomOff
        serEndOff = randomOff + sz
    arr <- MBA.new arrSize

    off1 <- MBA.serializeAt serStartOff arr val
    off1 `shouldBe` serEndOff
    pure (arr, serStartOff, serEndOff)

peekAndVerify ::
       forall a. (Eq a, Show a, MBA.Serialize a)
    => (MutByteArray, Int, Int)
    -> a
    -> IO ()
peekAndVerify (arr, serStartOff, serEndOff) val = do
    (off2, val2) <- MBA.deserializeAt serStartOff arr serEndOff
    val2 `shouldBe` val
    off2 `shouldBe` serEndOff
    let slice = Array.Array arr serStartOff serEndOff
    val `shouldBe` Array.deserialize slice
    clonedSlice <- Array.clone slice
    val `shouldBe` Array.deserialize clonedSlice

roundtrip
    :: forall a. (Eq a, Show a, MBA.Serialize a)
    => a
    -> IO ()
roundtrip val = do

    val `shouldBe` Array.deserialize (Array.serialize val)

    res <- poke val
    peekAndVerify res val

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testCases :: Spec
testCases = do

    prop "UTCTime"
        $ \(x :: UTCTime) -> roundtrip x

    prop "Scientific"
        $ \(x :: Scientific) -> roundtrip x

    prop "Strict Text"
        $ \(x :: TextS.Text) -> roundtrip x

    prop "Vector"
        $ \(x :: Vector.Vector String) -> roundtrip x

    prop "Strict HashMap"
        $ \(x :: SHashMap.HashMap TextS.Text TextS.Text) -> roundtrip x

    prop "Aeson"
        $ \(x :: Aeson.Value) -> roundtrip x

    prop "Lazy Text"
        $ \(x :: TextL.Text) -> roundtrip x

    prop "Strict ByteString"
        $ \(x :: StrictByteString.ByteString) -> roundtrip x

    prop "Lazy ByteString"
        $ \(x :: LazyByteString.ByteString) -> roundtrip x

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Main"

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
