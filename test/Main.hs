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
import Streamly.Internal.Data.Unbox (MutableByteArray, newBytes)
import Streamly.Data.Serialize.Instances ()
import Test.QuickCheck.Instances ()

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Serialize as Serialize

import Data.Time (UTCTime)
import Data.Scientific (Scientific)

import qualified Data.Text as TextS
import qualified Data.Text.Lazy as TextL
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector

import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString

import qualified Data.HashMap.Strict as SHashMap
import qualified Data.HashMap.Internal.Array as HArr

#if (!(MIN_VERSION_aeson(2,0,3)))
import Test.QuickCheck
#endif
import Test.Hspec.QuickCheck
import Test.Hspec as H

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Eq a => Eq (HArr.Array a) where
    (==) a b = HArr.sameArray1 (==) a b

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
       forall a. Serialize.Serialize a
    => a
    -> IO (MutableByteArray, Int, Int)
poke val = do
    let sz = Serialize.size 0 val

    let excessSize = 100
    randomOff <- randomRIO (10, excessSize)
    -- Use a proper slice to test instead of the array directly. This will catch
    -- any hardcoded 0 offsets
    let arrSize = sz + excessSize
        serStartOff = randomOff
        serEndOff = randomOff + sz
    arr <- newBytes arrSize

    off1 <- Serialize.serialize serStartOff arr val
    off1 `shouldBe` serEndOff
    pure (arr, serStartOff, serEndOff)

peekAndVerify ::
       forall a. (Eq a, Show a, Serialize.Serialize a)
    => (MutableByteArray, Int, Int)
    -> a
    -> IO ()
peekAndVerify (arr, serStartOff, serEndOff) val = do
    (off2, val2) <- Serialize.deserialize serStartOff arr serEndOff
    val2 `shouldBe` val
    off2 `shouldBe` serEndOff
    let slice = Array.Array arr serStartOff serEndOff
    val `shouldBe` Serialize.decode slice
    clonedSlice <- Array.clone slice
    val `shouldBe` Serialize.decode clonedSlice

roundtrip
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => a
    -> IO ()
roundtrip val = do

    val `shouldBe` Serialize.decode (Serialize.encode val)

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

    prop "HashMap (Array Int)"
        $ \(x :: [Int]) -> roundtrip (HArr.fromList (length x) x)

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
