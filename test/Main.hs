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

import qualified Data.Text as TextS
import qualified Data.Text.Lazy as TextL
import qualified Data.Vector as Vector

import Data.ByteString as StrictByteString
import Data.ByteString.Lazy as LazyByteString

import Test.Hspec.QuickCheck
import Test.Hspec as H

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

    prop "Strict Text"
        $ \(x :: TextS.Text) -> roundtrip x

    prop "Vector"
        $ \(x :: Vector.Vector String) -> roundtrip x

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
