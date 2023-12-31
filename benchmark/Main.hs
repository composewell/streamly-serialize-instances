-- |
-- Copyright   : (c) 2022 Composewell technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Monad (unless, replicateM)
import Data.Function ((&))
import Data.String (fromString)
import Data.Word (Word8)
import Control.DeepSeq (NFData(..), deepseq, force)
import System.Random (randomRIO)
import Streamly.Data.Serialize.Instances ()
import Test.QuickCheck (Gen, generate, arbitrary)
import Streamly.Internal.Data.MutByteArray (Serialize, MutByteArray)

import qualified Streamly.Internal.Data.MutByteArray as MBA
import qualified Streamly.Data.Stream as Stream
import qualified Data.Text as TextS
import qualified Data.Text.Lazy as TextL
import qualified Data.ByteString as StrictByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector

import Test.Tasty.Bench
-- import Gauge

-------------------------------------------------------------------------------
-- Size helpers
-------------------------------------------------------------------------------

{-# INLINE getSize #-}
getSize :: forall a. Serialize a => a -> Int
getSize = MBA.addSizeTo 0

-------------------------------------------------------------------------------
-- Common helpers
-------------------------------------------------------------------------------

-- Parts of "f" that are dependent on val will not be optimized out.
{-# INLINE loop #-}
loop :: Int -> (a -> IO b) -> a -> IO ()
loop count f val = go count val
    where

    go n x = do
        if n > 0
        then f x >> go (n-1) x
        else return ()

-- The first arg of "f" is the environment which is not threaded around in the
-- loop.
{-# INLINE loopWith #-}
loopWith :: Int -> (env -> a -> IO b) -> env -> a -> IO ()
loopWith count f e val = go count val
    where

    go n x = do
        if n > 0
        then f e x >> go (n-1) x
        else return ()

benchSink :: NFData b => String -> Int -> (Int -> IO b) -> Benchmark
benchSink name times f = bench name (nfIO (randomRIO (times, times) >>= f))

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

{-# INLINE poke #-}
poke :: Serialize a => MutByteArray -> a -> IO ()
poke arr val = MBA.serializeAt 0 arr val >> return ()

{-# INLINE pokeTimes #-}
pokeTimes :: Serialize a => a -> Int -> IO ()
pokeTimes val times = do
    let n = getSize val
    arr <- MBA.new n
    loopWith times poke arr val

-- There is peoblem with using tasty. For 'Value', 'encode' goes to an infinite
-- loop. This is possibly because of how tasty deals with outliers.
--
-- This only happens when direct binary serilaizatin is used for HashMap.
{-# INLINE encode #-}
encode :: Serialize a => a -> IO ()
encode val = do
    let n = getSize val
    arr <- MBA.new n
    MBA.serializeAt 0 arr val >> return ()

{-# INLINE encodeTimes #-}
encodeTimes :: Serialize a => a -> Int -> IO ()
encodeTimes val times = loop times encode val

{-# INLINE peek #-}
peek :: forall a. (NFData a, Serialize a) =>
    (a, Int) -> MutByteArray -> IO ()
peek (_val, n) arr = do
        (_, val1 :: a) <- MBA.deserializeAt 0 arr n
        -- If the datatype is not deeply strict or deepseq is not used then use
        -- Equality.
        -- Ensure that we are actually constructing the type and using it. This
        -- is important, otherwise the structure is created and discarded, the
        -- cost of creation of the structure is not accounted. Otherwise we may
        -- just read the values and discard them. The comparison adds to the
        -- cost though. We could use deepseq but then we need to write
        -- instances of NFData and ensure that they are correct and perform
        -- well. Equality check also ensures correctness.
        {-
        if (val1 /= val)
        then error "peek: no match"
        else return ()
        -}
        val1 `deepseq` return ()

{-# INLINE peekTimes #-}
peekTimes :: (NFData a, Serialize a) => Int -> a -> Int -> IO ()
peekTimes n val times = do
    arr <- MBA.new n
    _ <- MBA.serializeAt 0 arr val
    loopWith times peek (val, n) arr

{-# INLINE roundtrip #-}
roundtrip :: forall a. (NFData a, Serialize a) => a -> IO ()
roundtrip val = do
    let n = getSize val
    arr <- MBA.new n
    _ <- MBA.serializeAt 0 arr val
    (_, val1 :: a) <- MBA.deserializeAt 0 arr n
    -- Do not remove this or use deepseq, see the comments in peek.
    {-
    if (val1 /= val)
    then error "roundtrip: no match"
    else return ()
    -}
    -- Note: deepseq is not needed if the datatype is strict
    val1 `deepseq` return ()

{-# INLINE roundtripTimes #-}
roundtripTimes :: (NFData a, Serialize a) => a -> Int -> IO ()
roundtripTimes val times = loop times roundtrip val

--------------------------------------------------------------------------------
-- Benchmarks helpers
--------------------------------------------------------------------------------

bencher
    :: (NFData b, Serialize b)
    => String
    -> b
    -> Int
    -> Benchmark
bencher gname val times =
    bgroup gname
       [ benchSink "poke" times (pokeTimes val)
       , benchSink "encode" times (encodeTimes val)
       , let !n = getSize val
          in benchSink "peek" times (peekTimes n val)
       , benchSink "roundtrip" times (roundtripTimes val)
       ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Environment
    let !intList = force ([1 .. 1000] :: [Int])
    !strictText <- genStrictText 1000
    !lazyText <- do
        testSList <- Stream.replicateM 20 (genStrictText 50) & Stream.toList
        pure $ force $ TextL.fromChunks testSList
    !strictByteString <- genStrictByteString 1000
    !lazyByteString <- genLazyByteString 20 50 -- 20 chunks of bytestring that
                                               -- are of length 50 each.
    !vectorA <- genVectorA 1000
    let !aesonValue = genAesonValue 100

    -- Asserts
    unless (TextS.length strictText == 1000)
         (error "TextS.length strictText == 1000")
    unless (TextL.length lazyText == 1000)
         (error "TextL.length lazyText == 1000")
    unless (Vector.length vectorA == 1000)
         (error "Vector.length vectorA == 1000")
    unless (StrictByteString.length strictByteString == 1000)
         (error "StrictByteString.length strictByteString == 1000")
    unless (LazyByteString.length lazyByteString == 1000)
         (error "LazyByteString.length lazyByteString == 1000")

    -- Benchmarks
    defaultMain
        [ bencher "[Int]" intList 100
        , bencher "Strict.Text" strictText 100
        , bencher "Lazy.Text" lazyText 100
        , bencher "Strict.ByteString" strictByteString 100
        , bencher "Lazy.ByteString" lazyByteString 100
        , bencher "Vector" vectorA 100
        , bencher "Aeson.Value" aesonValue 100
        ]

    where

    genStrictText n = do
        let genChar = generate (arbitrary :: Gen Char)
        Stream.replicateM n genChar
            & Stream.toList
            & fmap (force . TextS.pack)

    genStrictByteString n = do
        let genWord8 = generate (arbitrary :: Gen Word8)
        Stream.replicateM n genWord8
            & Stream.toList
            & fmap (force . StrictByteString.pack)

    genLazyByteString n m = do
        LazyByteString.fromChunks <$> replicateM n (genStrictByteString m)

    genVectorA n = do
        let genInt = generate (arbitrary :: Gen Int)
        vectorA <- Vector.replicateM n genInt
        return $ force vectorA

    genAesonValue :: Int -> Aeson.Value
    genAesonValue 0 = Aeson.object []
    genAesonValue n =
        Aeson.object
            [ fromString "array"  Aeson..= ([1, 2, 3, 4, 5] :: [Int])
            , fromString "string" Aeson..= ("abcde" :: String)
            , fromString "number" Aeson..= (23.45 :: Double)
            , fromString "bool" Aeson..= (True :: Bool)
            , fromString "object" Aeson..= genAesonValue (n - 1)
            ]
