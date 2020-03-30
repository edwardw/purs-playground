module Benchmarks.Main where

import Prelude
import Benchotron.Core (Benchmark, benchFn, benchFn', mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Array ((..))
import Data.Array as A
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Foldable (foldr)
import Data.HashMap as HashMap
import Data.HashSet as HashSet
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..), uncurry)
import Data.UInt as UInt
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, vectorOf)


benchInsert :: Benchmark
benchInsert = mkBenchmark
  { slug: "insertion"
  , title: "insert into an empty map"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements to insert"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary :: Gen (Array (Tuple Int String))
  , functions: [ benchFn "Map.insert" (foldr (uncurry Map.insert) Map.empty)
               , benchFn "IntMap.insert" (foldr (uncurry IntMap.insert) IntMap.empty)
               , benchFn "HashMap.insert" (foldr (uncurry HashMap.insert) HashMap.empty)
               ]
  }


benchDelete :: Benchmark
benchDelete = mkBenchmark
  { slug: "delete"
  , title: "delete all entries in a map one by one"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements to delete"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary :: Gen (Array (Tuple Int String))
  , functions: [ benchFn' "Map.delete"
                          (\m -> Map.keys m # foldr Map.delete m)
                          Map.fromFoldable
               , benchFn' "IntMap.delete"
                          (\m -> IntMap.indices m # foldr IntMap.delete m)
                          IntMap.fromAssocArray
               , benchFn' "HashMap.delete"
                          (\m -> HashMap.keys m # foldr HashMap.delete m)
                          HashMap.fromFoldable
               ]
  }


benchUnion :: Benchmark
benchUnion = mkBenchmark
  { slug: "union"
  , title: "union two maps"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements in the result map"
  , inputsPerSize: 1
  , gen: \n -> do
          left <- vectorOf (n `div` 2) arbitrary
          right <- vectorOf (n `div` 2) arbitrary
          let res = Tuple left right :: Tuple (Array (Tuple Int String)) (Array (Tuple Int String))
          pure res
  , functions: [ benchFn' "Map.union"
                          (uncurry Map.union)
                          (Map.fromFoldable *** Map.fromFoldable)
               , benchFn' "IntMap.unionLeft"
                          (uncurry IntMap.unionLeft)
                          (IntMap.fromAssocArray *** IntMap.fromAssocArray)
               , benchFn' "HashMap.union"
                          (uncurry HashMap.union)
                          (HashMap.fromFoldable *** HashMap.fromFoldable)
               ]
  }


benchMember :: Benchmark
benchMember = mkBenchmark
  { slug: "membership"
  , title: "check membership of a set"
  , sizes: [8, 16, 32, 64, 128]
  , sizeInterpretation: "Number of elements in the set"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n (flip mod n <$> arbitrary) :: Gen (Array Int)
  , functions: [ benchFn' "HashSet.member"
                          (\(Tuple a s) -> flip HashSet.member s <$> a)
                          (\a -> Tuple a (HashSet.fromFoldable a))
               , benchFn "Array"
                         (\a -> isJust <$> A.index a <$> a)
               , benchFn' "ArrayBuffer"
                          (\(Tuple a arr) ->
                            isJust <$> (\i -> unsafePerformEffect (ArrayBuffer.at arr i)) <$> a
                          )
                          (\a ->
                            let arr = (unsafePerformEffect (ArrayBuffer.fromArray $ map UInt.fromInt a)) :: Uint8Array
                            in Tuple a arr
                          )
               ]
  }


main :: Effect Unit
main = runSuite [benchInsert, benchDelete, benchUnion, benchMember]
