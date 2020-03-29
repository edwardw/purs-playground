module Benchmarks.Main where

import Prelude
import Benchotron.Core (Benchmark, benchFn, benchFn', mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Array ((..))
import Data.Foldable (foldr)
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf)


benchInsert :: Benchmark
benchInsert = mkBenchmark
  { slug: "insertion"
  , title: "Insert into an empty map"
  , sizes: (1..5) <#> (_ * 1000)
  , sizeInterpretation: "Number of elements to insert"
  , inputsPerSize: 1
  , gen: \n -> vectorOf n arbitrary :: Gen (Array (Tuple Int String))
  , functions: [ benchFn "Map.insert" (foldr (\(Tuple k v) -> Map.insert k v) Map.empty)
               , benchFn "IntMap.insert" (foldr (\(Tuple k v) -> IntMap.insert k v) IntMap.empty)
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
                          (\m -> Map.keys m # foldr (\k -> Map.delete k) m)
                          Map.fromFoldable
               , benchFn' "IntMap.delete"
                          (\m -> IntMap.indices m # foldr (\k -> IntMap.delete k) m)
                          IntMap.fromAssocArray
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
          m <- chooseInt 0 n
          left <- vectorOf m arbitrary
          right <- vectorOf (n-m) arbitrary
          let res = Tuple left right :: Tuple (Array (Tuple Int String)) (Array (Tuple Int String))
          pure res
  , functions: [ benchFn' "Map.union"
                          (\(Tuple m1 m2) -> Map.union m1 m2)
                          (Map.fromFoldable *** Map.fromFoldable)
               , benchFn' "IntMap.unionLeft"
                          (\(Tuple m1 m2) -> IntMap.unionLeft m1 m2)
                          (IntMap.fromAssocArray *** IntMap.fromAssocArray)
               ]
  }


main :: Effect Unit
main = runSuite [benchInsert, benchDelete, benchUnion]
