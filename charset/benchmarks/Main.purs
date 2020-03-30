module Benchmarks.Main where

import Prelude
import Benchotron.Core (Benchmark, benchFn, benchFn', mkBenchmark)
import Benchotron.UI.Console (runSuite)
import Data.Array ((..))
import Data.Foldable (foldr)
import Data.HashMap as HashMap
import Data.IntMap as IntMap
import Data.Map as Map
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple(..))
import Effect (Effect)
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
  , functions: [ benchFn "Map.insert" (foldr (\(Tuple k v) -> Map.insert k v) Map.empty)
               , benchFn "IntMap.insert" (foldr (\(Tuple k v) -> IntMap.insert k v) IntMap.empty)
               , benchFn "HashMap.insert" (foldr (\(Tuple k v) -> HashMap.insert k v) HashMap.empty)
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
               , benchFn' "HashMap.delete"
                          (\m -> HashMap.keys m # foldr (\k -> HashMap.delete k) m)
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
                          (\(Tuple m1 m2) -> Map.union m1 m2)
                          (Map.fromFoldable *** Map.fromFoldable)
               , benchFn' "IntMap.unionLeft"
                          (\(Tuple m1 m2) -> IntMap.unionLeft m1 m2)
                          (IntMap.fromAssocArray *** IntMap.fromAssocArray)
               , benchFn' "HashMap.union"
                          (\(Tuple m1 m2) -> HashMap.union m1 m2)
                          (HashMap.fromFoldable *** HashMap.fromFoldable)
               ]
  }


main :: Effect Unit
main = runSuite [benchInsert, benchDelete, benchUnion]
