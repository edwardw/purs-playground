module Test.Main where

import Prelude
import Data.Array as A
import Data.BigInt (BigInt, fromInt, fromString)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List.Lazy as ZL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import WordNumbers (Count, Deriv(..), Nat(..), Volume, Wrap(..), ZSL, answer, answerUnsorted, ten6, ten9)

fromString' :: String -> BigInt
fromString' = fromMaybe (fromInt 0) <<< fromString

main :: Effect Unit
main = runTest do
  suite "word numbers, billion approaches" do
    test "part 1" do
      Assert.equal
        (ZL.length $ unwrap (ten6 :: ZSL))
        999999
      Assert.equal
        (map fromCharArray <<< ZL.head $ unwrap (ten6 :: ZSL))
        (Just "one")
      Assert.equal
        (map fromCharArray <<< ZL.last $ unwrap (ten6 :: ZSL))
        (Just "ninehundredninetyninethousandninehundredninetynine")
      Assert.equal
        (foldr (+) 0 <<< map A.length $ unwrap (ten6 :: ZSL))
        44872000
    test "part 2" do
      Assert.equal
        (ten9 :: Count)
        (Nat $ fromString' "999999999")
      Assert.equal
        (ten9 :: Deriv Count Volume)
        (Deriv (Nat $ fromString' "999999999") (Wrap <<< Nat $ fromString' "70305000000"))
    test "part 3" do
      Assert.equal
        (answerUnsorted $ fromString' "51000000000")
        ["sevenhundredthirtytwomil","l","ionsevenhundredninetysixthousandthreehundredsixtysix"]
    test "part 4" do
      Assert.equal
        answer
        (Right (Tuple "sixhundredseventysixmillionsevenhundredfortysixthousandfivehundredseventyfive"
                      (fromString' "413540008163475743")))
