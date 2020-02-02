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
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import WordNumbers (Count, Deriv(..), Nat(..), Volume, Wrap(..), ZSL, answer, answerUnsorted, ten6, ten9)

fromString' :: String -> BigInt
fromString' = fromMaybe (fromInt 0) <<< fromString

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "part 1" do
    it "ten6 length" do
      let ten6Len = ZL.length $ unwrap (ten6 :: ZSL)
      ten6Len `shouldEqual` 999999
    it "ten6 head" do
      let hd = map fromCharArray <<< ZL.head $ unwrap (ten6 :: ZSL)
      hd `shouldEqual` Just "one"
    it "ten6 last" do
      let hd = map fromCharArray <<< ZL.last $ unwrap (ten6 :: ZSL)
      hd `shouldEqual` Just "ninehundredninetyninethousandninehundredninetynine"
    it "ten6 all characters count" do
      let all = foldr (+) 0 <<< map A.length $ unwrap (ten6 :: ZSL)
      all `shouldEqual` 44872000
  describe "part 2" do
    it "ten9 length" do
      let ten9Len = ten9 :: Count
      ten9Len `shouldEqual` (Nat $ fromString' "999999999")
    it "ten9 length and all" do
      let all = ten9 :: Deriv Count Volume
      all `shouldEqual` Deriv (Nat $ fromString' "999999999") (Wrap <<< Nat $ fromString' "70305000000")
  describe "part 3" do
    it "answerUnsorted" do
      let answer = answerUnsorted $ fromString' "51000000000"
      answer `shouldEqual` ["sevenhundredthirtytwomil","l","ionsevenhundredninetysixthousandthreehundredsixtysix"]
  describe "part 4" do
    it "answer" $
      answer `shouldEqual` (Right (Tuple "sixhundredseventysixmillionsevenhundredfortysixthousandfivehundredseventyfive"
                                         (fromString' "413540008163475743")))
