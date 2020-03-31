module Data.CharSet where

import Prelude
import Data.Array as A
import Data.Enum (fromEnum)
import Data.HashSet (HashSet)
import Data.HashSet as S
import Data.Int.Bits (shr)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.Tuple (Tuple(..))



data CharSet = CharSet
  Boolean             -- Whether ByteSet and HashSet are negated
  (Array Boolean)     -- Set of head bytes
                      -- fixed length array of 128 to hold all ASCII characters
  (HashSet Int)       -- Set of CodePoint in the charset


charSet :: Boolean -> Array CodePoint -> CharSet
charSet b cs = CharSet b
                       (A.updateAtIndices ts fs)
                       (S.fromFoldable $ map fromEnum cs)
  where
  fs = A.replicate 128 false
  ts = map (\c -> Tuple (headBytes c) true) cs


headBytes :: CodePoint -> Int
headBytes c = case fromEnum c of
  i | i < 0x7f   -> i
  i | i < 0x7ff  -> 0x80 + (i `shr` 6)
  i | i < 0xffff -> 0xe0 + (i `shr` 12)
  i              -> 0xf0 + (i `shr` 18)


complement :: CharSet -> CharSet
complement = case _ of
  CharSet true b s  -> CharSet false b s
  CharSet false b s -> CharSet true b s


member :: CodePoint -> CharSet -> Boolean
member c cs = case cs of
  CharSet true b s -> case fromEnum c of
    i | i < 0x7f -> A.index b i == Just true
    i            -> S.member i s
  CharSet false b s -> case fromEnum c of
    i | i < 0x7f -> A.index b i /= Just true
    i            -> not $ S.member i s
