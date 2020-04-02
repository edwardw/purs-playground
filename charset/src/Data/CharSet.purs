module Data.CharSet
  ( CharSet(..)
  , complement
  , member
  , fromFoldable
  ) where

import Prelude
import Data.Array as A
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, foldr)
import Data.HashSet (HashSet)
import Data.HashSet as S
import Data.Int.Bits (shr)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint)
import Data.Tuple (Tuple(..))



data CharSet = CharSet
  Boolean         -- Whether Array and HashSet are negated
  (Array Boolean) -- Set of head bytes
                  -- fixed length array of 128 to look up ASCII characters
  (HashSet Int)   -- Set of CodePoint in the charset
                  -- CodePoint is not Hashable? That is a surprise.


charSet :: forall f. Foldable f => Boolean -> f CodePoint -> CharSet
charSet b cs = CharSet b
                       (A.updateAtIndices ts fs)
                       (foldr (S.insert <<< fromEnum) S.empty cs)
  where
  fs = A.replicate 128 false
  ts = map (\c -> Tuple (headBytes c) true) (A.fromFoldable cs)


headBytes :: CodePoint -> Int
headBytes c = case fromEnum c of
  i | i < 0x7f   -> i
  i | i < 0x7ff  -> 0x80 + (i `shr` 6)
  i | i < 0xffff -> 0xe0 + (i `shr` 12)
  i              -> 0xf0 + (i `shr` 18)


complement :: CharSet -> CharSet
complement (CharSet b a s) = CharSet (not b) a s


member :: CodePoint -> CharSet -> Boolean
member c cs = case cs of
  CharSet true a s -> case fromEnum c of
    i | i < 0x7f -> A.index a i == Just true
    i            -> S.member i s
  CharSet false a s -> case fromEnum c of
    i | i < 0x7f -> A.index a i == Just false
    i            -> not $ S.member i s


fromFoldable :: forall f. Foldable f => f CodePoint -> CharSet
fromFoldable = charSet true
