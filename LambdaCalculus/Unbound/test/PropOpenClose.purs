module Test.PropOpenClose (testOpenClose) where

import Prelude
import Control.Lazy (fix)
import Data.Array (uncons, (:))
import Data.Char.Gen (genAlphaLowercase)
import Data.Foldable (elem)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (fromNonEmpty, (:|))
import Data.String.CodeUnits as SCU
import Data.Typeable (class Typeable, class Typeable1, mkTyRep)
import Effect (Effect)
import Test.QuickCheck (Result(..), (<?>), (===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf1, elements, oneOf)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.QuickCheck (quickCheck)
import Unbound.LocallyNameless (class Alpha, Name, aeq, close, fv, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSwaps, initialCtx, makeName, namePatFind, nthPatFind, open)


isFreeIn :: forall a b. Typeable a => Alpha b => Name a -> b -> Boolean
isFreeIn v t = v `elem` (fv t)


notFreeIn :: forall a b. Typeable a => Alpha b => Name a -> b -> Boolean
notFreeIn = not <<< isFreeIn


testAeq :: forall a. Alpha a => a -> a -> Result
testAeq x y = (x `aeq` y)
          <?> (show x <> " not alpha equivalent to " <> show y)

infix 5 testAeq as =~=


testNotFreeIn :: forall a b. Typeable a => Alpha b => Name a -> b -> Result
testNotFreeIn v t = (v `notFreeIn` t)
                <?> (show v <> " should not be free in " <> show t)


-- Wrapper around 'Name a' that has an Arbitrary instance that generates free
-- names. Note that this doesn't guarantee *freshness*.  The name may clash with
-- some other one. But it will never be a bound name.
newtype FreeName a = FreeName (Name a)

instance showFreeName :: Show a => Show (FreeName a) where
  show (FreeName v) = "FreeName " <> show v

unFreeName :: forall a. FreeName a -> Name a
unFreeName (FreeName v) = v


instance arbFreeName :: Arbitrary (FreeName a) where
  arbitrary = do
    s <- arrayOf1 genAlphaLowercase
    n <- arbitrary
    let s' = SCU.fromCharArray $ fromNonEmpty (:) s
    pure <<< FreeName $ makeName s' n


-- Example data structure, with no bound names
data T a
  = Leaf a
  | V (Name (T a))
  | B (T a) (T a)

derive instance genericT :: Generic (T a ) _


instance showT :: Show a => Show (T a) where
  show x = genericShow x

instance typeable1T :: Typeable1 T where
  typeOf1 _ = mkTyRep "PropOpenClose" "T"

instance alphaT :: (Typeable a, Alpha a) => Alpha (T a) where
  aeq' ctx x y         = genericAeq ctx x y
  fvAny' ctx nfn x     = genericFvAny ctx nfn x
  close ctx b x        = genericClose ctx b x
  open ctx b x         = genericOpen ctx b x
  isPat x              = genericIsPat x
  isTerm x             = genericIsTerm x
  isEmbed _            = false
  nthPatFind x         = genericNthPatFind x
  namePatFind x        = genericNamePatFind x
  swaps' ctx perm x    = genericSwaps ctx perm x
  freshen' ctx x       = genericFreshen ctx x
  lfreshen' ctx a cont = genericLFreshen ctx a cont
  acompare' ctx x y    = genericACompare ctx x y

instance arbT :: Arbitrary a => Arbitrary (T a) where
  arbitrary = fix $ \p -> oneOf (hd :| tl p)
    where
    hd   = Leaf <$> arbitrary
    tl p = [ (V <<< unFreeName) <$> arbitrary
           , B <$> p <*> p
           ]


-- picks out one of the free variables of a tree and run test against it
arbVarsOf :: forall a. Typeable a => Alpha a => T a -> ((Name (T a)) -> Result) -> Gen Result
arbVarsOf t f = case uncons (fv t) of
  Just { head, tail } -> do
    v <- elements (head :| tail)
    pure $ f v
  Nothing -> pure Success


-- manual implementation of fv
fvSpec :: forall a. T a -> Array (Name (T a))
fvSpec t = case t of
  Leaf _  -> mempty
  V v     -> [v]
  B t1 t2 -> fvSpec t1 <> fvSpec t2



--------------------------------------------------------------------------------
-- Properties ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- every tree is alpha-equivalent to itself
propRefl :: T Int -> Result
propRefl x = x =~= x


-- generic fv gives the same answer as fvSpec
propfvSpec :: T Int -> Result
propfvSpec t = fv t === fvSpec t


-- if a name is already free opening it has no effect
propOpenIdempotent :: T Int -> Gen Result
propOpenIdempotent t = arbVarsOf t $ \v -> open initialCtx (nthPatFind v) t =~= t


-- if you close over a variable, then it is no longer free
propCloseBinds :: T Int -> Gen Result
propCloseBinds t =
  arbVarsOf t $ \v -> v `testNotFreeIn` close initialCtx (namePatFind v) t

testOpenClose :: Effect Unit
testOpenClose = runTest do
  suite "Property of open/close" do
    test "alpha-equivalent is reflective" do
      quickCheck propRefl
    test "fv" do
      quickCheck propfvSpec
    test "open an free variable is idempotent" do
      quickCheck propOpenIdempotent
    test "close means close" do
      quickCheck propCloseBinds
