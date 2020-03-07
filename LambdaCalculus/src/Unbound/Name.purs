module Unbound.Name where

import Prelude
import Data.Typeable (class Typeable, class Typeable1, TypeRep, mkTyRep, typeOf)
import Data.Typelevel.Undefined (undefined)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- | A `Name a` stands for a term of type `a`, which is used to distinguish
-- | these names from names that may stand for other sort of terms.
-- |
-- | Names may either be free or bound. Free names may be extracted from
-- | patterns. Bound names cannot be.
data Name a
    -- free names
  = Fn String Int
    -- bound names / binding level + pattern index
  | Bn Int Int

derive instance eqName :: Eq (Name a)
derive instance ordName :: Ord (Name a)

instance showName :: Show (Name a) where
  show = case _ of
    Fn "" n -> "_" <> show n
    Fn x 0  -> x
    Fn x n  -> x <> show n
    Bn x y  -> show x <> "@" <> show y

instance typeable1Name :: Typeable1 Name where
  typeOf1 _ = mkTyRep "Unbound" "Name"


isFreename :: forall a. Name a -> Boolean
isFreename = case _ of
  Fn _ _ -> true
  Bn _ _ -> false


string2Name :: forall a. String -> Name a
string2Name s = makeName s 0


s2n :: forall a. String -> Name a
s2n = string2Name


makeName :: forall a. String -> Int -> Name a
makeName = Fn


name2Int :: forall a. Name a -> Int
name2Int = case _ of
  Fn _ i -> i
  Bn _ _ -> undefined


name2String :: forall a. Name a -> String
name2String = case _ of
  Fn s _ -> s
  Bn _ _ -> undefined


-- | An `AnyName` is a name that stands for a term of some (existentially
-- | hidden) type.
data AnyName = AnyName (Exists Name)

-- | Existential quantification using the universal one.
-- |
-- | By Oleg Grenrus in "More GADTs in PureScript"
newtype Exists f = Exists { runExists :: forall r. (forall a. Typeable a => f a -> r) -> r }

mkExists :: forall f a. Typeable1 f => Typeable a => f a -> Exists f
mkExists x = Exists { runExists: \f -> f x }

instance eqAnyName :: Eq AnyName where
  eq (AnyName (Exists a)) (AnyName (Exists b)) = a' == b'
    where
    a' = a.runExists repr
    b' = b.runExists repr

instance ordAnyName :: Ord AnyName where
  compare (AnyName (Exists a)) (AnyName (Exists b)) = compare a' b'
    where
    a' = a.runExists repr
    b' = b.runExists repr

repr :: forall a. Typeable a => Name a -> { t :: TypeRep, v :: Name Int }
repr x = { t: typeOf (Proxy :: Proxy (Name a)), v: unsafeCoerce x }
