module Unbound.LocallyNameless.Name where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, class Typeable1, TypeRep, mkTyRep, typeOf)
import Effect.Exception.Unsafe (unsafeThrow)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)


-- | `Name`s are things that are get bound. The type parameter is a tag, or
-- | *sort*, which tells us what sorts of things this name may stand for. The
-- | sort must be typeable.
-- |
-- | To hide the sort of a name, use `AnyName`.
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


isFreeName :: forall a. Name a -> Boolean
isFreeName = case _ of
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
  Bn _ _ -> unsafeThrow "Internal Error: cannot call name2Int for bound names"


name2String :: forall a. Name a -> String
name2String = case _ of
  Fn s _ -> s
  Bn _ _ -> unsafeThrow "Internal Error: cannot call name2String for bound names"



-- | A name with a hidden (existentially quantified) sort. To hide the sort of
-- | a name, use the `AnyName` constructor directly; to extract a name with a
-- | hidden sort, use 'toSortedName'.
-- |
-- | NB: Purescript has no syntax for existential type (yet). To mimic it, one
-- | has to resort to Data.Exists package or Oleg Grenrus' construction in
-- | "More GADTs in PureScript":
-- |
-- |    ```
-- |    newtype Exists f = Exists { runExists :: forall r. (forall a. f a -> r) -> r }
-- |    mkExists :: forall f a. f a -> Exists f
-- |    mkExists x = Exists { runExists: \f -> f x }
-- |    ```
-- |
-- | But it seems there is no way to lift `runExists` into a Monad. So instead,
-- | a data structure with explicit type tag is used.
newtype AnyName = AnyName (Tuple TypeRep (Name Int))


instance eqAnyName :: Eq AnyName where
  eq (AnyName (Tuple t1 v1)) (AnyName (Tuple t2 v2)) = t1 == t2 && v1 == v2

instance ordAnyName :: Ord AnyName where
  compare (AnyName nm1) (AnyName nm2) = compare nm1 nm2

instance showAnyName :: Show AnyName where
  show (AnyName (Tuple _ v)) = show v


mkAnyName :: forall a. Typeable a => Name a -> AnyName
mkAnyName nm = AnyName $ Tuple (typeOf (Proxy :: Proxy (Name a)))
                               (unsafeCoerce nm)


toSortedName :: forall a. Typeable a => AnyName -> Maybe (Name a)
toSortedName (AnyName (Tuple t v)) =
  if t == typeOf (Proxy :: Proxy (Name a))
  then Just $ unsafeCoerce v
  else Nothing
