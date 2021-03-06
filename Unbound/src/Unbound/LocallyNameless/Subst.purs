module Unbound.LocallyNameless.Subst where

import Prelude
import Data.Array as A
import Data.Foldable (all)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from, to)
import Data.Leibniz (type (~), coerceSymm, runLeibniz)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst)
import Effect.Exception.Unsafe (unsafeThrow)
import Unbound.LocallyNameless.Alpha (class Alpha)
import Unbound.LocallyNameless.Bind (Bind)
import Unbound.LocallyNameless.Embed (Embed)
import Unbound.LocallyNameless.Ignore (Ignore)
import Unbound.LocallyNameless.Name (AnyName, Name, isFreeName)
import Unbound.LocallyNameless.Rebind (Rebind)
import Unbound.LocallyNameless.Rec (Rec, TRec)
import Unbound.LocallyNameless.Shift (Shift(..))



--------------------------------------------------------------------------------
-- Subst class -----------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Types that are instances of `Subst` may participate in capture-avoiding
-- | substitution
-- |
-- | The minimal definition is generic, provided your type is an instance of
-- | `Data.Generic.Rep.Generic`. The generic instance just propagate the
-- | substitution into the constituent factors.
-- |
-- | If you identity the variable occurrences by implementing `isvar` function,
-- | the derived `subst` function will be able to substitute a factor for a
-- | variable.
-- |
-- |    ```
-- |    data Factor
-- |      = V Var
-- |      | C Int
-- |      | Subexpr Expr
-- |    derive instance genericFactor :: Generic Factor _
-- |    instance substFactor :: Subst Factor where
-- |      isvar = case _ of
-- |        V v -> Just (SubstName identity v)
-- |        _   -> Nothing
-- |    ```
-- |
-- | Instances of `Subst b a` are terms of type `a` that may contain variables
-- | of type `b` that may participate in capture-avoiding substitution.
class Subst b a where
  -- | This is the only method that must be implemented.
  isvar :: a -> Maybe (SubstName a b)

  -- | This is an alternative version of `isvar`, usable in the case that the
  -- | substitution argument doesn't have *exactly* the same type as the term
  -- | it should be substituted into.
  -- | The default implementation always returns `Nothing`.
  isCoerceVar :: a -> Maybe (SubstCoerce a b)

  -- | `subst nm e tm` substitutes `e` for `nm` in `tm`. It has a generic
  -- | implementation in terms of `isvar`.
  subst :: Name b -> b -> a -> a

  substs :: Array (Tuple (Name b) b) -> a -> a


data SubstName a b
  = SubstName (a ~ b) (Name a)

data SubstCoerce a b
  = SubstCoerce (Name b) (b -> Maybe a)


genericSubst
  :: forall a b rep
   . Subst b a
  => Generic a rep
  => GenericSubst b rep
  => Name b -> b -> a -> a
genericSubst n u x =
  if isFreeName n
  then case isvar x of
    Just (SubstName proof m) | runLeibniz proof m == n -> coerceSymm proof u
    _ ->
      case isCoerceVar x of
        Just (SubstCoerce m f) | m == n -> maybe x identity (f u)
        _ -> to <<< gsubst n u $ from x
  else unsafeThrow $ "Cannot substitute for bound variable " <> show n


genericSubsts
  :: forall a b rep
   . Subst b a
  => Generic a rep
  => GenericSubst b rep
  => Array (Tuple (Name b) b) -> a -> a
genericSubsts ss x
  | all (isFreeName <<< fst) ss =
    case isvar x of
      Just (SubstName proof m)
        | Just (Tuple _ u) <- A.find ((_ == runLeibniz proof m) <<< fst) ss
        -> coerceSymm proof u

      _ -> case isCoerceVar x of
        Just (SubstCoerce m f)
          | Just (Tuple _ u) <- A.find ((_ == m) <<< fst) ss
          -> maybe x identity (f u)

        _ -> to <<< gsubsts ss $ from x

  | otherwise = unsafeThrow $ "Cannot substitute for bound variable in: " <> show (map fst ss)


genericSubst1
  :: forall a b f rep
   . Subst b (f a)
  => Generic (f a) rep
  => GenericSubst b rep
  => Name b -> b -> f a -> f a
genericSubst1 n u x =
  if isFreeName n
  then case isvar x of
    Just (SubstName proof m) | runLeibniz proof m == n -> coerceSymm proof u
    _ ->
      case isCoerceVar x of
        Just (SubstCoerce m f) | m == n -> maybe x identity (f u)
        _ -> to <<< gsubst n u $ from x
  else unsafeThrow $ "Cannot substitute for bound variable " <> show n


genericSubsts1
  :: forall a b f rep
   . Subst b (f a)
  => Generic (f a) rep
  => GenericSubst b rep
  => Array (Tuple (Name b) b) -> f a -> f a
genericSubsts1 ss x
  | all (isFreeName <<< fst) ss =
    case isvar x of
      Just (SubstName proof m)
        | Just (Tuple _ u) <- A.find ((_ == runLeibniz proof m) <<< fst) ss
        -> coerceSymm proof u

      _ -> case isCoerceVar x of
        Just (SubstCoerce m f)
          | Just (Tuple _ u) <- A.find ((_ == m) <<< fst) ss
          -> maybe x identity (f u)

        _ -> to <<< gsubsts ss $ from x

  | otherwise = unsafeThrow $ "Cannot substitute for bound variable in: " <> show (map fst ss)


genericSubst2
  :: forall a a' b f rep
   . Subst b (f a a')
  => Generic (f a a') rep
  => GenericSubst b rep
  => Name b -> b -> f a a' -> f a a'
genericSubst2 n u x =
  if isFreeName n
  then case isvar x of
    Just (SubstName proof m) | runLeibniz proof m == n -> coerceSymm proof u
    _ ->
      case isCoerceVar x of
        Just (SubstCoerce m f) | m == n -> maybe x identity (f u)
        _ -> to <<< gsubst n u $ from x
  else unsafeThrow $ "Cannot substitute for bound variable " <> show n


genericSubsts2
  :: forall a a' b f rep
   . Subst b (f a a')
  => Generic (f a a') rep
  => GenericSubst b rep
  => Array (Tuple (Name b) b) -> f a a' -> f a a'
genericSubsts2 ss x
  | all (isFreeName <<< fst) ss =
    case isvar x of
      Just (SubstName proof m)
        | Just (Tuple _ u) <- A.find ((_ == runLeibniz proof m) <<< fst) ss
        -> coerceSymm proof u

      _ -> case isCoerceVar x of
        Just (SubstCoerce m f)
          | Just (Tuple _ u) <- A.find ((_ == m) <<< fst) ss
          -> maybe x identity (f u)

        _ -> to <<< gsubsts ss $ from x

  | otherwise = unsafeThrow $ "Cannot substitute for bound variable in: " <> show (map fst ss)



--------------------------------------------------------------------------------
-- Subst instance for Generic --------------------------------------------------
--------------------------------------------------------------------------------


class GenericSubst b a where
  gsubst :: Name b -> b -> a -> a
  gsubsts :: Array (Tuple (Name b) b) -> a -> a


instance genericSubstNoConstructors :: GenericSubst b NoConstructors where
  gsubst _ _ = identity
  gsubsts _  = identity


instance genericSubstNoArguments :: GenericSubst b NoArguments where
  gsubst _ _ = identity
  gsubsts _  = identity


instance genericSubstConstructor
  :: (GenericSubst b a, IsSymbol name)
  => GenericSubst b (Constructor name a) where

  gsubst nm val (Constructor x) = Constructor $ gsubst nm val x
  gsubsts ss (Constructor x) = Constructor $ gsubsts ss x


instance genericSubstArgument :: Subst b a => GenericSubst b (Argument a) where
  gsubst nm val (Argument x) = Argument $ subst nm val x
  gsubsts ss (Argument x) = Argument $ substs ss x


instance genericSubstSum
  :: (GenericSubst b f, GenericSubst b g)
  => GenericSubst b (Sum f g) where

  gsubst nm val s = case s of
    Inl x -> Inl $ gsubst nm val x
    Inr y -> Inr $ gsubst nm val y
  gsubsts ss s = case s of
    Inl x -> Inl $ gsubsts ss x
    Inr y -> Inr $ gsubsts ss y


instance genericSubstProduct
  :: (GenericSubst b f, GenericSubst b g)
  => GenericSubst b (Product f g) where

  gsubst nm val (Product x y) =
    Product (gsubst nm val x) (gsubst nm val y)
  gsubsts ss (Product x y) =
    Product (gsubsts ss x) (gsubsts ss y)



--------------------------------------------------------------------------------
-- Subst instances for the usual types -----------------------------------------
--------------------------------------------------------------------------------


instance substInt :: Subst b Int where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity


instance substUnit :: Subst b Unit where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity


instance substTuple :: (Subst c a, Subst c b) => Subst c (Tuple a b) where
  isvar _               = Nothing
  isCoerceVar _         = Nothing
  subst n u (Tuple x y) = Tuple (subst n u x) (subst n u y)
  substs ss (Tuple x y) = Tuple (substs ss x) (substs ss y)



--------------------------------------------------------------------------------
-- Subst instances for Unbound primitives --------------------------------------
--------------------------------------------------------------------------------


instance substName :: Subst b (Name a) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity


instance substAnyName :: Subst b AnyName where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity


instance substEmbed :: Subst b a => Subst b (Embed a) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst1 n u x
  substs ss x   = genericSubsts1 ss x


instance substShift :: Subst b e => Subst b (Shift e) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing

  subst x b (Shift e) = Shift $ subst x b e
  substs ss (Shift e) = Shift $ substs ss e


instance substBind
  :: (Subst c a, Subst c b, Alpha a, Alpha b)
  => Subst c (Bind a b) where

  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst2 n u x
  substs ss x   = genericSubsts2 ss x


instance substRebind :: (Subst c p1, Subst c p2) => Subst c (Rebind p1 p2) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst2 n u x
  substs ss x   = genericSubsts2 ss x


instance substRec :: Subst b a => Subst b (Rec a) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst1 n u x
  substs ss x   = genericSubsts1 ss x


instance substTRec :: (Alpha a, Subst b a) => Subst b (TRec a) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst1 n u x
  substs ss x   = genericSubsts1 ss x


instance substIgnore :: Subst b (Ignore a) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity
