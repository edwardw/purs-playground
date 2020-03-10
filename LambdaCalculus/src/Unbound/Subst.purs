module Unbound.Subst where

import Prelude
import Data.Array as A
import Data.Foldable (all)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, NoConstructors, Product(..), Sum(..), from, to)
import Data.Leibniz (type (~), coerceSymm, runLeibniz)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst)
import Data.Typelevel.Undefined (undefined)
import Unbound.Alpha (class Alpha)
import Unbound.Bind (Bind)
import Unbound.Embed (Embed)
import Unbound.Ignore (Ignore)
import Unbound.Name (AnyName, Name, isFreeName)
import Unbound.Rebind (Rebind)
import Unbound.Rec (Rec, TRec)
import Unbound.Shift (Shift(..))


data SubstName a b
  = SubstName (a ~ b) (Name a)

data SubstCoerce a b
  = SubstCoerce (Name b) (b -> Maybe a)



--------------------------------------------------------------------------------
-- Subst class -----------------------------------------------------------------
--------------------------------------------------------------------------------


class Subst b a where
  isvar :: a -> Maybe (SubstName a b)

  isCoerceVar :: a -> Maybe (SubstCoerce a b)

  subst :: Name b -> b -> a -> a

  substs :: Array (Tuple (Name b) b) -> a -> a


defaultSubst
  :: forall a b f rep
   . Subst b (f a)
  => Generic (f a) rep
  => GenericSubst b rep
  => Name b -> b -> f a -> f a
defaultSubst n u x =
  if isFreeName n
  then case isvar x of
    Just (SubstName proof m) | runLeibniz proof m == n -> coerceSymm proof u
    _ ->
      case isCoerceVar x of
        Just (SubstCoerce m f) | m == n -> maybe x identity (f u)
        _ -> to <<< gsubst n u $ from x
  else undefined


defaultSubsts
  :: forall a b f rep
   . Subst b (f a)
  => Generic (f a) rep
  => GenericSubst b rep
  => Array (Tuple (Name b) b) -> f a -> f a
defaultSubsts ss x
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

  | otherwise = undefined


defaultSubst2
  :: forall a a' b f rep
   . Subst b (f a a')
  => Generic (f a a') rep
  => GenericSubst b rep
  => Name b -> b -> f a a' -> f a a'
defaultSubst2 n u x =
  if isFreeName n
  then case isvar x of
    Just (SubstName proof m) | runLeibniz proof m == n -> coerceSymm proof u
    _ ->
      case isCoerceVar x of
        Just (SubstCoerce m f) | m == n -> maybe x identity (f u)
        _ -> to <<< gsubst n u $ from x
  else undefined


defaultSubsts2
  :: forall a a' b f rep
   . Subst b (f a a')
  => Generic (f a a') rep
  => GenericSubst b rep
  => Array (Tuple (Name b) b) -> f a a' -> f a a'
defaultSubsts2 ss x
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

  | otherwise = undefined



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


instance genericSubstConstructor :: Subst b a => GenericSubst b (Constructor name a) where
  gsubst nm val (Constructor x) = Constructor $ subst nm val x
  gsubsts ss (Constructor x) = Constructor $ substs ss x


instance genericSubstArgument :: Subst b a => GenericSubst b (Argument a) where
  gsubst nm val (Argument x) = Argument $ subst nm val x
  gsubsts ss (Argument x) = Argument $ substs ss x


instance genericSubstSun
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


instance substInt :: Subst b Int where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity



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


instance substEmbed
  :: (Subst b a, Generic (Embed a) rep, GenericSubst b rep)
  => Subst b (Embed a) where

  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst         = defaultSubst
  substs        = defaultSubsts


instance substShift :: Subst b e => Subst b (Shift e) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing

  subst x b (Shift e) = Shift $ subst x b e
  substs ss (Shift e) = Shift $ substs ss e


instance substBind
  :: (Subst c a, Subst c b, Alpha a, Alpha b, Generic (Bind a b) rep, GenericSubst c rep)
  => Subst c (Bind a b) where

  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst         = defaultSubst2
  substs        = defaultSubsts2


instance substRebind
  :: (Subst c p1, Subst c p2, Generic (Rebind p1 p2) rep, GenericSubst c rep)
  => Subst c (Rebind p1 p2) where

  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst         = defaultSubst2
  substs        = defaultSubsts2


instance substRec
  :: (Subst b a, Generic (Rec a) rep, GenericSubst b rep)
  => Subst b (Rec a) where

  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst         = defaultSubst
  substs        = defaultSubsts


instance substTRec
  :: (Alpha a, Subst b a, Generic (TRec a) rep, GenericSubst b rep)
  => Subst b (TRec a) where

  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst         = defaultSubst
  substs        = defaultSubsts


instance substIgnore :: Subst b (Ignore a) where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst _ _     = identity
  substs _      = identity