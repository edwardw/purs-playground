module Unbound.Alpha where

import Prelude
import Data.Either
import Data.Foldable
import Data.Functor.Contravariant
import Data.Generic.Rep
import Data.Maybe
import Data.Monoid.Conj
import Data.Profunctor.Strong
import Data.Set (Set)
import Data.Set as S
import Data.Tuple
import Data.Typeable
import Data.Typelevel.Undefined (undefined)
import Type.Proxy
import Unbound.Fresh
import Unbound.LFresh
import Unbound.Name
import Unbound.PermM
import Unbound.PermM as PermM


data AlphaCtx = AlphaCtx { mode :: Mode, level :: Int }

data Mode = Term | Pat

derive instance eqMode :: Eq Mode


initialCtx :: AlphaCtx
initialCtx = AlphaCtx { mode: Term, level: 0 }

patternCtx :: AlphaCtx -> AlphaCtx
patternCtx (AlphaCtx ctx) = AlphaCtx $ ctx { mode = Pat }

termCtx :: AlphaCtx -> AlphaCtx
termCtx (AlphaCtx ctx) = AlphaCtx $ ctx { mode = Term }

isTermCtx :: AlphaCtx -> Boolean
isTermCtx = case _ of
  AlphaCtx { mode: Term } -> true
  _                       -> false

incrLevelCtx :: AlphaCtx -> AlphaCtx
incrLevelCtx (AlphaCtx ctx) = AlphaCtx $ ctx { level = ctx.level + 1 }

decrLevelCtx :: AlphaCtx -> AlphaCtx
decrLevelCtx (AlphaCtx ctx) = AlphaCtx $ ctx { level = ctx.level - 1 }

isZeroLevelCtx :: AlphaCtx -> Boolean
isZeroLevelCtx (AlphaCtx ctx) = ctx.level == 0



--------------------------------------------------------------------------------
-- Alpha class -----------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Types that are instances of `Alpha` may participate in name representation.
-- |
-- | Minimal instance is entirely empty, provided that your type is an instance
-- | of `Data.Generic.Rep.Generic`.
class Show a <= Alpha a where
  aeq' :: AlphaCtx -> a -> a -> Boolean

  fvAny' :: forall f
          . Contravariant f
         => Applicative f
         => AlphaCtx
         -> (AnyName -> f AnyName)
         -> a
         -> f a

  -- | Replace free names with bound names.
  close :: AlphaCtx -> NamePatFind -> a -> a

  -- | Replace bound names with free names.
  open :: AlphaCtx -> NthPatFind -> a -> a

  -- | `isPat x` dynamically checks whether `x` can be used as a valid pattern.
  isPat :: a -> Maybe (Set AnyName)

  -- | `isTerm x` dynamically checks whether `x` can be used as a valid term.
  isTerm :: a -> Conj Boolean

  -- | `isEmbed` is needed internally for the implementation of `isPat`.
  -- | `isEmbed` is true for terms wrapped in `Embed` and zero or more
  -- | occurrences of `Shift`. The default should simply be false.
  isEmbed :: a -> Boolean

  -- | If `a` is a pattern, finds the `n`th name in the patten (starting from
  -- | zero), returns the number of names encountered if not found.
  nthPatFind :: a -> NthPatFind

  -- | If `a` is a pattern, find the index of given name in the pattern.
  namePatFind :: a -> NamePatFind

  -- | Apply the given permutation of variable names to the given pattern.
  swaps' :: AlphaCtx -> Perm AnyName -> a -> a

  lfreshen' :: forall m b
             . LFresh m
            => AlphaCtx
            -> a
            -> (a -> Perm AnyName -> m b)
            -> m b

  -- | Rename the free variables in the given term to be distinct from all other
  -- | names seen in the monad `m`.
  freshen' :: forall m. Fresh m => AlphaCtx -> a -> m (Tuple a (Perm AnyName))

  -- | An alpha-respecting total order on terms involving binders.
  acompare' :: AlphaCtx -> a -> a -> Ordering



-- | The result of `nthPatFind a i` is
-- |
-- |    - `Left k` where `i-k` is the number of names in pattern `a`
-- |      (with `k < i`)
-- |    - or `Right x` where `x` is the `i`th name in `a`
newtype NthPatFind = NthPatFind { runNthPatFind :: Int -> Either Int AnyName }

instance semigroupNthPatFind :: Semigroup NthPatFind where
  append (NthPatFind f) (NthPatFind g) = NthPatFind { runNthPatFind:
    \i -> case f.runNthPatFind i of
      Left i' -> g.runNthPatFind i'
      found   -> found }

instance monoidNthPatFind :: Monoid NthPatFind where
  mempty = NthPatFind { runNthPatFind: Left }


-- | The result of `namePatFind a x` is either
-- |
-- |    - `Left i` if `a` is a pattern that contains `i` free names none of
-- |      which are `x`
-- |    - or `Right j` if `x` is the `j`th name in `a`
newtype NamePatFind
  = NamePatFind { runNamePatFind :: AnyName
                                    -- Left - names skipped over
                                    -- Right - index of the name found
                                 -> Either Int Int }

instance semigroupNamePatFind :: Semigroup NamePatFind where
  append (NamePatFind f) (NamePatFind g) = NamePatFind { runNamePatFind:
    \nm -> case f.runNamePatFind nm of
      res@(Right _) -> res
      Left n        -> case g.runNamePatFind nm of
        Left m  -> Left $ n + m
        Right i -> Right $ n + i }

instance monoidNamePatFind :: Monoid NamePatFind where
  mempty = NamePatFind { runNamePatFind: const $ Left 0 }



--------------------------------------------------------------------------------
-- Alpha instance for Generic  -------------------------------------------------
--------------------------------------------------------------------------------

class GenericAlpha a where
  gaeq :: AlphaCtx -> a -> a -> Boolean

  gfvAny :: forall g
          . Contravariant g
         => Applicative g
         => AlphaCtx
         -> (AnyName -> g AnyName)
         -> a
         -> g a

  gclose :: AlphaCtx -> NamePatFind -> a -> a
  gopen  :: AlphaCtx -> NthPatFind -> a -> a

  gisPat  :: a -> Maybe (Set AnyName)
  gisTerm :: a -> Conj Boolean

  gnthPatFind  :: a -> NthPatFind
  gnamePatFind :: a -> NamePatFind

  gswaps   :: AlphaCtx -> Perm AnyName -> a -> a
  gfreshen :: forall m
            . Fresh m
           => AlphaCtx
           -> a
           -> m (Tuple a (Perm AnyName))

  glfreshen :: forall m b
             . LFresh m
            => AlphaCtx
            -> a
            -> (a -> Perm AnyName -> m b)
            -> m b

  gacompare :: AlphaCtx -> a -> a -> Ordering


instance genericAlphaNoConstructors :: GenericAlpha NoConstructors where
  gaeq _ _ _ = true

  gfvAny _ _ a = pure a

  gclose _ _ a = a
  gopen _ _ a  = a

  gisPat _  = mempty
  gisTerm _ = mempty

  gnthPatFind _  = mempty
  gnamePatFind _ = mempty

  gswaps _ _ a = a
  gfreshen _ a = pure $ Tuple a mempty

  glfreshen _ a cont = cont a mempty

  gacompare _ _ _ = EQ


instance genericAlphaNoArguments :: GenericAlpha NoArguments where
  gaeq _ _ _ = true

  gfvAny _ _ _ = pure NoArguments

  gclose _ _ _ = NoArguments
  gopen _ _ _  = NoArguments

  gisPat _  = mempty
  gisTerm _ = mempty

  gnthPatFind _  = mempty
  gnamePatFind _ = mempty

  gswaps _ _ _ = NoArguments
  gfreshen _ _ = pure $ Tuple NoArguments mempty

  glfreshen _ _ cont = cont NoArguments mempty

  gacompare _ _ _ = EQ


instance genericAlphaConstructor :: Alpha a => GenericAlpha (Constructor name a) where
  gaeq ctx (Constructor x) (Constructor y) = aeq' ctx x y

  gfvAny ctx nfn (Constructor x) = map Constructor $ fvAny' ctx nfn x

  gclose ctx b (Constructor y) = Constructor $ close ctx b y

  gopen ctx b (Constructor y) = Constructor $ open ctx b y

  gisPat (Constructor x) = isPat x

  gisTerm (Constructor x) = isTerm x

  gnthPatFind (Constructor x) = nthPatFind x

  gnamePatFind (Constructor x) = namePatFind x

  gswaps ctx perm (Constructor x) = Constructor $ swaps' ctx perm x

  gfreshen ctx (Constructor x) = (first Constructor) <$> (freshen' ctx x)

  glfreshen ctx (Constructor x) cont = lfreshen' ctx x (cont <<< Constructor)

  gacompare ctx (Constructor x) (Constructor y) = acompare' ctx x y


instance genericAlphaSum :: (GenericAlpha a, GenericAlpha b) => GenericAlpha (Sum a b) where
  gaeq ctx sumx sumy = case Tuple sumx sumy of
    Tuple (Inl x) (Inl y) -> gaeq ctx x y
    Tuple (Inr x) (Inr y) -> gaeq ctx x y
    _                     -> false

  gfvAny ctx nfn sumx = case sumx of
    Inl f -> map Inl $ gfvAny ctx nfn f
    Inr g -> map Inr $ gfvAny ctx nfn g

  gclose ctx b sumx = case sumx of
    Inl f -> Inl $ gclose ctx b f
    Inr g -> Inr $ gclose ctx b g

  gopen ctx b sumx = case sumx of
    Inl f -> Inl $ gopen ctx b f
    Inr g -> Inr $ gopen ctx b g

  gisPat = case _ of
    Inl f -> gisPat f
    Inr g -> gisPat g

  gisTerm = case _ of
    Inl f -> gisTerm f
    Inr g -> gisTerm g

  gnthPatFind = case _ of
    Inl f -> gnthPatFind f
    Inr g -> gnthPatFind g

  gnamePatFind = case _ of
    Inl f -> gnamePatFind f
    Inr g -> gnamePatFind g

  gswaps ctx perm sumx = case sumx of
    Inl f -> Inl $ gswaps ctx perm f
    Inr g -> Inr $ gswaps ctx perm g

  gfreshen ctx sumx = case sumx of
    Inl f -> first Inl <$> gfreshen ctx f
    Inr g -> first Inr <$> gfreshen ctx g

  glfreshen ctx sumx cont = case sumx of
    Inl f -> glfreshen ctx f (cont <<< Inl)
    Inr g -> glfreshen ctx g (cont <<< Inr)

  gacompare ctx sumx sumy = case Tuple sumx sumy of
    Tuple (Inl _) (Inr _)   -> LT
    Tuple (Inr _) (Inl _)   -> GT
    Tuple (Inl f1) (Inl f2) -> gacompare ctx f1 f2
    Tuple (Inr g1) (Inr g2) -> gacompare ctx g1 g2


instance genericAlphaProduct :: (GenericAlpha a, GenericAlpha b) => GenericAlpha (Product a b) where
  gaeq ctx (Product f1 g1) (Product f2 g2) =
    gaeq ctx f1 f2 && gaeq ctx g1 g2

  gfvAny ctx nfn (Product f g) =
    Product <$> gfvAny ctx nfn f
            <*> gfvAny ctx nfn g

  gclose ctx b (Product f g) = Product (gclose ctx b f) (gclose ctx b g)

  gopen ctx b (Product f g) = Product (gopen ctx b f) (gopen ctx b g)

  gisPat (Product f g) = gisPat f <> gisPat g

  gisTerm (Product f g) = gisTerm f <> gisTerm g

  gnthPatFind (Product f g) = gnthPatFind f <> gnthPatFind g

  gnamePatFind (Product f g) = gnamePatFind f <> gnamePatFind g

  gswaps ctx perm (Product f g) = Product (gswaps ctx perm f) (gswaps ctx perm g)

  gfreshen ctx (Product f g) = do
    Tuple g' perm2 <- gfreshen ctx g
    Tuple f' perm1 <- gfreshen ctx (gswaps ctx perm2 f)
    pure $ Tuple (Product f' g') (perm1 <> perm2)

  glfreshen ctx (Product f g) cont =
    glfreshen ctx g $ \g' perm2 ->
    glfreshen ctx (gswaps ctx perm2 f) $ \f' perm1 ->
    cont (Product f' g') (perm1 <> perm2)

  gacompare ctx (Product f1 g1) (Product f2 g2) =
    (gacompare ctx f1 f2) <> (gacompare ctx g1 g2)



--------------------------------------------------------------------------------
-- Alpha instances for the usual types -----------------------------------------
--------------------------------------------------------------------------------

instance alphaInt :: Alpha Int where
  aeq' _ i j = i == j

  fvAny' _ _ i = pure i

  close _ _ i = i
  open _ _ i  = i

  isPat _  = mempty
  isTerm _ = mempty

  isEmbed _ = false

  nthPatFind _  = mempty
  namePatFind _ = mempty

  swaps' _ _ i = i
  freshen' _ i = pure $ Tuple i mempty
  lfreshen' _ i cont = cont i mempty

  acompare' _ i j = compare i j


instance alphaChar :: Alpha Char where
  aeq' _ i j = i == j

  fvAny' _ _ i = pure i

  close _ _ i = i
  open _ _ i  = i

  isPat _  = mempty
  isTerm _ = mempty

  isEmbed _ = false

  nthPatFind _  = mempty
  namePatFind _ = mempty

  swaps' _ _ i = i
  freshen' _ i = pure $ Tuple i mempty
  lfreshen' _ i cont = cont i mempty

  acompare' _ i j = compare i j



--------------------------------------------------------------------------------
-- Alpha instances for interesting types ---------------------------------------
--------------------------------------------------------------------------------

instance alphaName :: Typeable a => Alpha (Name a) where
  aeq' ctx n1 n2 =
    if isTermCtx ctx
    then n1 == n2 -- in terms, better be the same name
    else true     -- in a pattern, names are always equivalent (since
                  -- they are both bound, so they can vary.)

  fvAny' ctx nfn nm =
    if isTermCtx ctx && isFreeName nm
    then cmap mkAnyName (nfn (mkAnyName nm))
    else pure nm

  open (AlphaCtx ctx) (NthPatFind b) a = case a of
    Bn l k ->
      if ctx.mode == Term && ctx.level == l
      then
        case b.runNthPatFind k of
          Right (AnyName (Exists nm)) ->
            let Tuple t v = nm.runExists (repAs a)
            in if t == typeOf (Proxy :: Proxy a)
               then v
               else undefined
          Left _ -> undefined
      else a
    _ -> a

  close (AlphaCtx ctx) (NamePatFind b) a = case a of
    Fn _ _ ->
      if ctx.mode == Term
      then
        case b.runNamePatFind (mkAnyName a) of
          Right k -> Bn ctx.level k
          Left _  -> a
      else a
    _ -> a

  isPat n =
    if isFreeName n
    then Just $ S.singleton (mkAnyName n)
    else Nothing

  isTerm _ = mempty

  isEmbed _ = false

  nthPatFind nm = NthPatFind { runNthPatFind: \i ->
    if i == 0 then Right (mkAnyName nm) else Left $ i - 1 }

  namePatFind nm1 = NamePatFind { runNamePatFind: \(AnyName (Exists nm2)) ->
    let typ1 = typeOf (Proxy :: Proxy (Name a))
        Tuple t v = nm2.runExists (repAs nm1)
    in if typ1 == t && nm1 == v then Right 0 else Left 1 }

  swaps' ctx perm nm =
    if isTermCtx ctx
    then
      case PermM.apply perm (mkAnyName nm) of
        AnyName (Exists nm') ->
          let Tuple t v = nm'.runExists (repAs nm)
          in if t == typeOf (Proxy :: Proxy (Name a))
             then v
             else undefined
    else nm

  freshen' ctx nm =
    if not (isTermCtx ctx)
    then do
      nm' <- fresh nm
      pure $ Tuple nm' (single (mkAnyName nm) (mkAnyName nm'))
    else undefined

  lfreshen' ctx nm cont =
    if not (isTermCtx ctx)
    then do
      nm' <- lfresh nm
      avoid [mkAnyName nm'] <<< cont nm' $ single (mkAnyName nm) (mkAnyName nm')
    else undefined

  acompare' ctx nm1 nm2 = case Tuple nm1 nm2 of
    Tuple (Fn s1 i1) (Fn s2 i2) | isTermCtx ctx ->
      (compare s1 s2) <> (compare i1 i2)
    Tuple (Bn i1 j1) (Bn i2 j2) | isTermCtx ctx ->
      compare i1 i2 <> compare j1 j2
    Tuple (Fn _ _) (Bn _ _) | isTermCtx ctx -> LT
    Tuple (Bn _ _) (Fn _ _) | isTermCtx ctx -> GT
    Tuple _ _                               -> EQ


instance alphaAnyName :: Alpha AnyName where
  aeq' ctx x y =
    if x == y
    then true
    else
      -- in a term, unequal variables are unequal, in a pattern it's ok.
      not (isTermCtx ctx)

  fvAny' ctx nfn n@(AnyName (Exists nm)) =
    if isTermCtx ctx && nm.runExists isFreeName
    then nfn n
    else pure n

  isTerm _ = mempty

  isPat n@(AnyName (Exists nm)) =
    if nm.runExists isFreeName
    then Just $ S.singleton n
    else Nothing

  swaps' ctx perm n =
    if isTermCtx ctx
    then PermM.apply perm n
    else n

  freshen' ctx n@(AnyName (Exists nm)) =
    if isTermCtx ctx
    then undefined
    else do
      nm' <- nm.runExists fresh
      pure $ Tuple (mkAnyName nm') (single n (mkAnyName nm'))

  lfreshen' ctx n@(AnyName (Exists nm)) cont =
    if isTermCtx ctx
    then undefined
    else do
      nm' <- nm.runExists lfresh
      avoid [mkAnyName nm'] <<< cont (mkAnyName nm') $ single n (mkAnyName nm')

  open ctx b (AnyName (Exists nm)) =
    mkAnyName (nm.runExists (open ctx b))

  close ctx b (AnyName (Exists nm)) =
    mkAnyName (nm.runExists (close ctx b))

  nthPatFind nm = NthPatFind { runNthPatFind: \i ->
    if i == 0 then Right nm else Left $ i - 1 }

  namePatFind nmHave = NamePatFind { runNamePatFind: \nmWant ->
    if nmHave == nmWant then Right 0 else Left 1 }

  isEmbed _ = false

  acompare' ctx (AnyName (Exists nm1)) (AnyName (Exists nm2))
    | isTermCtx ctx =
      let Tuple t1 v1 = nm1.runExists repInt
          Tuple t2 v2 = nm2.runExists repInt
      in  case compare t1 t2 of
            EQ  -> acompare' ctx v1 v2
            ord -> ord
    | otherwise = EQ
