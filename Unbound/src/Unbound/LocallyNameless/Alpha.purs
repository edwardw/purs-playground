module Unbound.LocallyNameless.Alpha where

import Prelude
import Data.Either (Either(..))
import Data.Function (on)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), from, to)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj)
import Data.Profunctor.Strong (first)
import Data.Set (Set)
import Data.Set as S
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, typeOf)
import Effect.Exception.Unsafe (unsafeThrow)
import Type.Proxy (Proxy(..))
import Unbound.LocallyNameless.Fresh (class Fresh, fresh)
import Unbound.LocallyNameless.LFresh (class LFresh, avoid, lfresh)
import Unbound.LocallyNameless.Name (AnyName(..), Name(..), isFreeName, mkAnyName, toSortedName)
import Unbound.PermM (Perm, single)
import Unbound.PermM as PermM
import Unsafe.Coerce (unsafeCoerce)



--------------------------------------------------------------------------------
-- Overview --------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | We have two classes of types:
-- |    Terms (which contains variables) and
-- |    Patterns (which contains binders)
-- |
-- | Terms include
-- |    Names
-- |    Bind p t where p is a pattern and t is a term
-- |    Standard type constructors (Unit, Tuple, etc.)
-- |
-- | Patterns include
-- |    Names
-- |    Embed t where t is a term
-- |    Rebind p q where p and q are both patterns
-- |    Rec p where p is a pattern
-- |    Shift a where a is an Embed or some numbers of Shifts wrapped around Embed
-- |    Standard type constructors
-- |
-- | Terms support a number of operations, including alpha-equivalence, free
-- | variables, swapping, etc. Because patterns occur in terms, so they too
-- | support the same operations, but only for the annotations inside them.
-- |
-- | Therefore, both Terms and Patterns are instances of the `Alpha` type class
-- | which lists these operations. However, some types (such as Names) are both
-- | Terms and Patterns, and the behavior of the operations is different when we
-- | use Name as a term and as a pattern. We index the each of the operations
-- | with a mode to tell the operation context.
-- |
-- | Patterns also support a few extra operations that Terms do not for dealing
-- | with the binding variables. These are used to find the index of names
-- | inside patterns.



--------------------------------------------------------------------------------
-- Alpha class -----------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Types that are instances of `Alpha` may participate in name representation.
-- |
-- | Minimal instance is entirely generic, provided that your type is an
-- | instance of `Data.Generic.Rep.Generic`. E.g., the `Alpha` instance for
-- | `TRec`.
class Show a <= Alpha a where
  -- | See `Unbound.LocallyNameless.Operations.aeq`.
  aeq' :: AlphaCtx -> a -> a -> Boolean

  -- | See `Unbound.LocallyNameless.Operations.fvAny`.
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

  -- | See `Unbound.LocallyNameless.Operations.lfreshen`.
  lfreshen' :: forall m b
             . LFresh m
            => AlphaCtx
            -> a
            -> (a -> Perm AnyName -> m b)
            -> m b

  -- | See `Unbound.LocallyNameless.Operations.freshen`.
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
-- AlphaCtx --------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | An `AlphaCtx` records the current mode (Term/Pat) and current level, and
-- | gets passed along during operations which need to keep track of the mode
-- | and/or level.
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


instance genericAlphaConstructor
  :: (GenericAlpha a, IsSymbol name)
  => GenericAlpha (Constructor name a) where

  gaeq ctx (Constructor x) (Constructor y) = gaeq ctx x y

  gfvAny ctx nfn (Constructor x) = map Constructor $ gfvAny ctx nfn x

  gclose ctx b (Constructor y) = Constructor $ gclose ctx b y

  gopen ctx b (Constructor y) = Constructor $ gopen ctx b y

  gisPat (Constructor x) = gisPat x

  gisTerm (Constructor x) = gisTerm x

  gnthPatFind (Constructor x) = gnthPatFind x

  gnamePatFind (Constructor x) = gnamePatFind x

  gswaps ctx perm (Constructor x) = Constructor $ gswaps ctx perm x

  gfreshen ctx (Constructor x) = (first Constructor) <$> (gfreshen ctx x)

  glfreshen ctx (Constructor x) cont = glfreshen ctx x (cont <<< Constructor)

  gacompare ctx (Constructor x) (Constructor y) = gacompare ctx x y


instance genericAlphaArgument :: Alpha a => GenericAlpha (Argument a) where
  gaeq ctx (Argument x) (Argument y) = aeq' ctx x y

  gfvAny ctx nfn (Argument x) = map Argument $ fvAny' ctx nfn x

  gclose ctx b (Argument y) = Argument $ close ctx b y

  gopen ctx b (Argument y) = Argument $ open ctx b y

  gisPat (Argument x) = isPat x

  gisTerm (Argument x) = isTerm x

  gnthPatFind (Argument x) = nthPatFind x

  gnamePatFind (Argument x) = namePatFind x

  gswaps ctx perm (Argument x) = Argument $ swaps' ctx perm x

  gfreshen ctx (Argument x) = (first Argument) <$> (freshen' ctx x)

  glfreshen ctx (Argument x) cont = lfreshen' ctx x (cont <<< Argument)

  gacompare ctx (Argument x) (Argument y) = acompare' ctx x y


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


genericAeq
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => AlphaCtx -> a -> a -> Boolean
genericAeq ctx = gaeq ctx `on` from

genericFvAny
  :: forall a rep g
   . Generic a rep
  => GenericAlpha rep
  => Contravariant g
  => Applicative g
  => AlphaCtx -> (AnyName -> g AnyName) -> a -> g a
genericFvAny ctx nfn = map to <<< gfvAny ctx nfn <<< from

genericClose
  :: forall a rep
   . Generic a rep
   => GenericAlpha rep
   => AlphaCtx -> NamePatFind -> a -> a
genericClose ctx b = to <<< gclose ctx b <<< from

genericOpen
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => AlphaCtx -> NthPatFind -> a -> a
genericOpen ctx b = to <<< gopen ctx b <<< from

genericIsPat
  :: forall a rep
   . Generic a rep
   => GenericAlpha rep
   => a -> Maybe (Set AnyName)
genericIsPat = gisPat <<< from

genericIsTerm
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => a -> Conj Boolean
genericIsTerm = gisTerm <<< from

genericNthPatFind
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => a -> NthPatFind
genericNthPatFind = gnthPatFind <<< from

genericNamePatFind
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => a -> NamePatFind
genericNamePatFind = gnamePatFind <<< from

genericSwaps
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => AlphaCtx -> Perm AnyName -> a -> a
genericSwaps ctx perm = to <<< gswaps ctx perm <<< from

genericFreshen
  :: forall a rep m
   . Generic a rep
  => GenericAlpha rep
  => Fresh m
  => AlphaCtx -> a -> m (Tuple a (Perm AnyName))
genericFreshen ctx = liftM1 (first to) <<< gfreshen ctx <<< from

genericLFreshen
  :: forall a rep m b
   . Generic a rep
  => GenericAlpha rep
  => LFresh m
  => AlphaCtx -> a -> (a -> Perm AnyName -> m b) -> m b
genericLFreshen ctx a cont = glfreshen ctx (from a) (cont <<< to)

genericACompare
  :: forall a rep
   . Generic a rep
  => GenericAlpha rep
  => AlphaCtx -> a -> a -> Ordering
genericACompare ctx = gacompare ctx `on` from



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


instance alphaUnit :: Alpha Unit where
  aeq' _ _ _         = true
  fvAny' _ _ i       = pure i
  close _ _ i        = i
  open _ _ i         = i
  isPat _            = mempty
  isTerm _           = mempty
  isEmbed _          = false
  nthPatFind _       = mempty
  namePatFind _      = mempty
  swaps' _ _ i       = i
  freshen' _ i       = pure $ Tuple i mempty
  lfreshen' _ i cont = cont i mempty
  acompare' _ _ _    = EQ


instance alphaTuple :: (Alpha a, Alpha b) => Alpha (Tuple a b) where
  aeq' ctx (Tuple x1 y1) (Tuple x2 y2) =
    aeq' ctx x1 x2 && aeq' ctx y1 y2

  fvAny' ctx nfn (Tuple x y) =
    Tuple <$> fvAny' ctx nfn x
          <*> fvAny' ctx nfn y

  close ctx b (Tuple x y)     = Tuple (close ctx b x) (close ctx b y)
  open ctx b (Tuple x y)      = Tuple (open ctx b x) (open ctx b y)
  isPat (Tuple x y)           = isPat x <> isPat y
  isTerm (Tuple x y)          = isTerm x <> isTerm y
  isEmbed _                   = false
  nthPatFind (Tuple x y)      = nthPatFind x <> nthPatFind y
  namePatFind (Tuple x y)     = namePatFind x <> namePatFind y
  swaps' ctx perm (Tuple x y) = Tuple (swaps' ctx perm x) (swaps' ctx perm y)

  freshen' ctx (Tuple x y) = do
    Tuple y' perm2 <- freshen' ctx y
    Tuple x' perm1 <- freshen' ctx (swaps' ctx perm2 x)
    pure $ Tuple (Tuple x' y') (perm1 <> perm2)

  lfreshen' ctx (Tuple x y) cont =
    lfreshen' ctx y $ \y' perm2 ->
    lfreshen' ctx (swaps' ctx perm2 x) $ \x' perm1 ->
    cont (Tuple x' y') (perm1 <> perm2)

  acompare' ctx (Tuple x1 y1) (Tuple x2 y2) =
    acompare' ctx x1 x2 <> acompare' ctx y1 y2



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
          Right (AnyName (Tuple t v)) ->
            if t == typeOf (Proxy :: Proxy (Name a))
            then unsafeCoerce v
            else unsafeThrow "open: inconsistent sorts"
          Left _ -> unsafeThrow "inconsistency - pattern had too few variables"
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

  namePatFind nm = NamePatFind
    { runNamePatFind: \anynm ->
      case toSortedName anynm of
        Just v | v == nm -> Right 0
        _                -> Left 1
    }

  swaps' ctx perm nm =
    if isTermCtx ctx
    then
      case PermM.apply perm (mkAnyName nm) of
        AnyName (Tuple t v) ->
          if t == typeOf (Proxy :: Proxy (Name a))
          then unsafeCoerce v
          else unsafeThrow "Internal error swaps' on a Name returned permuted name of wrong sort"
    else nm

  freshen' ctx nm =
    if not (isTermCtx ctx)
    then do
      nm' <- fresh nm
      pure $ Tuple nm' (single (mkAnyName nm) (mkAnyName nm'))
    else unsafeThrow "freshen' on a Name in term position"

  lfreshen' ctx nm cont =
    if not (isTermCtx ctx)
    then do
      nm' <- lfresh nm
      avoid [mkAnyName nm'] <<< cont nm' $ single (mkAnyName nm) (mkAnyName nm')
    else unsafeThrow "lfreshen' on a Name in term position"

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

  fvAny' ctx nfn n@(AnyName (Tuple _ v)) =
    if isTermCtx ctx && isFreeName v
    then nfn n
    else pure n

  isTerm _ = mempty

  isPat n@(AnyName (Tuple _ v)) =
    if isFreeName v
    then Just $ S.singleton n
    else Nothing

  swaps' ctx perm n =
    if isTermCtx ctx
    then PermM.apply perm n
    else n

  freshen' ctx nm@(AnyName (Tuple t v)) =
    if isTermCtx ctx
    then unsafeThrow "freshen' on AnyName in Term mode"
    else do
      v' <- fresh v
      let nm' = AnyName $ Tuple t v'
      pure $ Tuple nm' (single nm nm')

  lfreshen' ctx nm@(AnyName (Tuple t v)) cont =
    if isTermCtx ctx
    then unsafeThrow "lfreshen' on AnyName in Term mode"
    else do
      v' <- lfresh v
      let nm' = AnyName $ Tuple t v'
      avoid [nm'] <<< cont nm' $ single nm nm'

  open ctx b (AnyName (Tuple t v)) =
    let v' = open ctx b v
    in  AnyName $ Tuple t v'

  close ctx b (AnyName (Tuple t v)) =
    let v' = close ctx b v
    in  AnyName $ Tuple t v'

  nthPatFind nm = NthPatFind { runNthPatFind: \i ->
    if i == 0 then Right nm else Left $ i - 1 }

  namePatFind nmHave = NamePatFind { runNamePatFind: \nmWant ->
    if nmHave == nmWant then Right 0 else Left 1 }

  isEmbed _ = false

  acompare' ctx (AnyName (Tuple t1 v1)) (AnyName (Tuple t2 v2))
    | isTermCtx ctx =
      case compare t1 t2 of
        EQ  -> acompare' ctx v1 v2
        ord -> ord
    | otherwise = EQ
