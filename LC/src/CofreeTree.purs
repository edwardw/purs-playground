-- https://brianmckenna.org/blog/type_annotation_cofree

module CofreeTree where

import Prelude
import Control.Apply (lift2)
import Control.Comonad (extend)
import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Control.Monad.State (State, evalState, get, modify_)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldl, foldlDefault, foldrDefault)
import Data.Functor.Mu (Mu(..))
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe, maybe')
import Data.Ord (class Ord1)
import Data.TacitString as TS
import Data.Text.Prettyprint.Doc (defaultLayoutOptions, hardline, indent, layoutPretty, pretty, vsep, (<+>))
import Data.Text.Prettyprint.Doc.Render.Text (render)
import Data.Traversable (class Traversable, sequence, sequenceDefault)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)



data AST a
  = ALambda String a
  | AApply a a
  | ANumber Int
  | AString String
  | AIdent String


derive instance eqAST :: Eq a => Eq (AST a)
derive instance eq1AST :: Eq1 AST
derive instance ordAST :: Ord a => Ord (AST a)
derive instance ord1AST :: Ord1 AST
derive instance functorAST :: Functor AST


instance showAST :: Show a => Show (AST a) where
  show = case _ of
    ALambda s x -> renderDoc (pretty "ALambda" <+> pretty s <+> hardline <+> indent 2 (pretty (show x)))
    AApply x y  -> renderDoc (pretty "AApply" <+> hardline <+> indent 2 (vsep [pretty (show x), pretty (show y)]))
    ANumber i   -> "ANumber " <> show i
    AString s   -> "AString " <> s
    AIdent s    -> "AIdent " <> s

    where

    renderDoc = render <<< layoutPretty defaultLayoutOptions


instance foldableAST :: Foldable AST where
  foldMap f ast = case ast of
    ALambda s x -> f x
    AApply x y  -> f x <> f y
    ANumber _   -> mempty
    AString _   -> mempty
    AIdent _    -> mempty

  foldr f y xs = foldrDefault f y xs
  foldl f y xs = foldlDefault f y xs


instance traversableAST :: Traversable AST where
  traverse f ast = case ast of
    ALambda s x -> ALambda s <$> f x
    AApply x y  -> AApply <$> f x <*> f y
    ANumber i   -> pure $ ANumber i
    AString s   -> pure $ AString s
    AIdent s    -> pure $ AIdent s

  sequence x = sequenceDefault x


data Type
  = TLambda Type Type
  | TVar Int
  | TNumber
  | TString


instance showType :: Show Type where
  show = case _ of
    TLambda x y -> "TLambda " <> show x <> " " <> show y
    TVar i      -> "TVar " <> show i
    TNumber     -> "TNumber"
    TString     -> "TString"


data Constraint = EqualityConstraint Type Type


instance showConstraint :: Show Constraint where
  show (EqualityConstraint x y) = "EqualityConstraint " <> show x <> " "  <> show y


data TypeResult
  = TypeResult
    { constraints :: Array Constraint
    , assumptions :: Map String (Array Type)
    }


instance showTypeResult :: Show TypeResult where
  show (TypeResult r) = "TypeResult { constraints: "
                     <> show r.constraints
                     <> ", assumptions: "
                     <> show r.assumptions


instance semigroupTypeResult :: Semigroup TypeResult where
  append (TypeResult r1) (TypeResult r2) =
    TypeResult { constraints: r1.constraints <> r2.constraints
               , assumptions: r1.assumptions <> r2.assumptions
               }


instance monoidTypeResult :: Monoid TypeResult where
  mempty = TypeResult { constraints: mempty, assumptions: mempty }


data TypeState t m = TypeState Int (Map t m)


type TypeCheck t = State (TypeState t (Tuple Type TypeResult)) (Tuple Type TypeResult)


freshVarId :: forall t m. State (TypeState t m) Type
freshVarId = do
  TypeState i _ <- get
  modify_ $ \(TypeState j memo) -> TypeState (j+1) memo
  pure $ TVar i


memoizedTC :: forall c. Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
memoizedTC f c = do
  TypeState _ memo <- get
  maybe' memoize pure $ M.lookup c memo
  where
  memoize _ = do
    r <- f c
    modify_ $ \(TypeState i memo) -> TypeState i $ M.insert c r memo
    pure r


cofreeMu :: forall f. Functor f => Mu f -> Cofree f Unit
cofreeMu (In f) = unit :< map cofreeMu f


attribute :: Cofree AST Unit -> Cofree AST (Tuple Type TypeResult)
attribute c =
  let initial = TypeState 0 M.empty
  in evalState (sequence $ extend (memoizedTC generateConstraints) c) initial


generateConstraints :: Cofree AST Unit -> TypeCheck (Cofree AST Unit)
generateConstraints c = case tail c of
  ANumber _ -> pure $ Tuple TNumber mempty
  AString _ -> pure $ Tuple TString mempty
  AIdent s  -> do
    var <- freshVarId
    pure $ Tuple var (TypeResult { constraints: mempty
                                 , assumptions: M.singleton s [var]
                                 }
                     )

  ALambda s b -> do
    var <- freshVarId
    Tuple t (TypeResult tr) <- memoizedTC generateConstraints b
    let cs = maybe [] (map $ EqualityConstraint var) (M.lookup s tr.assumptions)
        as' = M.delete s tr.assumptions
    pure $ Tuple (TLambda var t)
                 (TypeResult { constraints: tr.constraints <> cs
                             , assumptions: as'
                             }
                 )

  AApply x y -> do
    var <- freshVarId
    Tuple xt xtr <- memoizedTC generateConstraints x
    Tuple yt ytr <- memoizedTC generateConstraints y
    pure $ Tuple var
                 (xtr <> ytr
                  <> TypeResult { constraints: [EqualityConstraint xt $ TLambda yt var]
                                , assumptions: mempty
                                }
                 )


solveConstraints :: Array Constraint -> Maybe (Map Int Type)
solveConstraints =
  foldl (\b a -> lift2 append (solve b a) b) $ Just M.empty
  where
  solve maybeSubs (EqualityConstraint a b) = do
    subs <- maybeSubs
    mostGeneralUnifier (substitute subs a) (substitute subs b)


mostGeneralUnifier :: Type -> Type -> Maybe (Map Int Type)
mostGeneralUnifier (TVar i) b = Just $ M.singleton i b
mostGeneralUnifier a (TVar i) = Just $ M.singleton i a
mostGeneralUnifier TNumber TNumber = Just M.empty
mostGeneralUnifier TString TString = Just M.empty
mostGeneralUnifier (TLambda a b) (TLambda c d) = do
  s1 <- mostGeneralUnifier a c
  lift2 append (mostGeneralUnifier (substitute s1 b) (substitute s1 d)) $ Just s1
mostGeneralUnifier _ _ = Nothing


substitute :: Map Int Type -> Type -> Type
substitute subs v@(TVar i) = maybe v (substitute subs) $ M.lookup i subs
substitute subs (TLambda a b) = TLambda (substitute subs a) (substitute subs b)
substitute _ t = t


typeTree :: Cofree AST Unit -> Maybe (Cofree AST Type)
typeTree c =
  let result = attribute c
      Tuple _ (TypeResult tr) = head result
  in solveConstraints tr.constraints >>= \subs ->
     Just $ map (substitute subs <<< fst) result


-- Current implementation of `Data.Typeable` can't handle kind such as `Mu`.
-- Now the polykind feature has landed, need to revisit and try to define AST
-- using Unbound in Purescript 0.14.
example :: Mu AST
example =
  In $ AApply (In (ALambda "x" (In $ AIdent "x"))) (In $ ANumber 2) -- (λx.x)2


main :: Effect Unit
main = do
  log $ showCofree (cofreeMu example)
  log $ showCofree (attribute (cofreeMu example))
  log $ case typeTree (cofreeMu example) of
    Just ast -> "Just " <> showCofree ast
    Nothing  -> "Nothing"
  where
  showCofree
    :: forall f a
     . Show (f TS.TacitString)
    => Functor f
    => Show a
    => Cofree f a
    -> String
  showCofree c =
    "("
    <> show (head c)
    <> " :< "
    <> show ((tail c) <#> (showCofree >>> TS.hush))
    <> ")"
