-- https://crypto.stanford.edu/~blynn/lambda/pcf.html
-- https://github.com/quchen/articles/blob/master/hindley-milner/src/HindleyMilner.hs

module HindleyMilner where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, some, zip)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl, foldr, intercalate, notElem)
import Data.Int (fromString)
import Data.List.Lazy as LL
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as S
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (words)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (replicateA)
import Run (Run, SProxy(..))
import Run.Console (CONSOLE, error, log, logShow)
import Run.Except (EXCEPT, catchAt, runExceptAt, throwAt)
import Run.Node.ReadLine (READLINE, prompt, setPrompt)
import Run.State (STATE, evalState, evalStateAt, get, getAt, modify, modifyAt, putAt)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, option, optional, try)
import Text.Parsing.Parser.String (anyChar, eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum, digit)



-- | Suppose we allow local let expression:
-- |
-- |    let _ = _ in _
-- |
-- | anywhere we expect a term. Then it might be reasonable to permit the following:
-- |
-- |    one = let f = \x.x in f succ (f 0)
-- |
-- | Apparently two occurrences of `f` have different types. Allowing different uses
-- | of a definition to have different types is called *let-polymorphism*.
-- |
-- | We demonstrate it with PCF (Programming Computable Functions), a simply typed
-- | lambda calculus with the base type `Nat` with the constant 0 and extended with:
-- |
-- |    - `pred`, `succ: these functions have the type `Nat -> Nat`.
-- |      Evaluating `pred 0` anywhere returns a `Err` which represent this exception.
-- |
-- |    - `ifz-then`else`: when given 0, evaluates to its `then` branch, otherwise
-- |      evaluates to its `else` branch.
-- |
-- |    - `fix`: the fix point operator, allowing recursion (but breaking normalization).
-- |
-- | We also provide an `undefined` keyword that throws an error.
-- |
-- | We say that
-- |
-- |    id = \x.x
-- |
-- | has type:
-- |
-- |    ∀X.X -> X
-- |
-- | The symbol ∀ indicates a given type variable is generalized. Lambda calculus with
-- | generalized type variables from let-polymorphism is known as the *Hindley-Milner*,
-- | or HM for short. HM is strongly normalizing.
-- |
-- | We are surprisingly close to Haskell 98.



-- #############################################################################
-- #############################################################################
-- * Preliminaries
-- #############################################################################
-- #############################################################################



-- #############################################################################
-- ** Names
-- #############################################################################


-- | A *name* is an identifier in the PCF we are going to typecheck.
-- | Variables on both the term and type level have `Name`s, for example.
newtype Name = Name String

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

instance showName :: Show Name where
  show (Name n) = n



-- #############################################################################
-- ** Monotypes
-- #############################################################################


-- | A monotype is an unquantified/unparametric type, in other words it contains
-- | no `forall`s. Monotypes are the inner building blocks of all types.
-- | Examples of mototypes are `Nat`, `a`, `a -> b`.
-- |
-- | In formal notation, `MType`s are often called τ (tau) types.
data MType
  = TNat
  | TVar Name
  | TFn MType MType

derive instance eqMType :: Eq MType

instance showMType :: Show MType where
  show = case _ of
    TNat          -> "Nat"
    TVar (Name n) -> n
    TFn a b       -> showL a <> " -> " <> show b
      where
      showL = case _ of
        TFn _ _ -> "(" <> show a <> ")"
        _       -> show a

-- | The free variables of an `MType`. This is simply the collection of all the
-- | individual type variables occurring inside of it.
-- |
-- | Example: the free variables of `a -> b` are `a` and `b`.
freeMType :: MType -> Set Name
freeMType = case _ of
  TNat    -> mempty
  TVar a  -> S.singleton a
  TFn a b -> freeMType a <> freeMType b

-- | Substitute all the type variables mentioned in the substitution, and
-- | leave everything else alone.
instance substitutableMType :: Substitutable MType where
  applySubst s@(Subst s') mt = case mt of
    i@TNat     -> i
    v@(TVar a) -> fromMaybe v $ M.lookup a s'
    TFn a b    -> TFn (applySubst s a) (applySubst s b)



-- #############################################################################
-- ** Polytypes
-- #############################################################################


-- | A polytype is a monotype universally quantified over a number of type
-- | variables. In Haskell, all definitions have polytypes, but since the
-- | `forall` is implicit they look a bit like monotypes. For examples, the type
-- | of `1 : Int` is actually `forall <nothing> Int`, and the type of `id` is
-- | `forall a. a -> a`.
-- |
-- | A polytype claims to work "for all imaginable type parameters", very
-- | similar to how a lambda claims to work "for imaginable value parameters".
-- | We can insert a value into a lambda's parameter to evaluate it to a new
-- | value, and similarly we'll later insert types into a polytype's quantified
-- | variables to gain new types.
-- |
-- | Example: in a definition `id :: forall a. a -> a`, the `a` after ∀
-- | (`forall`) is the collection of type variables, and `a -> a` is the `MType`
-- | quantified over. When we have such an `a`, we also have its specialized
-- | version `Int -> Int` available. This process will be the topic of the type
-- | inference/unification algorithms.
-- |
-- | In formal notation, `PType`s are often called σ (sigma) types.
-- |
-- | The purpose of having monotypes and polytypes is that we'd like to only
-- | have universal quantification at the top level, restricting our language
-- | to rank-1 polymorphism, where type inference is total (all types can be
-- | inferred) and simple (only a handful of typing rules). Weakening this
-- | constraint would be easy: if we allowed universal quantification within
-- | function types we would get rank-N polymorphism. Taking it even further
-- | to allow it anywhere, effectively replacing all occurrences of `MType`
-- | with `PType`, yields impredicative types. Both these extensions make the
-- | type system *significantly* more complex though.
data PType = Forall (Set Name) MType -- ^ ∀{α}. τ

derive instance eqPType :: Eq PType
derive instance ordMType :: Ord MType

instance showPType :: Show PType where
  show (Forall qs mt) = "∀" <> universals <> ". " <> show mt
    where
    universals
      | S.isEmpty qs = "∅"
      | otherwise    = intercalate " " $ S.map show qs


-- | The free variables of a `PType` are the free variables of contained
-- | `MType`, except those universally quantified.
freePType :: PType -> Set Name
freePType (Forall qs mt) = freeMType mt `S.difference` qs


-- | Substitute all free type variables.
instance substitutablePType :: Substitutable PType where
  applySubst (Subst s) (Forall qs mt) =
    let s' = M.filterKeys (_ `notElem` qs) s
    in Forall qs $ applySubst (Subst s') mt



-- #############################################################################
-- ** The environment
-- #############################################################################


-- | The environment consists of all the values available in scope, and their
-- | associated polytypes. Other common names for it include "(typing) context",
-- | and because of the commonly used symbol for it sometimes directly "Gamma",
-- | or Γ.
-- |
-- | There are two kinds of membership in an environment,
-- |
-- |    - ∈: an environment Γ can be viewed as a set of (value, type) pairs,
-- |      and we can test whether something us *literally contained* by it via
-- |      `x:σ ∈ Γ`
-- |    - ⊢, pronounced *entails*, describes all the things that are well-types,
-- |      given an environment Γ. `Γ ⊢ x:τ` can thus be seen as a judgment that
-- |      `x:τ` is *figuratively contained* in Γ.
-- |
-- | For example, the environment `{x:Int}` literally contains `x`, but given
-- | this, it also entails `λy. x`, `λy z. x`, `let id = λy. y in id x` and so
-- | on.
-- |
-- | In Purescript term, the environment consists of all the things you
-- | currently have available, or that can be built by combining them. If you
-- | import the Prelude, your environment entails
-- |
-- |    ```
-- |    identity      ∀ t a. Category a => a t t
-- |    map           ∀ a b f. Functor f => (a -> b) -> f a -> f b
-- |    show          ∀ a. Show a => a -> String
-- |    ...
-- |    identity map  ∀ a b f. Functor f => (a -> b) -> f a -> f b
-- |    map identity  ∀ f a. Functor f => f a -> f a
-- |    map show      ∀ f a. Functor f => Show a -> f a -> f String
-- |    ...
-- |    ```
newtype Gamma = Gamma (Map Name PType)

instance showEnv :: Show Gamma where
  show (Gamma env) = "Γ = { " <> intercalate "\n    , " showBindings <> " }"
    where
    bindings = M.toUnfoldable env :: Array (Tuple Name PType)
    showBinding (Tuple (Name n) ptype) = n <> " : " <> show ptype
    showBindings = map showBinding bindings

--| The free variables of an environment are all the free variables of the
--| `PType`s it contains.
freeEnv :: Gamma -> Set Name
freeEnv (Gamma env) =
  let all = M.values env
  in S.unions $ map freePType all

-- | Performing a substitution in an environment means performing that
-- | substitution on all the contained `PType`s.
instance substitutableEnv :: Substitutable Gamma where
  applySubst s (Gamma env) = Gamma $ map (applySubst s) env



-- #############################################################################
-- ** Substitutions
-- #############################################################################


-- | A substitution is a mapping from type variables to `MType`s. Applying a
-- | substitution means applying those replacements. For example, the
-- | substitution `a -> Int` applied to `a -> a` yields `Int -> Int`.
-- |
-- | A key concept behind Hindley-Milner is that once we dive deeper into an
-- | expression, we learn more about our type variables. We might learn that
-- | `a` has to be specialized to `b -> b`, and then later on that `b` is
-- | actually `Int`. Substitutions are an organized way of carrying this
-- | information along.
newtype Subst = Subst (Map Name MType)

instance showSubst :: Show Subst where
  show (Subst s) = "{ " <> intercalate "\n, " showSs <> " }"
    where
    ss = M.toUnfoldable s :: Array (Tuple Name MType)
    showS (Tuple (Name n) mt) = n <> " --> " <> show mt
    showSs = map showS ss

-- | Combine two substitutions by applying all substitutions mentioned in the
-- | first argument to the type variables contained in the second.
instance semigroupSubst :: Semigroup Subst where
  append s1 s2 = Subst (m1 `M.union` m2)
    where
    Subst m1 = s1
    Subst m2 = applySubst s1 s2

instance monoidSubst :: Monoid Subst where
  mempty = Subst M.empty

-- Laws:
--
--    ```
--    applySubst mempty ≡ id
--    applySubst (s1 <> s2) ≡ applySubst s1 <<< applySubst s2
--    ```
class Substitutable a where
  applySubst :: Subst -> a -> a


instance substitutableTuple
  :: (Substitutable a, Substitutable b)
  => Substitutable (Tuple a b) where
  applySubst s (Tuple a b) = Tuple (applySubst s a) (applySubst s b)

-- | `applySubst s1 s2` applies one substitution to another, replacing all the
-- | bindings in the second argument with their values mentions in the first
-- | one.
instance substitutableSubst :: Substitutable Subst where
  applySubst s (Subst target) = Subst $ map (applySubst s) target



-- #############################################################################
-- #############################################################################
-- * Typechecking
-- #############################################################################
-- #############################################################################


-- | Typechecking does two things:
-- |
-- |  1. If two types are not immediately identical, attempt to 'unify' them
-- |     to get a type compatible with both
-- |  2. 'infer' the most general type of a value by comparing the values in its
-- |     definition with the environment



-- #############################################################################
-- ** Inference context
-- #############################################################################
type Infer r a = Run (infererr :: EXCEPT InferError, inferenv :: STATE (LL.List Name) | r) a

_infererr = SProxy :: SProxy "infererr"
_inferenv = SProxy :: SProxy "inferenv"


-- | Errors that can happen during the type inference process.
data InferError
  -- | Twp types that don't match were attempted to be unified.
  -- |
  -- | For example, `a -> a` and `Int` don't unify.
  = CannotUnify MType MType

  -- | A `TVar` is bound to an `MType` that already contains it.
  -- |
  -- | The canonical example of this is `x. x x`, where the first `x` in the
  -- | body has to have type `a -> b`, and the second one `a`. Since they're
  -- | both the same `x`, this requires unification of `a` with `a -> b`, which
  -- | works iff `a = a -> b = (a -> b) -> b = ...`, yielding an infinite type.
  | OccursCheckFailed Name MType

  -- | The value of an unknown identifier was read.
  | UnknownIdentifier Name


instance showInferError :: Show InferError where
  show = case _ of
    CannotUnify mt1 mt2           -> "CannotUnify: " <> show mt1 <> " with " <> show mt2
    OccursCheckFailed (Name n) mt -> "OccursCheckFailed: " <> n <> " " <> show mt
    UnknownIdentifier (Name n)    -> "UnknownIdentifier: " <> n


-- | Evaluate a value in an inference context.
runInfer
  :: forall r a
   . Infer r a
  -> Run r (Either InferError a)
runInfer = evalStateAt _inferenv infiniteNames <<< runExceptAt _infererr
  where
  infiniteNames = map (\i -> Name $ "_" <> show i) $ LL.iterate (_ + 1) 0



-- #############################################################################
-- ** Unification
-- #############################################################################

-- Unification describes the process of making two different types compatible
-- by specializing them where needed. A desirable property to have here is being
-- able to find the most general unifier.


-- | The unification of two `MType`s is the most general substitution that can be
-- | applied to both of them in order to yield the same result.
unify :: forall r. Tuple MType MType -> Infer r Subst
unify = case _ of
  Tuple (TFn a b) (TFn x y) -> unifyBinary (Tuple a b) (Tuple x y)
  Tuple (TVar v)  x         -> v `bindVariableTo` x
  Tuple x         (TVar v)  -> v `bindVariableTo` x
  Tuple TNat      TNat      -> pure mempty
  Tuple a         b         -> throwAt _infererr $ CannotUnify a b

  where

    -- Build a substitution that binds a `Name` of a `TVar` to a `MType`. The
    -- resulting substitution should be idempotent.
    --
    -- Substituting a `Name` with a `TVar` with the same name unifies a type
    -- variable with itself, and the resulting substitution does nothing new.
    -- If the `Name` we are trying to bind to an `MType` already occurs in that
    -- `MType`, the resulting substitution would not be idempotent: the `MType`
    -- would be replaces again, yielding a different result. This is known as
    -- the *Occurs Check*.
    bindVariableTo name mt = case mt of
      TVar v | name == v     -> pure mempty
      _ | name `occursIn` mt -> throwAt _infererr $ OccursCheckFailed name mt
      _                      -> pure <<< Subst $ M.singleton name mt
      where
      occursIn n ty = n `S.member` freeMType ty


-- | Unification of binary type constructors, such as functions. Unification
-- | is first done for the first pair, and assuming the required substitution,
-- | for the second one.
unifyBinary :: forall r. Tuple MType MType -> Tuple MType MType -> Infer r Subst
unifyBinary (Tuple a b) (Tuple x y) = do
  s1 <- unify $ Tuple a x
  s2 <- unify $ applySubst s1 (Tuple b y)
  pure $ s1 <> s2



-- #############################################################################
-- ** Type inference
-- #############################################################################

-- | Type inference is the act of finding out a value's type by looking at the
-- | environment it is in, in order to make it compatible with it.
-- |
-- | In literature, the Hindley-Damas-Milner inference  algorithm ("Algorithm W")
-- | is often presented in the style of logic formulas. These formulas look a
-- | bit like fractions, where the "numerator" is a  collection of premises, and
-- | the "denominator" is the consequence if all of them hold.
-- |
-- | Example:
-- |
-- |    ```
-- |    Γ ⊢ even : Int -> Bool   Γ ⊢ 1 : Int
-- |    ------------------------------------
-- |             Γ ⊢ even 1 : Bool
-- |    ```
-- |
-- | means that if we have a value of type `Int -> Bool` called "even" and a
-- | value of type `Int` called `1`, then we also have a value of type `Bool`
-- | via `even 1` available to us.
-- |
-- | The actual inference rules are polymorphic versions of this example.



-- -----------------------------------------------------------------------------
-- *** The language: PCF
-- -----------------------------------------------------------------------------

-- | De Bruijn Index
newtype Dbi = Dbi Int

derive instance eqDbi :: Eq Dbi


data Term
  = Var Name Dbi
  | App Term Term
  | Lam Name Term

  -- Natural number literal
  | Nat Int

  | Let Name Term Term
  | Ifz Term Term Term

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Lam x y) = showLam x y
    where
    showLam x' y'     = "λ" <> show x' <> "." <> showB y'
    showB (Lam x' y') = showLam x' y'
    showB expr        = show expr

  show (Var s i) = showVar s i

  show (App x y) = showL x <> showR y
    where
    showL (Lam _ _) = "(" <> show x <> ")"
    showL _         = show x
    showR (Var s i) = " " <> showVar s i
    showR _         = "(" <> show y <> ")"

  show (Nat i)     = show i

  show (Ifz x y z) =
    "ifz " <> show x <> " then " <> show y <> " else " <> show z

  show (Let x y z) =
    "let " <> show x <> " = " <> show y <> " in " <> show z

showVar :: Name -> Dbi -> String
showVar (Name s) (Dbi i) = s
  <> if i == 0 then mempty else "@"
  <> show i

data PCFLine
  = Blank
  | TopLet Name Term
  | Run Term

derive instance eqPCFLine :: Eq PCFLine

instance showPCFLine :: Show PCFLine where
  show = case _ of
    Blank      -> "Blank"
    TopLet s t -> "Let " <> show s <> " = " <> show t
    Run t      -> "Run " <> show t



-- -----------------------------------------------------------------------------
-- *** Some useful definitions
-- -----------------------------------------------------------------------------

-- | Generate a fresh `Name` in a type inference context. An example use case of
-- | this is η expansion, which transforms `f` into `λx. f x`, where `x` is a
-- | new name, i.e. unbounded in the current context.
fresh :: forall r. Infer r MType
fresh = drawFromSupply >>= case _ of
  Right name -> pure $ TVar name
  Left err   -> throwAt _infererr err
  where
  drawFromSupply = do
    names <- getAt _inferenv
    let { head, tail } = fromMaybe { head: Name "_", tail: LL.nil } $ LL.uncons names
    putAt _inferenv tail
    pure $ Right head


-- | Add a new binding to the environment.
-- |
-- | The FP equivalent would be defining a new value, for example in module
-- | scope or in a `let` block. This corresponds to the "comma" operation used
-- | in formal definition:
-- |
-- |    ```
-- |    Γ, x:σ  ≡  extendGamma Γ (x,σ)
-- |    ```
extendGamma :: Gamma -> Tuple Name PType -> Gamma
extendGamma (Gamma env) (Tuple name pt) = Gamma $ M.insert name pt env


-- -----------------------------------------------------------------------------
-- *** Inferring the types of all language constructs
-- -----------------------------------------------------------------------------


infer :: forall r. Gamma -> Term -> Infer r (Tuple Subst MType)
infer env t = case t of
  Var name _ -> inferVar env name
  App f x    -> inferApp env f x
  Lam x e    -> inferLam env x e
  Nat _      -> pure $ Tuple (Subst M.empty) TNat
  Let x e e' -> inferLet env x e e'
  Ifz b e e' -> inferIfz env b e e'


-- | Inferring the type of a variable is done via
-- |
-- |    ```
-- |    x:σ ∈ Γ   τ = instantiate(σ)
-- |    ---------------------------- [Var]
-- |              Γ ⊢ x:τ
-- |    ```
-- |
-- | This means that if `Γ` *literally contains* (`∈`) a value, then it also
-- | *entails it* (`⊢`) in all its instantiations.
inferVar :: forall r. Gamma -> Name -> Infer r (Tuple Subst MType)
inferVar env name = do
  sigma <- lookupGamma env name -- x:σ ∈ Γ
  tau <- instantiate sigma    -- τ = instantiate(σ)
                              -- ------------------
  pure $ Tuple mempty tau     -- Γ ⊢ x:τ


lookupGamma :: forall r. Gamma -> Name -> Infer r PType
lookupGamma (Gamma env) name
  | name == Name "undefined" = Forall S.empty <$> fresh
  | otherwise =
    case M.lookup name env of
      Just x  -> pure x
      Nothing -> throwAt _infererr $ UnknownIdentifier name


instantiate :: forall r. PType -> Infer r MType
instantiate (Forall qs t) = do
  s <- substituteAllWithFresh qs
  pure $ applySubst s t
  where
  substituteAllWithFresh xs = do
    freshes <- replicateA (S.size xs) fresh
    let freshSubst = M.fromFoldable $ zip (S.toUnfoldable xs) freshes
    pure $ Subst freshSubst


inferApp :: forall r. Gamma -> Term -> Term -> Infer r (Tuple Subst MType)
inferApp env f x = do
  Tuple s1 fTau <- infer env f
  Tuple s2 xTau <- infer (applySubst s1 env) x
  fxTau <- fresh
  s3 <- unify $ Tuple (applySubst s2 fTau) (TFn xTau fxTau)
  let s = s1 <> s2 <> s3
  pure <<< Tuple s $ applySubst s3 fxTau


inferLam :: forall r. Gamma -> Name -> Term -> Infer r (Tuple Subst MType)
inferLam env x e = do
  tau <- fresh
  let sigma = Forall S.empty tau
  let env' = extendGamma env (Tuple x sigma)
  Tuple s tau' <- infer env' e
  pure <<< Tuple s $ TFn (applySubst s tau) tau'


inferLet :: forall r. Gamma -> Name -> Term -> Term -> Infer r (Tuple Subst MType)
inferLet env x e e' = do
  Tuple s1 tau <- infer env e
  let env' = applySubst s1 env
  let sigma = generalize env' tau
  let env'' = extendGamma env' $ Tuple x sigma
  Tuple s2 tau' <- infer env'' e'
  pure $ Tuple (s1 <> s2) tau'


inferIfz :: forall r. Gamma -> Term -> Term -> Term -> Infer r (Tuple Subst MType)
inferIfz env b e e' = do
  Tuple s1 bTau <- infer env b
  s2 <- unify $ Tuple TNat bTau

  resTau <- fresh
  Tuple s3 eTau <- infer env e
  Tuple s4 eTau' <- infer env e'
  s5 <- unify $ Tuple resTau eTau
  let resTau' = applySubst s5 resTau
  s6 <- unify $ Tuple resTau' eTau'

  pure $ Tuple (s1 <> s2 <> s3 <> s4 <> s5 <> s6) resTau'



generalize :: Gamma -> MType -> PType
generalize env mt = Forall qs mt
  where
  qs = freeMType mt `S.difference` freeEnv env




-- #############################################################################
-- #############################################################################
-- * Parsing
-- #############################################################################
-- #############################################################################


line :: Parser String PCFLine
line = between ws eof <<<
  option Blank $
  try (TopLet <$> var <*> (str "=" *> term))
  <|> (Run <$> term)


term :: Parser String Term
term = dbi M.empty <$> term'
  where
  term'  = fix $ \p -> lam p <|> app p <|> letx p <|> ifz p <|> nat

  lam p  = flip (foldr Lam)
       <$> between lam0 lam1 (some var)
       <*> p
  lam0   = str "\\" <|> str "λ"
  lam1   = str "."

  app p  = foldl App <$> app0 p <*> many (app0 p)
  app0 p = nat
       <|> flip Var (Dbi 0) <$> var
       <|> between (str "(") (str ")") p

  letx p = Let
    <$> (str "let" *> var)
    <*> (str "="   *> p)
    <*> (str "in"  *> p)

  ifz p  = Ifz
    <$> (str "ifz"  *> p)
    <*> (str "then" *> p)
    <*> (str "else" *> p)

  nat = try $ do
    n <- fromString <<< fromCharArray <$> some digit
    case n of
      Just n' -> do
        ws
        pure $ Nat n'
      Nothing -> fail "not a number"


str :: String -> Parser String Unit
str = (_ *> ws) <<< string


var :: Parser String Name
var = try $ do
  s <- fromCharArray <$> some alphaNum
  when (s `elem` words "ifz then else let in") $ fail "unexpected keyword"
  ws
  pure $ Name s


ws :: Parser String Unit
ws  = whiteSpace *> optional (try $ string "--" *> many anyChar)


-- | De Bruijn Indices
type DebruijnIx = Map Name Dbi


-- | Assign De Bruijn indices to `Var` terms, eliminating the need of
-- | renaming them.
dbi :: DebruijnIx -> Term -> Term
dbi ix t = case t of
  Var s _   -> Var s <<< fromMaybe (Dbi 0) $ M.lookup s ix
  Lam s m   -> Lam s $ dbi (ix' s) m
  App m n   -> App (dbi ix m) (dbi ix n)
  i@(Nat _) -> i
  Let s m n -> Let s (dbi ix m) $ dbi (ix' s) n
  Ifz m n o -> Ifz (dbi ix m) (dbi ix n) (dbi ix o)
  where
  -- The `Lam` and `Let` terms are binders, so increase every known `Var`s
  -- De Bruijn Index. Also overwrite the `Var` with the same name as `s`;
  -- it starts fresh.
  ix' s = M.insert s (Dbi 0) $ map (\(Dbi i) -> Dbi $ i + 1) ix




-- #############################################################################
-- #############################################################################
-- * Evaluation
-- #############################################################################
-- #############################################################################


-- #############################################################################
-- ** Evaluation context
-- #############################################################################
type Eval r a = Run (evalerr :: EXCEPT EvalError, evalenv :: STATE (Map Name Term) | r) a

_evalerr = SProxy :: SProxy "evalerr"
_evalenv = SProxy :: SProxy "evalenv"


-- | Errors that can happen during evaluation process.
data EvalError
  = ZeroHasNoPred
  | ExpectNat String
  | Undefined


instance showEvalError :: Show EvalError where
  show = case _ of
    ZeroHasNoPred -> "ZeroHasNoPred"
    ExpectNat msg -> "ExpectNat: " <> msg
    Undefined     -> "Undefined"


-- | Evaluate a value in an evaluation context.
runEval
  :: forall r a
   . Eval r a
  -> Run r (Either EvalError a)
runEval = evalStateAt _evalenv M.empty <<< runExceptAt _evalerr


-- The interesting this is, once we are certain a closed term is well-types,
-- we can ignore the types and evaluate as we would in untyped lambda calculus.
eval :: forall r. Term -> Eval r Term
eval t = do
  env <- getAt _evalenv
  case t of
    Var s _ | Just t' <- M.lookup s env -> eval t'
            | s == Name "undefined"     -> throwAt _evalerr Undefined

    App e e' -> do
      e'' <- eval e
      case e'' of
        Lam x f -> eval $ beta f x e'
        Var (Name "pred") _ -> do
          nat <- eval e'
          case nat of
            Nat 0 -> throwAt _evalerr ZeroHasNoPred
            Nat i -> pure $ Nat (i - 1)
            t'    -> pure $ App e'' t'
        Var (Name "succ") _ -> do
          nat <- eval e'
          case nat of
            Nat i -> pure $ Nat (i + 1)
            t'    -> pure $ App e'' t'
        Var (Name "fix") _ -> eval $ App e' (App e'' e')
        _ -> pure $ App e'' e'

    Let x e e' -> eval $ beta e' x e

    Ifz x e e' -> do
      nat <- eval x
      case nat of
        Nat 0   -> eval e
        Nat _   -> eval e'
        Var _ _ -> pure t
        t'      -> throwAt _evalerr <<< ExpectNat $ show t'

    _ -> pure t


norm :: forall r. Term -> Eval r Term
norm t = do
  t' <- eval t
  case t' of
    v@(Var _ _) -> pure v
    Lam x f     -> Lam x <$> norm f
    App e e'    -> App   <$> norm e <*> norm e'
    Let x e e'  -> Let x <$> norm e <*> norm e'
    Ifz x e e'  -> Ifz   <$> norm x <*> norm e <*> norm e'
    n@(Nat _)   -> pure n


-- Beta reduction under De Bruijn Index:
--
--    (λ. t) u ~> ↑(-1,0)(t[0 := ↑(1,0)u])
--
beta :: Term -> Name -> Term -> Term
beta t v u = shift (-1) 0 <<< subst t v (Dbi 0) $ shift 1 0 u


-- De Bruijn Substitution: The substitution of a term `s` for variable number
-- `j` in a term `t`, written `t[j := s]`, is defined as follows:
--
--    k[j := s]       =  | s  if k = j
--                       | k  otherwise
--    (λ. t)[j := s]  =  λ. t[j+1 := ↑(1,0)s]
--    (t u)[j := s]   =  (t[j := s]  u[j := s])
--
subst :: Term -> Name -> Dbi -> Term -> Term
subst t v j@(Dbi j') s = case t of
  Var x k | x == v && k == j -> s
          | otherwise        -> t
  Lam x m | x == v    -> t
          | otherwise -> Lam x (subst m v (Dbi $ j'+1) (shift 1 0 s))
  App m n   -> App (rec m) (rec n)
  Let y m n -> Let y (rec m) (rec n)
  Ifz m n o -> Ifz (rec m) (rec n) (rec o)
  n@(Nat _) -> n
  where
  rec t' = subst t' v j s


-- De Bruijn Shifting: The `d`-place shift of a term t above cutoff `c`, written
-- `↑(d,c)t`, is defined as follows:
--
--    ↑(d,c)k       =  | k     if k < c
--                     | k + d if k >= c
--    ↑(d,c)(λ. t)  =  λ. ↑(d,c+1)t
--    ↑(d,c)(t u)   =  (↑(d,c)t ↑(d,c)u)
--
-- The `Let` term acts as a binder, so the term `n` in:
--
--    let s = m in n
--
-- should also has cutoff `c + 1`.
--
shift :: Int -> Int -> Term -> Term
shift d c t = case t of
  Var s k@(Dbi k')
    | k' < c    -> Var s k
    | otherwise -> Var s (Dbi $ k'+d)
  Lam s t'      -> Lam s $ shift d (c+1) t'
  App m n       -> App (rec m) (rec n)
  Let s m n     -> Let s (rec m) (shift d (c+1) n)
  Ifz m n o     -> Ifz (rec m) (rec n) (rec o)
  n@(Nat _)     -> n
  where
  rec = shift d c




-- #############################################################################
-- #############################################################################
-- * REPL
-- #############################################################################
-- #############################################################################



-- #############################################################################
-- ** Prelude
-- #############################################################################


-- | Some predefined types.
prelude :: Gamma
prelude = Gamma $ M.fromFoldable
  [ Tuple (Name "pred")   (Forall S.empty (TFn TNat TNat))
  , Tuple (Name "succ")   (Forall S.empty (TFn TNat TNat))
  , Tuple (Name "fix")    (Forall (S.singleton nameFix) (TFn (TFn tyFix tyFix) tyFix))
  ]
  where
  nameFix = Name "__fix"
  tyFix = TVar nameFix


repl
  :: forall r
   . Run (console :: CONSOLE, readline :: READLINE | r)
         (Either EvalError (Either InferError Unit))
repl = go
  # catchAt _infererr logShow
  # catchAt _evalerr logShow
  # evalState prelude
  # runInfer
  # runEval

  where

  go = do
    setPrompt "λ> "
    input <- prompt
    when (input /= "\\d") do
      case runParser input line of
        Left err ->
          error $ "parse error: " <> show err
        Right Blank -> pure unit
        Right (Run lambda) -> do
          _ <- typeOf lambda
          res <- norm lambda
          logShow res
        Right (TopLet s lambda) -> do
          ty <- typeOf lambda
          log $ "[" <> show s <> " : " <> show ty <> "]"
          modify $ \(Gamma gamma) -> Gamma $ M.insert s ty gamma
          modifyAt _evalenv $ M.insert s lambda
      go

  typeOf t = do
    gamma <- get
    t # infer gamma
      # map (generalize gamma <<< uncurry applySubst)
