-- https://crypto.stanford.edu/~blynn/lambda/pcf.html

module PCF
  ( PCFLine(..)
  , Term(..)
  , Type(..)
  , eval
  , line
  , norm
  , repl
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, some, uncons, union, (:))
import Data.Either (Either(..))
import Data.Foldable (elem, foldMap, foldl, foldr)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Profunctor.Strong ((***))
import Data.Set (Set, member)
import Data.Set as S
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (words)
import Data.Tuple (Tuple(..), lookup, snd)
import Run (Run)
import Run.Console (CONSOLE, error, log, logShow)
import Run.Node.ReadLine (READLINE, prompt, setPrompt)
import Run.State (get, modify, runState)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, chainr1, option, optional, try)
import Text.Parsing.Parser.String (anyChar, eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum)

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
data Type
  = Nat
  -- Type variable
  | TV String
  -- Generalized type variable
  | GV String
  | Fn Type Type

derive instance eqType :: Eq Type

instance showType :: Show Type where
  show Nat = "Nat"
  show (TV s) = s
  show (GV s) = "@" <> s
  show (Fn t u) = showL t <> " -> " <> show u
    where
    showL (Fn _ _) = "(" <> show t <> ")"
    showL _        = show t

data Term
  = Var String Int
  | App Term Term
  | Lam (Tuple String Type) Term
  | Ifz Term Term Term
  | Let String Term Term
  | Err

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Lam (Tuple x t) y) = showLam x t y
    where
    showLam x' t' y'             = "λ" <> x' <> ":" <> show t' <> "." <> showB y'
    showB (Lam (Tuple x' t') y') = showLam x' t' y'
    showB expr                   = show expr
  show (Var s i) = showVar s i
  show (App x y) = showL x <> showR y
    where
    showL (Lam _ _) = "(" <> show x <> ")"
    showL _         = show x
    showR (Var s i) = " " <> showVar s i
    showR _         = "(" <> show y <> ")"
  show (Ifz x y z) =
    "ifz " <> show x <> " then " <> show y <> " else " <> show z
  show (Let x y z) =
    "let " <> x <> " = " <> show y <> " in " <> show z
  show Err         = "*exception*"

showVar :: String -> Int -> String
showVar s i = s
  <> if i == 0 || (isJust $ fromString s) then mempty else "@"
  <> show i

data PCFLine
  = Blank
  | TopLet String Term
  | Run Term

derive instance eqPCFLine :: Eq PCFLine

instance showPCFLine :: Show PCFLine where
  show = case _ of
    Blank      -> "Blank"
    TopLet s t -> "Let " <> s <> " = " <> show t
    Run t      -> "Run " <> show t


--- Parsing

line :: Parser String PCFLine
line = between ws eof $ option Blank $
  try (TopLet <$> var <*> (str "=" *> term))
  <|> (Run <$> term)
  where
    term   = dbi M.empty <$> (fix $ \p -> ifz p <|> letx p <|> lam p <|> app p)
    letx p = Let <$> (str "let" *> var) <*> (str "=" *> p)
      <*> (str "in" *> p)
    ifz p  = Ifz <$> (str "ifz" *> p) <*> (str "then" *> p)
      <*> (str "else" *> p)
    lam p  = flip (foldr Lam) <$> between lam0 lam1 (some vt) <*> p
    lam0   = str "\\" <|> str "λ"
    lam1   = str "."
    vt     = Tuple <$> var <*> option (TV "_") (str ":" *> typ)
    typ    = fix $ \p ->
      ((str "Nat" *> pure Nat) <|> (TV <$> var) <|> between (str "(") (str ")") p)
      `chainr1` (str "->" *> pure Fn)
    app p  = foldl App <$> app0 p <*> many (app0 p)
    app0 p = (flip Var 0) <$> var <|> between (str "(") (str ")") p
    var    = try $ do
      s <- fromCharArray <$> some alphaNum
      when (s `elem` words "ifz then else let in") $ fail "unexpected keyword"
      ws
      pure s
    str = (_ *> ws) <<< string
    ws  = whiteSpace *> optional (try $ string "--" *> many anyChar)


--- Type Inference

-- Types of variables
type Gamma = Array (Tuple String Type)

-- Type constraints
type TyCs = { ty :: Type, cs :: Array (Tuple Type Type), ix :: Int }

-- Type instantiations
type TyIs = { ty :: Type, ins :: Gamma, ix :: Int }

gather :: Gamma -> Int -> Term -> TyCs
gather gamma i term = case term of
  Var "undefined" _ -> { ty: TV $ "_" <> show i, cs: [], ix: i + 1 }
  Var "fix" _ -> { ty: Fn (Fn a a) a, cs: [], ix: i + 1 }
    where a = TV $ "_" <> show i
  Var "pred" _ -> { ty: Fn Nat Nat, cs: [], ix: i }
  Var "succ" _ -> { ty: Fn Nat Nat, cs: [], ix: i }
  Var s _
    | Just _ <- fromString s -> { ty: Nat, cs: [], ix: i }
    | Just t <- lookup s gamma ->
      let { ty: t', ins: _, ix: j } = instantiate t i in { ty: t', cs: [], ix: j }
    | otherwise -> { ty: TV "_", cs: [Tuple (GV $ "undefined: " <> s) (GV "?")], ix: i }
  Lam (Tuple s (TV "_")) u -> { ty: Fn x tu, cs: cs, ix: j }
    where
    x = TV $ "_" <> show i
    { ty: tu, cs, ix: j } = gather (Tuple s x : gamma) (i + 1) u
  Lam (Tuple s t) u -> { ty: Fn t tu, cs, ix: j }
    where
    { ty: tu, cs, ix: j } = gather (Tuple s t : gamma) i u
  App t u -> { ty: x, cs: [Tuple tt (Fn tu x)] `union` cs1 `union` cs2, ix: k + 1 }
    where
    { ty: tt, cs: cs1, ix: j } = gather gamma i t
    { ty: tu, cs: cs2, ix: k } = gather gamma j u
    x = TV $ "_" <> show k
  Ifz s t u -> { ty: tt
               , cs: foldl union [Tuple ts Nat, Tuple tt tu] [cs1, cs2, cs3]
               , ix: l
               }
    where
    { ty: ts, cs: cs1, ix: j } = gather gamma i s
    { ty: tt, cs: cs2, ix: k } = gather gamma j t
    { ty: tu, cs: cs3, ix: l } = gather gamma k u
  Let s t u -> { ty: tu, cs: cs1 `union` cs2, ix: k }
    where
    { ty: tt, cs: cs1, ix: j } = gather gamma i t
    gen = generalize (foldMap (freeTV <<< snd) gamma) tt
    { ty: tu, cs: cs2, ix: k } = gather (Tuple s gen : gamma) j u
  Err -> { ty: TV "_", cs: [Tuple (GV "error") (GV "?")], ix: i }

-- | Generates fresh type variables for generalized type variables.
instantiate :: Type -> Int -> TyIs
instantiate = go []
  where
  go m ty i = case ty of
    GV s | Just t <- lookup s m -> { ty: t, ins: m, ix: i }
         | otherwise            -> { ty: x, ins: Tuple s x : m, ix: i + 1 }
           where x = TV $ "_" <> show i
    Fn t u -> { ty: Fn t' u', ins: m'', ix: i'' }
      where
      { ty: t', ins: m', ix: i' } = go m t i
      { ty: u', ins: m'', ix: i'' } = go m' u i'
    _ -> { ty: ty, ins: m, ix: i }

generalize :: Set String -> Type -> Type
generalize fvs ty = case ty of
  TV s | not $ member s fvs -> GV s
  Fn s t                    -> Fn (generalize fvs s) (generalize fvs t)
  _                         -> ty

freeTV :: Type -> Set String
freeTV = case _ of
  Fn a b -> freeTV a <> freeTV b
  TV tv  -> S.singleton tv
  _      -> S.empty

unify :: Array (Tuple Type Type) -> Either String Gamma
unify tys = case uncons tys of
  Nothing -> Right []
  Just { head: Tuple (GV s) (GV "?"), tail: _ } -> Left s
  Just { head: Tuple s t, tail } | s == t       -> unify tail
  Just { head: Tuple (TV x) t, tail }
    | x `member` freeTV t ->
      Left $ "infinite: " <> x <> " = " <> show t
    | otherwise ->
      (Tuple x t : _) <$> unify (join (***) (substTy $ Tuple x t) <$> tail)
  Just { head: Tuple s (TV y), tail } ->
    unify $ Tuple (TV y) s : tail
  Just { head: Tuple (Fn s1 s2) (Fn t1 t2), tail } ->
    unify $ Tuple s1 t1 : Tuple s2 t2 : tail
  Just { head: Tuple s t, tail: _ } ->
    Left $ "mismatch: " <> show s <> " /= " <> show t

substTy :: Tuple String Type -> Type -> Type
substTy p@(Tuple x t) ty = case ty of
  Fn a b        -> Fn (substTy p a) (substTy p b)
  TV y | y == x -> t
  _             -> ty

-- | Applies all the substitutions found during unify to the type expression
-- | returned by gather to compute the principal type of a given closed term
-- | in a given context.
typeOf :: Gamma -> Term -> Either String Type
typeOf gamma term = foldl (flip substTy) ty <$> unify cs
  where
  { ty, cs, ix: _ } = gather gamma 0 term


--- Evaluation

type Lets = Map String Term

-- The interesting this is, once we are certain a closed term is well-types,
-- we can ignore the types and evaluate as we would in untyped lambda calculus.
eval :: Lets -> Term -> Term
eval env term = case term of
  Var "undefined" _ -> Err
  Ifz m n o -> case eval env m of
    Err -> Err
    Var s _ -> case fromString s of
      Just 0 -> eval env n
      Just _ -> eval env o
      _      -> term
    _ -> term
  Let s m n -> eval env $ beta n s m
  App m n -> let m' = eval env m in case m' of
    Err -> Err
    Lam (Tuple v _) f -> eval env $ beta f v n
    Var "pred" _ -> case eval env n of
      Err -> Err
      v@(Var s ix) -> case fromString s of
        Just 0 -> Err
        Just i -> Var (show $ i - 1) ix
        _      -> App m' v
      t -> App m' t
    Var "succ" _ -> case eval env n of
      Err -> Err
      v@(Var s ix) -> case fromString s of
        Just i -> Var (show $ i + 1) ix
        _      -> App m' v
      t -> App m' t
    Var "fix" _ -> eval env (App n (App m' n))
    _ -> App m' n
  Var s _ | Just t <- M.lookup s env -> eval env t
  _ -> term

norm :: Lets -> Term -> Term
norm env term = case eval env term of
  Err         -> Err
  v@(Var _ _) -> v
  Lam vt m    -> Lam vt (rec m)
  App m n     -> App (rec m) (rec n)
  Ifz m n o   -> Ifz (rec m) (rec n) (rec o)
  Let s m n   -> Err
  where
  rec = norm env

-- Beta reduction under De Bruijn Index:
--
--    (λ. t) u ~> ↑(-1,0)(t[0 := ↑(1,0)u])
--
beta :: Term -> String -> Term -> Term
beta t v u = shift (-1) 0 <<< subst t v 0 $ shift 1 0 u

-- De Bruijn Substitution: The substitution of a term `s` for variable number
-- `j` in a term `t`, written `t[j := s]`, is defined as follows:
--
--    k[j := s]       =  | s  if k = j
--                       | k  otherwise
--    (λ. t)[j := s]  =  λ. t[j+1 := ↑(1,0)s]
--    (t u)[j := s]   =  (t[j := s]  u[j := s])
--
subst :: Term -> String -> Int -> Term -> Term
subst t v j s = case t of
  Var x k | x == v && k == j -> s
          | otherwise        -> t
  Lam x@(Tuple y _) m | y == v    -> t
                      | otherwise -> Lam x (subst m v (j+1) (shift 1 0 s))
  App m n   -> App (rec m) (rec n)
  Ifz m n o -> Ifz (rec m) (rec n) (rec o)
  Let y m n -> Let y (rec m) (rec n)
  Err       -> Err
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
  Var s k | k < c     -> Var s k
          | otherwise -> Var s (k+d)
  Lam s t'            -> Lam s $ shift d (c+1) t'
  App m n             -> App (rec m) (rec n)
  Ifz m n o           -> Ifz (rec m) (rec n) (rec o)
  Let s m n           -> Let s (rec m) (shift d (c+1) n)
  Err                 -> Err
  where
  rec = shift d c

-- De Bruijn Index
type DebruijnIndex = Map String Int

-- Assign De Bruijn indices to `Var` terms, eliminating the need of renaming them.
dbi :: DebruijnIndex -> Term -> Term
dbi ix term = case term of
  Var s _             -> Var s <<< fromMaybe 0 $ M.lookup s ix
  Lam v@(Tuple s _) m -> Lam v $ dbi (ix' s) m
  App m n             -> App (dbi ix m) (dbi ix n)
  Ifz m n o           -> Ifz (dbi ix m) (dbi ix n) (dbi ix o)
  Let s m n           -> Let s (dbi ix m) $ dbi (ix' s) n
  Err                 -> Err
  where
  -- The `Lam` and `Let` terms are binders, so increase every known `Var`s
  -- De Bruijn Index. Also overwrite the `Var` with the same name as `s`;
  -- it starts fresh.
  ix' s = M.insert s 0 $ map (_ + 1) ix


-- REPL
type Env = Tuple Gamma Lets

repl :: forall r. Run (readline :: READLINE , console :: CONSOLE | r) (Tuple Env Unit)
repl = runState (Tuple [] M.empty) repl'
  where
  repl' = do
    setPrompt "λ> "
    input <- prompt
    when (input /= "\\d") do
      case runParser input line of
        Left err ->
          error $ "parse error: " <> show err
        Right Blank -> pure unit
        Right (Run term) -> do
          Tuple gamma lets <- get
          case typeOf gamma term of
            Left msg -> error $ "bad type: " <> msg
            Right t -> do
              logShow $ norm lets term
        Right (TopLet s term) -> do
          Tuple gamma lets <- get
          case typeOf gamma term of
            Left msg -> error $ "bad type: " <> msg
            Right t -> do
              log $ "[" <> s <> " : " <> show t <> "]"
              let gamma' = Tuple s (generalize S.empty t) : gamma
              let lets' = M.insert s term lets
              modify $ const (Tuple gamma' lets')
      repl'
