-- https://crypto.stanford.edu/~blynn/lambda/

module LambdaCalculus
  ( Term(..)
  , LambdaLine(..)
  , eval
  , line
  , norm
  , term
  , program
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, some)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Run (Run)
import Run.Console (CONSOLE, error, logShow)
import Run.Node.ReadLine (READLINE, prompt, setPrompt)
import Run.State (STATE, get, modify)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (between, option, optional, try)
import Text.Parsing.Parser.String (anyChar, eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum)

-- | The Lambda Terms
-- |
-- | What's a bit special here is the `Int` field of `Var` term. It is to
-- | disambiguate variables with the same name in the same vein as De Bruijn
-- | Index [dbi]. Zero refers to the nearest bound variable with that name
-- | and the index increases by one for each binder:
-- |
-- |       +-refers to--+
-- |       |            |
-- |       v            |
-- |      λx -> λx -> x@1 x@0
-- |             ^          |
-- |             |          |
-- |             +-refers to+
-- |
-- | If all variables has the same name, then this `Int` behaves just like
-- | De Bruijn index.
-- |
-- | Under this scheme, the name of a variable combined with its De Bruijn
-- | index uniquely identifies it. There is no need to rename variables in
-- | order to avoid name clash any more.
-- |
-- | When pretty printing a term, only De Bruijn index greater than zero will
-- | be shown.
-- |
-- | This revised De Bruijn index naming scheme is inspired by
-- | Haskell-Morte-Library [morte].
-- |
-- | [dbi]: http://en.wikipedia.org/wiki/De_Bruijn_index
-- | [morte]: https://github.com/Gabriel439/Haskell-Morte-Library
data Term
  = Var String Int
  | App Term Term
  | Lam String Term

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Lam s t) = "λ" <> s <> "." <> showB t
    where
    showB (Lam x y) = "λ" <> x <> "." <> showB y
    showB expr      = show expr
  show (Var s i) = showVar s i
  show (App x y) = showL x <> showR y
    where
    showL (Lam _ _) = "(" <> show x <> ")"
    showL _         = show x
    showR (Var s i) = " " <> showVar s i
    showR _         = "(" <> show y <> ")"

showVar :: String -> Int -> String
showVar s i = s <> if i == 0 then mempty else "@" <> show i

-- | Extend the lambda term with a *let* statement.
data LambdaLine
  = Blank
  | Let String Term
  | Run Term

derive instance eqLambdaLine :: Eq LambdaLine

instance showLambdaLine :: Show LambdaLine where
  show = case _ of
    Blank   -> "Blank"
    Let s t -> "Let " <> s <> " = " <> show t
    Run t   -> "Run " <> show t

ws :: Parser String Unit
ws = whiteSpace *> optional (try $ string "--" *> many anyChar)

str :: String -> Parser String Unit
str = (_ *> ws) <<< string

var :: Parser String String
var = (fromCharArray <$> some alphaNum) <* ws

-- | Follow Haskell and interpret the backslash also as lambda. Also accept
-- | `->` in lieu of periods, and support line comments.
term :: Parser String Term
term = dbi M.empty <$> (fix $ \p -> lam p <|> app p)
  where
  lam p = flip (foldr Lam) <$> between lam0 lam1 (some var) <*> p
  lam0 = str "\\" <|> str "λ"
  lam1 = str "->" <|> str "."
  app p = foldl App <$> app0 p <*> many (app0 p)
  app0 p = (flip Var 0) <$> var <|> between (str "(") (str ")") p

line :: Parser String LambdaLine
line = between ws eof $ option Blank $
  try (Let <$> var <*> (str "=" *> term))
  <|> (Run <$> term)

-- To remember already defined *let* statements.
type Env = Map String Term

-- The `eval` function terminates once no more top-level β-reductions are possible.
eval :: Env -> Term -> Term
eval env (App (Var "quote" _) t)            = quote env t
eval env (App m a) | Lam v f <- eval env m  =
  eval env $ beta f v a
eval env (Var v _) | Just x <- lookup v env = eval env x
eval _ t                                    = t

-- Recursively call `eval` on child nodes to reduce other function applications
-- throughout the tree, resulting in the *normal form* of the lambda term.
norm :: Env -> Term -> Term
norm env t = case eval env t of
    Var v i -> Var v i
    Lam v m -> Lam v (rec m)
    App m n -> App (rec m) (rec n)
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
  Lam x m | x == v           -> t
          | otherwise        -> Lam x (subst m v (j+1) (shift 1 0 s))
  App m n                    -> App (subst m v j s) (subst n v j s)

-- De Bruijn Shifting: The `d`-place shift of a term t above cutoff `c`, written
-- `↑(d,c)t`, is defined as follows:
--
--    ↑(d,c)k       =  | k     if k < c
--                     | k + d if k >= c
--    ↑(d,c)(λ. t)  =  λ. ↑(d,c+1)t
--    ↑(d,c)(t u)   =  (↑(d,c)t ↑(d,c)u)
--
shift :: Int -> Int -> Term -> Term
shift d c t = case t of
  Var s k | k < c     -> Var s k
          | otherwise -> Var s (k+d)
  Lam s t'            -> Lam s $ shift d (c+1) t'
  App m n             -> App (shift d c m) (shift d c n)

-- In "Efficient Self-Interpretation in Lambda Calculus", Mogensen describes a
-- self-encoding of lambda terms. If we denote the encoding of a term `T` with
-- `[T]`, then we can recursively encode any term with the following rules:
--
--    ```
--    [x]    = λabc.ax        ~> shift 3 0 x
--    [MN]   = λabc.b[M][N]   ~> shift 3 0 [M] && shift 3 0 [N]
--    [λx.M] = λabc.c(λx.[M]) ~> M is already under one binder,
--                               thus the cutoff should be 1:
--                               shift 3 1 [M]
--    ```
--
-- We also need to shift the encoded term accordingly to maintain the invariant
-- under De Bruijn Index.
--
quote :: Env -> Term -> Term
quote env t = case t of
  Var x _ | Just t' <- lookup x env -> rec t'
          | otherwise               -> f "a" 2 (\v -> App v $ shift 3 0 t)
  App m n -> f "b" 1 (\v -> App (App v (shift 3 0 $ rec m)) (shift 3 0 $ rec n))
  Lam x m -> f "c" 0 (\v -> App v <<< Lam x <<< shift 3 1 $ rec m)
  where
  rec = quote env
  f s i g = Lam "a" (Lam "b" (Lam "c" (g $ Var s i)))

-- De Bruijn Index
type DebruijnIndex = Map String Int

-- Assign De Bruijn indices to `Var` terms, eliminating the need of renaming them.
dbi :: DebruijnIndex -> Term -> Term
dbi ix (Var s _) = Var s <<< fromMaybe 0 $ lookup s ix
dbi ix (Lam s m) = Lam s (rec m)
  where
  -- The `Lam` term is the binder, so increase every known `Var`s De Bruijn index.
  -- Also overwrite the `Var` with the same name as `s`; it starts fresh.
  ix' = M.insert s 0 $ map (_ + 1) ix
  rec = dbi ix'
dbi ix (App m n) = App (rec m) (rec n)
  where
  rec = dbi ix


-- REPL
program :: forall r. Run ( state :: STATE Env
                         , readline :: READLINE
                         , console :: CONSOLE
                         | r) Unit
program = do
  setPrompt "λ> "
  s <- prompt
  when (s /= "\\d") do
    case runParser s line of
      Left err ->
        error $ "parse error: " <> show err
      Right Blank -> pure unit
      Right (Run t) -> do
        env <- get
        logShow $ norm env t
      Right (Let v t) ->
        modify $ M.insert v t
    program
