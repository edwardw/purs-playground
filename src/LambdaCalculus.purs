-- https://crypto.stanford.edu/~blynn/lambda/

module LambdaCalculus
  ( Term(..)
  , LambdaLine(..)
  , Env
  , term
  , line
  , eval
  , norm
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, some, (!!))
import Data.Foldable (foldl, foldr)
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser)
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

eval :: Env -> Term -> Term
eval env (App (Var "quote" _) t)            = quote env t
eval env (App m a) | Lam v f <- eval env m  =
  eval env $ beta env (Tuple v a) 0 f
eval env (Var v _) | Just x <- lookup v env = eval env x
eval _ t                                    = t

-- The 3rd argument is the number of binders seen so far.
beta :: Env -> Tuple String Term -> Int -> Term -> Term
beta env (Tuple v a) ix t = case t of
  Var s i | s == v && i == ix -> case a of
                                   Var x _   -> Var x i
                                   otherwise -> a
          | otherwise         -> Var s i
  Lam s m | s == v        -> Lam s m
          | otherwise     -> Lam s (rec (ix+1) m)
  App m n                 -> App (rec ix m) (rec ix n)
  where
  rec = beta env (Tuple v a)

norm :: Env -> Term -> Term
norm env t = case eval env t of
    Var v i -> Var v i
    Lam v m -> Lam v (rec m)
    App m n -> App (rec m) (rec n)
    where
    rec = norm env

quote :: Env -> Term -> Term
quote env t = case t of
  Var x _ | Just t' <- lookup x env -> rec t'
          | otherwise               -> f 0 (\v -> App v $ Var x 0)
  App m n                           -> f 1 (\v -> App (App v (rec m)) (rec n))
  Lam x m                           -> f 2 (\v -> App v <<< Lam x $ rec m)
  where
  rec = quote env
  abc = ["a", "b", "c"]
  f n g = Lam "a" (Lam "b" (Lam "c" (g <<< (flip Var 0) $ fromMaybe "a" (abc!!n))))

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
