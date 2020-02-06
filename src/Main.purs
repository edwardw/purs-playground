-- https://crypto.stanford.edu/~blynn/lambda/

module Main where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (dropWhile, many, reverse, some, union, (:), (!!))
import Data.Char.Unicode (isDigit)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl, foldr, notElem)
import Data.List.Lazy as ZL
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.ReadLine as NR
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (between, option, optional, try)
import Text.Parsing.Parser.String (anyChar, eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum)

data Term
  = Var String
  | App Term Term
  | Lam String Term

derive instance eqTerm :: Eq Term

instance showTerm :: Show Term where
  show (Lam s t) = "λ" <> s <> showB t
    where
    showB (Lam x y) = " " <> x <> showB y
    showB expr      = "." <> show expr
  show (Var s)   = s
  show (App x y) = showL x <> showR y
    where
    showL (Lam _ _) = "(" <> show x <> ")"
    showL _         = show x
    showR (Var s)   = " " <> s
    showR _         = "(" <> show y <> ")"

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

term :: Parser String Term
term = fix $ \p -> lam p <|> app p
  where
  lam p = flip (foldr Lam) <$> between lam0 lam1 (some var) <*> p
  lam0 = str "\\" <|> str "λ"
  lam1 = str "->" <|> str "."
  app p = foldl App <$> app0 p <*> many (app0 p)
  app0 p = Var <$> var <|> between (str "(") (str ")") p

line :: Parser String LambdaLine
line = between ws eof $ option Blank $
  try (Let <$> var <*> (str "=" *> term))
  <|> (Run <$> term)

type Env = Map String Term

eval :: Env -> Term -> Term
eval env (App (Var "quote") t)             = quote env t
eval env (App m a) | Lam v f <- eval env m =
  eval env $ beta env (Tuple v a) f
eval env (Var v)  | Just x <- lookup v env = eval env x
eval _ t                                   = t

beta :: Env -> Tuple String Term -> Term -> Term
beta env (Tuple v a) t = case t of
  Var s   | s == v        -> a
          | otherwise     -> Var s
  Lam s m | s == v        -> Lam s m
          | s `elem` fvs  -> Lam s1 <<< rec $ rename s s1 m
            where
            s1 = newName s $ fvs `union` fv env [] m
          | otherwise     -> Lam s (rec m)
  App m n                 -> App (rec m) (rec n)
  where
  rec = beta env (Tuple v a)
  fvs = fv env [] a

fv :: Env -> Array String -> Term -> Array String
fv env vs (Var s) | s `elem` vs            = []
                  | Just x <- lookup s env = fv env (s:vs) x
                  | otherwise              = [s]
fv env vs (App x y)                        = fv env vs x `union` fv env vs y
fv env vs (Lam s f)                        = fv env (s:vs) f

newName :: String -> Array String -> String
newName x ys = fromMaybe ""
  <<< ZL.head
  <<< ZL.filter (_ `notElem` ys)
  $ ((s <> _) <<< show) <$> (ZL.iterate (_ + 1) 1)
  where
  s = fromCharArray <<< reverse <<< dropWhile isDigit <<< reverse $ toCharArray x

rename :: String -> String -> Term -> Term
rename x x1 t = case t of
  Var s   | s == x    -> Var x1
          | otherwise -> t
  Lam s b | s == x    -> t
          | otherwise -> Lam s (rec b)
  App a b             -> App (rec a) (rec b)
  where
  rec = rename x x1

norm :: Env -> Term -> Term
norm env t = case eval env t of
  Var v   -> Var v
  Lam v m -> Lam v (rec m)
  App m n -> App (rec m) (rec n)
  where
  rec = norm env

quote :: Env -> Term -> Term
quote env t =case t of
  Var x   | Just t' <- lookup x env -> rec t'
          | otherwise               -> f 0 (\v -> App v $ Var x)
  App m n                           -> f 1 (\v -> App (App v (rec m)) (rec n))
  Lam x m                           -> f 2 (\v -> App v <<< Lam x $ rec m)
  where
  rec = quote env
  fvs = fv env [] t
  renameIfNeeded s | s `elem` fvs = newName s fvs
                   | otherwise    = s
  abc = renameIfNeeded <$> ["a", "b", "c"]
  a = fromMaybe "a" (abc!!0)
  b = fromMaybe "b" (abc!!1)
  c = fromMaybe "c" (abc!!2)
  f n g = Lam a (Lam b (Lam c (g <<< Var $ fromMaybe "a" (abc!!n))))

prompt :: NR.Interface -> NR.LineHandler Unit -> Effect Unit
prompt console handler = do
  NR.prompt console
  NR.setLineHandler console handler

repl :: NR.Interface -> Env -> NR.LineHandler Unit
repl console env s = do
  case runParser s line of
    Left err -> do
      log $ "parse error: " <> show err
      prompt console $ repl console env
    Right Blank -> do
      prompt console $ repl console env
    Right (Run t) -> do
      logShow $ norm env t
      prompt console $ repl console env
    Right (Let v t) -> do
      prompt console <<< repl console $ M.insert v t env

main :: Effect Unit
main = do
  console <- NR.createConsoleInterface NR.noCompletion
  NR.setPrompt "> " 2 console
  prompt console $ repl console M.empty
