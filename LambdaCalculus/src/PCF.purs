module PCF
  ( PCFLine(..)
  , Term(..)
  , Type(..)
  , line
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, some)
import Data.Foldable (elem, foldl, foldr)
import Data.Map (Map, lookup)
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, chainr1, option, optional, try)
import Text.Parsing.Parser.String (anyChar, eof, string, whiteSpace)
import Text.Parsing.Parser.Token (alphaNum)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (words)

data Type
  = Nat
  | TV String
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
    showR (Var s i) = showVar s i
    showR _         = "(" <> show y <> ")"
  show (Ifz x y z) =
    "ifz" <> show x <> " then " <> show y <> " else " <> show z
  show (Let x y z) =
    "let" <> x <> " = " <> show y <> " in " <> show z
  show Err         = "*exception*"

showVar :: String -> Int -> String
showVar s i = s <> if i == 0 then mempty else "@" <> show i

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

-- De Bruijn Index
type DebruijnIndex = Map String Int

-- Assign De Bruijn indices to `Var` terms, eliminating the need of renaming them.
dbi :: DebruijnIndex -> Term -> Term
dbi ix (Var s _)             = Var s <<< fromMaybe 0 $ lookup s ix
dbi ix (Lam v@(Tuple s _) m) = Lam v (rec m)
  where
  -- The `Lam` term is the binder, so increase every known `Var`s De Bruijn index.
  -- Also overwrite the `Var` with the same name as `s`; it starts fresh.
  ix' = M.insert s 0 $ map (_ + 1) ix
  rec = dbi ix'
dbi ix (App m n)   = App (dbi ix m) (dbi ix n)
dbi ix (Ifz m n o) = Ifz (dbi ix m) (dbi ix n) (dbi ix o)
dbi ix (Let s m n) = Let s (dbi ix m) (dbi ix n)
dbi _ (Err)        = Err
