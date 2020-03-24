module LCSmallStep where

import Prelude
import Control.Lazy (fix)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.MonadPlus (empty, (<|>))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Choice ((+++))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (chainl1)
import Text.Parsing.Parser.Language (haskellDef)
import Text.Parsing.Parser.Token (makeTokenParser)
import Unbound.LocallyNameless (class Alpha, class Subst, Bind, FreshM, Name, SubstName(..), bind_, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSubst, genericSubsts, genericSwaps, runFreshM, string2Name, subst, unbind_)


data Term
  = Var (Name Term)
  | App Term Term
  | Lam (Bind (Name Term) Term)

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show = case _ of
    Var x     -> "(Var " <> show x <> ")"
    App e1 e2 -> "(App " <> show e1 <> " " <> show e2 <> ")"
    Lam b     -> "Lam " <> show b

instance typeableTerm :: Typeable Term where
  typeOf _ = mkTyRep "LCSmallStep" "Term"

instance alphaTerm :: Alpha Term where
  aeq' ctx x y         = genericAeq ctx x y
  fvAny' ctx nfn x     = genericFvAny ctx nfn x
  close ctx b x        = genericClose ctx b x
  open ctx b x         = genericOpen ctx b x
  isPat x              = genericIsPat x
  isTerm x             = genericIsTerm x
  isEmbed _            = false
  nthPatFind x         = genericNthPatFind x
  namePatFind x        = genericNamePatFind x
  swaps' ctx perm x    = genericSwaps ctx perm x
  freshen' ctx x       = genericFreshen ctx x
  lfreshen' ctx a cont = genericLFreshen ctx a cont
  acompare' ctx x y    = genericACompare ctx x y

instance substTerm :: Subst Term Term where
  isvar = case _ of
    Var x -> Just $ SubstName identity x
    _     -> Nothing

  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


step :: Term -> MaybeT FreshM Term
step = case _ of
  Var _ -> empty
  Lam _ -> empty
  App (Lam b) t2 -> do
    Tuple x t1 <- unbind_ b
    pure $ subst x t2 t1
  App t1 t2 ->
    App <$> step t1 <*> pure t2 <|>
    App <$> pure t1 <*> step t2

tc :: forall m a. Monad m => (a -> MaybeT m a) -> (a -> m a)
tc f a = do
  ma' <- runMaybeT (f a)
  case ma' of
    Just a' -> tc f a'
    Nothing -> pure a

eval :: Term -> Term
eval x = runFreshM (tc step x)


-- Some example terms

lam :: String -> Term -> Term
lam x t = Lam $ bind_ (string2Name x) t

var :: String -> Term
var = Var <<< string2Name

idT = lam "y" (var "y") :: Term

foo = lam "z" (var "y") :: Term

trueT  = lam "x" (lam "y" (var "x")) :: Term
falseT = lam "x" (lam "x" (var "x")) :: Term


parseTerm :: Parser String Term
parseTerm = fix $ \p -> parseAtom p `chainl1` (pure App)
  where
  lexer    = makeTokenParser haskellDef
  parens   = lexer.parens
  brackets = lexer.brackets
  ident    = lexer.identifier

  parseAtom p = parens p
            <|> var <$> ident
            <|> lam <$> (brackets ident) <*> p

runTerm :: String -> Either ParseError Term
runTerm s = (identity +++ eval) $ runParser s parseTerm


-- example, 2 + 3 = 5:
--
--    ```
--    > runTerm "([m][n][s][z] m s (n s z)) ([s] [z] s (s z)) ([s][z] s (s (s z))) s z"
--    (Right (App (Var s) (App (Var s) (App (Var s) (App (Var s) (App (Var s) (Var z)))))))
--    ```
