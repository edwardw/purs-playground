module PCFUnbound where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.MonadPlus (empty)
import Data.Array (foldl, many, some)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep)
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, chainr1, option, try)
import Text.Parsing.Parser.Language (haskellStyle)
import Text.Parsing.Parser.String (eof, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(..), LanguageDef, digit, makeTokenParser, unGenLanguageDef)
import Unbound.LocallyNameless (class Alpha, class Subst, Bind, Embed(..), FreshM, Name, SubstName(..), bind_, fvSet, genericACompare, genericAeq, genericClose, genericFreshen, genericFvAny, genericIsPat, genericIsTerm, genericLFreshen, genericNamePatFind, genericNthPatFind, genericOpen, genericSubst, genericSubsts, genericSwaps, string2Name, subst, unbind_)


-- | PCF but implemented using Unbound.

data Ty = TNat | TVar (Name Ty) | Arr Ty Ty

derive instance eqTy :: Eq Ty
derive instance genericTy :: Generic Ty _

instance showTy :: Show Ty where
  show x = genericShow x

instance typeableTy :: Typeable Ty where
  typeOf _ = mkTyRep "PCFUnbound" "Ty"


data Term
  = Var (Name Term)
  | Lam (Bind (Tuple (Embed Ty) (Name Term)) Term)
  | App Term Term
  | Let (Bind (Name Term) Term) Term
  | Ifz Term Term Term
  | Nat Int

derive instance genericTerm :: Generic Term _

instance showTerm :: Show Term where
  show x = genericShow x

instance typeableTerm :: Typeable Term where
  typeOf _ = mkTyRep "PCFUnbound" "Term"


data PCFLine
  = Blank
  | TopLet (Name Term) Term
  | Run Term

instance showPCFLine :: Show PCFLine where
  show = case _ of
    Blank      -> "Blank"
    TopLet s t -> "TopLet " <> show s <> " = " <> show t
    Run t      -> "Run " <> show t


instance alphaTy :: Alpha Ty where
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


instance substTyTy :: Subst Ty Ty where
  isvar = case _ of
    TVar x -> Just $ SubstName identity x
    _      -> Nothing

  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


instance substTermTy :: Subst Term Ty where
  isvar _       = Nothing
  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x


instance substTerm :: Subst Term Term where
  isvar = case _ of
    Var x -> Just $ SubstName identity x
    _     -> Nothing

  isCoerceVar _ = Nothing
  subst n u x   = genericSubst n u x
  substs ss x   = genericSubsts ss x



--------------------------------------------------------------------------------
-- Parsing ---------------------------------------------------------------------
--------------------------------------------------------------------------------

pcfDef :: LanguageDef
pcfDef = LanguageDef (unGenLanguageDef haskellStyle)
          { reservedNames = ["ifz", "then", "else", "let", "in"]
          }


line :: Parser String PCFLine
line = between ws eof (option Blank pcf)
  where
  lexer  = makeTokenParser pcfDef
  ident  = lexer.identifier
  symbol = lexer.symbol
  colon  = lexer.colon
  dot    = lexer.dot
  ws     = lexer.whiteSpace

  pcf = try (TopLet <$> (string2Name <$> ident) <*> (symbol "=" *> term))
        <|> (Run <$> term)

  term = fix $ \p -> letx p <|> lam p <|> app p <|> ifz p

  letx expr = Let
          <$> do
                nm <- symbol "let" *> ident
                tm <- symbol "="   *> (nat <|> expr)
                pure $ bind_ (string2Name nm) tm
          <*> (symbol "in"  *> expr)

  lam expr = flip (foldr (\nm tm -> Lam $ bind_ nm tm))
         <$> between lam0 dot (some vt)
         <*> expr
  lam0     = oneOf ['\\', 'λ']

  app expr  = foldl App <$> app0 expr <*> many (app0 expr)
  app0 expr = (Var <<< string2Name) <$> ident
          <|> nat
          <|> lexer.parens expr

  ifz expr = Ifz
         <$> (symbol "ifz"  *> (nat <|> expr))
         <*> (symbol "then" *> (nat <|> expr))
         <*> (symbol "else" *> (nat <|> expr))

  vt    = flip Tuple
      <$> (string2Name <$> ident)
      <*> (Embed <$> option (TVar $ string2Name "t") (colon *> ty))
  ty    = fix $ \p -> ty0 p `chainr1` (symbol "->" *> pure Arr)
  ty0 p = (symbol "Nat" *> pure TNat)
      <|> ((TVar <<< string2Name) <$> ident)
      <|> lexer.parens p

  nat = do
    cs <- some digit
    case fromString $ SCU.fromCharArray cs of
      Just i  -> pure $ Nat i
      Nothing -> fail "not a number"



--------------------------------------------------------------------------------
-- Evaluation ------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Parallel beta-eta reduction
red :: Term -> FreshM Term
red = case _ of
  x@(Var _) -> pure x
  x@(Nat _) -> pure x

  Lam bnd -> do
    Tuple (Tuple ty x) e <- unbind_ bnd
    e' <- red e
    case e of
      -- possible eta-conversion
      App e1 (Var y) | y == x && not (S.member x (fvSet e1)) -> pure e1
      _ -> pure $ Lam (bind_ (Tuple ty x) e')

  App e1 e2 -> do
    e1' <- red e1
    e2' <- red e2
    case e1' of
      -- look for a beta-reduction
      Lam bnd -> do
        Tuple (Tuple _ x) e1'' <- unbind_ bnd
        pure $ subst x e2' e1''
      _ -> pure $ App e1' e2'

  Let bnd e2 -> do
    Tuple x e1 <- unbind_ bnd
    e1' <- red e1
    e2' <- red e2
    pure $ subst x e2' e1'

  Ifz e1 e2 e3 -> do
    e1' <- red e1
    case e1' of
      Nat 0 -> red e2
      Nat _ -> red e3
      _     -> do
        e2' <- red e2
        e3' <- red e3
        pure $ Ifz e1' e2' e3'


-- Small step reduction and its transitive closure
step :: Term -> MaybeT FreshM Term
step = case _ of
  Var _ -> empty
  Nat _ -> empty
  Lam _ -> empty

  App (Lam bnd) e2 -> do
    Tuple (Tuple _ x) e1 <- unbind_ bnd
    pure $ subst x e2 e1
  App e1 e2 ->
    App <$> step e1 <*> pure e2 <|>
    App <$> pure e1 <*> step e2

  Let bnd e2 -> do
    Tuple x e1 <- unbind_ bnd
    pure $ subst x e2 e1

  Ifz e1 e2 e3 -> do
    e1' <- step e1
    case e1' of
      Nat 0 -> step e2
      Nat _ -> step e3
      _     -> pure $ Ifz e1' e2 e3

tc :: forall m a. Monad m => (a -> MaybeT m a) -> a -> m a
tc f a = do
  ma' <- runMaybeT (f a)
  case ma' of
    Just a' -> tc f a'
    Nothing -> pure a


-- Recursively call reduction on child nodes to reduce other function
-- applications throughout the tree, resulting in the *normal form* of the
-- lambda term.
norm :: Term -> FreshM Term
norm e = red e >>= case _ of
  x@(Var _) -> pure x
  x@(Nat _) -> pure x

  Lam bnd -> do
    Tuple (Tuple ty x) e1 <- unbind_ bnd
    e1' <- norm e1
    pure $ Lam (bind_ (Tuple ty x) e1')

  App e1 e2 -> App <$> norm e1 <*> norm e2

  Let bnd e2 -> do
    Tuple x e1 <- unbind_ bnd
    e1' <- norm e1
    e2' <- norm e2
    pure $ subst x e2' e1'

  Ifz e1 e2 e3 -> Ifz <$> norm e1 <*> norm e2 <*> norm e3
