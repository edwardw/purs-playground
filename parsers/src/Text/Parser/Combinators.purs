module Text.Parser.Combinators where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.MonadPlus (class MonadPlus)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.RWS (RWSResult(..), RWST(..))
import Control.Monad.State (StateT(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT(..))
import Control.Lazy (class Lazy)
import Data.Array ((:))
import Data.Array as A
import Data.Array.NonEmpty as NEA
import Data.Foldable (oneOf)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)



-- | `choice ps` tries to apply the parsers in the list `ps` in order, until
-- | one of them succeeds. Returns the value of the succeeding parser.
choice ::forall m a. Alternative m => Array (m a) -> m a
choice = oneOf


-- | `option x p` tries to apply parser `p`. If `p` fails without consuming
-- | input, it returns the value `x`, otherwise the value returned by `p`.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    priority = option 0 (digitToInt <$> digit)
-- |    ```
option :: forall m a. Alternative m => a -> m a -> m a
option x p = p <|> pure x


-- | `skipOptional p` tries to apply parser `p`.  It will parse `p` or nothing.
-- | It only fails if `p` fails after consuming input. It discards the result
-- | of `p`.
skipOptional :: forall m a. Alternative m => m a -> m Unit
skipOptional p = (unit <$ p) <|> pure unit


-- | `between open close p` parses `open`, followed by `p` and `close`. Return
-- | the value returned by `p`.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    braces  = between (symbol "{") (symbol "}")
-- |    ```
between :: forall m bra ket a. Applicative m => m bra -> m ket -> m a -> m a
between bra ket p = bra *> p <* ket


-- | `p \`surroundedBy\` f` is `p` surrounded by `f`. Shortcut for
-- | `between f f p`. As in `between`, returns the value returned by `p`.
surroundedBy :: forall m sur a. Applicative m => m a -> m sur -> m a
surroundedBy p bound = between bound bound p


-- | `sepBy p sep` parses zero or more occurrences of `p`, separated by `sep`.
-- | Returns an array of values returned by `p`.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    commaSep p  = p `sepBy` (symbol ",")
-- |    ```
sepBy
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (Array a)
sepBy p sep = sepBy1 p sep <|> pure []


-- | `sepBy1 p sep` parses one or more occurrences of `p`, separated by `sep`.
-- | Returns an array of values returned by `p`.
sepBy1
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (Array a)
sepBy1 p sep = (NEA.toArray <<< NEA.fromNonEmpty) <$> sepByNonEmpty p sep


-- | `sepByNonEmpty p sep` parses one or more occurrences of `p`, separated
-- | by `sep`. Returns a non-empty array of values returned by `p`.
sepByNonEmpty
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (NonEmpty Array a)
sepByNonEmpty p sep = (:|) <$> p <*> A.many (sep *> p)


-- | `sepEndBy1 p sep` parses one or more occurrences of `p`, separated and
-- | optionally ended by `sep`. Returns an array of values returned by `p`.
sepEndBy1
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (Array a)
sepEndBy1 p sep = (NEA.toArray <<< NEA.fromNonEmpty) <$> sepEndByNonEmpty p sep


-- | `sepEndByNonEmpty p sep` parses one or more occurrences of `p`, separated
-- | and optionally ended by `sep`. Returns a non-empty array of values
-- | returned by `p`.
sepEndByNonEmpty
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (NonEmpty Array a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])


-- | `sepEndBy p sep` parses zero or more occurrences of `p`, separated and
-- | optionally ended by `sep`, i.e. Haskell style statements. Returns an array
-- | of values returned by `p`.
-- |
-- | E.g,
-- |
-- |    ```
-- |    haskellStatements  = haskellStatement `sepEndBy` semi
-- |    ```
sepEndBy
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (Array a)
sepEndBy p sep = sepEndBy1 p sep <|> pure []


-- | `endBy1 p sep` parses one or more occurrences of `p`, separated and ended
-- | by `sep`. Returns an array of values returned by `p`.
endBy1
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (Array a)
endBy1 p sep = A.some (p <* sep)


-- | `endByNonEmpty p sep` parses one or more occurrences of `p`, separated and
-- | ended by `sep`. Returns a non-empty array of values returned by `p`.
endByNonEmpty
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (NonEmpty Array a)
endByNonEmpty p sep = (:|) <$> (p <* sep) <*> A.many (p <* sep)


-- | `endBy p sep` parses zero or more occurrences of `p`, separated and ended
-- | by `sep`. Returns an array of values returned by `p`.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    cStatements  = cStatement `endBy` semi
-- |    ```
endBy
  :: forall m sep a
   . Alternative m
  => Lazy (m (Array a))
  => m a
  -> m sep
  -> m (Array a)
endBy p sep = A.many (p <* sep)


-- | `count n p` parses `n` occurrences of `p`. If `n` is smaller or equal to
-- | zero, the parser equals to `pure []`. Returns an array of `n` values
-- | returned by `p`.
count :: forall m a. Applicative m => Int -> m a -> m (Array a)
count n p | n <= 0    = pure []
          | otherwise = sequence (A.replicate n p)


-- | `chainr p op x` parses zero or more occurrences of `p`, separated by
-- | by `op`. Returns a value obtained by a right associative application of
-- | all functions returned by `op` to the values returned by `p`. If there
-- | are no occurrences of `p`, the value `x` is returned.
chainr :: forall m a. Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p op x = chainr1 p op <|> pure x


-- | `chainr1 p op x` parses one or more occurrences of `p`, separated by
-- | `op`. Returns a value obtained by a right associative application of
-- | all functions returned by `op` to the values returned by `p`.
chainr1 :: forall m a. Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = ((flip <$> op <*> chainr1 p op) <|> pure identity) <*> p


-- | `chainl p op x` parses zero or more occurrences of `p`, separated by
-- | `op`. Returns a value obtained by a left associative application of
-- | all functions returned by `op` to the values returned by `p`. If there
-- | are zero occurrences of `p`, the value `x` is returned.
chainl :: forall m a. Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = chainl1 p op <|> pure x


-- | `chainl1 p op x` parses one or more occurrences of `p`, separated by
-- | `op` Returns a value obtained by a /left/ associative application of
-- | all functions returned by `op` to the values returned by `p`. . This
-- | parser can for example be used to eliminate left recursion which typically
-- |  occurs in expression grammars.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    expr   = term   `chainl1` addop
-- |    term   = factor `chainl1` mulop
-- |    factor = parens expr <|> integer
-- |
-- |    mulop  = (*) <$ symbol "*"
-- |         <|> div <$ symbol "/"
-- |
-- |    addop  = (+) <$ symbol "+"
-- |         <|> (-) <$ symbol "-"
-- |    ```
chainl1 :: forall m a. Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = rst p op <*> p
  where
  rst :: m a -> m (a -> a -> a) -> m (a -> a)
  rst p' op' = (\f y g x -> g (f x y)) <$> op'<*> p'<*> rst p' op'
           <|> pure identity


-- | `manyTill p end` applies parser `p` zero or more times until parser `end`
-- | succeeds. Returns the array of values returned by `p`. This parser can be
-- | used to scan comments:
-- |
-- |    ```
-- |    simpleComment   = do{ string "<!--"
-- |                        ; manyTill anyChar (try (string "-->"))
-- |                        }
-- |    ```
-- |
-- | Note the overlapping parsers `anyChar` and `string \"-->\"`, and therefore
-- | therefore the use of the 'try' combinator.
manyTill :: forall m end a. Alternative m => m a -> m end -> m (Array a)
manyTill p end = ([] <$ end) <|> ((:) <$> p <*> manyTill p end)


-- | Additional functionality needed to describe parsers independent of input
-- | type.
class Alternative m <= Parsing m where
  -- | Take a parser that may consume input, and on failure, go back to
  -- | where we started and fail as if we didn't consume input.
  try :: forall a. m a -> m a

  -- | Give a parser a name
  nameParser :: forall a. m a -> String -> m a

  -- | A version of many that discards its input. Specialized because it
  -- | can often be implemented more cheaply.
  -- |
  -- | The default:
  -- |    skipMany p = unit <$ many p
  skipMany :: forall a. m a -> m Unit

  -- | `skipSome p` applies the parser `p` one or more times, skipping
  -- | its result.
  -- |
  -- | The default:
  -- |    skipSome p = p *> skipMany p
  skipSome :: forall a. m a -> m Unit

  -- | Used to emit an error on an unexpected token
  unexpected :: forall a. String -> m a

  -- | This parser only succeeds at the end of the input. This is not a
  -- | primitive parser but it is defined using 'notFollowedBy'.
  -- |
  -- |    eof  = notFollowedBy anyChar <?> "end of input"
  eof :: m Unit

  -- | `notFollowedBy p` only succeeds when parser `p` fails. This parser
  -- | does not consume any input. This parser can be used to implement the
  -- | *longest match* rule. For example, when recognizing keywords (for
  -- | example `let`), we want to make sure that a keyword is not followed
  -- | by a legal identifier character, in which case the keyword is
  -- | actually an identifier (for example `lets`). We can program this
  -- | behavior as follows:
  -- |
  -- |    keywordLet  = try $ string "let" <* notFollowedBy alphaNum
  notFollowedBy :: forall a. Show a => m a -> m Unit

infixr 0 nameParser as <?>


instance parsingStateT :: (Parsing m, MonadPlus m) => Parsing (StateT s m) where
  try (StateT m) = StateT $ try <<< m

  nameParser (StateT m) l = StateT $ \s -> m s <?> l

  skipMany p = unit <$ A.many p
  skipSome p = p *> skipMany p

  unexpected = lift <<< unexpected

  eof = lift eof

  notFollowedBy (StateT m) = StateT $ \s ->
    notFollowedBy (fst <$> m s) *> pure (Tuple unit s)


instance parsingReaderT :: (Parsing m, MonadPlus m) => Parsing (ReaderT e m) where
  try (ReaderT m) = ReaderT $ try <<< m

  nameParser (ReaderT m) l = ReaderT $ \e -> m e <?> l

  skipMany (ReaderT m) = ReaderT $ skipMany <<< m
  skipSome p = p *> skipMany p

  unexpected = lift <<< unexpected

  eof = lift eof

  notFollowedBy (ReaderT m) = ReaderT $ notFollowedBy <<< m


instance parsingWriterT :: (Parsing m, MonadPlus m, Monoid w) => Parsing (WriterT w m) where
  try (WriterT m) = WriterT $ try m

  nameParser (WriterT m) l = WriterT $ m <?> l

  skipMany (WriterT m) = WriterT $ Tuple <$> skipMany m <*> pure mempty
  skipSome p = p *> skipMany p

  unexpected = lift <<< unexpected

  eof = lift eof

  notFollowedBy (WriterT m) = WriterT $
    notFollowedBy (fst <$> m) >>= \x -> pure $ Tuple x mempty


instance parsingRWST :: (Parsing m, MonadPlus m, Monoid w) => Parsing (RWST r w s m) where
  try (RWST m) = RWST $ \r s -> try (m r s)

  nameParser (RWST m) l = RWST $ \r s -> m r s <?> l

  skipMany p = unit <$ A.many p
  skipSome p = p *> skipMany p

  unexpected = lift <<< unexpected

  eof = lift eof

  notFollowedBy (RWST m) = RWST $ \r s ->
    notFollowedBy ((\(RWSResult _ a _) -> a) <$> m r s) >>= \x -> pure $ RWSResult s x mempty
    -- Haskell RWST is:
    --    newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }
    -- Purescript RWST is:
    --    newtype RWST r w s m a = RWST (r -> s -> m (RWSResult s a w))
    -- N.B. the `s` and `a` switched position!
