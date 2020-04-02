module Text.Parser.Char
  ( oneOf
  , noneOf
  , oneOfSet
  , noneOfSet
  , spaces
  , newline
  , tab
  , upper
  , lower
  , alphaNum
  , letter
  , digit
  , hexDigit
  , octDigit
  , satisfyRange
  , class CharParsing, satisfy, char, notChar, anyChar, string
  ) where

import Prelude
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Control.MonadPlus (class MonadPlus)
import Data.Char.Unicode.Internal (uIswalnum, uIswalpha, uIswlower, uIswspace, uIswupper)
import Data.CharSet (CharSet)
import Data.CharSet as CharSet
import Data.Enum (fromEnum)
import Data.String (CodePoint, codePointFromChar)
import Text.Parser.Combinators (class Parsing, skipMany, (<?>))



-- | `oneOf cs` succeeds if the current character is in the supplied array of
-- | characters `cs`. Returns the parsed character. See also 'satisfy'.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    vowel  = oneOf "aeiou"
-- |    ```
oneOf :: forall m. CharParsing m => Array CodePoint -> m CodePoint
oneOf xs = oneOfSet (CharSet.fromFoldable xs)


-- | As the dual of 'oneOf', `noneOf cs` succeeds if the current character is
-- | not in the supplied list of characters `cs`. Returns the parsed character.
-- |
-- | E.g.,
-- |
-- |    ```
-- |    consonant = noneOf "aeiou"
-- |    ```
noneOf :: forall m. CharParsing m => Array CodePoint -> m CodePoint
noneOf xs = noneOfSet (CharSet.fromFoldable xs)


-- | `oneOfSet cs` succeeds if the current character is in the supplied set of
-- | characters `cs`. Returns the parsed character. See also 'satisfy'.
oneOfSet :: forall m. CharParsing m => CharSet -> m CodePoint
oneOfSet cs  = satisfy (\c -> CharSet.member c cs)


-- | As the dual of 'oneOf', `noneOf cs` succeeds if the current character is
-- | not in the supplied list of characters `cs`. Returns the parsed character.
noneOfSet :: forall m. CharParsing m => CharSet -> m CodePoint
noneOfSet cs = oneOfSet (CharSet.complement cs)


-- | Skips zero or more white space characters. See also 'skipMany'.
spaces :: forall m. CharParsing m => m Unit
spaces = skipMany space <?> "white space"


-- | Parses a white space character (any character which satisfies 'isSpace')
-- | Returns the parsed character.
space :: forall m. CharParsing m => m CodePoint
space = satisfy isSpace <?> "space"


-- | Parses a newline character (`\n`). Returns a newline character.
newline :: forall m. CharParsing m => m CodePoint
newline = char (codePointFromChar '\n') <?> "new-line"


-- | Parses a tab character (`\t`). Returns a tab character.
tab :: forall m. CharParsing m => m CodePoint
tab = char (codePointFromChar '\t') <?> "tab"


-- | Parses an upper case letter. Returns the parsed character.
upper :: forall m. CharParsing m => m CodePoint
upper = satisfy isUpper <?> "uppercase letter"


-- | Parses a lower case character. Returns the parsed character.
lower :: forall m. CharParsing m => m CodePoint
lower = satisfy isLower <?> "lowercase letter"


-- | Parses a letter or digit. Returns the parsed character.
alphaNum :: forall m. CharParsing m => m CodePoint
alphaNum = satisfy isAlphaNum <?> "letter or digit"


-- | Parses a letter (an upper case or lower case character). Returns the
-- | parsed character.
letter :: forall m. CharParsing m => m CodePoint
letter = satisfy isAlpha <?> "letter"


-- | Parses a digit. Returns the parsed character.
digit :: forall m. CharParsing m => m CodePoint
digit = satisfy isDigit <?> "digit"


-- | Parses a hexadecimal digit (a digit or a letter between `a` and `f` or
-- | `A` and `F`). Returns the parsed character.
hexDigit :: forall m. CharParsing m => m CodePoint
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"


-- | Parses an octal digit (a character between `0` and `7`). Returns the
-- | parsed character.
octDigit :: forall m. CharParsing m => m CodePoint
octDigit = satisfy isOctDigit <?> "octal digit"


satisfyRange :: forall m. CharParsing m => CodePoint -> CodePoint -> m CodePoint
satisfyRange a z = satisfy (\c -> c >= a && c <= z)


-- | Additional functionality needed to parse character streams.
class Parsing m <= CharParsing m where
  -- | Parse a single character of the input, with UTF-8 decoding
  satisfy :: (CodePoint -> Boolean) -> m CodePoint

  -- | `char c` parses a single character `c`. Returns the parsed
  -- | character (i.e. `c`).
  -- |
  -- | E.g.,
  -- |
  -- |    ```
  -- |    semiColon = char ';'
  -- |    ```
  char :: CodePoint -> m CodePoint

  -- | `notChar c` parses any single character other than `c`. Returns the
  -- | parsed character.
  -- |
  -- | The default:
  -- |    notChar c = satisfy (c /= _)
  notChar :: CodePoint -> m CodePoint

  -- | This parser succeeds for any character. Returns the parsed character.
  -- |
  -- | The default:
  -- |    anyChar = satisfy (const true)
  anyChar :: m CodePoint

  -- | `string s` parses a sequence of characters given by `s`. Returns
  -- | the parsed string (i.e. `s`).
  -- |
  -- | E.g.,
  -- |
  -- |    ```
  -- |    divOrMod = string "div"
  -- |           <|> string "mod"
  -- |    ```
  string :: String -> m String


instance charParsingStateT :: (CharParsing m, MonadPlus m) => CharParsing (StateT s m) where
  satisfy = lift <<< satisfy
  char    = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar
  string  = lift <<< string


instance charParsingReaderT :: (CharParsing m, MonadPlus m) => CharParsing (ReaderT e m) where
  satisfy = lift <<< satisfy
  char    = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar
  string  = lift <<< string


instance charParsingWriterT :: (CharParsing m, MonadPlus m, Monoid w) => CharParsing (WriterT w m) where
  satisfy = lift <<< satisfy
  char    = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar
  string  = lift <<< string


instance charParsingRWST :: (CharParsing m, MonadPlus m, Monoid w) => CharParsing (RWST r w s m) where
  satisfy = lift <<< satisfy
  char    = lift <<< char
  notChar = lift <<< notChar
  anyChar = lift anyChar
  string  = lift <<< string


-------------------------------------------------------------------------------
-- Auxiliary functions (probably deserve to be in their own module) -----------
-------------------------------------------------------------------------------


isSpace :: CodePoint -> Boolean
isSpace = uIswspace <<< fromEnum


isUpper :: CodePoint -> Boolean
isUpper = uIswupper <<< fromEnum


isLower :: CodePoint -> Boolean
isLower = uIswlower <<< fromEnum


isAlphaNum :: CodePoint -> Boolean
isAlphaNum = uIswalnum <<< fromEnum


isAlpha :: CodePoint -> Boolean
isAlpha = uIswalpha <<< fromEnum


isDigit :: CodePoint -> Boolean
isDigit c =
  let diff = fromEnum c - fromEnum '0'
  in 0 <= diff && diff <= 9


isHexDigit :: CodePoint -> Boolean
isHexDigit c =
  isDigit c
  || (let diff = (fromEnum c - fromEnum 'A') in 0 <= diff && diff <= 5)
  || (let diff = (fromEnum c - fromEnum 'a') in 0 <= diff && diff <= 5)


isOctDigit :: CodePoint -> Boolean
isOctDigit c =
  let diff = fromEnum c - fromEnum '0'
  in 0 <= diff && diff <= 7
