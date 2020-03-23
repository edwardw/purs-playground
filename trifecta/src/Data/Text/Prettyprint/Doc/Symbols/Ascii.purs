module Data.Text.Prettyprint.Doc.Symbols.Ascii where

import Prelude
import Data.String (codePointFromChar)
import Data.Text.Prettyprint.Doc.Internal (Doc(..), enclose)



--------------------------------------------------------------------------------
-- Common symbols composed out of the ASCII subset of Unicode ------------------
--------------------------------------------------------------------------------


-- | '·'
squotes :: forall ann. Doc ann -> Doc ann
squotes = enclose squote squote

-- | "·"
dquotes :: forall ann. Doc ann -> Doc ann
dquotes = enclose dquote dquote

-- | (·)
parens :: forall ann. Doc ann -> Doc ann
parens = enclose lparen rparen

-- | <·>
angles :: forall ann. Doc ann -> Doc ann
angles = enclose langle rangle

-- | [·]
brackets :: forall ann. Doc ann -> Doc ann
brackets = enclose lbracket rbracket

-- | {·}
braces :: forall ann. Doc ann -> Doc ann
braces = enclose lbrace rbrace

-- | '
squote :: forall ann. Doc ann
squote = Char $ codePointFromChar  '\''

-- | "
dquote :: forall ann. Doc ann
dquote = Char $ codePointFromChar '"'

-- | (
lparen :: forall ann. Doc ann
lparen = Char $ codePointFromChar  '('

-- | )
rparen :: forall ann. Doc ann
rparen = Char $ codePointFromChar ')'

-- | <
langle :: forall ann. Doc ann
langle = Char $ codePointFromChar '<'

-- | >
rangle :: forall ann. Doc ann
rangle = Char $ codePointFromChar '>'

-- | [
lbracket :: forall ann. Doc ann
lbracket = Char $ codePointFromChar '['

-- | ]
rbracket :: forall ann. Doc ann
rbracket = Char $ codePointFromChar ']'

-- | {
lbrace :: forall ann. Doc ann
lbrace = Char $ codePointFromChar '{'

-- | }
rbrace :: forall ann. Doc ann
rbrace = Char $ codePointFromChar '}'

-- | ;
semi :: forall ann. Doc ann
semi = Char $ codePointFromChar ';'

-- | :
colon :: forall ann. Doc ann
colon = Char $ codePointFromChar ':'

-- | ,
comma :: forall ann. Doc ann
comma = Char $ codePointFromChar ','

-- | This is mostly used via @'<+>'@,
space :: forall ann. Doc ann
space = Char $ codePointFromChar ' '

-- | .
dot :: forall ann. Doc ann
dot = Char $ codePointFromChar '.'

-- | /
slash :: forall ann. Doc ann
slash = Char $ codePointFromChar '/'

-- | \\
backslash :: forall ann. Doc ann
backslash = Text 1 "\\"

-- | =
equals :: forall ann. Doc ann
equals = Char $ codePointFromChar '='

-- | |
pipe :: forall ann. Doc ann
pipe = Char $ codePointFromChar '|'
