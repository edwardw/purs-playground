module Test.InternalDoctest (testInternalDoctest) where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Text.Prettyprint.Doc.Internal (align, concatWith, emptyDoc, enclose, encloseSep, flatAlt, group, hsep, line, line', nest, plural, pretty, punctuate, surround, vsep, (<+>))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)



-- | The basic doctest extracted from the original implementation.
testInternalDoctest :: Effect Unit
testInternalDoctest = runTest do
  suite "basic doctest" do
    test "basic doctest" do
      equal "hello\nworld"
            (show $ vsep [pretty "hello", pretty "world"])
      equal "helloworld"
            (show $ pretty "hello" <> pretty "world")
      equal "1 hello 3.14159"
            (show $ pretty 1 <+> pretty "hello" <+> pretty 3.14159)
      equal "[[(1, [2, 2]), (1, [2, 2])], [(1, [2, 2]), (1, [2, 2])]]"
            (show $ pretty (A.replicate 2 (A.replicate 2 (Tuple 1 (A.replicate 2 2)))))
      equal "[1, 2, 3]"
            (show $ pretty [1,2,3])
      equal "true"
            (show $ pretty true)
      equal "foo"
            (show $ pretty 'f' <> pretty 'o' <> pretty 'o')
      equal "(123, hello)"
            (show $ pretty (Tuple 123 "hello"))
      equal "true"
            (show $ pretty (Just true))
      equal "[1, 3]"
            (show $ pretty [Just 1, Nothing, Just 3, Nothing])
      equal "hello world"
            (show $ group (pretty "hello\nworld"))
      equal "[]"
            (show $ pretty ([] :: Array Unit))
      equal "hello\n\nworld"
            (show $ vsep [pretty "hello", emptyDoc, pretty "world"])
      equal "lorem\n    ipsum\n    dolor\nsit\namet"
            (show $ vsep [nest 4 (vsep [pretty "lorem", pretty "ipsum", pretty "dolor"]), pretty "sit", pretty "amet"])

      let doc = pretty "lorem ipsum" <> line <> pretty "dolor sit amet"
      equal "lorem ipsum\ndolor sit amet" (show doc)
      equal "lorem ipsum dolor sit amet" (show $ group doc)

      let doc' = pretty "lorem ipsum" <> line' <> pretty "dolor sit amet"
      equal "lorem ipsum\ndolor sit amet" (show doc')
      equal "lorem ipsumdolor sit amet" (show $ group doc')

      let open = flatAlt emptyDoc (pretty "{ ")
          close = flatAlt emptyDoc (pretty " }")
          sep = flatAlt emptyDoc (pretty "; ")
          prettyDo xs = group (pretty "do" <+> align (encloseSep open close sep xs))
          statements  = [pretty "name:_ <- getArgs", pretty "let greet = \"Hello, \" <> name", pretty "putStrLn greet"]
      equal """do { name:_ <- getArgs; let greet = "Hello, " <> name; putStrLn greet }"""
            (show $ prettyDo statements)

      equal "lorem ipsum\n      dolor"
            (show $ pretty "lorem" <+> align (vsep [pretty "ipsum", pretty "dolor"]))
      equal "Data.Text.Prettyprint.Doc"
            (show $ concatWith (surround (pretty '.')) [pretty "Data", pretty "Text", pretty "Prettyprint", pretty "Doc"])
      equal "lorem, lorem, dolor, sit, amet"
            (show $ hsep (punctuate (pretty ',') (map pretty ["lorem", "lorem", "dolor", "sit", "amet"])))

      let things = [true]
          amount = A.length things
      equal "[true] has 1 entry"
            (show $ pretty things <+> pretty "has" <+> pretty amount <+> plural (pretty "entry") (pretty "entries") amount)

      equal "A·Z"
            (show $ enclose (pretty "A") (pretty "Z") (pretty "·"))
      equal "A·Z"
            (show $ surround (pretty "·") (pretty "A") (pretty "Z"))
