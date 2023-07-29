module MarkupParsingSpec where

import HsBlog.Markup
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    simple
    multiline

simple :: Spec
simple = do
  describe "Markup parsing tests" $ do
    it "empty" $
      shouldBe (parse "") []

    it "paragraph" $
      shouldBe (parse "hello world") [Paragraph "hello world"]

    it "heading 1" $
      shouldBe
        (parse "* Heading 1")
        [Heading 1 "Heading 1"]

    it "code" $
      shouldBe
        (parse "> main = putStrLn \"hello world!\"")
        [CodeBlock ["main = putStrLn \"hello world!\""]]

multiline :: Spec
multiline = do
  describe "Multi-line tests" $ do
    it "example3" $
      shouldBe (parse example3) example3Result

example3 :: String
example3 =
  [r|
Remember that multiple lines with no separation
are grouped together into a single paragraph
but list items remain separate.

# Item 1 of a list
# Item 2 of the same list
|]

example3Result :: Document
example3Result =
  [ Paragraph "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate."
  , OrderedList
      [ "Item 1 of a list"
      , "Item 2 of the same list"
      ]
  ]
