module Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

instance Semigroup Structure where
  (<>) a b = Structure (getStructureString a <> getStructureString b)

instance Monoid Structure where
  mempty = empty_

type Title = String

getStructureString :: Structure -> String
getStructureString (Structure str) = str

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ level = Structure . el ("h" <> show level) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

empty_ :: Structure
empty_ = Structure ""

render :: Html -> String
render (Html document) = document

escape :: String -> String
escape = 
  let 
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concatMap escapeChar

-- No longer needed, can just use mconcat/fold now that Structure has a monoid instance
-- concatStructure :: [Structure] -> Structure
-- concatStructure list =
--   case list of
--     [] -> empty_
--     x : xs -> x <> concatStructure xs
