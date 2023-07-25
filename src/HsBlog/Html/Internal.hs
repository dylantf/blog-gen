{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concatMap" #-}

module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

newtype Content = Content String

type Title = String

-- * EDSL

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

-- * Structure

p_ :: Content -> Structure
p_ = Structure . el "p" . escape . getContentString

h_ :: Natural -> Content -> Structure
h_ level = Structure . el ("h" <> show level) . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
  (<>) a b = Structure (getStructureString a <> getStructureString b)

instance Monoid Structure where
  mempty = Structure ""

-- * Content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content =
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

-- Since this is a one-off tag we can just hardcode it
img_ :: FilePath -> Content
img_ path = Content $ "<img src=\"" <> escape path <> "\" />"

b_ :: Content -> Content
b_ content = Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content = Content $ el "i" (getContentString content)

instance Semigroup Content where
  (<>) a b = Content (getContentString a <> getContentString b)

instance Monoid Content where
  mempty = Content ""

-- * Rendering

render :: Html -> String
render (Html document) = document

-- * Utils

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

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- Same as el but supports tag attributes
elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure str) = str

getContentString :: Content -> String
getContentString (Content c) = c

-- No longer needed, can just use mconcat/fold now that Structure has a monoid instance
-- concatStructure :: [Structure] -> Structure
-- concatStructure list =
--   case list of
--     [] -> empty_
--     x : xs -> x <> concatStructure xs
