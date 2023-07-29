module HsBlog.Convert (convert, convertStructure) where

import HsBlog.Env (Env (..))
import HsBlog.Html qualified as Html
import HsBlog.Markup qualified as Markup
import Prelude hiding (head)

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading level txt ->
      Html.h_ level $ Html.txt_ txt
    Markup.Paragraph p ->
      Html.p_ $ Html.txt_ p
    Markup.UnorderedList list ->
      Html.ul_ $ map (Html.p_ . Html.txt_) list
    Markup.OrderedList list ->
      Html.ol_ $ map (Html.p_ . Html.txt_) list
    Markup.CodeBlock list ->
      Html.code_ $ unlines list

convert :: Env -> String -> Markup.Document -> Html.Html
convert env title doc =
  let
    head =
      Html.title_ (eBlogName env <> " - " <> title)
        <> Html.stylesheet_ (eStylesheetPath env)
    article =
      foldMap convertStructure doc
    websiteTitle =
      Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
    body =
      websiteTitle <> article
   in
    Html.html_ head body
