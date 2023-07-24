module HsBlog
  ( convertSingle,
    convertDirectory,
    process,
  )
where

import HsBlog.Convert (convert)
import HsBlog.Html qualified as Html
import HsBlog.Markup qualified as Markup
import System.IO

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

convertDirectory :: FilePath -> FilePath -> IO ()
convertDirectory = error "Not implemented"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse
