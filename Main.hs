module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Convert (convert)
import Html qualified
import Markup qualified

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- No args: stdin -> stdout
    [] -> do
      content <- getContents
      putStrLn $ process "Empty Title" content
    [input, output] -> do
      content <- readFile input
      exists <- doesFileExist output
      let writeResult = writeFile output (process input content)
      if exists
        then whenIO confirm writeResult
        else writeResult
    _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm = do
  putStrLn "Are you sure? (y/n)"
  response <- getLine
  case response of
    "y" -> pure True
    "n" -> pure False
    _ -> putStrLn "Invalid response, use y or n" *> confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action = do
  result <- cond
  if result then action else pure ()
