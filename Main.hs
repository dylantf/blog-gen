module Main (main) where

import Html

main :: IO ()
main = putStrLn (render myHtml)

myHtml :: Html
myHtml =
  html_
    "Hello title"
    (append_ (h1_ "Dylan's Blog") (p_ "Welcome to my \"blog\""))
