module Main (main) where

import Html

main :: IO ()
main = putStrLn (render myHtml)

myHtml :: Html
myHtml =
  html_
    "Hello title"
    ( h_ 1 "Dylan's Blog"
        <> (p_ "Welcome to..." <> p_ "my blog!")
    )
