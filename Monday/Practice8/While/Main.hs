module Main where

import System.Environment

import ParserBase
import ParserWhile

main :: IO ()
main = putStrLn "Hello World"

test :: String -> IO ()
test file = do
  s <- readFile file
  let parsedProg = evalParser statement s
  print parsedProg