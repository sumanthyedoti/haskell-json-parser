module Main where

import qualified System.Environment as Env

import Parser (parser)

main :: IO ()
main = do
  file <- Env.getArgs
  content <- readFile $ head file
  print $ content ++ "\n"
  putStrLn ""
  print $ parser content
