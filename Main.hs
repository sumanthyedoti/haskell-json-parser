module Main where

import qualified System.Environment as Env

x = Just 10

import Parser (parser)

main :: IO ()
main = do
  file <- Env.getArgs
  content <- readFile $ head file
  print $ content ++ "\n"
  putStrLn ""
  print $ parser content
