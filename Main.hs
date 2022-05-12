module Main where

import qualified System.Environment as Env

import Parser (parser)

main :: IO ()
main = do
  file <- Env.getArgs
  print file
  content <- readFile $ head file
  print content
  print $ parser content
