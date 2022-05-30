module Main where

import Parser (parser)
import qualified System.Environment as Env

main :: IO ()
main = do
  file <- Env.getArgs
  content <- readFile $ head file
  -- print $ content ++ "\n"
  putStrLn ""
  print $ parser content
