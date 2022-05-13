module Main where

import qualified System.Environment as Env

import Parser (parser)

main :: IO ()
main = do
  file <- Env.getArgs
  content <- readFile $ head file
  putStrLn $ content ++ "\n"
  putStrLn . show $ parser content
