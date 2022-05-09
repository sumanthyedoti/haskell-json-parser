module Main where

import Data.Char

data JSON
  = NULL
  | BOOLEAN Bool
  | STRING String
  | ARRAY [JSON]
  | NUMBER Double
  | OK
  deriving (Show)

type Parser = String -> Maybe (JSON, String)

nullParser, boolParser, colonParser, commaParser, numberParser, stringParser ::
     Parser
nullParser input
  | take 4 input == "null" = Just (NULL, drop 4 input)
  | otherwise = Nothing

boolParser input
  | take 4 input == "true" = Just (BOOLEAN True, drop 4 input)
  | take 5 input == "false" = Just (BOOLEAN False, drop 5 input)
  | otherwise = Nothing

removeSpace :: String -> String
removeSpace (x:xs)
  | isSpace x = removeSpace xs
removeSpace input = input

colonParser (x:xs)
  | x == ':' = Just (OK, xs)
colonParser _ = Nothing

commaParser (x:xs)
  | x == ',' = Just (OK, xs)
commaParser _ = Nothing

numberParser input =
  case reads input :: [(Double, String)] of
    [(num, remaing)] -> Just (NUMBER num, remaing)
    _ -> Nothing

validEscapeChars :: [Char]
validEscapeChars = ['\"', '\\', '\b', '\f', '\n', '\r', '\t']

-- ! TODO: handle unicode
stringParser "" = Nothing
stringParser ('"':'"':xs) = Just (STRING "", xs)
stringParser ('"':x:xs) =
  case x of
    '\\'
      | head xs `elem` validEscapeChars ->
        case stringParser ("\"" ++ drop 1 xs) of
          Just (STRING str, remaing) ->
            Just (STRING (x : head xs : str), remaing)
          _ -> Nothing
      | otherwise -> Nothing
    _ ->
      case stringParser ("\"" ++ xs) of
        Just (STRING str, remaing) -> Just (STRING (x : str), remaing)
        _ -> Nothing
stringParser _ = Nothing

parsers :: [Parser]
parsers = [nullParser, boolParser, numberParser, stringParser]

valueParser :: [Parser] -> String -> Maybe (JSON, String)
valueParser [] _ = Nothing
valueParser (p:ps) input =
  let res = p input
   in case res of
        Nothing -> valueParser ps input
        _ -> res

parser :: String -> JSON
parser input =
  case valueParser parsers input of
    Just (val, "") -> val
    _ -> error "could not parse"

main :: IO ()
main = undefined
