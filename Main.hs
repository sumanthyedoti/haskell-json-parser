module Main where

import qualified Data.Char as C
import qualified Data.Map as M

data JSON
  = JSNull
  | JSBoolean Bool
  | JSNumber Double
  | JSString String
  | JSArray [JSON]
  | JSObject (M.Map String JSON)
  | Ok
  deriving (Show)

type Parser = String -> Maybe (JSON, String)

nullParser, boolParser, numberParser, stringParser, arrayParser, objectParser ::
     Parser
nullParser input
  | take 4 input == "null" = Just (JSNull, drop 4 input)
  | otherwise = Nothing

boolParser input
  | take 4 input == "true" = Just (JSBoolean True, drop 4 input)
  | take 5 input == "false" = Just (JSBoolean False, drop 5 input)
  | otherwise = Nothing

removeSpace :: String -> String
removeSpace (x:xs)
  | C.isSpace x = removeSpace xs
removeSpace input = input

numberParser input =
  case reads input :: [(Double, String)] of
    [(num, remaing)] -> Just (JSNumber num, remaing)
    _ -> Nothing

validEscapeChars :: [Char]
validEscapeChars = ['\"', '\\', '\b', '\f', '\n', '\r', '\t']

-- ! TODO: handle unicode
stringParser "" = Nothing
stringParser ('"':'"':xs) = Just (JSString "", xs)
stringParser ('"':x:xs) =
  case x of
    '\\'
      | head xs `elem` validEscapeChars ->
        case stringParser ("\"" ++ drop 1 xs) of
          Just (JSString str, remaing) ->
            Just (JSString (x : head xs : str), remaing)
          _ -> Nothing
      | otherwise -> Nothing
    _ ->
      case stringParser ("\"" ++ xs) of
        Just (JSString str, remaing) -> Just (JSString (x : str), remaing)
        _ -> Nothing
stringParser _ = Nothing

arrayParser "" = Nothing
arrayParser ('[':']':xs) = Just (JSArray [], xs)
arrayParser ('[':xs) =
  case valueParser parsers (removeSpace xs) of
    Just (val, afterVal) ->
      let (x:afterVal') = removeSpace afterVal
       in case x of
            ']' -> Just (JSArray [val], afterVal')
            ',' ->
              case arrayParser ('[' : removeSpace afterVal') of
                Just (JSArray remainingVals, afterVals) ->
                  Just (JSArray (val : remainingVals), afterVals)
                _ -> Nothing
            _ -> Nothing
    _ -> Nothing
arrayParser _ = Nothing

objectParser "" = Nothing
objectParser ('{':'}':xs) = Just (JSObject (M.fromList []), xs)
objectParser ('{':xs) =
  case stringParser (removeSpace xs) of
    Just (JSString key, ':':afterKey) ->
      case valueParser parsers (removeSpace afterKey) of
        Just (val, afterVal) ->
          let (x:afterVal') = removeSpace afterVal
           in case x of
                '}' -> Just (JSObject (M.fromList [(key, val)]), afterVal')
                ',' ->
                  case objectParser ('{' : removeSpace afterVal') of
                    Just (JSObject remainingKVs, afterKV) ->
                      Just (JSObject (M.insert key val remainingKVs), afterKV)
                    _ -> Nothing
                _ -> Nothing
        _ -> Nothing
    _ -> Nothing
objectParser _ = Nothing

parsers :: [Parser]
parsers =
  [ nullParser
  , boolParser
  , numberParser
  , stringParser
  , arrayParser
  , objectParser
  ]

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
