module Parser
  ( parser
  ) where

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import Data.Monoid
import qualified Numeric

data JSON
  = JSNull
  | JSBoolean Bool
  | JSNumber Double
  | JSString String
  | JSArray [JSON]
  | JSObject (M.Map String JSON)
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

isDecimalChar c = C.generalCategory c == C.DecimalNumber

numberParser ('0':n:rem)
  | n == 'x' || isDecimalChar n = Nothing
numberParser ('-':'0':n:rem)
  | n == 'x' || isDecimalChar n = Nothing
numberParser input =
  case reads input :: [(Double, String)] of
    [(num, remaing)] -> Just (JSNumber num, remaing)
    _ -> Nothing

findChar :: Char -> Maybe Char
findChar c
  | null charFound = Nothing
  | otherwise = Just (snd $ head charFound)
  where
    validEscapeChars :: [(Char, Char)]
    validEscapeChars =
      [ ('\"', '\"')
      , ('\\', '\\')
      , ('b', '\b')
      , ('f', '\f')
      , ('n', '\n')
      , ('r', '\r')
      , ('t', '\t')
      ]
    charFound :: [(Char, Char)]
    charFound = filter (\x -> fst x == c) validEscapeChars

invaildCharLiterals = ['\t', '\n']

convertUTF8 :: String -> String
convertUTF8 xs =
  case Numeric.readHex xs of
    [(hex, rem)] -> C.chr hex : ""
    _ -> ""

stringParser "" = Nothing
stringParser ('"':'"':xs) = Just (JSString "", xs)
stringParser ('"':x:xs)
  | x == '\\' =
    case head xs of
      'u' ->
        stringParser ("\"" ++ drop 5 xs) >>= \(JSString str, rem) ->
          Just (JSString (convertUTF8 . take 4 . drop 1 $ xs ++ str), rem)
      h
        | Maybe.isJust (findChar h) ->
          stringParser ("\"" ++ drop 1 xs) >>= \(JSString str, rem) ->
            Just (JSString (Maybe.fromJust (findChar h) : str), rem)
      _ -> Nothing
  | x `notElem` invaildCharLiterals =
    stringParser ("\"" ++ xs) >>= \(JSString str, remaing) ->
      Just (JSString (x : str), remaing)
  | otherwise = Nothing
stringParser _ = Nothing

arrayParser "" = Nothing
arrayParser ('[':']':xs) = Just (JSArray [], xs)
arrayParser ('[':xs) =
  valueParser parsers (removeSpace xs) >>= \(val, afterVal) ->
    let (x:afterVal') = removeSpace afterVal
     in case x of
          ']' -> Just (JSArray [val], afterVal')
          ',' ->
            case removeSpace afterVal' of
              (']':xs) -> Nothing
              _ ->
                arrayParser ('[' : removeSpace afterVal') >>=
                (\(JSArray remainingVals, afterVals) ->
                   Just (JSArray (val : remainingVals), afterVals))
          _ -> Nothing
arrayParser _ = Nothing

objectParser "" = Nothing
objectParser ('{':'}':xs) = Just (JSObject M.empty, xs)
objectParser ('{':xs) =
  case stringParser (removeSpace xs) of
    Just (JSString key, afterKey) ->
      case removeSpace afterKey of
        ':':afterKey' ->
          valueParser parsers (removeSpace afterKey') >>= \(val, afterVal) ->
            let (x:afterVal') = removeSpace afterVal
             in case x of
                  '}' -> Just (JSObject (M.fromList [(key, val)]), afterVal')
                  ',' ->
                    case removeSpace afterVal' of
                      ('}':xs) -> Nothing
                      _ ->
                        objectParser ('{' : removeSpace afterVal') >>=
                        (\(JSObject remainingKVs, afterKV) ->
                           Just
                             (JSObject (M.insert key val remainingKVs), afterKV))
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
valueParser parsers input =
  getFirst . foldMap (First . (\f -> f input)) $ parsers

type Error = String

parser :: String -> Either Error JSON
parser input =
  case input' of
    ('[':_) -> parse
    ('{':_) -> parse
    _ -> Left "FAILED: not a valid JSON"
  where
    input' = removeSpace input
    parse =
      case valueParser parsers input' of
        Just (val, rem)
          | null $ removeSpace rem -> Right val
        _ -> Left "FAILED: not a valid JSON"
