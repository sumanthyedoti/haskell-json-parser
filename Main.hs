import Data.Char
import Data.List

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonNumber Double
  | Ok
  deriving (Show)

type Parser = String -> Maybe (JsonValue, String)

nullParser, boolParser, spaceParser, colonParser, commaParser, numberParser ::
     Parser
nullParser input
  | take 4 input == "null" = Just ((JsonNull, drop 4 input))
  | otherwise = Nothing

boolParser input
  | take 4 input == "true" = Just (JsonBool True, drop 4 input)
  | take 5 input == "false" = Just (JsonBool False, drop 5 input)
  | otherwise = Nothing

spaceParser (x:rem)
  | isSpace x = spaceParser rem
spaceParser input = Just (Ok, input)

colonParser (x:rem)
  | x == ':' = Just (Ok, rem)
colonParser _ = Nothing

commaParser (x:rem)
  | x == ',' = Just (Ok, rem)
commaParser _ = Nothing

numberParser input =
  case (reads input) :: [(Double, String)] of
    [(num, rem)] -> Just (JsonNumber num, rem)
    _ -> Nothing

validEscapeChars = ['"', '\\', 'b', 'f', 'n', 'r', 't', 'u']

stringParser [] = Nothing
stringParser input
  | input !! 0 /= '"' = Nothing
  | otherwise =
    let str = string (drop 1 input) 0
     in case str of
          Just (str) -> Just (JsonString str, drop (length str) input)
          _ -> Nothing
  where
    string [] _ = Nothing
    string (x:xs) i =
      case x of
        '\\'
          | (xs !! 0) `elem` validEscapeChars -> string (drop 1 xs) (i + 2)
          | otherwise -> Nothing
        '"' -> Just (take (i + 2) input)
        _ -> string xs (i + 1)

parsers :: [Parser]
parsers = [nullParser, boolParser, numberParser, stringParser]

valueParser :: [Parser] -> String -> Maybe (JsonValue, String)
valueParser [] input = Nothing
valueParser (p:parsers) input =
  let res = (p input)
   in case res of
        Nothing -> (valueParser parsers input)
        _ -> res

parser :: String -> [JsonValue]
parser input =
  case (valueParser parsers input) of
    Just (val, rem) -> val : parser rem
    Nothing -> []

main :: IO ()
main = undefined
