import Data.Char

data JsonValue
  = JsonNull
  | JsonBool Bool
  | String
  | Integer
  | Ok
  deriving (Show)

type Parser = String -> Maybe (JsonValue, String)

nullParser, boolParser, spaceParser, colonParser, commaParser :: Parser
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

parsers :: [Parser]
parsers = [nullParser, boolParser]

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