-- Based off of [this gist for a JSON parser using Parsec](https://gist.github.com/fero23/51f63a33d733055d53b4)

{-# LANGUAGE ExistentialQuantification #-}

module Text.Parser.HOCON.Internal
  ( objectParser
  , stringParser
  , parseProps
  , parseLabel
  , arrayParser
  , numberParser
  , booleanParser
  , nullParser
  , preProcessing
  , hoconParser
  , postProcessing
  )
where

import Data.Bifunctor.Extra (mapValues)
import Data.HOCON (Config(..))
import Data.List.Split (splitOn)
import Data.Map (groupBy, sortByKey, Map)
import Data.String.Utils (replace, join, strip)
import Text.ParserCombinators.Parsec
  (char, Parser, alphaNum, digit, letter, noneOf, oneOf, space, string, between, sepBy, (<|>), many, many1, skipMany, try)

whitespace :: Parser ()
whitespace = skipMany space

objectParser :: Parser Config
objectParser = do
  whitespace >> char '{' >> whitespace
  props <- sepBy parseProps (whitespace >> (char ',' <|> char '\n') >> whitespace)
  whitespace >> char '}' >> whitespace
  return $ HOCONNode props

parseString :: Parser String
parseString = parseStr <|> parseOpenStr

parseStr :: Parser String
parseStr = between (char '\"') (char '\"') (many $ noneOf "\"" <|> try (string "\"\"" >> return '"'))

parseOpenStr :: Parser String
parseOpenStr = do
  l   <- letter
  str <- many (alphaNum <|> oneOf "._-")
  return $ l : str

parseProps :: Parser (String, Config)
parseProps = do
  label <- parseString
  value <- whitespace >> (parseJsonLikeValue <|> parseObjectValue)
  return $ splitProp label value
 where
  parseJsonLikeValue = do
    (char ':' <|> char '=') >> whitespace
    objectParser <|> arrayParser <|> booleanParser <|> nullParser <|> stringParser <|> numberParser
  parseObjectValue = objectParser

parseLabel :: Parser String
parseLabel = do
  whitespace
  label <- parseString
  whitespace >> (char ':' <|> char '=') >> whitespace
  return label

numberParser :: Parser Config
numberParser = do
  whitespace
  digits <- many1 (digit <|> oneOf ".-")
  whitespace
  return . HOCONNumber $ read digits

stringParser :: Parser Config
stringParser = do
  whitespace
  str <- parseStr <|> parseOpenStr
  whitespace
  return $ HOCONString str

booleanParser :: Parser Config
booleanParser = do
  whitespace
  bool <- try (string "true" <|> string "false")
  whitespace
  return $ if bool == "true" then HOCONBool True else HOCONBool False

nullParser :: Parser Config
nullParser = do
  whitespace >> string "null" >> whitespace
  return HOCONNull

arrayParser :: Parser Config
arrayParser = do
  whitespace >> char '[' >> whitespace
  array <- sepBy
    (objectParser <|> arrayParser <|> booleanParser <|> nullParser <|> stringParser <|> numberParser)
    (whitespace >> char ',' >> whitespace)
  whitespace >> char ']' >> whitespace
  return $ HOCONList array

splitProp :: String -> Config -> (String, Config)
splitProp label value = case splitOn "." label of
  [l         ] -> (l, value)
  (l : nested) -> (l, join nested value)
 where
  join [l     ] value = HOCONNode [(l, value)]
  join (l : ls) value = HOCONNode [(l, join ls value)]


preProcessing :: String -> String
preProcessing =
  replace "\n" ","
    . replace "[\n" "["
    . replace "\n]" "]"
    . replace "{\n" "{"
    . replace "\n}" "}"
    . replace ",\n" ","
    . join "\n"
    . filter (not . null)
    . map strip
    . splitOn "\n"

postProcessing :: Config -> Config
postProcessing (HOCONNode nodes) = HOCONNode . sort . removeDuplicates . group $ nodes

group :: Map String Config -> Map String Config
group nodes = do
  let grouped = groupBy fst nodes
  (key, configs) <- mapValues (map snd) grouped
  return (key, if all isNode configs then mergeAll configs else head configs)
 where
  isNode n = case n of
    HOCONNode _ -> True
    _           -> False

sort :: Map String Config -> Map String Config
sort = sortByKey

hoconParser :: Parser Config
hoconParser = postProcessing <$> parser
 where
  parser = objectParser <|> do
    values <- sepBy parseProps (char ',')
    return $ HOCONNode values

removeDuplicates :: Map String Config -> Map String Config
removeDuplicates = foldl (\acc e -> if any ((== fst e) . fst) acc then acc else e : acc) []

mergeAll :: [Config] -> Config
mergeAll = foldl1 merge

merge :: Config -> Config -> Config
merge (HOCONNode a) (HOCONNode b) = HOCONNode (a ++ b)
