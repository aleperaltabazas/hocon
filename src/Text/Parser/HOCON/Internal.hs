{-# LANGUAGE ExistentialQuantification #-}

module Text.Parser.HOCON.Internal where

import Data.HOCON (Config(..))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.ParserCombinators.Parsec
  (Parser, char, alphaNum, digit, letter, noneOf, oneOf, space, string, between, sepBy, (<?>), (<|>), many, skipMany, try)

eol :: Parser String
eol = try (string "\n\r") <|> try (string "\r\n") <|> string "\n" <|> string "\r" <?> "EOL"

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
  str <- many (alphaNum <|> char '.')
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
  digits <- many (digit <|> oneOf ".-")
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
  bool <- string "true" <|> string "false"
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
