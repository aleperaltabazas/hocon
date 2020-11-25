module Text.Parser.HOCON
  ( parseHOCON
  )
where

import Data.Bifunctor
import Data.HOCON (Config(..))
import Data.Map (groupBy)
import Data.String.Utils (replace)
import Text.Parser.HOCON.Internal (objectParser, parseProps)
import Text.ParserCombinators.Parsec (ParseError, char, Parser, newline, sepBy, (<|>), parse)

parseHOCON :: String -> Either ParseError Config
parseHOCON = parse hoconParser "hocon" . preProcessing

preProcessing :: String -> String
preProcessing = replace "\n" "," . replace ",\n" ","

hoconParser :: Parser Config
hoconParser = do
  values <- sepBy (openObjectParser <|> parseProps) (char ',')
  let grouped = groupAndMerge values
  return $ HOCONNode grouped
 where
  openObjectParser = do
    object <- objectParser
    return ("root", object)
  groupAndMerge values = do
    (key, mixedConfigsAndKeys) <- groupBy fst values
    let configs = map snd mixedConfigsAndKeys
    return (key, mergeAll configs)

mergeAll :: [Config] -> Config
mergeAll = foldl1 merge

merge :: Config -> Config -> Config
merge (HOCONNode a) (HOCONNode b) = HOCONNode (a ++ b)
