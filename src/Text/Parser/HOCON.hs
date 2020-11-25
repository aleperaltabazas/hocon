module Text.Parser.HOCON where

import Data.HOCON
import Data.Map
import Text.Parser.HOCON.Internal
import Text.ParserCombinators.Parsec

hoconParser :: Parser Config
hoconParser = do
  values <- sepBy (objectParser <|> openPropParser) (char ',' <|> char '\n')
  return $ HOCONList (simplifyAndMerge values)
 where
  openPropParser = do
    (label, value) <- parseProps
    return $ HOCONNode [(label, value)]

simplifyAndMerge :: [Config] -> [Config]
simplifyAndMerge []       = []
simplifyAndMerge (c : cs) = case c of
  HOCONNode nodes -> let confs = groupBy fst nodes in undefined
  _               -> c : simplifyAndMerge cs
  where matchesWith label (label', _) = label == label'
