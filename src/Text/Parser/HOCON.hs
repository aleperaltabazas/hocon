module Text.Parser.HOCON
  ( parseHOCON
  )
where

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
  values <- sepBy (objectParser <|> openPropParser) (newline <|> char ',')
  return $ HOCONList (map simplify values)
 where
  openPropParser = do
    (label, value) <- parseProps
    return $ HOCONNode [(label, value)]

simplify :: Config -> Config
simplify (HOCONNode nodes) =
  let confs = map (\(k, vs) -> (k, HOCONList $ map (simplify . snd) vs)) . groupBy fst $ nodes in HOCONNode confs
simplify c = c
