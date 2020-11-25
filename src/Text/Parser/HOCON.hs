module Text.Parser.HOCON
  ( parseHOCON
  , ParseError(..)
  )
where

import Data.HOCON (Config(..))
import Text.Parser.HOCON.Internal
import Text.ParserCombinators.Parsec (ParseError, parse)

parseHOCON :: String -> Either ParseError Config
parseHOCON = parse hoconParser "hocon" . preProcessing
