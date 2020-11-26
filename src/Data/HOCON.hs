module Data.HOCON
  ( Config(..)
  , getNode
  , hasPath
  , getString
  , getNumber
  , getList
  , getBool
  , isNull
  , mapNode
  )
where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Maybe (isJust)

data Config =
  HOCONNode (Map String Config) |
  HOCONString String |
  HOCONNumber Double |
  HOCONList [Config] |
  HOCONBool Bool |
  HOCONNull
  deriving (Show, Eq)

isNull :: Config -> Bool
isNull HOCONNull = True
isNull _         = False

mapNode :: (Map String Config -> Config) -> Config -> Config
mapNode f (HOCONNode nodes) = f nodes
mapNode _ conf              = conf

getNode :: String -> Config -> Maybe (Map String Config)
getNode key conf = do
  conf' <- getConfig key conf
  case conf' of
    HOCONNode nodes -> return nodes
    _               -> Nothing

getConfig :: String -> Config -> Maybe Config
getConfig key = go (splitOn "." key)
 where
  go [k] conf = do
    nodes      <- getNodes conf
    nestedConf <- find ((== k) . fst) nodes
    return $ snd nestedConf
  go (k : ks) conf = do
    nodes      <- getNodes conf
    nestedConf <- find ((== k) . fst) nodes
    go ks (snd nestedConf)

getNumber :: String -> Config -> Maybe Double
getNumber key conf = do
  conf' <- getConfig key conf
  case conf' of
    HOCONNumber n -> return n
    _             -> Nothing

getString :: String -> Config -> Maybe String
getString key conf = do
  conf' <- getConfig key conf
  case conf' of
    HOCONString s -> return s
    _             -> Nothing

getList :: String -> Config -> Maybe [Config]
getList key conf = do
  conf' <- getConfig key conf
  case conf' of
    HOCONList l -> return l
    _           -> Nothing

getBool :: String -> Config -> Maybe Bool
getBool key conf = do
  conf' <- getConfig key conf
  case conf' of
    HOCONBool b -> return b
    _           -> Nothing

hasPath :: String -> Config -> Bool
hasPath key = isJust . getConfig key

getNodes :: Config -> Maybe [(String, Config)]
getNodes (HOCONNode nodes) = Just nodes
getNodes _                 = Nothing
