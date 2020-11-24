module Data.HOCON
  ( Config(..)
  )
where

data Config =
  HOCONNode [(String, Config)] |
  HOCONString String |
  HOCONNumber Double |
  HOCONList [Config] |
  HOCONBool Bool |
  HOCONNull
  deriving (Show, Eq)
