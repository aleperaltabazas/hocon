module Text.Parser.HOCONSpec where

import Data.HOCON
import Test.Hspec
import Text.Parser.HOCON (parseHOCON)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

spec :: Spec
spec = describe "parseHOCON" $ do
  it "works" $ do
    parseHOCON "foo = bar" `shouldBe` Right (HOCONNode [("foo", HOCONString "bar")])
    parseHOCON "foo = bar\nbar = 123\nbaz = \"nani?!\""
      `shouldBe` Right (HOCONNode [("bar", HOCONNumber 123), ("baz", HOCONString "nani?!"), ("foo", HOCONString "bar")])
  it "fails" $ do
    parseHOCON "foo = bar\n{\n  foo = bar\n}" `shouldSatisfy` isLeft
    parseHOCON "{" `shouldSatisfy` isLeft