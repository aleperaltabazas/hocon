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
    parseHOCON "{\n  \"foo\": \"bar\"\n}" `shouldBe` Right (HOCONNode [("foo", HOCONString "bar")])
    parseHOCON
        "foo = bar\nbar = true\nsomeObject {\n  someField = [\n    foo,\n    bar,\n    baz\n  ]\n  someOtherField = [\n    {\n      foo = bar\n    }\n  ]\n}"
      `shouldBe` Right
                   (HOCONNode
                     [ ("bar", HOCONBool True)
                     , ("foo", HOCONString "bar")
                     , ( "someObject"
                       , HOCONNode
                         [ ("someField"     , HOCONList [HOCONString "foo", HOCONString "bar", HOCONString "baz"])
                         , ("someOtherField", HOCONList [HOCONNode [("foo", HOCONString "bar")]])
                         ]
                       )
                     ]
                   )
  it "fails" $ do
    parseHOCON "foo = bar\n{\n  foo = bar\n}" `shouldSatisfy` isLeft
    parseHOCON "{" `shouldSatisfy` isLeft
