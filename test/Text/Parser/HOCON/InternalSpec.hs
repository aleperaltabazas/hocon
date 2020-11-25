module Text.Parser.HOCON.InternalSpec
  ( spec
  )
where

import Data.HOCON (Config(..))
import Test.Hspec (describe, it, shouldBe, shouldSatisfy, Spec)
import Text.Parser.HOCON.Internal (arrayParser, numberParser, objectParser, parseProps, stringParser, preProcessing)
import Text.ParserCombinators.Parsec (parse)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

spec :: Spec
spec = do
  describe "stringParser" $ do
    it "succeeds" $ do
      parse stringParser "string" "\"saraza\"" `shouldBe` Right (HOCONString "saraza")
      parse stringParser "string" "saraza" `shouldBe` Right (HOCONString "saraza")
    it "fails" $ parse stringParser "string" "\"saraza" `shouldSatisfy` isLeft

  describe "numberParser" $ it "succeeds" $ do
    parse numberParser "number" "123456" `shouldBe` Right (HOCONNumber 123456)
    parse numberParser "number" "123.456" `shouldBe` Right (HOCONNumber 123.456)

  describe "arrayParser" $ do
    it "succeeds" $ do
      parse arrayParser "array" "[1,2,3]" `shouldBe` Right (HOCONList [HOCONNumber 1, HOCONNumber 2, HOCONNumber 3])
      parse arrayParser "array" "[1,\"a\", a]" `shouldBe` Right (HOCONList [HOCONNumber 1, HOCONString "a", HOCONString "a"])
    it "fails" $ do
      parse arrayParser "array" "[1" `shouldSatisfy` isLeft
      parse arrayParser "array" "[1, a?" `shouldSatisfy` isLeft

  describe "parseProps" $ it "succeeds" $ do
    parse parseProps "props" "foo: 123" `shouldBe` Right ("foo", HOCONNumber 123)
    parse parseProps "props" "foo = \"bar\"" `shouldBe` Right ("foo", HOCONString "bar")
    parse parseProps "props" "\"foo\":\"bar\"" `shouldBe` Right ("foo", HOCONString "bar")
    parse parseProps "props" "foo {\nbar = baz\n}" `shouldBe` Right ("foo", HOCONNode [("bar", HOCONString "baz")])
    parse parseProps "props" "foo.bar = baz" `shouldBe` Right ("foo", HOCONNode [("bar", HOCONString "baz")])

  describe "objectParser" $ do
    it "succeeds" $ do
      parse objectParser "object" "{foo = bar,\"baz\": \"baz\"}"
        `shouldBe` Right (HOCONNode [("foo", HOCONString "bar"), ("baz", HOCONString "baz")])
      parse objectParser "object" "{}" `shouldBe` Right (HOCONNode [])
    it "fails" $ do
      parse objectParser "object" "{\nfoo = bar\n\"baz\": \"baz\"\n" `shouldSatisfy` isLeft
      parse objectParser "object" "{\nfoo = bar\n\"baz\": \"baz\\n}" `shouldSatisfy` isLeft
  describe "preProcessing" $ do
    it "replaces al commas followed by newline with just the coma" $ do
      preProcessing "foo,\nbar,\nbaz" `shouldBe` "foo,bar,baz"
      preProcessing "foo, bar, baz" `shouldBe` "foo, bar, baz"
    it "replaces all opening brackets followed by newline with just the bracket" $ do
      preProcessing "{\nfoo = bar}" `shouldBe` "{foo = bar}"
      preProcessing "{foo = bar}" `shouldBe` "{foo = bar}"
    it "replaces all newlines followed by closing bracket with just the bracket" $ do
      preProcessing "{foo = bar\n}" `shouldBe` "{foo = bar}"
      preProcessing "{foo = bar}" `shouldBe` "{foo = bar}"
      preProcessing "{foo = bar\n  }" `shouldBe` "{foo = bar}"
