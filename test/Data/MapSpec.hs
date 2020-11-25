module Data.MapSpec where

import Data.Map
import Test.Hspec

spec :: Spec
spec =
  describe "groupBy"
    $          it "joins all the items whose key is the same"
    $          groupBy length ["abc", "ab", "a", "atlas", "hades", "orpheus"]
    `shouldBe` [(1, ["a"]), (2, ["ab"]), (3, ["abc"]), (5, ["hades", "atlas"]), (7, ["orpheus"])]
