module Class02Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class02

spec :: Spec
spec = do
  describe "sum'" $ do
    it "sum' list1 returns 0" $
      sum' list1 `shouldBe` 0
    it "sum' list2 returns 2" $
      sum' list2 `shouldBe` 2
    it "sum' list3 returns 5" $
      sum' list3 `shouldBe` 5
    it "sum' list4 returns -3" $
      sum' list4 `shouldBe` -3

  describe "max1" $ do
    it "max' list1 returns 0" $
      max' list1 `shouldBe` (minBound :: Int)
    it "max' list2 returns 2" $
      max' list2 `shouldBe` 2
    it "max' list3 returns 2" $
      max' list3 `shouldBe` 3
    it "max' list4 returns 2" $
      max' list4 `shouldBe` 2