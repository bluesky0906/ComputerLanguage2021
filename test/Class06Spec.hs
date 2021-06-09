module Class06Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class06

spec :: Spec
spec = do
  -- 6b.mlのテスト
  describe "zigzag length test" $ do
    it "length endo == 0" $
      Class06.length 0 endo `shouldBe` 0
    it "length zagEnd == 1" $
      Class06.length 1 zagEnd `shouldBe` 1
    it "length zigZagEnd == 2" $
      Class06.length 0 zigZagEnd `shouldBe` 2
    it "length zagZigZagEnd == 3" $
      Class06.length 1 zagZigZagEnd `shouldBe` 3
    it "length zigZagZigZagEnd == 4" $
      Class06.length 0 zigZagZigZagEnd `shouldBe` 4
    
  -- 問１
  describe "countVarF test" $ do
    it "countVar lambda1 == 1" $
      countVar 0 lambda1 `shouldBe` 1
    it "countVar lambda2 == 1" $
      countVar 1 lambda2 `shouldBe` 1
    it "countVar lambda3 == 1" $
      countVar 0 lambda3 `shouldBe` 1
    it "countVar lambda4 == 1" $
      countVar 1 lambda4 `shouldBe` 1
    it "countVar lambda5 == 2" $
      countVar 1 lambda5 `shouldBe` 2