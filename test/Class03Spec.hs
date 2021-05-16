module Class03Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class03

spec :: Spec
spec = do
  describe "sample in class" $ do
    it "0 did not occur in tree1" $
      occur' 0 tree1 `shouldBe` False
    it "0 did not occur in tree2" $
      occur' 0 tree2 `shouldBe` True
    it "1 did not occur in tree3" $
      occur' 1 tree2 `shouldBe` False
    it "0 did not occur in tree4" $
      occur' 0 tree4 `shouldBe` True
    it "1 did not occur in tree4" $
      occur' 1 tree4 `shouldBe` True
    it "2 did not occur in tree4" $
      occur' 2 tree4 == False