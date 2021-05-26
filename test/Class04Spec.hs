module Class04Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class04

spec :: Spec
spec = do
  describe "nat test" $ do
    it "length zero == 0" $
      Class04.length zero `shouldBe` 0
    it "length one == 1" $
      Class04.length one `shouldBe` 1
    it "length two == 2" $
      Class04.length two `shouldBe` 2
    it "length three == 3" $
      Class04.length three `shouldBe` 3

  describe "test countEmpty" $ do
    it "tree1 has 1 empty" $
      countEmpty tree1 `shouldBe` 1
    it "tree2 has 2 empty" $
      countEmpty tree2 `shouldBe` 2
    it "tree3 has 5 empty" $
      countEmpty tree3 `shouldBe` 5
    it "tree4 has 8 empty" $
      countEmpty tree4 `shouldBe` 8

  describe "test size" $ do
    it "tree1 has 0 node" $
      size tree1 `shouldBe` 0
    it "tree2 has 1 node" $
      size tree2 `shouldBe` 1
    it "tree3 has 2 node" $
      size tree3 `shouldBe` 4
    it "tree4 has 3 node" $
      size tree4 `shouldBe` 7