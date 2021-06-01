module Class05Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class05

spec :: Spec
spec = do
  -- 4b.mlのテスト
  describe "nat length test" $ do
    it "length zero == 0" $
      Class05.length zero `shouldBe` 0
    it "length one == 1" $
      Class05.length one `shouldBe` 1
    it "length two == 2" $
      Class05.length two `shouldBe` 2
    it "length three == 3" $
      Class05.length three `shouldBe` 3

  -- 4b.mlのテスト
  describe "lengthList test" $ do
    it "lengthList empty == 0" $
      Class05.lengthList empty `shouldBe` 0
    it "lengthList lst1 == 1" $
      Class05.lengthList lst1 `shouldBe` 1
    it "lengthList lst2 == 2" $
      Class05.lengthList lst2 `shouldBe` 2
    it "lengthList lst3 == 0" $
      Class05.lengthList lst3 `shouldBe` 4
  
  -- 4b.mlのテスト
  describe "sumList test" $ do
    it "sumList empty == 0" $
      Class05.sumList empty `shouldBe` 0
    it "lengthList lst1 == 1" $
      Class05.sumList lst1 `shouldBe` 1
    it "lengthList lst2 == 2" $
      Class05.sumList lst2 `shouldBe` 3
    it "lengthList lst3 == 0" $
      Class05.sumList lst3 `shouldBe` 6

  describe "Question4. test countEmpty" $ do
    it "tree1 has 1 empty" $
      countEmpty tree1 `shouldBe` 1
    it "tree2 has 2 empty" $
      countEmpty tree2 `shouldBe` 2
    it "tree3 has 5 empty" $
      countEmpty tree3 `shouldBe` 5
    it "tree4 has 8 empty" $
      countEmpty tree4 `shouldBe` 7
    it "tree5 has 2 empty" $
      countEmpty tree5 `shouldBe` 2
    it "tree6 has 5 empty" $
      countEmpty tree6 `shouldBe` 5
    it "tree7 has 7 empty" $
      countEmpty tree7 `shouldBe` 7

  describe "Question5. test size" $ do
    it "tree1 has 0 node" $
      size tree1 `shouldBe` 0
    it "tree2 has 1 node" $
      size tree2 `shouldBe` 1
    it "tree3 has 4 node" $
      size tree3 `shouldBe` 4
    it "tree4 has 5 node" $
      size tree4 `shouldBe` 6
    it "tree5 has 1 node" $
      size tree5 `shouldBe` 1
    it "tree6 has 4 node" $
      size tree6 `shouldBe` 4
    it "tree7 has 5 node" $
      size tree7 `shouldBe` 6

  describe "Question6 test sumTree" $ do
    it "the sum of tree1 is 0" $
      sumTree tree1 `shouldBe` 0
    it "the sum of tree2 is 1" $
      sumTree tree2 `shouldBe` 1
    it "the sum of tree3 is 3" $
      sumTree tree3 `shouldBe` 4
    it "the sum of tree4 is 1" $
      sumTree tree4 `shouldBe` 8