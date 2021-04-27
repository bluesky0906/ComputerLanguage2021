module Class01Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class01

spec :: Spec
spec = do
  describe "add1" $ do
    it "add1 [] returns []" $
      add1 [] `shouldBe` []
    it "add1 [1] returns [2]" $
      add1 [1] `shouldBe` [2]
    it "add1 [1..199] returns [2..200]" $
      add1 [1..199] `shouldBe` [2..200]
    it "add1 [-10..10] returns [-9..11]" $
      add1 [-10..10] `shouldBe` [-9..11]

  describe "double" $ do
    it "double [] returns []" $
      double [] `shouldBe` []
    it "double [1] returns [2]" $
      double [1] `shouldBe` [2]
    it "double [1..20] returns [2,4..40]" $
      double [1..20] `shouldBe` [2,4..40]
    it "double [1..20] returns [(-40),(-38)..8]" $
      double [(-20)..4] `shouldBe` [(-40),(-38)..8]
  
  describe "prod" $ do
    it "prod [] returns []" $
      prod [] `shouldBe` 1
    it "prod [2] returns 2" $
      prod [2] `shouldBe` 2
    it "prod [2,3,4] returns 24" $
      prod [2,3,4] `shouldBe` 24
    it "prod [1..10] returns 3628800" $
      prod [1..10] `shouldBe` 3628800
    it "prod [(-10)..(-1)] returns 3628800" $
      prod [(-10)..(-1)] `shouldBe` 3628800

  describe "max" $ do
    it "max [] returns 0" $
      max' [] `shouldBe` 0
    it "max [1] returns 1" $
      max' [1] `shouldBe` 1
    it "max [3,5,1] returns 5" $
      max' [3,5,1] `shouldBe` 5
    it "max (shuffle [1..100]) returns 100" $ do
      list <- sample' $ shuffle [1..100]
      max' (head list) `shouldBe` 100
    
  describe "sumProdTree" $ do
    it "sumProdTree Empty returns 0" $
      sumProdTree Empty `shouldBe` 0
    it "sumProdTree (Node (Empty, 2, 3, Empty)) returns 6" $
      sumProdTree (Node (Empty, 2, 3, Empty)) `shouldBe` 6
    it "sumProdTree (Node (Node (Empty, 3, 4, Node (Empty, 5, 6, Empty)), -2, 3, Node (Node (Empty, 8, -3, Empty), 3, 4, Empty))) returns 24" $
      sumProdTree (Node (Node (Empty, 3, 4, Node (Empty, 5, 6, Empty)), -2, 3, Node (Node (Empty, 8, -3, Empty), 3, 4, Empty))) `shouldBe` 24

  describe "zeroString" $ do
    it "zeroString Empty returns empty ''" $
      zeroString Empty `shouldBe` ""
    it "zeroString (Node (Empty, 'a', 2, Empty)) returns '" $
      zeroString (Node (Empty, "a", 2, Empty)) `shouldBe` ""
    it "zeroString (Node (Empty, 'a', 0, Empty)) returns 'a" $
      zeroString (Node (Empty, "a", 0, Empty)) `shouldBe` "a"
    it "zeroString (Node (Node (Empty, 'abc', 4, Node (Empty, 'def', 0, Empty)), 'ghi', -2, Node (Node (Empty, 'jklm', 0, Empty), 'no', 0, Empty))) returns defjklmno" $
      zeroString (Node (Node (Empty, "abc", 4, Node (Empty, "def", 0, Empty)), "ghi", -2, Node (Node (Empty, "jklm", 0, Empty), "no", 0, Empty))) `shouldBe` "defjklmno"