module Class03Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Class03

spec :: Spec
spec = do
  describe "occur function for trees" $ do
    it "0 did not occur in tree1" $
      occur' 0 tree1 `shouldBe` False
    it "0 occur in tree2" $
      occur' 0 tree2 `shouldBe` True
    it "1 did not occur in tree3" $
      occur' 1 tree2 `shouldBe` False
    it "0 occur in tree4" $
      occur' 0 tree4 `shouldBe` True
    it "1 occur in tree4" $
      occur' 1 tree4 `shouldBe` True
    it "2 did not occur in tree4" $
      occur' 2 tree4 `shouldBe` False

  -- 1
  describe "occur function for types" $ do
    it "0 did not occur in type1" $
      occur' 0 type1 `shouldBe` False
    it "0 did not occur in type2" $
      occur' 0 type2 `shouldBe` True
    it "1 did not occur in type3" $
      occur' 1 type2 `shouldBe` False
    it "1 did not occur in type4" $
      occur' 1 type4 `shouldBe` False
    it "0 occur in type5" $
      occur' 0 type5 `shouldBe` True
    it "1 did not occur in type5" $
      occur' 1 type5 `shouldBe` False
    it "3 occur in type5" $
      occur' 3 type5 `shouldBe` True

  -- 2
  describe "draw function for trees" $ do
    it "print tree1" $
      draw' tree1 `shouldBe` "[]"
    it "print tree2" $
      draw' tree2 `shouldBe` "[[], a, 3, {0}]"
    it "print tree3" $
      draw' tree3 `shouldBe` "[[[], a, 3, {0}], b, 4, [[], c, 1, []]]"
    it "print tree4" $
      draw' tree4 `shouldBe` "[[[], a, 3, {0}], b, 0, [{1}, c, 0, []]]"
  
  describe "draw function for types" $ do
    it "print types1" $
      draw' type1 `shouldBe` "Int"
    it "print types2" $
      draw' type2 `shouldBe` "Int -> {0}"
    it "print types3" $
      draw' type3 `shouldBe` "(Int, Int)"
    it "print types4" $
      draw' type4 `shouldBe` "(Int -> Int, Int) -> (Int, Int)"
    it "print types5" $
      draw' type5 `shouldBe` "(Int -> {0}, Int) -> (Int, Int) -> {3}"
    