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