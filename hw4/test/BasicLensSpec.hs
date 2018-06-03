module BasicLensSpec where

import           BasicLens
import           Data.Function
import           Test.Hspec

spec :: Spec
spec = do
       it "Test: set" $ do
         ((1, 2) & _1 .~ 3) `shouldBe` (3, 2)
         ((1, 2) & _2 .~ 3) `shouldBe` (1, 3)
       it "Test: view" $ do
         (1, 2)^._1 `shouldBe` 1
         (1, 2)^._2 `shouldBe` 2
       it "Test: over" $ do
         ((1, 2) & _1 %~ (+ 1)) `shouldBe` (2, 2)
         ((1, 2) & _2 %~ (+ 1)) `shouldBe` (1, 3)
