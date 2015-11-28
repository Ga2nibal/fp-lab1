module Main where

import Modules.FCM
import Modules.Distance
import Modules.InitializeFcmMethods
import Test.Hspec
import Data.Vector as V

main :: IO ()
main = hspec $ do
  describe "Distancies" $ do
    describe "Hemming" $ do
      it "Hemming dist == 9" $ do
        let v1 =  [2, 2, 2]
            v2 =  [4, 5, 6]
        hammingDistance v1 v2 `shouldBe` (9 :: Double)
      it "Hemming dist == 0" $ do
        let v1 =  [1, 2, 3]
            v2 =  [1, 2, 3]
        hammingDistance v1 v2 `shouldBe` (0 :: Double)

    describe "Euclid" $ do
      it "Euclid == 5" $ do
        let v1 =  [1, 2]
            v2 =  [4, 6]
        euclideanDistance v1 v2 `shouldBe` (5 :: Double)
      it "Euclid == 0" $ do
        let v1 =  [1, 2, 3]
            v2 =  [1, 2, 3]
        euclideanDistance v1 v2 `shouldBe` (0 :: Double)

    describe "calculateMatrixDiff" $ do
      it "calculateMatrixDiff == 4" $ do
        let m1 = [[3, 2, 1], [9, 5, 6]]
            m2 = [[1, 5, 4], [5, 2, 8]]
        calculateMatrixDiff m1 m2 `shouldBe` (4 :: Double)

      it "calculateMatrixDiff == 0" $ do
        let m1 = [[1, 2, 3], [4, 5, 6]]
            m2 = [[1, 2, 3], [4, 5, 6]]
        calculateMatrixDiff m1 m2 `shouldBe` (0 :: Double)