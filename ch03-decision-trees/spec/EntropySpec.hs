{-# LANGUAGE RecordWildCards #-}

module EntropySpec
    ( main
    , spec
    ) where

import           Ch03DecisionTrees.Entropy
import           MLUtil.Test
import           Test.Hspec

dataSet :: [Record]
dataSet =
    [ ([1, 1], "yes")
    , ([1, 1], "yes")
    , ([1, 0], "no")
    , ([0, 1], "no")
    , ([0, 1], "no")
    ]

spec :: Spec
spec = do
    describe "calculateShannonEntropy" $ do
        it "should return correct value" $
            calculateShannonEntropy dataSet `shouldRoundTo` 0.97095

    describe "splitDataSet" $ do
        it "should split 0, 0" $
            splitDataSet dataSet 0 0 `shouldBe` [([1], "no"), ([1], "no")]

        it "should split 0, 1" $
            splitDataSet dataSet 0 1 `shouldBe` [([1], "yes"), ([1], "yes"), ([0], "no")]

        it "should split 1, 0" $
            splitDataSet dataSet 1 0 `shouldBe` [([1], "no")]

        it "should split 1, 1" $
            splitDataSet dataSet 1 1 `shouldBe` [([1], "yes"), ([1], "yes"), ([0], "no"), ([0], "no")]

    describe "chooseBestFeatureToSplit" $ do
        it "calculate correctly" $ do
            let (gain, idx) = chooseBestFeatureToSplit dataSet
            gain `shouldRoundTo` 0.41997
            idx `shouldBe` 0

main :: IO ()
main = hspec spec
