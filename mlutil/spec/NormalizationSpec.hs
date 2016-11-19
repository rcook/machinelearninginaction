{-# LANGUAGE RecordWildCards #-}

module NormalizationSpec
  ( main
  , spec
  ) where

import           MLUtil
import           Test.Hspec

import           Expectations

m :: Matrix R
m = matrix 3 [1.0, 2.0, 3.0, 4.0, 5.0, 100.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]

spec :: Spec
spec = do
    describe "normalizeColumn" $ do
        it "normalizes column 0" $ do
            let ColumnNormalization{..} = normalizeColumn m 0
            cnValues `shouldRoundTo` vector [0.0, 0.375, 0.625, 1.0]
            cnRange `shouldBe` 8.0
            cnMin `shouldBe` 1.0
        it "normalizes column 1" $ do
            let ColumnNormalization{..} = normalizeColumn m 1
            cnValues `shouldRoundTo` vector [0.0, 0.375, 0.625, 1.0]
            cnRange `shouldBe` 8.0
            cnMin `shouldBe` 2.0
        it "normalizes column 2" $ do
            let ColumnNormalization{..} = normalizeColumn m 2
            cnValues `shouldRoundTo` vector [0.0, 1.0, 0.05155, 0.08247]
            cnRange `shouldBe` 97.0
            cnMin `shouldBe` 3.0

main :: IO ()
main = hspec spec
