{-# LANGUAGE RecordWildCards #-}

module Classify0Spec
    ( main
    , spec
    ) where

import           DataFiles
import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as M
import           MLAlgs.Classify0
import           MLUtil
import           Test.Hspec

-- cf kNN.createDataSet
values :: Matrix R
values = matrix 2
    [ 1.0, 1.1
    , 1.0, 1.0
    , 0.0, 0.0
    , 0.0, 0.1
    ]

-- cf kNN.createDataSet
labelIds :: VU.Vector LabelId
labelIds = VU.fromList
    [ 1
    , 1
    , 2
    , 2
    ]

k :: Int
k = 3

intFraction :: R -> Int -> Int
intFraction r x = round $ r * fromIntegral x

errorRate :: LabelledMatrix -> R -> R
errorRate m testRatio =
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)
        rowCount = rows mnValues
        columnCount = cols mnValues
        testRowCount = intFraction testRatio rowCount
        testMatrix = subMatrix (0, 0) (testRowCount, columnCount) mnValues
        trainingMatrix = subMatrix (testRowCount, 0) (rowCount - testRowCount, columnCount) mnValues
        trainingLabelIds = VU.slice testRowCount (rowCount - testRowCount) (lmLabelIds m)
        (passCount, errorCount) = forFold (0, 0) [0..testRowCount - 1] $ \r (passCount', errorCount') ->
            let testVector = subMatrix (r, 0) (1, columnCount) testMatrix
                actualLabelId = classify0 testVector trainingMatrix trainingLabelIds 3
                expectedLabelId = (VU.!) (lmLabelIds m) r
            in if actualLabelId == expectedLabelId
                then (passCount' + 1, errorCount')
                else (passCount', errorCount' + 1)
    in fromIntegral errorCount / fromIntegral (passCount + errorCount)

spec :: Spec
spec = do
    describe "classify0" $ do
        it "should classify small matrix correctly" $ do
            classify0 (row [0.0, 0.0]) values labelIds k `shouldBe` 2
            classify0 (row [1.0, 1.2]) values labelIds k `shouldBe` 1

        -- cf kNN.datingClassTest
        it "should classify large matrix correctly" $ do
            path <- getDataFileName "datingTestSet2.txt"
            Just m <- readLabelledMatrix path
            100.0 * errorRate m 0.2 `shouldBe` 8.0
            100.0 * errorRate m 0.1 `shouldBe` 5.0
            100.0 * errorRate m 0.05 `shouldBe` 2.0
            100.0 * errorRate m 0.02 `shouldBe` 0.0
            100.0 * errorRate m 0.01 `shouldBe` 0.0

main :: IO ()
main = hspec spec
