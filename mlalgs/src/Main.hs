{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           MLAlgs.Classify0
import           MLUtil

dataPath :: FilePath
dataPath = "../Ch02/datingTestSet.txt"

renderFigures :: IO ()
renderFigures = do
    Just m <- readLabelledMatrix dataPath

    -- Figure 2.4
    renderSVG "Ice cream vs. video games" "figure-2.4.svg" (plots m 1 2)

    -- Figure 2.5
    renderSVG "Video games vs. frequent flyer miles" "figure-2.5.svg" (plots m 0 1)

    -- Normalized version of figure 2.5
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)
        m' = m { lmValues = mnValues }
    renderSVG "Normalized: video games vs. frequent flyer miles" "figure-2.5-normalized.svg" (plots m' 0 1)
    print mnRanges
    print mnMins

intFraction :: Float -> Int -> Int
intFraction r x = round $ r * fromIntegral x

datingClassTest :: IO ()
datingClassTest = do
    Just m <- readLabelledMatrix dataPath
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)
        testRatio = 0.1
        rowCount = rows mnValues
        columnCount = cols mnValues
        testRowCount = intFraction testRatio rowCount
        testMatrix = subMatrix (0, 0) (testRowCount, columnCount) mnValues
        trainingMatrix = subMatrix (testRowCount, 0) (rowCount - testRowCount, columnCount) mnValues
    (passCount, errorCount) <- forFoldM (0, 0) [0..testRowCount - 1] $ \(passCount', errorCount') r -> do
        let testVector = subMatrix (r, 0) (1, columnCount) testMatrix
            actualLabelId = classify0 testVector trainingMatrix (lmLabelIds m) 3
            expectedLabelId = (VU.!) (lmLabelIds m) r
        return $ if actualLabelId == expectedLabelId
            then (passCount' + 1, errorCount')
            else (passCount', errorCount' + 1)
    print passCount
    print errorCount

main :: IO ()
main = do
    --renderFigures
    datingClassTest
