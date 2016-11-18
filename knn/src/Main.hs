{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           MLUtil

group :: Matrix R
group = matrix 2
  [ 1.0, 1.1
  , 1.0, 1.0
  , 0.0, 0.0
  , 0.0, 0.1
  ]

labels :: M.Map LabelId String
labels = M.fromList [(1, "A"), (2, "B")]

labelIds :: VU.Vector LabelId
labelIds = VU.fromList [1, 1, 2, 2]

dataPath :: FilePath
dataPath = "../Ch02/datingTestSet.txt"

demoClassify0 :: IO ()
demoClassify0 = do
    let r = classify0 (row [0.0, 0.0]) group labelIds 3
        Just label = M.lookup r labels
    print label

    let r = classify0 (row [1.0, 1.2]) group labelIds 3
        Just label = M.lookup r labels
    print label

input :: LabelledMatrix
input = LabelledMatrix
    (matrix 2 [1.0, 1.1, 2.0, 2.1, 3.0, 3.1, 4.0, 4.1, 5.0, 5.1, 6.0, 6.1])
    (VU.fromList [10, 10, 20, 20, 10, 30])
    (M.fromList [("ten", 10), ("twenty", 20), ("thirty", 30)])
    (M.fromList [(10, "ten"), (20, "twenty"), (30, "thirty")])

renderChapter2Figures :: IO ()
renderChapter2Figures = do
    m <- readLabelledMatrix dataPath

    -- Figure 2.4
    renderSVG "Ice cream vs. video games" "figure-2.4.svg" (plots m 1 2)

    -- Figure 2.5
    renderSVG "Video games vs. frequent flyer miles" "figure-2.5.svg" (plots m 0 1)

testNormalizeMatrixColumns :: IO ()
testNormalizeMatrixColumns = do
    m <- readLabelledMatrix dataPath
    let m' = m { lmValues = mnValues $ normalizeMatrixColumns (lmValues m) }
    renderSVG "Normalized: video games vs. frequent flyer miles" "figure-2.5-normalized.svg" (plots m' 0 1)

main :: IO ()
main = do
    demoClassify0
    renderChapter2Figures
    testNormalizeMatrixColumns

    --let r = classify0 (matrix 3 [0.0, 0.0, 0.0]) _values _labelIds 3
    --print r

classify0 :: Matrix R -> Matrix R -> VU.Vector LabelId -> Int -> LabelId
classify0 inX dataSet labelIds k =
    let dataSetSize = rows dataSet
        diffMat = repmat inX dataSetSize 1 - dataSet
        sqDiffMat = diffMat ** 2
        sqDistances = sumRows sqDiffMat
        distances = sqDistances ** 0.5
        sortedDistIndices :: IndexVector
        sortedDistIndices = argSort (flatten distances)
        classCounts = VU.foldr
            (\i m ->
                let labelId = (VU.!) labelIds (fromIntegral i)
                in M.alter (\mb -> case mb of Just n -> Just (n + 1); Nothing -> Just 1) labelId m)
            M.empty
            (VU.take k sortedDistIndices)
        sortedClassCounts = L.sortBy (\(_, n0) (_, n1) -> compare n1 n0) (M.toList classCounts)
    in (fst . head) sortedClassCounts
