{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra
import           Numeric.LinearAlgebra.Devel

import           LAUtil hiding (plot)
import qualified LAUtil

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

plot :: LAUtil.Plot a => a -> IO ()
plot = LAUtil.plot "temp"

dataPath :: FilePath
dataPath = "../Ch02/datingTestSet.txt"

main :: IO ()
main = do
    --LabelledMatrix{..} <- readLabelledMatrix dataPath
    --let r = classify0 (matrix 3 [0.0, 0.0, 0.0]) _values _labelIds 3
    --print r

    let r = classify0 (matrix 2 [0.0, 0.0]) group labelIds 3
        Just label = M.lookup r labels
    print label
    let r = classify0 (matrix 2 [1.0, 1.2]) group labelIds 3
        Just label = M.lookup r labels
    print label

    --plot (sin . cos)
    --plot $ Data2D [Title "Sample Data"] [] [(1, 2), (2, 4), (3, 6), (4, 8), (5, 10)]

classify0 :: Matrix R -> Matrix R -> VU.Vector LabelId -> Int -> LabelId
classify0 inX dataSet labelIds k =
    let dataSetSize = rows dataSet
        diffMat = repmat inX dataSetSize 1 - dataSet
        sqDiffMat = diffMat ** 2
        sqDistances = sumRows sqDiffMat
        distances = sqDistances ** 0.5
        sortedDistIndices :: IndexVector
        sortedDistIndices = argSort (unsafeMatrixToVector distances)
        classCounts = VU.foldr
            (\i m ->
                let labelId = (VU.!) labelIds (fromIntegral i)
                in M.alter (\mb -> case mb of Just n -> Just (n + 1); Nothing -> Just 1) labelId m)
            M.empty
            (VU.take k sortedDistIndices)
        sortedClassCounts = L.sortBy (\(_, n0) (_, n1) -> compare n1 n0) (M.toList classCounts)
    in (fst . head) sortedClassCounts
