module MLAlgs.Classify0 (classify0) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as VU
import           MLUtil

-- cf kNN.classify0
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
