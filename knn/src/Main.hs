module Main (main) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra

import           LAUtil hiding (plot)
import qualified LAUtil

group :: Matrix R
group = matrix 2
  [ 1.0, 1.1
  , 1.0, 1.0
  , 0.0, 0.0
  , 0.0, 0.1
  ]

labels :: V.Vector String
labels = V.fromList ["A", "A", "B", "B"]

plot :: LAUtil.Plot a => a -> IO ()
plot = LAUtil.plot "temp"

main :: IO ()
main = do
    let r = classify0 (matrix 2 [0.0, 0.0]) group labels 3
    print r
    let r = classify0 (matrix 2 [1.0, 1.2]) group labels 3
    print r
    plot (sin . cos)
    plot $ Data2D [Title "Sample Data"] [] [(1, 2), (2, 4), (3, 6), (4, 8), (5, 10)]

classify0 :: Matrix R -> Matrix R -> V.Vector String -> Int -> String
classify0 inX dataSet labels k =
    let dataSetSize = rows dataSet
        diffMat = repmat inX dataSetSize 1 - dataSet
        sqDiffMat = diffMat ** 2
        sqDistances = sumRows sqDiffMat
        distances = sqDistances ** 0.5
        sortedDistIndices :: Vector Z
        sortedDistIndices = argSort (unsafeToVector distances)
        classCounts = VS.foldr
            (\i m ->
                let label = (V.!) labels (fromIntegral i)
                in M.alter (\mb -> case mb of Just n -> Just (n + 1); Nothing -> Just 1) label m)
            M.empty
            (VS.take k sortedDistIndices)
        sortedClassCounts = L.sortBy (\(_, n0) (_, n1) -> compare n1 n0) (M.toList classCounts)
    in (fst . head) sortedClassCounts
