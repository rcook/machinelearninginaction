module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra

constantVec :: Double -> Int -> Vector R
constantVec value n = VS.replicate n value

constantMat :: Double -> Int -> Int -> Matrix R
constantMat value r c = matrix c (replicate (r * c) value)

group :: Matrix R
group = matrix 2
  [ 1.0, 1.1
  , 1.0, 1.0
  , 0.0, 0.0
  , 0.0, 0.1
  ]

labels :: V.Vector String
labels = V.fromList ["A", "A", "B", "B"]

sumColumns :: Matrix R -> Matrix R
sumColumns m = constantMat 1.0 1 (rows m) <> m

sumRows :: Matrix R -> Matrix R
sumRows m = constantMat 1.0 1 (cols m) <> tr m

main :: IO ()
main = do
    let r = classify0 (matrix 2 [0.0, 0.0]) group labels 3
    print r

classify0 :: Matrix R -> Matrix R -> V.Vector String -> Int -> Matrix R
classify0 inX dataSet labels k =
    let dataSetSize = rows dataSet
        diffMat = repmat inX dataSetSize 1 - dataSet
        sqDiffMat = diffMat ** 2
        sqDistances = sumRows sqDiffMat
        distances = sqDistances ** 0.5
    in distances

{-
def classify0(in_x, data_set, labels, k):
  data_set_size = data_set.shape[0]
  diff_mat = tile(in_x, (data_set_size, 1)) - data_set
  sq_diff_mat = diff_mat ** 2
  sq_distances = sq_diff_mat.sum(axis=1)
  distances = sq_distances ** 0.5
  sorted_dist_indices = distances.argsort()
  class_count = {}
  for i in range(k):
    label = labels[sorted_dist_indices[i]]
    class_count[label] = class_count.get(label, 0) + 1
  sorted_class_count = sorted(class_count.iteritems(), key=operator.itemgetter(1), reverse=True)
  return sorted_class_count[0][0]

group, labels = create_data_set()
print(classify0([0, 0], group, labels, 3))
print(classify0([1, 1.2], group, labels, 3))
-}
