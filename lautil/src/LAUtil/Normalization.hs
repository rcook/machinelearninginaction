module LAUtil.Normalization
  ( ColumnNormalization (..)
  , MatrixNormalization (..)
  , normalizeColumn
  , normalizeMatrixColumns
  ) where

import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra

import           LAUtil.Folding

data ColumnNormalization = ColumnNormalization
  { cnValues :: Vector R
  , cnRange :: R
  , cnMin :: R
  } deriving Show

data MatrixNormalization = MatrixNormalization
  { mnValues :: Matrix R
  , mnRanges :: [R]
  , mnMins :: [R]
  } deriving Show

normalizeColumn :: Matrix R -> Int -> ColumnNormalization
normalizeColumn m c =
    let initialMinMax = columnHead m c
        (xMin, xMax) = foldColumn (\x (xMin, xMax) -> (min x xMin, max x xMax)) (initialMinMax, initialMinMax) m c
        range = xMax - xMin
    in ColumnNormalization
        (VS.generate (rows m) (\r -> (m `atIndex` (r, c) - xMin) / range))
        range
        xMin

-- normalizeMatrixColumns is our equivalent to kNN.autoNorm
normalizeMatrixColumns :: Matrix R -> MatrixNormalization
normalizeMatrixColumns m =
    let (columns, ranges, mins) = foldr (\c (columns, ranges, mins) -> (columns, ranges, mins)) ([], [], []) [0..cols m - 1]
    in MatrixNormalization (fromColumns columns) ranges mins
