module LAUtil.Normalization
  ( normalizeColumn
  , normalizeMatrixColumns
  ) where

import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra

import           LAUtil.Folding

normalizeColumn :: Matrix R -> Int -> Vector R
normalizeColumn m c =
    let initialMinMax = columnHead m c
        (xMin, xMax) = foldColumn (\x (xMin, xMax) -> (min x xMin, max x xMax)) (initialMinMax, initialMinMax) m c
        range = xMax - xMin
    in VS.generate (rows m) (\r -> (m `atIndex` (r, c) - xMin) / range)

normalizeMatrixColumns :: Matrix R -> Matrix R
normalizeMatrixColumns m = fromColumns $ map (normalizeColumn m) [0..cols m - 1]
