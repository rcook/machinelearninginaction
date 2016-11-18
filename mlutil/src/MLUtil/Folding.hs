module MLUtil.Folding
  ( columnHead
  , foldColumn
  ) where

import           Numeric.LinearAlgebra

columnHead :: Matrix R -> Int -> R
columnHead m c = m `atIndex` (0, c)

foldColumn :: (R -> b -> b) -> b -> Matrix R -> Int -> b
foldColumn f acc m c = foldr (\r acc' -> f (m `atIndex` (r, c)) acc') acc [0..rows m - 1]
