module LAUtil.Sorting
  ( argSort
  )
  where

import qualified Data.List as L
import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra

argSort :: Vector R -> Vector Z
argSort xs = VS.fromList (L.map snd $ L.sortBy (\(x0, _) (x1, _) -> compare x0 x1) (L.zip (VS.toList xs) [0..]))
