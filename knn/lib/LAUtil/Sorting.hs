module LAUtil.Sorting
  ( argSort
  )
  where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

argSort :: VS.Vector Double -> V.Vector Int
argSort xs = V.fromList (L.map snd $ L.sortBy (\(x0, _) (x1, _) -> compare x0 x1) (L.zip (VS.toList xs) [0..]))
