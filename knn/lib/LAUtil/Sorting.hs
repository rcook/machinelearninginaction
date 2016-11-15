module LAUtil.Sorting
  ( argSort
  )
  where

import           Control.Monad
import           Control.Monad.ST
import qualified Data.List as L
import           Data.Ord
import qualified Data.Vector.Algorithms.Intro as VAI
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Numeric.LinearAlgebra

-- Naive implementation
{-
argSort :: Vector R -> Vector Z
argSort xs = VS.fromList (L.map snd $ L.sortBy (\(x0, _) (x1, _) -> compare x0 x1) (L.zip (VS.toList xs) [0..]))
-}

argSort :: Vector R -> VU.Vector Int
argSort xs = runST $ do
    let l = VS.length xs
    t0 <- VUM.new l
    forM_ [0..l - 1] $
        \i -> VUM.unsafeWrite t0 i (i, (VS.!) xs i)
    VAI.sortBy (comparing snd) t0
    t1 <- VUM.new l
    forM_ [0..l - 1] $
        \i -> VUM.unsafeRead t0 i >>= \(x, _) -> VUM.unsafeWrite t1 i x
    VU.freeze t1
