module LAUtil.Data
  ( constantMatrix
  , constantVector
  ) where

import qualified Data.Vector.Storable as VS
import           Numeric.LinearAlgebra

constantVector :: Double -> Int -> Vector R
constantVector value n = VS.replicate n value

constantMatrix :: Double -> Int -> Int -> Matrix R
constantMatrix value r c = matrix c (replicate (r * c) value)
