module LAUtil.Arithmetic
  ( sumColumns
  , sumRows
  ) where

import           Numeric.LinearAlgebra

import           LAUtil.Data

sumColumns :: Matrix R -> Matrix R
sumColumns m = constantMatrix 1.0 1 (rows m) <> m

sumRows :: Matrix R -> Matrix R
sumRows m = constantMatrix 1.0 1 (cols m) <> tr m
