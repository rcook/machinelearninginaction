module LAUtil.Convert
  ( toVector
  , unsafeToVector
  ) where

import           Numeric.LinearAlgebra

toVector :: Matrix R -> Maybe (Vector R)
toVector m
    | rows m == 1 = Just $ (head . toRows) m
    | cols m == 1 = Just $ (head . toColumns) m
    | otherwise = Nothing

unsafeToVector :: Matrix R -> Vector R
unsafeToVector m = let Just v = toVector m in v
