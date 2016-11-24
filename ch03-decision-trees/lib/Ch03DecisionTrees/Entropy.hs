{-# LANGUAGE LambdaCase #-}

module Ch03DecisionTrees.Entropy
    ( Labelled (..)
    , calculateShannonEntropy
    ) where

import qualified Data.Map as M

class Labelled a where
    label :: a -> String

-- cf trees.calcShannonEnt
calculateShannonEntropy :: Labelled a => [a] -> Double
calculateShannonEntropy rs =
    let count = length rs
        labelCounts = foldr
                        (\r m -> M.alter (\case { Nothing -> Just 1; Just n -> Just $ n + 1 }) (label r) m)
                        M.empty
                        rs
    in foldr
        (\n entropy -> let prob = probability n count in entropy -  prob * log2 prob)
        0.0
        labelCounts
    where probability n count = fromIntegral n / fromIntegral count
          log2 x = logBase 2 x
