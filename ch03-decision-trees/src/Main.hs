{-# LANGUAGE LambdaCase #-}

module Main (main) where

import           Ch03DecisionTrees.Foo
import qualified Data.Map as M
import           DataFiles

type Record = (Int, Int, String)

dataSet :: [Record]
dataSet =
    [ (1, 1, "yes")
    , (1, 1, "yes")
    , (1, 0, "no")
    , (0, 1, "no")
    , (0, 1, "no")
    ]

labels :: [String]
labels = ["no surfacing", "flippers"]

-- cf trees.calcShannonEnt
calculateShannonEntropy :: [Record] -> M.Map String Int
calculateShannonEntropy rs =
    let count = length rs
    in foldr (\(_, _, label) m -> M.alter (\case { Nothing -> Just 1; Just n -> Just $ n + 1 }) label m) M.empty rs

main :: IO ()
main = do
    let e = calculateShannonEntropy dataSet
    print e
    putStrLn "Done"
