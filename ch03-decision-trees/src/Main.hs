{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import           Ch03DecisionTrees.Entropy
import           DataFiles

dataSet :: [Record]
dataSet =
    [ ([1, 1], "yes")
    , ([1, 1], "yes")
    , ([1, 0], "no")
    , ([0, 1], "no")
    , ([0, 1], "no")
    ]

labels :: [String]
labels = ["no surfacing", "flippers"]

main :: IO ()
main = do
    let sp = splitDataSet dataSet 0 1
    print sp
    putStrLn "Done"
