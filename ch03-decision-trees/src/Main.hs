module Main (main) where

import           Ch03DecisionTrees.Entropy
import           DataFiles

data Record = R Int Int String deriving Show
instance Labelled Record where
    label (R _ _ s) = s

dataSet :: [Record]
dataSet =
    [ R 1 1 "yes"
    , R 1 1 "yes"
    , R 1 0 "no"
    , R 0 1 "no"
    , R 0 1 "no"
    ]

labels :: [String]
labels = ["no surfacing", "flippers"]

main :: IO ()
main = do
    let e = calculateShannonEntropy dataSet
    print e
    putStrLn "Done"
