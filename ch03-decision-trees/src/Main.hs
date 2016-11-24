module Main (main) where

import           Ch03DecisionTrees.Foo
import           DataFiles

main :: IO ()
main = do
    x <- getDataFileName "x"
    foo
    putStrLn "Done"
