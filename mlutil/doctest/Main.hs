module Main (main) where

import           System.FilePath.Glob
import           Test.DocTest

includeDirs :: [FilePath]
includeDirs = []

doctestWithIncludeDirs :: [FilePath] -> IO ()
doctestWithIncludeDirs fs = doctest (map ("-I" ++) includeDirs ++ fs)

main :: IO ()
main = glob "src/**/*.hs" >>= doctestWithIncludeDirs
