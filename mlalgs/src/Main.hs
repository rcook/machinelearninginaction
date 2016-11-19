{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           MLAlgs.Classify0
import           MLUtil
import           Text.Printf

dataPath :: FilePath
dataPath = "../Ch02/datingTestSet.txt"

renderFigures :: IO ()
renderFigures = do
    Just m <- readLabelledMatrix dataPath

    -- Figure 2.4
    renderSVG "Ice cream vs. video games" "figure-2.4.svg" (plots m 1 2)

    -- Figure 2.5
    renderSVG "Video games vs. frequent flyer miles" "figure-2.5.svg" (plots m 0 1)

    -- Normalized version of figure 2.5
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)
        m' = m { lmValues = mnValues }
    renderSVG "Normalized: video games vs. frequent flyer miles" "figure-2.5-normalized.svg" (plots m' 0 1)
    print mnRanges
    print mnMins

main :: IO ()
main = do
    --renderFigures
    putStrLn "Done"
