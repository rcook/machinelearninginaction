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

    -- Figure 2.3
    renderSVG
        "figure-2.3.svg"
        defaultChartLabels { clTitle = Just "Figure 2.3", clYAxisLabel = Just "Ice cream", clXAxisLabel = Just "Video games" }
        [simplePlot m 1 2]

    -- Figure 2.4
    renderSVG
        "figure-2.4.svg"
        defaultChartLabels { clTitle = Just "Figure 2.4", clYAxisLabel = Just "Ice cream", clXAxisLabel = Just "Video games" }
        (colouredSeriesPlots m 1 2)

    -- Figure 2.5
    renderSVG
        "figure-2.5.svg"
        defaultChartLabels { clTitle = Just "Figure 2.5", clYAxisLabel = Just "Video games", clXAxisLabel = Just "Frequent flyer miles" }
        (colouredSeriesPlots m 0 1)

    -- Normalized version of figure 2.5
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)
        m' = m { lmValues = mnValues }
    renderSVG
        "figure-2.5-normalized.svg"
        defaultChartLabels { clTitle = Just "Figure 2.5 (normalized)", clYAxisLabel = Just "Video games", clXAxisLabel =  Just "Frequent flyer miles" }
        (colouredSeriesPlots m' 0 1)

main :: IO ()
main = do
    renderFigures
    putStrLn "Done"
