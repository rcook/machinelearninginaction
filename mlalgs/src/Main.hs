{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           DataFiles
import           MLAlgs.Classify0
import           MLUtil
import           System.Exit
import           System.IO
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

-- cf kNN.classifyPerson
classifyPerson :: IO ()
classifyPerson = do
    result :: Maybe R <- prompt "Time playing video games: "
    when (isNothing result) (putStrLn "Quit" >> exitSuccess)
    let Just videoGameTime = result

    result :: Maybe R <- prompt "Frequent flyer miles: "
    when (isNothing result) (putStrLn "Quit" >> exitSuccess)
    let Just frequentFlyerMiles = result

    result :: Maybe R <- prompt "Litres of ice cream: "
    when (isNothing result) (putStrLn "Quit" >> exitSuccess)
    let Just litresIceCream = result

    path <- getDataFileName "datingTestSet.txt"
    Just m <- readLabelledMatrix path
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)

    let testMatrix = row [frequentFlyerMiles, videoGameTime, litresIceCream]
        normalizedTestMatrix = (testMatrix - row mnMins) / row mnRanges
        r = classify0 normalizedTestMatrix mnValues (lmLabelIds m) 3
        Just label = M.lookup r (lmLabelMap m)

    putStrLn $ "Result: " ++ label

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of [(x, "")] -> Just x
                              _ -> Nothing

prompt :: Read a => String -> IO (Maybe a)
prompt s = do
    putStr s
    hFlush stdout
    s <- getLine
    return $ readMaybe s

main :: IO ()
main = do
    --renderFigures
    classifyPerson
    putStrLn "Done"
