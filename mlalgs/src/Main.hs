{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad
import qualified Data.List as L
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           DataFiles
import           MLAlgs.Classify0
import           MLUtil
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Strict as IOS
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
    videoGameTime <- prompt "Time playing video games: "
    frequentFlyerMiles <- prompt "Frequent flyer miles: "
    litresIceCream <- prompt "Litres of ice cream: "

    path <- getDataFileName "datingTestSet.txt"
    Just m <- readLabelledMatrix path
    let MatrixNormalization{..} = normalizeMatrixColumns (lmValues m)

    let testMatrix = row [frequentFlyerMiles, videoGameTime, litresIceCream]
        normalizedTestMatrix = (testMatrix - row mnMins) / row mnRanges
        r = classify0 normalizedTestMatrix mnValues (lmLabelIds m) 3
        Just label = M.lookup r (lmLabelMap m)

    putStrLn $ "Result: " ++ label

prompt :: Read a => String -> IO a
prompt s = do
    putStr s
    hFlush stdout
    readLn

-- cf kNN.img2vector
readImageVector :: Int -> Int -> FilePath -> IO [R]
readImageVector height width path = do
    ls <- lines <$> IOS.readFile path
    let xs = concat (take height $ map (\l -> take width $ map (\c -> if c == '0' then 0.0 else 1.0) l) ls)
    return xs

-- cf kNN.handwritingClassTest
blah :: IO ()
blah = do
    let height = 32
        width = 32
        columnCount = height * width
        dir = "data/digits/trainingDigits"
    paths <- listDirectory dir
    (values, labelIds) <- forFoldM ([], []) paths $ \(values', labelIds') p -> do
        let labelId = read $ head (splitOneOf ['_'] p)
        xs <- readImageVector 32 32 (dir </> p)
        return $ (values' ++ xs, labelIds' ++ labelId)
    let trainingMatrix = matrix columnCount values
        trainingLabelIds :: VU.Vector LabelId
        trainingLabelIds = VU.fromList labelIds
    print $ trainingLabelIds

main :: IO ()
main = do
    --renderFigures
    --classifyPerson
    blah
    putStrLn "Done"
