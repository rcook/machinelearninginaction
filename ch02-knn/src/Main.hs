{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Ch02KNN.Classify0
import           Control.Monad
import qualified Data.List as L
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import           DataFiles
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

k :: Int
k = 3

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
        r = classify0 normalizedTestMatrix mnValues (lmLabelIds m) k
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
handwritingClassTest :: IO ()
handwritingClassTest = do
    let height = 32
        width = 32
        columnCount = height * width
        trainingDataDir = "data/digits/trainingDigits"
        testDataDir = "data/digits/testDigits"
    trainingPaths <- listDirectory trainingDataDir
    testPaths <- listDirectory testDataDir

    (values, labelIds) <- forFoldM ([], []) (L.reverse trainingPaths) $ \(values', labelIds') p -> do
        let labelId = read $ head (splitOneOf ['_'] p)
        xs <- readImageVector 32 32 (trainingDataDir </> p)
        return $ (xs ++ values', labelId : labelIds')

    putStrLn "Loading training data"

    let trainingMatrix = matrix columnCount values
        trainingLabelIds = VU.fromList labelIds

    putStrLn "Classifying test data"

    (errorCount, passCount) <- forFoldM (0, 0) testPaths $ \(errorCount', passCount') p -> do
        let labelId = read $ head (splitOneOf ['_'] p)
        m <- row <$> readImageVector 32 32 (testDataDir </> p)
        let r = classify0 m trainingMatrix trainingLabelIds k
        return $ if r == labelId
            then (errorCount', passCount' + 1)
            else (errorCount' + 1, passCount')

    let errorRate :: R
        errorRate = 100.0 * fromIntegral errorCount / fromIntegral (errorCount + passCount)
    putStrLn $ printf "Error rate: %0.1f%%" errorRate

main :: IO ()
main = do
    --renderFigures
    --classifyPerson
    handwritingClassTest
    putStrLn "Done"
