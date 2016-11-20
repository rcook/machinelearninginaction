{-# LANGUAGE RecordWildCards #-}

module MLUtil.LabelledMatrixPlot
    ( colouredSeriesPlots
    , simplePlot
    ) where

import qualified Data.Map as M
import           Data.Vector.Storable as VS hiding (foldr, map)
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports
import           MLUtil.LabelledMatrix
import           MLUtil.RRScatterPlot

type Coordinate = (R, R)
type CoordinateList = [Coordinate]
type PlotSpec = (String, CoordinateList)

partitionIndices :: Ord a => [a] -> M.Map a [Int]
partitionIndices xs = foldr f M.empty (zip [0..] xs)
    where f (i, x) m = M.alter (\mb -> case mb of Nothing -> Just [i]; Just is -> Just (i : is)) x m

colouredSeriesPlotSpecs :: LabelledMatrix -> Int -> Int -> [PlotSpec]
colouredSeriesPlotSpecs LabelledMatrix{..} xColumnIndex yColumnIndex =
    let columns = toColumns lmValues
        xColumn = columns !! xColumnIndex
        yColumn = columns !! yColumnIndex
        labelIds = VU.toList lmLabelIds
        partitions = M.toList $ partitionIndices labelIds
    in (flip map) partitions $ \(labelId, indices) ->
        let subseries = foldr f [] indices
                          where f i cs = let c = ((VS.!) xColumn i, (VS.!) yColumn i) in c : cs
            Just labelText = M.lookup labelId lmLabelMap
        in (labelText, subseries)

colouredSeriesPlots :: LabelledMatrix -> Int -> Int -> [RRScatterPlot]
colouredSeriesPlots m xColumnIndex yColumnIndex = map
    (uncurry points)
    (colouredSeriesPlotSpecs m xColumnIndex yColumnIndex)

simplePlot :: LabelledMatrix -> Int -> Int -> RRScatterPlot
simplePlot LabelledMatrix{..} xColumnIndex yColumnIndex =
    let columns = toColumns lmValues
        xColumn = columns !! xColumnIndex
        yColumn = columns !! yColumnIndex
    in points "Series" (zip (VS.toList xColumn) (VS.toList yColumn))
