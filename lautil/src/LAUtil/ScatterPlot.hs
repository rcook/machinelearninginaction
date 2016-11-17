{-# LANGUAGE RecordWildCards #-}

module LAUtil.ScatterPlot
  ( RRPlot
  , plots
  ) where

import qualified Data.Map as M
import           Data.Vector.Storable as VS hiding (foldr, map)
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.Chart.Easy hiding (Matrix, Vector)
import           LAUtil.LabelledMatrix
import           Numeric.LinearAlgebra

type RRPlot = EC (Layout R R) (PlotPoints R R)
type Coordinate = (R, R)
type CoordinateList = [Coordinate]
type PlotSpec = (String, CoordinateList)

partitionIndices :: Ord a => [a] -> M.Map a [Int]
partitionIndices xs = foldr f M.empty (zip [0..] xs)
    where f (i, x) m = M.alter (\mb -> case mb of Nothing -> Just [i]; Just is -> Just (i : is)) x m

plotSpecs :: LabelledMatrix -> Int -> Int -> [PlotSpec]
plotSpecs LabelledMatrix{..} xColumnIndex yColumnIndex =
    let columns = toColumns _values
        column0 = columns !! xColumnIndex
        column1 = columns !! yColumnIndex
        labelIds = VU.toList _labelIds
        partitions = M.toList $ partitionIndices labelIds
    in (flip map) partitions $ \(labelId, indices) ->
        let subseries = foldr f [] indices
                          where f i cs = let c = ((VS.!) column0 i, (VS.!) column1 i) in c : cs
            Just labelText = M.lookup labelId _labelMap
        in (labelText, subseries)

plots :: LabelledMatrix -> Int -> Int -> [RRPlot]
plots m xColumnIndex yColumnIndex = map (uncurry points) (plotSpecs m xColumnIndex yColumnIndex)
