{-# LANGUAGE RecordWildCards #-}

module LAUtil.ScatterPlot
  ( Input (..)
  , RRPlot
  , plots
  ) where

import qualified Data.Map as M
import           Data.Vector.Storable as VS hiding (foldr, map)
import qualified Data.Vector.Unboxed as VU
import           Graphics.Rendering.Chart.Easy hiding (Matrix, Vector)
import           LAUtil.LabelledMatrix
import           Numeric.LinearAlgebra

type RRPlot l = EC l (PlotPoints R R)
type Coordinate = (R, R)
type CoordinateList = [Coordinate]
type PlotSpec = (String, CoordinateList)

data Input = Input
  { _xvalues :: Matrix R
  , _xlabelIds :: VU.Vector LabelId
  , _xlabels :: M.Map LabelId String
  } deriving Show

partitionIndices :: Ord a => [a] -> M.Map a [Int]
partitionIndices xs = foldr f M.empty (zip [0..] xs)
    where f (i, x) m = M.alter (\mb -> case mb of Nothing -> Just [i]; Just is -> Just (i : is)) x m

plotSpecs :: Input -> [PlotSpec]
plotSpecs Input{..} =
    let columns = toColumns _xvalues
        column0 = columns !! 0
        column1 = columns !! 1
        labelIds = VU.toList _xlabelIds
        partitions = M.toList $ partitionIndices labelIds
    in (flip map) partitions $ \(labelId, indices) ->
        let subseries = foldr f [] indices
                          where f i cs = let c = ((VS.!) column0 i, (VS.!) column1 i) in c : cs
            Just labelText = M.lookup labelId _xlabels
        in (labelText, subseries)

plots :: Input -> [RRPlot l]
plots = map (uncurry points) . plotSpecs
