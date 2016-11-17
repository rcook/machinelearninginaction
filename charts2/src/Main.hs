module Main (main) where

import           Control.Monad
import qualified Data.Map as M
import           Data.Vector.Storable as VS hiding (foldr, forM_, map, mapM_)
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy hiding (Matrix, Vector)
import           Numeric.LinearAlgebra

type Coordinate = (R, R)
type CoordinateList = [Coordinate]
type SeriesPlotSpec = (String, CoordinateList)

series :: Matrix R
series = matrix 2
  [ 1.0, 1.1
  , 2.0, 2.1
  , 3.0, 3.1
  , 4.0, 4.1
  , 5.0, 5.1
  , 6.0, 6.1
  ]

seriesLabelIds :: Vector Z
seriesLabelIds = fromList [10, 10, 20, 20, 10, 30]

seriesLabels :: M.Map Z String
seriesLabels = M.fromList [(10, "ten"), (20, "twenty"), (30, "thirty")]

partitionIndices :: [Z] -> M.Map Z [Int]
partitionIndices xs = foldr f M.empty (zip [0..] xs)
    where f (i, x) m = M.alter (\mb -> case mb of Nothing -> Just [i]; Just is -> Just (i : is)) x m

seriesPlotSpecs :: Matrix R -> Vector Z -> M.Map Z String -> [SeriesPlotSpec]
seriesPlotSpecs series seriesLabelIds seriesLabels =
    let columns = toColumns series
        column0 = columns !! 0
        column1 = columns !! 1
        labelIds = VS.toList seriesLabelIds
        ps = partitionIndices labelIds
    in (flip map) (M.toList ps) $ \(labelId, indices) ->
        let subseries = foldr f [] indices
                          where f i ps = let p = ((VS.!) column0 i, (VS.!) column1 i) in p : ps
            Just labelText = M.lookup labelId seriesLabels
        in (labelText, subseries)

seriesPlots :: Matrix R -> Vector Z -> M.Map Z String -> [EC l (PlotPoints R R)]
seriesPlots series seriesLabelIds seriesLabels = map
    (uncurry points)
    (seriesPlotSpecs series seriesLabelIds seriesLabels)

main :: IO ()
main = toFile def "example.svg" $ do
    layout_title .= "Amplitude Modulation"
    let ps = seriesPlots series seriesLabelIds seriesLabels
    mapM_ plot ps
