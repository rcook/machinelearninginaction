{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad
import qualified Data.Map as M
import           Data.Vector.Storable as VS hiding (foldr, forM_, map, mapM_)
import qualified Graphics.Rendering.Chart.Backend.Diagrams as D
import           Graphics.Rendering.Chart.Easy hiding (Matrix, Vector)
import           Numeric.LinearAlgebra

type RRPlot l = EC l (PlotPoints R R)
type Coordinate = (R, R)
type CoordinateList = [Coordinate]
type PlotSpec = (String, CoordinateList)

data Input = Input
  { _values :: Matrix R
  , _labelIds :: Vector Z
  , _labels :: M.Map Z String
  } deriving Show

input :: Input
input = Input
    (matrix 2 [1.0, 1.1, 2.0, 2.1, 3.0, 3.1, 4.0, 4.1, 5.0, 5.1, 6.0, 6.1])
    (fromList [10, 10, 20, 20, 10, 30])
    (M.fromList [(10, "ten"), (20, "twenty"), (30, "thirty")])

partitionIndices :: [Z] -> M.Map Z [Int]
partitionIndices xs = foldr f M.empty (zip [0..] xs)
    where f (i, x) m = M.alter (\mb -> case mb of Nothing -> Just [i]; Just is -> Just (i : is)) x m

plotSpecs :: Input -> [PlotSpec]
plotSpecs Input{..} =
    let columns = toColumns _values
        column0 = columns !! 0
        column1 = columns !! 1
        labelIds = VS.toList _labelIds
        partitions = M.toList $ partitionIndices labelIds
    in (flip map) partitions $ \(labelId, indices) ->
        let subseries = foldr f [] indices
                          where f i cs = let c = ((VS.!) column0 i, (VS.!) column1 i) in c : cs
            Just labelText = M.lookup labelId _labels
        in (labelText, subseries)

plots :: Input -> [RRPlot l]
plots = map (uncurry points) . plotSpecs

main :: IO ()
main = D.toFile def "example.svg" $ do
    layout_title .= "Categories"
    mapM_ plot (plots input)
