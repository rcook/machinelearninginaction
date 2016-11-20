{-# LANGUAGE RecordWildCards #-}

module MLUtil.Charting
    ( ChartLabels (..)
    , defaultChartLabels
    , renderSVG
    ) where

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports
import           MLUtil.RRScatterPlot

data ChartLabels = ChartLabels
    { clTitle :: Maybe String
    , clXAxisLabel :: Maybe String
    , clYAxisLabel :: Maybe String
    } deriving Show

defaultChartLabels :: ChartLabels
defaultChartLabels = ChartLabels Nothing Nothing Nothing

renderSVG :: FilePath -> ChartLabels ->[RRScatterPlot] -> IO ()
renderSVG path ChartLabels{..} ps = toFile def path $ do
    let setMaybe p (Just x) = p .= x
        setMaybe p Nothing = return ()
    setMaybe layout_title clTitle
    setMaybe (layout_x_axis . laxis_title) clXAxisLabel
    setMaybe (layout_y_axis . laxis_title) clYAxisLabel
    mapM_ plot ps
