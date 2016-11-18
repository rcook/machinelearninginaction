module MLUtil.RRScatterPlot (RRScatterPlot) where

import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports

type RRScatterPlot = EC (Layout R R) (PlotPoints R R)
