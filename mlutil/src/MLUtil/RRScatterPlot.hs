module MLUtil.RRScatterPlot (RRScatterPlot) where

import           Graphics.Rendering.Chart.Easy
import           Numeric.LinearAlgebra

type RRScatterPlot = EC (Layout R R) (PlotPoints R R)
