module MLUtil.Charting (renderSVG) where

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           MLUtil.Imports
import           MLUtil.RRScatterPlot

renderSVG :: String -> FilePath -> [RRScatterPlot] -> IO ()
renderSVG title path ps = toFile def path $ do
    layout_title .= title
    mapM_ plot ps
