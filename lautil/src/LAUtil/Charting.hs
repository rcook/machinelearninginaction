module LAUtil.Charting (renderSVG) where

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           LAUtil.ScatterPlot

renderSVG :: String -> FilePath -> [RRPlot] -> IO ()
renderSVG title path ps = toFile def path $ do
    layout_title .= title
    mapM_ plot ps
