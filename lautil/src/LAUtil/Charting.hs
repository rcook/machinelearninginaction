module LAUtil.Charting (plot) where

import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy hiding (plot)
import qualified Graphics.Rendering.Chart.Easy as CE
import           LAUtil.ScatterPlot

--plot :: [RRPlot l] -> IO ()
plot ps = toFile def "example.svg" $ do
    layout_title .= "Categories"
    mapM_ CE.plot ps
