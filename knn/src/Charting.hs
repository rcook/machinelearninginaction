module Charting (plot') where

import           Control.Monad
import           Graphics.Rendering.Chart.Backend.Diagrams
import           Graphics.Rendering.Chart.Easy
import           LAUtil hiding (plot)

--plot' :: [RRPlot l] -> IO ()
plot' ps = toFile def "example.svg" $ do
    layout_title .= "Categories"
    mapM_ plot ps
