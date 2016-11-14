module LAUtil.Charting
  ( plot
  ) where

import           Control.Exception
import           Control.Monad
import qualified Graphics.EasyPlot as EP

data ChartingException = ChartingException String deriving Show
instance Exception ChartingException

plot :: EP.Plot a => a -> IO ()
plot p = do
    let tt = EP.PNG "plot.png"
    status <- EP.plot tt p
    unless status (throwIO $ ChartingException "Graphics.EasyPlot.plot failed")
