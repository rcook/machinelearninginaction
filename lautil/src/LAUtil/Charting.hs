{-# LANGUAGE CPP #-}

module LAUtil.Charting
  ( EP.Color (..)
  , EP.Graph2D (..)
  , EP.Option (..)
  , EP.Plot
  , EP.Style (..)
  , plot
  ) where

import           Control.Exception
import           Control.Monad
import qualified Graphics.EasyPlot as EP
import           System.Directory
import           System.IO.Temp
import           System.Process

data ChartingException = ChartingException String deriving Show
instance Exception ChartingException

plot :: EP.Plot a => FilePath -> a -> IO ()
plot dir p = withCurrentDirectory dir $ do
    (tempPath, _) <- openTempFile "." "plot.png"
    status <- EP.plot (EP.PNG tempPath) p
    unless status (throwIO $ ChartingException "Graphics.EasyPlot.plot failed")
#ifdef OS_OSX
    callCommand $ "open " ++ tempPath
#endif
