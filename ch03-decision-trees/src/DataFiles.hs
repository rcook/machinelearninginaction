module DataFiles (getDataFileName) where

import qualified Paths_ch03_decision_trees as P

getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
