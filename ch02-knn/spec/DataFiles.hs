module DataFiles (getDataFileName) where

import qualified Paths_ch02_knn as P

getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
