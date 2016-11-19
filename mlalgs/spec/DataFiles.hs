module DataFiles (getDataFileName) where

import qualified Paths_mlalgs as P

getDataFileName :: FilePath -> IO FilePath
getDataFileName = P.getDataFileName
