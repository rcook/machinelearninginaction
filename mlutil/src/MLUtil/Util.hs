module MLUtil.Util
  ( forFold
  , forFoldM
  ) where

import           Control.Monad

forFold :: Foldable t => b -> t a -> (a -> b -> b) -> b
forFold = flip . flip foldr

forFoldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
forFoldM = flip . flip foldM
