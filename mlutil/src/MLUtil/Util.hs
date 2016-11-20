module MLUtil.Util
    ( forFold
    , forFoldM
    ) where

import           Control.Monad

-- |Description of 'forFold'
--
-- Examples:
--
-- >>> forFold 0 [1, 2, 3, 4] (+)
-- 10
forFold :: Foldable t => b -> t a -> (a -> b -> b) -> b
forFold = flip . flip foldr

-- |Description of 'forFoldM'
--
-- Examples:
--
-- >>> forFoldM 0 [1, 2, 3, 4] (\acc x -> return $ acc + x)
-- 10
forFoldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
forFoldM = flip . flip foldM
