{-# LANGUAGE TypeSynonymInstances #-}

module MLUtil.Roundable
  ( Roundable (..)
  ) where

import qualified Data.Vector.Storable as VS
import           MLUtil.Imports

defaultPrecision :: Int
defaultPrecision = 5

class Roundable a where
    roundToDefaultPrecision :: a -> a

instance Roundable R where
    -- http://stackoverflow.com/questions/12450501/round-number-to-specified-number-of-digits
    roundToDefaultPrecision x = (fromInteger $ round $ x * (10 ^ n)) / (10.0 ^^ n)
        where n = defaultPrecision

instance (Roundable a, VS.Storable a) => Roundable (Vector a) where
    roundToDefaultPrecision = VS.map roundToDefaultPrecision
