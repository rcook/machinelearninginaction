{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main (main) where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

diagram :: Diagram B
diagram = strokeT (hilbert 6) # lc silver
                              # opacity 0.3

main :: IO ()
main = mainWith diagram
