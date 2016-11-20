{-# LANGUAGE RecordWildCards #-}

module LabelledMatrixSpec
    ( main
    , spec
    ) where

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as VU
import           MLUtil
import           Test.Hspec

m :: Matrix R
m = matrix 3
    [ 1.0, 2.0, 3.0
    , 4.0, 5.0, 100.0
    , 6.0, 7.0, 8.0
    , 9.0, 10.0, 11.0
    ]

spec :: Spec
spec = do
    describe "mkLabelledMatrix" $ do
        it "matrix and labels match expected" $ do
            let input =
                  [ "1.0 2.0 3.0 label0"
                  , "4.0 5.0 100.0 label1"
                  , "6.0 7.0 8.0 label0"
                  , "9.0 10.0 11.0 label0"
                  ]
                Just LabelledMatrix{..} = mkLabelledMatrix input
            lmValues `shouldBe` m
            lmLabelIds `shouldBe` VU.fromList [0, 1, 0, 0]
            lmLabelIdMap `shouldBe` M.fromList [("label0", 0), ("label1", 1)]
            lmLabelMap `shouldBe` M.fromList [(0, "label0"), (1, "label1")]

main :: IO ()
main = hspec spec
