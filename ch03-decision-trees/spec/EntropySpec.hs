{-# LANGUAGE RecordWildCards #-}

module EntropySpec
    ( main
    , spec
    ) where

import           Ch03DecisionTrees.Entropy
import           MLUtil.Test
import           Test.Hspec

data Record = R Int Int String deriving Show
instance Labelled Record where
    label (R _ _ s) = s

spec :: Spec
spec = do
    describe "calculateShannonEntropy" $ do
        it "should return correct value" $
            let dataSet =
                    [ R 1 1 "yes"
                    , R 1 1 "yes"
                    , R 1 0 "no"
                    , R 0 1 "no"
                    , R 0 1 "no"
                    ]
            in calculateShannonEntropy dataSet `shouldRoundTo` 0.97095

main :: IO ()
main = hspec spec
