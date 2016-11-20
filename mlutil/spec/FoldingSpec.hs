module FoldingSpec
    ( main
    , spec
    ) where

import           Control.Exception
import           MLUtil
import           Test.Hspec

m :: Matrix R
m = matrix 3 [1.0, 2.0, 3.0, 4.0, 5.0, 100.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]

spec :: Spec
spec = do
    describe "columnHead" $ do
        it "reads column head 0" $
            columnHead m 0 `shouldBe` 1.0
        it "reads column head 1" $
            columnHead m 1 `shouldBe` 2.0
        it "reads column head 2" $
            columnHead m 2 `shouldBe` 3.0
        it "fails if out of bounds" $
            evaluate (columnHead m 3) `shouldThrow` errorCall "matrix indexing out of range"

    describe "foldColumn" $ do
        it "folds column 0" $
            foldColumn (+) 0.0 m 0 `shouldBe` 20.0
        it "folds column 1" $
            foldColumn (+) 0.0 m 1 `shouldBe` 24.0
        it "folds column 2" $
            foldColumn (+) 0.0 m 2 `shouldBe` 122.0
        it "fails if out of bounds" $
            evaluate (foldColumn (+) 0.0 m 3) `shouldThrow` errorCall "matrix indexing out of range"

main :: IO ()
main = hspec spec
