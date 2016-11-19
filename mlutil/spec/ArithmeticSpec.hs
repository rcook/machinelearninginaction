module ArithmeticSpec
  ( main
  , spec
  ) where

import           MLUtil
import           Test.Hspec

m :: Matrix R
m = matrix 3 [1.0, 2.0, 3.0, 4.0, 5.0, 100.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0]

spec :: Spec
spec = do
    describe "sumColumns" $ do
        it "sums columns" $ do
            sumColumns m `shouldBe` matrix 3 [20.0, 24.0, 122.0]

    describe "sumRows" $ do
        it "sums rows" $ do
            sumRows m `shouldBe` matrix 4 [6.0, 109.0, 21.0, 30.0]

main :: IO ()
main = hspec spec
