module RoundableSpec
    ( main
    , spec
    ) where

import           MLUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "roundToPrecision" $ do
        it "should round 0.123456 to 0.12346" $
            roundToPrecision 5 0.123456 `shouldBe` (0.12346 :: R)
        it "should round 0.123455 to 0.12346" $
            roundToPrecision 5 0.123455 `shouldBe` (0.12346 :: R)
        it "should round 0.123454 to 0.12345" $
            roundToPrecision 5 0.123454 `shouldBe` (0.12345 :: R)

        it "should round [0.123456, 0.123455, 0.123454] correctly" $
            roundToPrecision 5 (vector [0.123456, 0.123455, 0.123454])
                `shouldBe` vector [0.12346, 0.12346, 0.12345]

        it "should round [[0.123456, 0.123455], [0.123454, 0.123453]] correctly" $
            roundToPrecision 5 (matrix 2 [0.123456, 0.123455, 0.123454, 0.123453])
                `shouldBe` matrix 2 [0.12346, 0.12346, 0.12345, 0.12345]

main :: IO ()
main = hspec spec
