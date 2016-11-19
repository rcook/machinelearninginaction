module Classify0Spec
  ( main
  , spec
  ) where

import           Test.Hspec

spec :: Spec
spec = do
    describe "roundToPrecision" $ do
        it "should round 0.123456 to 0.12346" $
            0.0 `shouldBe` 1.0

main :: IO ()
main = hspec spec
