module SortingSpec
    ( main
    , spec
    ) where

import qualified Data.Vector.Unboxed as VU
import           MLUtil
import           Test.Hspec

spec :: Spec
spec = do
    describe "argSort" $ do
        it "should return ordered indices" $
            argSort (vector [40.0, 30.0, 10.0, 11.0]) `shouldBe` VU.fromList [2, 3, 1, 0]

main :: IO ()
main = hspec spec
