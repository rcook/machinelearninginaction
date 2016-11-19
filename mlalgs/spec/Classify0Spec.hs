module Classify0Spec
  ( main
  , spec
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Map as M
import           MLAlgs.Classify0
import           MLUtil
import           Test.Hspec

dataSet :: Matrix R
dataSet = matrix 2
  [ 1.0, 1.1
  , 1.0, 1.0
  , 0.0, 0.0
  , 0.0, 0.1
  ]

labelIds :: VU.Vector LabelId
labelIds = VU.fromList
  [ 1
  , 1
  , 2
  , 2
  ]

labels :: M.Map LabelId String
labels = M.fromList [(1, "A"), (2, "B")]

k :: Int
k = 3

spec :: Spec
spec = do
    describe "classify0" $ do
        it "should correctly classify" $ do
            classify0 (row [0.0, 0.0]) dataSet labelIds k `shouldBe` 2
            classify0 (row [1.0, 1.2]) dataSet labelIds k `shouldBe` 1

main :: IO ()
main = hspec spec
