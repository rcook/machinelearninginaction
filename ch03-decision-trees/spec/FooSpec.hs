{-# LANGUAGE RecordWildCards #-}

module FooSpec
    ( main
    , spec
    ) where

import           Ch03DecisionTrees.Foo
import           DataFiles
import           Test.Hspec

spec :: Spec
spec = do
    describe "foo" $ do
        it "should foo" $ do
            x <- getDataFileName "foo"
            foo

main :: IO ()
main = hspec spec
