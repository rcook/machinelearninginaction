{-# LANGUAGE ImplicitParams #-}

module Expectations (shouldRoundTo) where

import           GHC.Stack
import           MLUtil
import           Test.Hspec

defaultPrecision :: Int
defaultPrecision = 5

infix 1 `shouldRoundTo`
shouldRoundTo :: (?loc :: CallStack, Show a, Eq a, Roundable a) => a -> a -> Expectation
actual `shouldRoundTo` expected = (roundToDefaultPrecision actual) `shouldBe` (roundToDefaultPrecision expected)
    where roundToDefaultPrecision = roundToPrecision defaultPrecision
