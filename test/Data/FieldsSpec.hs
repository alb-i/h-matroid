{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.FieldsSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Test.Fields

-- | the main routine
main :: IO ()
main = hspec spec

-- | all the tests
spec :: Spec
spec = do
    describe "Rational" $ do fieldPropertiesSuite (arbitrary :: Gen Rational)
