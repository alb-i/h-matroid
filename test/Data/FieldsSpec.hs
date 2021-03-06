{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.FieldsSpec (main, spec) where

import Data.Fields.Zp
import Test.Hspec
import Test.QuickCheck

import Test.Fields

-- | the main routine
main :: IO ()
main = hspec spec

-- | all the tests
spec :: Spec
spec = do
    describe "Z/Z141650939" $ do fieldPropertiesSuite ((do x <- chooseInt(0,141650938)
                                                           return $ prjModP 141650939 x) :: Gen XModP)
    describe "Z/Z4001" $ do fieldPropertiesSuite ((do x <- chooseInt(0,4000)
                                                      return $ prjModP 4001 x) :: Gen XModP)
    describe "Z/Z4003" $ do fieldPropertiesSuite ((do x <- chooseInt(0,4002)
                                                      return $ prjModP 4003 x) :: Gen XModP)
    describe "Z/Z13" $ do fieldPropertiesSuite ((do x <- chooseInt(0,4002)
                                                    return $ prjModP 13 x) :: Gen XModP)
    describe "Rational" $ do fieldPropertiesSuite (arbitrary :: Gen Rational)
