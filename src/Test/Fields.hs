{-|
Module      : Test.Fields
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module exports routines that may be used to write test cases for
implementations of fields.

It is also used in the unit tests of the matroid library.

-}

module Test.Fields where

import Data.Fields
import Test.QuickCheck
import Test.Hspec

{-| tests the field properties
-}
fieldPropertiesSuite :: Field f => Gen f {- ^ generator for field elements -} -> SpecWith ()
fieldPropertiesSuite genF = context "field properties" $ do
    it "left unit law" $ property $ do
        x <- genF
        return $ (x `mulF` unitF) `eqF` x
    it "right unit law" $ property $ do
        x <- genF
        return $ (unitF `mulF` x) `eqF` x
    it "left zero law" $ property $ do
        x <- genF
        return $ (x `addF` zeroF) `eqF` x
    it "right zero law" $ property $ do
        x <- genF
        return $ (zeroF `addF` x) `eqF` x
    it "multiplicative associativity" $ property $ do
        x <- genF
        y <- genF
        z <- genF
        return $ ((x `mulF` y) `mulF` z) `eqF` (x `mulF` (y `mulF` z))
    it "additive associativity" $ property $ do
        x <- genF
        y <- genF
        z <- genF
        return $ ((x `addF` y) `addF` z) `eqF` (x `addF` (y `addF` z))
    it "multiplicative left inverse" $ property $ do
        x <- genF
        return $ isZeroF x || unitF `eqF` (invF x `mulF` x)
    it "multiplicative right inverse" $ property $ do
        x <- genF
        return $ isZeroF x || unitF `eqF` (x `mulF` invF x)
    it "additive left inverse" $ property $ do
        x <- genF
        return $ isZeroF $ negF x `addF` x
    it "additive right inverse" $ property $ do
        x <- genF
        return $ isZeroF $ x `addF` negF x
    it "left distributivity" $  property $ do
        x <- genF
        y <- genF
        z <- genF
        let criterion = ((x `addF` y) `mulF` z) `eqF` ((x `mulF` z) `addF` (y `mulF` z))
            f True = True
            f False = error ("x = " ++ show x ++ " y = " ++ show y ++ " z = " ++ show z ++ "  L = " ++ show ((x `addF` y) `mulF` z) ++ "  R = " ++ show ((x `mulF` z) `addF` (y `mulF` z)))
        return $ f criterion

    it "right distributivity" $ property $ do
        x <- genF
        y <- genF
        z <- genF
        return $ (z `mulF` (x `addF` y)) `eqF` ((z `mulF` x) `addF` (z `mulF` y))
    it "additive commutativity" $ property $ do
        x <- genF
        y <- genF
        return $ (x `addF` y) `eqF` (y `addF` x)
    it "multiplicative commutativity" $ property $ do
        x <- genF
        y <- genF
        return $ (x `mulF` y) `eqF` (y `mulF` x)