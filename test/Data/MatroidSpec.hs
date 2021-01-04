{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.MatroidSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Matroid
import Data.Matroid.Uniform
import Data.Matroid.Internal (RkMatroid, BasisFilterMatroid, IndepMatroid)

import Test.Matroid
import Test.Matroid.Helpers

import Control.Exception (evaluate)

import Data.Set (Set)
import qualified Data.Set as S
                    


-- | tests whether show returns a non-empty string
test_show_nonempty :: Show a => Gen a -> SpecWith ()
test_show_nonempty g = 
  it "show is implemented and non-empty" $ property $ do 
    x <- g
    return $ 0 /= (length $ show x)

-- | tests for sanity of == and <
test_eq_ord :: (Eq a, Ord a) => Gen a -> SpecWith ()
test_eq_ord g = do
    it "== is implemented" $ property $ do 
      x <- g
      y <- g
      
      return $ x == y `seq` x == x
    it "compare is implemented" $ property $ do 
        x <- g
        y <- g
        let cmp = compare x y
            result
               | cmp == EQ = x == y
               | otherwise = x /= y
         in return $ result
      

-- | matroid + Show test
matroid_suite g = do
  matroidSuite g
  test_show_nonempty g
         
-- | matroid + Eq/Ord sanity tests
matroid_suite_eq_ord g = do
  matroid_suite g
  test_eq_ord g
  
-- | the main routine
main :: IO ()
main = hspec spec

-- | all the tests
spec :: Spec
spec = do
    describe "Data.Matroid.Uniform.uniform" $ do
        matroid_suite_eq_ord genUniformMatroids
        it "wrong arguments should produce errors" $ do
            evaluate (uniform (-1) 0) `shouldThrow` anyErrorCall
            evaluate (uniform 0 (-1)) `shouldThrow` anyErrorCall
            evaluate (uniform 0 1) `shouldThrow` anyErrorCall
          
    describe "Data.Matroid.Uniform.freeOn" $ matroid_suite_eq_ord genFreeMatroids
    describe "Data.Matroid.fromRk" $ matroid_suite $ viaRank genUniformMatroids
    describe "Data.Matroid.fromIndep" $ matroid_suite $ viaIndep genUniformMatroids
    describe "Data.Matroid.fromBasisFilter" $ matroid_suite $ viaBasisFilter genUniformMatroids
