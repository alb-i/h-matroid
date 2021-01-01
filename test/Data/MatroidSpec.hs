{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.MatroidSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Matroid
import Data.Matroid.Uniform
import Data.Matroid.Internal.Tests

import Data.Set (Set)
import qualified Data.Set as S


{-

This is a todo list of things that could be tested:

  
  -  indep X  &&  Y\subseteq X => indep Y (hereditary)
  -  indep \emptyset
  -  indep X && indep Y && |Y| > |X| => exists y\in Y: indep X+{y} (exchange property)

  -  basis X \subseteq X
  -  indep (basis X)
  -  y in X\(basis X) => not indep (basis X)+{y}
  
  -  cl is monotone
  -  cl(X) \subseteq E
  -  cl(cl(X)) == cl(X) (idempotence)
  -  e \in E\X, y\in cl(X+{e})\cl(X) => e \in cl(X+{y})

-}


-- | a generator for uniform matroids of a reasonable size
gen_uniform_matroids :: Gen (UniformMatroid Int)
gen_uniform_matroids = do r <- (arbitrary :: Gen Int) `suchThat` (>= 0) `suchThat` (<= 25) 
                          n <- (arbitrary :: Gen Int) `suchThat` (>= r) `suchThat` (<= 100)
                          return $ uniform n r
                        
  
                          
{- | test suite for rank axioms
 
 The following properties are verified:

  -  rk is monotone increasing
  -  rk(X+{x}) <= rk(X) + 1  (unit increasing)
  
  -  rk(\emptyset) = 0 (important special case)
  -  rk(X) <= |X| (subcardinal)

  -  rk(A/\B) + rk(A\/B) <= rk(A) + rk(B) (submodular)
 
-}
rk_properties_suite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
rk_properties_suite genMatroids = context "rk properties" $ do
   it "rk is monotone increasing" $ property $ do
    m <- genMatroids
    e <- shuffle $ S.toList $ groundset m
    return $ is_monotone_unit_increasing (rk m) e
   it "rk({}) == 0" $ property $ do
     m <- genMatroids
     return $ rk m S.empty == 0
   it "rk is subcardinal" $ property $ do
      m <- genMatroids
      x <- sublistOf $ S.toList $ groundset m
      return $ (rk m) (S.fromList x) <= length x
   it "rk is submodular" $ property $ do
      m <- genMatroids
      a0 <- sublistOf $ S.toList $ groundset m
      b0 <- sublistOf $ S.toList $ groundset m
      let a = S.fromList a0
          b = S.fromList b0
        in return $ (rk m (a `S.union` b)) + (rk m (a `S.intersection` b)) <= (rk m a) + (rk m b)
  
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Data.Matroid.Uniform.uniform" $ do
      rk_properties_suite gen_uniform_matroids

      
      

{-
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Data.Matroid.Uniform.uniform" $ do
  it "test" $ True `shouldBe` True
  -}