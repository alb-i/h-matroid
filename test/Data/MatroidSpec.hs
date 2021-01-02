{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.MatroidSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Matroid
import Data.Matroid.Uniform
import TestHelpers

import Data.Set (Set)
import qualified Data.Set as S

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

{- | test suite for indep properties

The following properties are verified:
  -  indep X  &&  Y\subseteq X => indep Y (hereditary)
  -  indep \emptyset
  -  indep X && indep Y && |Y| > |X| => exists y\in Y: indep X+{y} (exchange property)

-}
indep_properties_suite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
indep_properties_suite genMatroids = context "indep properties" $ do
  it "indep is hereditary" $ property $ do
    m <- genMatroids
    e <- shuffle $ S.toList $ groundset m
    return $ is_monotone_decreasing_bool (indep m) e
  it "indep({}) == True" $ property $ do
    m <- genMatroids
    return $ indep m S.empty == True
  it "indep obeys the exchange property" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    y0 <- sublistOf $ S.toList $ groundset m
    let x1 = basis m $ S.fromList x0
        y1 = basis m $ S.fromList y0
      in return $ has_exchange_property (indep m) x1 y1
      
{- | test suite for basis properties

The following properties are verified:
  -  basis X \subseteq X
  -  indep (basis X)
  -  y in X\(basis X) => not indep (basis X)+{y}

-}
basis_properties_suite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
basis_properties_suite genMatroids = context "basis properties" $ do
  it "basis(X) is a subset of X" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    let x = S.fromList x0
        b = basis m x
      in return $ b `S.isSubsetOf` x
  it "basis(X) is independent" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    let x = S.fromList x0
        b = basis m x
      in return $ indep m b
  it "basis(X) is maximally independent" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    let x = S.fromList x0
        b = basis m x
        result
          | indep m x = b == x
          | otherwise = all augNotIndep $ S.difference x b
        augNotIndep y = (==) False $ indep m $ S.insert y x
      in return $ result
{- | test suite for closure operator properties 

The following properties are verified:
  -  cl is monotone
  -  cl(X) \subseteq E
  -  cl(cl(X)) == cl(X) (idempotence)
  -  e \in E\X, y\in cl(X+{e})\cl(X) => e \in cl(X+{y})
  -  rk(X) == rk(cl(X)) and cl(X) is maximal with this property

-}
cl_properties_suite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
cl_properties_suite genMatroids = context "cl properties" $ do
  it "cl is extensive" $ property $ do
    m <- genMatroids
    e <- shuffle $ S.toList $ groundset m
    return $ is_isotone_set_map (cl m) e
  it "cl(E) == E" $ property $ do
    m <- genMatroids
    return $ cl m (groundset m) == groundset m
  it "cl is idempotent" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    let x = S.fromList x0
        c = cl m x
        cc = cl m c
     in return $ c == cc
  it "cl satisfies the exchange property" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    let x = S.fromList x0 
        cx = cl m x -- cl(X)
        es = (groundset m) `S.difference` x -- E\X
        cxy y = cl m $ S.insert y x -- cl(X+{y})
        result = all testAllExchanges es  -- binds e\in E\X
        testAllExchanges e_ = all (testExchange e_) -- binds y
           $ (cl m $ S.insert e_ x) `S.difference` cx -- cl(X+{e})\cl(X)
        testExchange e__ y_ = e__ `elem` cxy y_ -- e \in cl(X+{y})
        in return $ result
  it "rk(cl(X)) == rk(X)" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    let x = S.fromList x0 
        cx = cl m x
      in return $ rk m x == rk m cx
      
      
-- | all tests that any matroid should/must pass
matroid_suite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
matroid_suite g = do
  rk_properties_suite g
  indep_properties_suite g
  basis_properties_suite g
  cl_properties_suite g
  {- test rk,indep,basis,cl for equality against the default implementations wrt. rk,indep,basis -}

  
-- | the main routine
main :: IO ()
main = hspec spec

-- | all the tests
spec :: Spec
spec = do
    describe "Data.Matroid.Uniform.uniform" $ matroid_suite gen_uniform_matroids
