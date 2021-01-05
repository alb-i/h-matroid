{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Test.Matroid.Suites
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module contains hspec test suites that check certain matroid properties.

-}

module Test.Matroid.Suites where
    
import Test.Matroid.Generators
import Test.Matroid.Helpers
import Test.QuickCheck
import Test.Hspec

import Data.Matroid


import Data.Set (Set)
import qualified Data.Set as S

-- | all tests that any matroid should/must pass
matroidSuite :: (Matroid m a, Show (m a)) => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
matroidSuite g = do
  rkPropertiesSuite g
  indepPropertiesSuite g
  basisPropertiesSuite g
  clPropertiesSuite g
  viaConsistencySuite g


{- | test suite for rank axioms 

 The following properties are verified:
  -  rk is monotone increasing
  -  rk(X+{x}) <= rk(X) + 1  (unit increasing)
  -  rk(\emptyset) = 0 (important special case)
  -  rk(X) <= |X| (subcardinal)
  -  rk(A/\B) + rk(A\/B) <= rk(A) + rk(B) (submodular)
-}
rkPropertiesSuite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
rkPropertiesSuite genMatroids = context "rk properties" $ do
   it "rk is monotone increasing" $ property $ do
    m <- genMatroids
    e <- shuffle $ S.toList $ groundset m
    return $ isMonotoneUnitIncreasing (rk m) e
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
indepPropertiesSuite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
indepPropertiesSuite genMatroids = context "indep properties" $ do
  it "indep is hereditary" $ property $ do
    m <- genMatroids
    e <- shuffle $ S.toList $ groundset m
    return $ isMonotoneDecreasingBool (indep m) e
  it "indep({}) == True" $ property $ do
    m <- genMatroids
    return $ indep m S.empty == True
  it "indep obeys the exchange property" $ property $ do
    m <- genMatroids
    x0 <- sublistOf $ S.toList $ groundset m
    y0 <- sublistOf $ S.toList $ groundset m
    let x1 = basis m $ S.fromList x0
        y1 = basis m $ S.fromList y0
      in return $ hasIndepExchangeProperty (indep m) x1 y1
      
{- | test suite for basis properties

The following properties are verified:
  -  basis X \subseteq X
  -  indep (basis X)
  -  y in X\(basis X) => not indep (basis X)+{y}

-}
basisPropertiesSuite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
basisPropertiesSuite genMatroids = context "basis properties" $ do
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
clPropertiesSuite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
clPropertiesSuite genMatroids = context "cl properties" $ do
  it "cl is extensive" $ property $ do
    m <- genMatroids
    e <- shuffle $ S.toList $ groundset m
    return $ isIsotoneSetMap (cl m) e
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
      
{- | tests a given matroid implementation for equality against the default implementations
    
  rk, indep, and cl should produce the same output when given the same input.
  
  basis however is only required to produce a basis of the given subset, and which basis is
  produced really depends on how the basis is derived. Here, we are ok if b1 and b2 have
    cl1(b1) == cl1(b2) == cl2(b1) == cl2(b2).
-}
viaConsistencySuite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
viaConsistencySuite genMatroids = context "implementation consistency" $ do
  let via_rk m_ = fromRk (groundset m_) (rk m_)
      via_indep m_ = fromIndep (groundset m_) (indep m_)
      via_basis m_ = fromBasisFilter (groundset m_) (basis m_)
      check_eq via_x fn1 fn2 = -- fn1 and fn2 are the same function but wrt. different types
       property $ do 
        m1 <- genMatroids
        x0 <- sublistOf $ S.toList $ groundset m1
        let m2 = via_x m1
            x = S.fromList x0
          in return $ fn1 m1 x == fn2 m2 x
      check_basis via_x =
       property $ do
        m1 <- genMatroids
        x0 <- sublistOf $ S.toList $ groundset m1
        let m2 = via_x m1
            x = S.fromList x0
            b1 = basis m1 x
            b2 = basis m2 x
            c1b1 = cl m1 b1
            c1b2 = cl m1 b2
            c2b1 = cl m2 b1
            c2b2 = cl m2 b2
          in return $ (c1b1 == c1b2) && (c1b1 == c2b1) && (c1b1 == c2b2)
    in do
      it "indep wrt. rk matroid" $  check_eq via_rk indep indep
      it "cl wrt. rk matroid" $  check_eq via_rk cl cl
      it "basis wrt. rk matroid" $  check_basis via_rk
      it "rk wrt. indep matroid" $  check_eq via_indep rk rk
      it "cl wrt. indep matroid" $  check_eq via_indep cl cl
      it "basis wrt. indep matroid" $  check_basis via_indep
      it "indep wrt. basis matroid" $  check_eq via_basis indep indep
      it "rk wrt. basis matroid" $  check_eq via_basis rk rk
      it "cl wrt. basis matroid" $  check_eq via_basis cl cl
