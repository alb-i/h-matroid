{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Data.MatroidSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Matroid
import Data.Matroid.Uniform
import Data.Matroid.Internal (RkMatroid, BasisFilterMatroid, IndepMatroid)
import TestHelpers
import Control.Exception (evaluate)

import Data.Set (Set)
import qualified Data.Set as S

-- | a generator for uniform matroids of a reasonable size
gen_uniform_matroids :: Gen (UniformMatroid Int)
gen_uniform_matroids = do r <- (arbitrary :: Gen Int) `suchThat` (>= 0) `suchThat` (<= 25) 
                          n <- (arbitrary :: Gen Int) `suchThat` (>= r) `suchThat` (<= 100)
                          return $ uniform n r
                          
-- | a generator for free matroids of a reasonable size
gen_free_matroids :: Gen (FreeMatroid Int)
gen_free_matroids = do n <- (arbitrary :: Gen Int) `suchThat` (<= 30)
                       return $ freeOn $ S.fromList [1..n]
                       
-- | a generator for consintency matroid type based on another generator
gen_via_rank :: Matroid m a => Gen (m a) -> Gen (RkMatroid a)
gen_via_rank g = do
                    m_ <- g
                    return $ fromRk (groundset m_) (rk m_)
-- | a generator for consintency matroid type based on another generator
gen_via_indep :: Matroid m a => Gen (m a) -> Gen (IndepMatroid a)
gen_via_indep g = do
                    m_ <- g
                    return $ fromIndep (groundset m_) (indep m_)
-- | a generator for consintency matroid type based on another generator
gen_via_basis :: Matroid m a => Gen (m a) -> Gen (BasisFilterMatroid a)
gen_via_basis g = do
                    m_ <- g
                    return $ fromBasisFilter (groundset m_) (basis m_)

                    
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
      
  {- | tests rk,indep,basis,cl for equality against the default implementations wrt. rk,indep,basis
    
  rk, indep, and cl should produce the same output when given the same input.
  
  basis however is only required to produce a basis of the given subset, and which basis is
  produced really depends on how the basis is derived. Here, we are ok if b1 and b2 have
    cl1(b1) == cl1(b2) == cl2(b1) == cl2(b2) [technically, we are overchecking here.]
  
   -}
consistency_suite :: Matroid m a => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
consistency_suite genMatroids = context "implementation consistency" $ do
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
      
-- | all tests that any matroid should/must pass
matroid_suite :: (Matroid m a, Show (m a)) => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
matroid_suite g = do
  test_show_nonempty g
  rk_properties_suite g
  indep_properties_suite g
  basis_properties_suite g
  cl_properties_suite g
  consistency_suite g

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
        matroid_suite_eq_ord gen_uniform_matroids
        it "wrong arguments should produce errors" $ do
            evaluate (uniform (-1) 0) `shouldThrow` anyErrorCall
            evaluate (uniform 0 (-1)) `shouldThrow` anyErrorCall
            evaluate (uniform 0 1) `shouldThrow` anyErrorCall
          
    describe "Data.Matroid.Uniform.freeOn" $ matroid_suite_eq_ord gen_free_matroids
    describe "Data.Matroid.fromRk" $ matroid_suite $ gen_via_rank gen_uniform_matroids
    describe "Data.Matroid.fromIndep" $ matroid_suite $ gen_via_indep gen_uniform_matroids
    describe "Data.Matroid.fromBasisFilter" $ matroid_suite $ gen_via_basis gen_uniform_matroids
