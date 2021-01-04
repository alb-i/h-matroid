{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Test.Matroid.Generators
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module contains QuickCheck generators for testing purposes.

-}

module Test.Matroid.Generators where

import Data.Matroid
import Data.Matroid.Internal
import Test.QuickCheck


import Data.Set (Set)
import qualified Data.Set as S


-- | a generator for uniform matroids of a reasonable size
genUniformMatroids :: Gen (UniformMatroid Int)
genUniformMatroids = do r <- (arbitrary :: Gen Int) `suchThat` (>= 0) `suchThat` (<= 25) 
                        n <- (arbitrary :: Gen Int) `suchThat` (>= r) `suchThat` (<= 100)
                        return $ uniform n r
                          
-- | a generator for free matroids of a reasonable size
genFreeMatroids :: Gen (FreeMatroid Int)
genFreeMatroids = do n <- (arbitrary :: Gen Int) `suchThat` (<= 30)
                     return $ freeOn $ S.fromList [1..n]
                       
-- | a generator for consintency matroid type based on another generator
viaRank :: Matroid m a => Gen (m a) -> Gen (RkMatroid a)
viaRank g = do
                    m_ <- g
                    return $ fromRk (groundset m_) (rk m_)
                    
-- | a generator for consintency matroid type based on another generator
viaIndep :: Matroid m a => Gen (m a) -> Gen (IndepMatroid a)
viaIndep g = do
                    m_ <- g
                    return $ fromIndep (groundset m_) (indep m_)
-- | a generator for consintency matroid type based on another generator

viaBasisFilter :: Matroid m a => Gen (m a) -> Gen (BasisFilterMatroid a)
viaBasisFilter g = do
                    m_ <- g
                    return $ fromBasisFilter (groundset m_) (basis m_)