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


import qualified Data.Set as S


-- | a generator for uniform matroids of a reasonable size
genUniformMatroids :: Gen (UniformMatroid Int)
genUniformMatroids = do r <- chooseInt (0,25)
                        n <- chooseInt (r,100)
                        return $ uniform n r

-- | a generator for uniform matroids of small size
genSmallUniformMatroids :: Gen (UniformMatroid Int)
genSmallUniformMatroids = do r <- chooseInt (0,6)
                             n <- chooseInt (r,15)
                             return $ uniform n r


-- | a generator for graphic matroids of reasonable size
genGraphicMatroids :: Gen (GraphicMatroid Int (Int,Int,Int))
genGraphicMatroids =  do v_ <- chooseInt (1,8)
                         n_ <- chooseInt (0,100)
                         let genEdges v n -- :: Gen [(Int,Int,Int)]
                                    | n == 0 = return []
                                    | otherwise = do s <- chooseInt (1,v)
                                                     t <- chooseInt (1,v) `suchThat` (>= s)
                                                     gs <- genEdges v (n-1)
                                                     return $ (n,s,t) : gs
                           in do
                               labeledEdges <- genEdges v_ n_
                               let inc (_,a,b) = (a,b)
                                in return $ fromGraph (S.fromList labeledEdges) inc

-- | a generator for graphic matroids of small size
genSmallGraphicMatroids :: Gen (GraphicMatroid Int (Int,Int,Int))
genSmallGraphicMatroids =
                      do v_ <- chooseInt (1,8)
                         n_ <- chooseInt (0,20)
                         let genEdges v n -- :: Gen [(Int,Int,Int)]
                                    | n == 0 = return []
                                    | otherwise = do s <- chooseInt (1,v)
                                                     t <- chooseInt (1,v) `suchThat` (>= s)
                                                     gs <- genEdges v (n-1)
                                                     return $ (n,s,t) : gs
                           in do
                               labeledEdges <- genEdges v_ n_
                               let inc (_,a,b) = (a,b)
                                in return $ fromGraph (S.fromList labeledEdges) inc


-- | a generator for M(K_n) matroids of reasonable size
genMKnMatroids :: Gen (GraphicMatroid Int (Int,Int))
genMKnMatroids = do n <- chooseInt(1,8)
                    return $ mK n


-- | a generator for free matroids of a reasonable size
genFreeMatroids :: Gen (FreeMatroid Int)
genFreeMatroids = do n <- chooseInt (0,10) --(arbitrary :: Gen Int) `suchThat` (<= 30)
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

-- | a generator for matroids of the form M|X based on a generator for M
viaRestriction :: Matroid m a => Gen (m a) -> Gen (AMatroid a)
viaRestriction g = do
                    m_ <- g
                    e0 <- sublistOf $ S.toList $ groundset m_
                    let e = S.fromList e0
                     in return $ m_ `restriction` e

-- | a generator for matroids of the form M.X based on a generator for M
viaContraction :: Matroid m a => Gen (m a) -> Gen (AMatroid a)
viaContraction g = do
                    m_ <- g
                    e0 <- sublistOf $ S.toList $ groundset m_
                    let e = S.fromList e0
                     in return $ m_ `contraction` e

-- | a generator for matroids of the form M^* based on a generator for M
viaDual :: Matroid m a => Gen (m a) -> Gen (AMatroid a)
viaDual g = do
                dual <$> g