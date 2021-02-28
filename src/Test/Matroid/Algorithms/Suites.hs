{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Test.Matroid.Algorithms.Suites
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module contains hspec test suites that check certain properties of the algorithms

-}

module Test.Matroid.Algorithms.Suites where

import Data.Matroid

import Data.Matroid.Algorithms.Enumerate
import Data.Matroid.Algorithms.Greedy

import qualified Data.Set as S
import qualified Data.List as L

import Test.Matroid.Helpers
import Test.QuickCheck
import Test.Hspec

greedyOptimizationTestSuite :: (Matroid m a) => Gen (m a) {- ^ matroid test case generator -} -> SpecWith ()
greedyOptimizationTestSuite genMatroids = context "greedy algorithm" $ do
    it "Algorithms.greedy gives optimal basis" $ property $ do
      m <- genMatroids
      e <- shuffle $ S.toList $ groundset m
      let cost x = maybe ((length e) + 1) id $ L.elemIndex x e
          optimal = greedy m e
          get_cost s = S.foldr add_cost 0 s
          add_cost x c0 = c0 + (cost x)
          optimal_cost = get_cost optimal
          bases = enumerateBases m
          not_better_than_optimal s = (&&) $ (get_cost s) >= optimal_cost 
        in return $ foldr not_better_than_optimal ((indep m optimal) && (rk m optimal == rk m (groundset m))) bases
