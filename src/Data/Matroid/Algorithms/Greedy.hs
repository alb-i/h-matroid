{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Algorithms.Greedy
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of the greedy algorithm for
optimization on matroids.

-}

module Data.Matroid.Algorithms.Greedy where

import Data.Matroid.Typeclass

import Data.Set (Set)
import qualified Data.Set as S


{- |  Obtains an independent set of the given matroid
      that is optimal wrt. some optimization problem.
      
      This version uses a ranking of the benefits of the elements of the groundset
      and the indep function in order to obtain an optimal result.
-}
greedy :: Matroid m a => 
      (m a) {- ^ matroid to optimize on -} 
   -> [a]   {- ^ elements of the groundset of the matroid, ordered from the best (most revenue, least cost, ...) element to the worst yet still improving element -}
   -> Set a
greedy = greedyStep S.empty
  where greedyStep x0 m (r:rs) -- r gives the most revenue wrt. the elements not in x0
          | indep m x0r = greedyStep x0r m rs  -- we may add r to x0 and stay independent, do it
          | otherwise   = greedyStep x0  m rs  -- we cannot add r to x0, do not add it and continue
            where x0r = S.insert r x0 
        greedyStep x0 _ _ = x0 -- no more profitable elements in the queue

{- |  Obtains an independent set of the given matroid
      that is optimal wrt. some optimization problem.
      
      This version uses a choice function that selects the best
      element from a list of allowed choices, or Nothing to stop
      the process.
-}
greedy1 :: Matroid m a =>
      (m a) {- ^ matroid to optimize on -} 
   -> (Set a -> Maybe a) {- ^ picks the best element from the set, or nothing if adding an element would result in a worse outcome -}
   -> Set a
greedy1 m choice = greedyStep S.empty $ loops m
  where e = groundset m
        greedyStep x0 clx0
          | chosen == Nothing = x0     -- cannot add any further element
          | otherwise = greedyStep x0c clx0c -- add the best choice;
            where chosen = choice (S.difference e clx0)
                  Just c = chosen
                  x0c = S.insert c x0
                  clx0c = cl m $ S.insert c clx0