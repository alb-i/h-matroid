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
