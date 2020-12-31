{-|
Module      : Data.Matroid
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides type classes and functionalities that allow you to work with matroids and related structures.

A [matroid](https://en.wikipedia.org/wiki/Matroid) is also called a combinatorial pre-geometry and is a structure 
that abstracts (linear) dependence. Matroids occur naturally in settings where the greedy algorithm works, although 
they usually stay under the radar.

-}
module Data.Matroid where

import Data.Matroid.Internal

import Data.Set (Set)
import qualified Data.Set as S


{-| 
    Typeclass that provides the full matroid interface.
    
    The type parameter @a@ is the type of the elements of the matroid,
    it is most commonly used in a @Set a@ type.
     
-}
class Ord a => Matroid a
    where
    -- | The ground set of the matroid, its elements are usually called edges. This set is finite.
    groundset :: Set a
    
    -- | returns the rank of the set
    rk ::
         Set a -- ^ set of matroid elements
      -> Int
    rk = length . basis
    
    -- | tests whether a given set is independent
    indep :: 
         Set a -- ^ set of matroid elements
      -> Bool
    indep x = rk x == length x -- a set is independent iff its rank equals its cardinality
    
    -- | obtains an independent subset with maximal cardinality
    basis :: 
         Set a -- ^ set of matroid elements
      -> Set a
    basis = S.foldl' augmentIndep (S.empty :: Set a)
         where 
             augmentIndep b0 x {- adds x to b0 if b0 + {x} is independent -}
               | indep b_aug = b_aug
               | otherwise = b0
               where 
                   b_aug = S.insert x b0
                   
    -- | computes the closure of a given set
    cl :: 
      Set a -- ^ set of matroid elements 
      -> Set a
    cl x = S.foldl' augmentDep x groundset
        where 
           rank_x = rk x
           augmentDep f0 e {- adds e to f0 if the rank of f0+{e} stays the same -}
             | rk f_aug == rank_x = f_aug
             | otherwise = f0
             where 
               f_aug = S.insert e f0
      
    {-# MINIMAL groundset, (rk | indep | basis) #-}