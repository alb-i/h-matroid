{-# LANGUAGE MultiParamTypeClasses #-}

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
    
    In this typeclass, we assume that every set of matroid elements
    passed to any of the routines is actually a subset of (groundset m).
    Behaviour for other sets shall be considered undefined.
     
-}
class Ord a => Matroid m a 
    where
    -- | The ground set of the matroid, its elements are usually called edges. This set is finite.
    groundset :: m a -> Set a
    
    -- | returns the rank of the set
    rk :: m a -- ^ the matroid 
      -> Set a -- ^ set of matroid elements
      -> Int
    rk m = length . basis m
    
    -- | tests whether a given set is independent
    indep :: m a -- ^ the matroid 
      -> Set a -- ^ set of matroid elements
      -> Bool
    indep m x = rk m x == length x -- a set is independent iff its rank equals its cardinality
    
    -- | obtains an independent subset with maximal cardinality
    basis :: m a -- ^ the matroid
      -> Set a -- ^ set of matroid elements
      -> Set a
    basis m = S.foldl' augmentIndep (S.empty :: Set a) -- beware that I have not tested this fold as of now
         where 
             augmentIndep b0 x {- adds x to b0 if b0 + {x} is independent -}
               | indep m b_aug = b_aug
               | otherwise = b0
               where 
                   b_aug = S.insert x b0
                   
    -- | computes the closure of a given set
    cl :: m a -- ^ the matroid
      -> Set a -- ^ set of matroid elements 
      -> Set a
    cl m x = S.foldl' augmentDep x $ groundset m -- beware that I have not tested this fold as of now
        where 
           rank_x = rk m x
           augmentDep f0 e {- adds e to f0 if the rank of f0+{e} stays the same -}
             | rk m f_aug == rank_x = f_aug
             | otherwise = f0
             where 
               f_aug = S.insert e f0
      
    {-# MINIMAL groundset, (rk | indep | basis) #-}
    
-- | matroid constructor given groundset and rank function
fromRk :: Matroid m a => (Set a) {- ^ ground set -} -> (Set a -> Int) {- ^ rank function -} -> (m a)
fromRk = Data.Matroid.Internal.fromRk
