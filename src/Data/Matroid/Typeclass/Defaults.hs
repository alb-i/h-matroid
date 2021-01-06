{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Typeclass.Defaults
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides default implementations for the members of the Matroid typeclass.

-}

module Data.Matroid.Typeclass.Defaults where
    
import Data.Set (Set)
import qualified Data.Set as S
        
    
-- | returns the rank of the set, wrt. to the given basis filter
rk :: (Set a -> Set a) -- ^ basis filter of the matroid
      -> Set a -- ^ set of matroid elements
      -> Int
rk basis_m = length . basis_m


-- | tests whether a given set is independent
indep :: (Set a -> Int) -- ^ the rank function of the matroid 
    -> Set a -- ^ set of matroid elements
    -> Bool
indep rk_m x = rk_m x == length x -- a set is independent iff its rank equals its cardinality

-- | obtains an independent subset with maximal cardinality
basis :: Ord a => (Set a -> Bool)-- ^ the independence test of the matroid
    -> Set a -- ^ set of matroid elements
    -> Set a
basis indep_m = S.foldl' augmentIndep (S.empty :: Set a) -- beware that I have not tested this fold as of now
        where 
            augmentIndep b0 x {- adds x to b0 if b0 + {x} is independent -}
              | indep_m b_aug = b_aug
              | otherwise = b0
              where 
                b_aug = S.insert x b0
                
-- | computes the closure of a given set
cl :: Ord a => 
       (Set a -> Int) -- ^ the rank function of the matroid 
    -> Set a -- ^ the groundset of the matroid
    -> Set a -- ^ set of matroid elements 
    -> Set a
cl rk_m groundset_m x = S.foldl' augmentDep x $ groundset_m -- beware that I have not tested this fold as of now
    where 
        rank_x = rk_m x
        augmentDep f0 e {- adds e to f0 if the rank of f0+{e} stays the same -}
            | rk_m f_aug == rank_x = f_aug
            | otherwise = f0
            where 
            f_aug = S.insert e f0
            

-- | returns the loops in the matroid
loops :: (Set a -> Set a) {- ^ the closure operator of the matroid -} -> Set a
loops cl_m = cl_m S.empty -- i.e. all elements e with rk({e}) = 0; i.e. all elements with not indep {e}; ...

-- | rank function of the dual matroid
coRk :: Ord a =>
      (Set a -> Int) {- ^ the rank function of the matroid -} 
    -> Set a {- ^ the ground set of the matroid -}
    -> Set a {- ^ set of matroid elements -} -> Int
coRk rk_m groundset_m x = (rk_m e_minus_x) + (length x) - (rk_m groundset_m)
    where e_minus_x = groundset_m `S.difference` x
    
-- | returns the coloops in the matroid
coloops :: Ord a =>
           (Set a -> Int) {- ^ the rank function of the matroid -} 
        -> Set a {- ^ the ground set of the matroid -}
        -> Set a
coloops rk_m e = isColoop `S.filter` e
    where   rkM = rk_m e
            isColoop x = -- a coloop c is in every basis, thus a basis of E\{c} cannot be a basis of E. 
              rk_m (e `S.difference` S.singleton x) == rkM - 1
