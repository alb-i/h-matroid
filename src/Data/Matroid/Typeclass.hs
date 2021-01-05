{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Typeclass
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides the Matroid typeclass.

-}

module Data.Matroid.Typeclass where
  
import Data.Matroid.Ops.Unary.Internal

import qualified Data.Matroid.Typeclass.Defaults as D
    
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
    
    {--- I. The routines in this section are worth to look at in 
            your implementation of a Matroid instance ---}
            
    {-# MINIMAL groundset, (rk | indep | basis) #-}
    
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
               
    {--- II. We provide standard implementations of these operations, 
             but you probably want to roll your own,
             in order to improve performance ---}
             
    -- | returns this matroid as the result of the unary identity operation on matroids
    identity :: m a {- ^ matroid -} -> UnaryDerivedMatroid m a
    identity = mopInject
             
    -- | returns the restricted matroid M|X as result of the unary matroid operation *|X
    restriction :: m a {- ^ the matroid -} -> Set a {- ^ restricts the ground set to this set-}
       -> UnaryDerivedMatroid m a
    restriction m x = mopRestriction m $ x `S.intersection` groundset m
    
    -- | returns the contracted matroid M.X as a result of the unary matroid operation *.X
    contraction :: m a {- ^ the matroid -} -> Set a {- ^ contracts the ground set onto this set -}
       -> UnaryDerivedMatroid m a
    contraction m x = mopContraction m (groundset m) (basis m) $ x `S.intersection` groundset m
    
    {--- III. There is generally less to gain from implementing the following 
              routines in your own Matroid instances. ---}
    
    -- | returns the loops in the matroid
    loops :: m a {- ^ the matroid -} -> Set a
    loops m = cl m S.empty -- i.e. all elements e with rk({e}) = 0; i.e. all elements with not indep {e}; ...
    
    -- | rank function of the dual matroid
    coRk :: m a {- ^ the matroid -} -> Set a {- ^ set of matroid elements -} -> Int
    coRk m x = (rk m e_minus_x) + (length x) - (rk m x)
       where e_minus_x = groundset m `S.difference` x
       
    -- | returns the coloops in the matroid
    coloops :: m a {- ^ the matroid -} -> Set a
    coloops m = isColoop `S.filter` groundset m
       where e = groundset m
             rkM = rk m e
             isColoop x = -- a coloop c is in every basis, thus a basis of E\{c} cannot be a basis of E. 
              rk m (e `S.difference` S.singleton x) == rkM - 1
