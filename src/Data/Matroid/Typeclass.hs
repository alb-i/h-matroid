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
    rk m = D.rk (basis m)
    
    -- | tests whether a given set is independent
    indep :: m a -- ^ the matroid 
      -> Set a -- ^ set of matroid elements
      -> Bool
    indep m = D.indep (rk m)
    
    -- | obtains an independent subset with maximal cardinality
    basis :: m a -- ^ the matroid
      -> Set a -- ^ set of matroid elements
      -> Set a
    basis m = D.basis (indep m)
                   
    -- | computes the closure of a given set
    cl :: m a -- ^ the matroid
      -> Set a -- ^ set of matroid elements 
      -> Set a
    cl m = D.cl (rk m) (groundset m)
               
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
    loops m = D.loops (cl m)
    
    -- | rank function of the dual matroid
    coRk :: m a {- ^ the matroid -} -> Set a {- ^ set of matroid elements -} -> Int
    coRk m = D.coRk (rk m) (groundset m)
       
    -- | returns the coloops in the matroid
    coloops :: m a {- ^ the matroid -} -> Set a
    coloops m = D.coloops (rk m) (groundset m)
