{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

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

module Data.Matroid.Typeclass (
    Matroid
  , groundset
  , rk
  , indep
  , basis
  , cl
  , abstract
  , restriction
  , contraction
  , loops
  , coRk
  , coloops
  , AMatroid
  , wrapUp
) where
  
--import Data.Matroid.Ops.Unary.Internal

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
    abstract :: m a {- ^ matroid -} -> AMatroid a
    abstract m = wrapUp m
             
    -- | returns the restricted matroid M|X as result of the unary matroid operation *|X
    restriction :: m a {- ^ the matroid -} -> Set a {- ^ restricts the ground set to this set-}
       -> AMatroid a
    restriction m x = undefined -- mopRestriction m $ x `S.intersection` groundset m
    
    -- | returns the contracted matroid M.X as a result of the unary matroid operation *.X
    contraction :: m a {- ^ the matroid -} -> Set a {- ^ contracts the ground set onto this set -}
       -> AMatroid a
    contraction m x = undefined -- mopContraction m (groundset m) (basis m) $ x `S.intersection` groundset m
    
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

-- | abstract matroid data type with elements of a given type; its purpose is to hide the underlying type from the type system.
data AMatroid a = WrappedMatroid {
  {--- I. ---}
     
    
    w_groundset :: Set a 
  , w_rk :: Set a -> Int
  , w_indep :: Set a -> Bool
  , w_basis :: Set a -> Set a
  , w_cl :: Set a -> Set a
  {--- II. ---}
  
  {--- III. ---}
  ,  w_loops ::  Set a
  ,  w_coRk :: Set a -> Int
  ,  w_coloops ::  Set a   
}

-- | takes an object of a type that implements the Matroid typeclass, and turns it into an AMatroid record.
wrapUp :: Matroid m a => m a -> AMatroid a
wrapUp m = WrappedMatroid{ w_groundset = groundset m
                         , w_rk = rk m
                         , w_indep = indep m
                         , w_basis = basis m
                         , w_cl = cl m
                         , w_loops = loops m
                         , w_coRk = coRk m
                         , w_coloops = coloops m
                         }
                
instance Show a => Show (AMatroid a) where
  show x = "wrapUp (" ++ show (w_groundset x) ++ ") (...)"
  
instance Ord a => Matroid AMatroid a where
  {--- I. ---}
  groundset = w_groundset
  rk = w_rk
  indep = w_indep
  basis = w_basis
  cl = w_cl
  {--- II. ---}
  abstract = id -- it has been wrapped before
  {--- III. ---}
  loops = w_loops
  coRk = w_coRk
  coloops = w_coloops