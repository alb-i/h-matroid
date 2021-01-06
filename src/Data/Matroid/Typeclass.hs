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
  
import Data.Matroid.Ops

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
    
    In order to keep things DRY, the default implementations are fumbled through
    the (AMatroid a) instance definition below through wrapUp and setting the
    corresponding record value to Nothing.
     
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
    rk m = rk (abstract m)
    
    -- | tests whether a given set is independent
    indep :: m a -- ^ the matroid 
      -> Set a -- ^ set of matroid elements
      -> Bool
    indep m = indep (abstract m)
    
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
             
    -- | returns this matroid as abstract matroid object
    abstract :: m a {- ^ matroid -} -> AMatroid a
    abstract m = wrapUp m
    
    -- | returns the dual of this matroid as abstract matroid object
    dual :: m a {- ^ matroid -} -> AMatroid a
    dual m = undefined
             
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

-- | takes an object of a type that implements the Matroid typeclass, and turns it into an AMatroid record.
wrapUp :: Matroid m a => m a -> AMatroid a
wrapUp m = wrappedMatroid {
  {--- I. ---}  
    w_groundset = groundset m
  , w_rk = Just $ rk m
  , w_indep = Just $ indep m
  , w_basis = Just $ basis m
  , w_cl = Just $ cl m
  {--- II. ---}
  , w_abstract = Nothing
  , w_dual = Just $ dual m
  , w_restriction = Just $ restriction m
  , w_contraction = Just $ contraction m
  {--- III. ---}
  , w_loops = Just $ loops m
  , w_coRk = Just $ coRk m
  , w_coloops = Just $ coloops m
  }
                
instance Show a => Show (AMatroid a) where
  show x = "wrapUp (" ++ show (w_groundset x) ++ ") (...)"

-- | little helper that either chooses the implementation of a typeclass member from the record, or uses the default implementation
defaultsTo :: (AMatroid a -> Maybe a1) {- ^ record getter function -} 
            -> AMatroid a {- ^ the matroid -}           
            -> (a1) {- ^ default implementation of the typeclass member -}
            -> a1
defaultsTo w_op m defImp = maybe defImp id $ w_op m

instance Ord a => Matroid AMatroid a where
  {--- I. ---}
  groundset = w_groundset
  rk m = defaultsTo w_rk m $ D.rk (basis m)
  indep m = defaultsTo w_indep m $ D.indep (rk m)
  basis m = defaultsTo w_basis m $ D.basis (indep m)
  cl m = defaultsTo w_cl m $ D.cl (rk m) (groundset m)
  {--- II. ---}
  abstract m = defaultsTo w_abstract m $ m -- it has been wrapped before
  dual m = defaultsTo w_dual m $ undefined
  restriction m = defaultsTo w_restriction m $ undefined
  contraction m = defaultsTo w_contraction m $ undefined
  {--- III. ---}
  loops m = defaultsTo w_loops m $ D.loops (cl m)
  coRk m = defaultsTo w_coRk m $ D.coRk (rk m) (groundset m)
  coloops m = defaultsTo w_coloops m $ D.coloops (rk m) (groundset m)