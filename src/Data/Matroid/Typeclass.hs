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

module Data.Matroid.Typeclass where

import Data.Matroid.Ops

import qualified Data.Matroid.Typeclass.Defaults as D

import Data.Set (Set)
-- import qualified Data.Set as S

import Data.Matroid.Internal.Helpers

{-| 
    Typeclass that provides the full matroid interface.
    
    The type parameter @a@ is the type of the elements of the matroid,
    it is most commonly used in a @('Set' a)@ type.
    
    In this typeclass, we assume that every set of matroid elements
    passed to any of the routines is actually a subset of @'groundset' m@.
    Behaviour for other sets shall be considered undefined.
    
    In order to keep things DRY, the default implementations are fumbled through
    the @'AMatroid' a@ instance definition below through 'wrapUp' and setting the
    corresponding record value to 'Nothing'.
     
-}
class (Ord a, Show a) => Matroid m a
    where

    {--- I. The routines in this section are worth to look at in 
            your implementation of a 'Matroid' instance ---}

    {-# MINIMAL groundset, (rk | indep | basis) #-}

    -- | The ground set of the matroid, its elements are usually called edges. This set is finite.
    groundset :: m a -> Set a

    -- | name of the matroid, may be used for show
    showName :: m a -> String
    showName m = showName (abstract m) { w_showName = Nothing }

    -- | returns the rank of the set
    rk :: m a -- ^ the matroid 
      -> Set a -- ^ set of matroid elements
      -> Int
    rk m = rk (abstract m) { w_rk = Nothing }

    -- | tests whether a given set is independent
    indep :: m a -- ^ the matroid 
      -> Set a -- ^ set of matroid elements
      -> Bool
    indep m = indep (abstract m) { w_indep = Nothing }

    {- | obtains an independent subset of the input subset, which has maximal cardinality among all such subsets.
    
      Please note that this routine returns a basis of the input subset wrt. the matroid, which is not necessarily
      also a basis of the matroid itself. You can obtain at least one basis of the matroid via
      
        > basis m $ groundset m
      
    -}
    basis :: m a -- ^ the matroid
      -> Set a -- ^ set of matroid elements
      -> Set a
    basis m = basis (abstract m) { w_basis = Nothing }

    -- | computes the closure of a given set
    cl :: m a -- ^ the matroid
      -> Set a -- ^ set of matroid elements 
      -> Set a
    cl m = cl (abstract m) { w_cl = Nothing }

    {--- II. We provide standard implementations of these operations, 
             but you probably want to roll your own,
             in order to improve performance ---}

    -- | returns this matroid as abstract matroid object
    abstract :: m a {- ^ matroid -} -> AMatroid a
    abstract m = wrapUp m

    -- | returns the dual of this matroid as abstract matroid object
    dual :: m a {- ^ matroid -} -> AMatroid a
    dual m = dual (abstract m) { w_dual = Nothing }

    -- | returns the restricted matroid M|X as result of the unary matroid operation *|X
    restriction :: m a {- ^ the matroid -} -> Set a {- ^ restricts the ground set to this set-}
       -> AMatroid a
    restriction m = restriction (abstract m) { w_restriction = Nothing }

    -- | returns the contracted matroid M.X as a result of the unary matroid operation *.X
    contraction :: m a {- ^ the matroid -} -> Set a {- ^ contracts the ground set onto this set -}
       -> AMatroid a
    contraction m  = contraction (abstract m) { w_contraction = Nothing }

    {--- III. There is generally less to gain from implementing the following 
              routines in your own 'Matroid' instances. ---}

    -- | returns the loops in the matroid
    loops :: m a {- ^ the matroid -} -> Set a
    loops m = loops (abstract m) { w_loops = Nothing }

    -- | rank function of the dual matroid
    coRk :: m a {- ^ the matroid -} -> Set a {- ^ set of matroid elements -} -> Int
    coRk m = coRk (abstract m) { w_coRk = Nothing }

    -- | returns the coloops in the matroid
    coloops :: m a {- ^ the matroid -} -> Set a
    coloops m = coloops (abstract m) { w_coloops = Nothing }


-- | takes an object of a type that implements the 'Matroid' typeclass, and turns it into an 'AMatroid' record.
wrapUp :: (Matroid m a) => m a -> AMatroid a
wrapUp m = wrappedMatroid {
  {--- I. ---}
    w_groundset = groundset m
  , w_showName = Just $ "abstract $ " ++ showName m
  , w_rk = Just $ rk m
  , w_indep = Just $ indep m
  , w_basis = Just $ basis m
  , w_cl = Just $ cl m
  {--- II. ---}
  , w_abstract = Nothing -- abstract shall be idempotent
  , w_dual = Just $ dual m
  , w_restriction = Just $ restriction m
  , w_contraction = Just $ contraction m
  {--- III. ---}
  , w_loops = Just $ loops m
  , w_coRk = Just $ coRk m
  , w_coloops = Just $ coloops m

  }





-- | This instance contains the default implementations of the members of the 'Matroid' typeclass.
instance (Ord a, Show a) => Matroid AMatroid a where
  {--- I. ---}                              --   vv. default 'Matroid' implementations go here .vv
  groundset = w_groundset
  showName m = defaultsTo w_showName m "Matroid instance"
  rk m = defaultsTo w_rk m                      $ D.rk (basis m)
  indep m = defaultsTo w_indep m                $ D.indep (rk m)
  basis m = defaultsTo w_basis m                $ D.basis (indep m)
  cl m = defaultsTo w_cl m                      $ D.cl (rk m) (groundset m)
  {--- II. ---}
  abstract m = defaultsTo w_abstract m m -- it has been wrapped before
  dual m = defaultsTo w_dual   m                $ a_dual m
  restriction m = defaultsTo w_restriction m    $ a_restriction m
  contraction m = defaultsTo w_contraction m    $ a_contraction m
  {--- III. ---}
  loops m = defaultsTo w_loops m                $ D.loops (cl m)
  coRk m = defaultsTo w_coRk m                  $ D.coRk (rk m) (groundset m)
  coloops m = defaultsTo w_coloops m            $ D.coloops (rk m) (groundset m)