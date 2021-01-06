{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Ops
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of common operations on matroids
and an abstract matroid data type.

-}

module Data.Matroid.Ops where 

import qualified Data.Matroid.Typeclass.Defaults as D
  
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad
    

{- | abstract matroid data type with elements of a given type

  Its purpose is to hide the underlying type from the type system.
  The records resemble the typeclass Matroid, where everything except for the
  groundset has been placed inside the Maybe-Monad. A value of Nothing
  indicates that the default implementation of the typeclass should be used.
  
-}
data AMatroid a = WMatroid {
  {--- I. ---}
    w_groundset :: Set a 
  , w_showName :: Maybe (String)
  , w_rk :: Maybe (Set a -> Int)
  , w_indep :: Maybe (Set a -> Bool)
  , w_basis :: Maybe (Set a -> Set a)
  , w_cl :: Maybe (Set a -> Set a)
  {--- II. ---}
  , w_abstract :: Maybe (AMatroid a)
  , w_dual :: Maybe (AMatroid a)
  , w_restriction :: Maybe (Set a -> AMatroid a)
  , w_contraction :: Maybe (Set a -> AMatroid a)
  {--- III. ---}
  ,  w_loops ::  Maybe (Set a)
  ,  w_coRk :: Maybe (Set a -> Int)
  ,  w_coloops ::  Maybe (Set a)
}

instance Show a => Show (AMatroid a) where
  show x = maybe defShow id $ w_showName x
    where defShow = "WMatroid (" ++ show (w_groundset x) ++ ") (...)"
    


-- | defaults for WMatroid
wrappedMatroid = WMatroid {
  {--- I. ---}
    w_groundset = S.empty
  , w_showName = Nothing
  , w_rk = Nothing
  , w_indep = Nothing
  , w_basis = Nothing
  , w_cl = Nothing
  {--- II. ---}
  , w_abstract = Nothing
  , w_dual = Nothing
  , w_restriction = Nothing
  , w_contraction = Nothing
  {--- III. ---}
  , w_loops = Nothing
  , w_coRk = Nothing
  , w_coloops = Nothing
}


{- | returns the restriction of a given matroid

  Note that this routine implements the correct routines provided that the prerequisite members
  in the input matroid are defined. Routines which have missing prerequisite members in the input
  matroid will be left to Nothing. Data.Matroid.Typeclass.wrapUp fills all AMatroid record members.
-}
a_restriction :: (Show a, Ord a) => AMatroid a {- ^ input matroid -} -> Set a {- ^ restriction of ground set -} -> AMatroid a
a_restriction m x0 = a_namedRestriction name m x0
            where e = x0 `S.intersection` (w_groundset m)
                  name = "(" ++ m_name ++ ") `restriction` (" ++ show e ++ ")"
                  m_name = maybe "M" id $ w_showName m
                  
{- | returns the restriction of a given matroid, named

  Note that this routine implements the correct routines provided that the prerequisite members
  in the input matroid are defined. Routines which have missing prerequisite members in the input
  matroid will be left to Nothing. Data.Matroid.Typeclass.wrapUp fills all AMatroid record members.
-}
a_namedRestriction :: Ord a => String {- ^ name -} -> AMatroid a {- ^ input matroid -} -> Set a {- ^ restriction of ground set -} -> AMatroid a
a_namedRestriction name m x0 = wrappedMatroid {
    w_groundset = e
  , w_showName = Just name
  , w_rk = w_rk m
  , w_indep = w_indep m
  , w_basis = w_basis m
  , w_cl = intersectWithE <*> w_cl m
  , w_loops = pure (S.intersection e) <*> w_loops m
  , w_coRk = pure (\rk_ -> D.coRk rk_ e) <*> (w_rk m)
  , w_coloops = pure (\rk_ -> D.coloops rk_ e) <*> (w_rk m)
} where e = x0 `S.intersection` (w_groundset m)
        intersectWithE = pure (\f -> \x -> e `S.intersection` f x)
