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
wrappedMatroid :: AMatroid a
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
  , w_restriction = w_restriction m -- (M|X0)|X1 = M|X1 whenever defined.
} where e = x0 `S.intersection` (w_groundset m)
        intersectWithE = pure (\f -> \x -> e `S.intersection` f x)

{- | returns the contraction of a given matroid

  Note that this routine implements the correct routines provided that the prerequisite members
  in the input matroid are defined. Routines which have missing prerequisite members in the input
  matroid will be left to Nothing. Data.Matroid.Typeclass.wrapUp fills all AMatroid record members.
-}
a_contraction :: (Show a, Ord a) => AMatroid a {- ^ input matroid -} -> Set a {- ^ contract the ground set onto this set -} -> AMatroid a
a_contraction m x0 = a_namedContraction name m x0
            where e = x0 `S.intersection` (w_groundset m)
                  name = "(" ++ m_name ++ ") `contraction` (" ++ show e ++ ")"
                  m_name = maybe "M" id $ w_showName m
                  
{- | returns the contraction of a given matroid, named

  Note that this routine implements the correct routines provided that the prerequisite members
  in the input matroid are defined. Routines which have missing prerequisite members in the input
  matroid will be left to Nothing. Data.Matroid.Typeclass.wrapUp fills all AMatroid record members.
-}
a_namedContraction :: Ord a => String {- ^ name -} -> AMatroid a {- ^ input matroid -} -> Set a {- ^ contract the ground set onto this set -} -> AMatroid a
a_namedContraction name m x0 = wrappedMatroid {
    w_groundset = e
  , w_showName = Just name
  , w_rk =    new_rk
  , w_indep = new_indep
  , w_basis = pure (\indep_ -> D.basis indep_) <*> new_indep
  , w_cl =    pure (\cl_ -> \x -> (cl_ (x `S.union` t)) `S.intersection` e) <*> (w_cl m)
  , w_loops = pure (\cl_ -> (cl_ t) `S.intersection` e)                     <*> (w_cl m)
  , w_coRk =  pure (\rk_ -> D.coRk rk_ e) <*> new_rk
  , w_coloops = pure (S.intersection e) <*> (w_coloops m)
  , w_contraction = w_contraction m -- (M.X0).X1 = M.X1 whenever defined.
} where e = x0 `S.intersection` (w_groundset m)
        t = (w_groundset m) `S.difference` e -- deleted edges
        bt = w_basis m <*> pure t -- maybe the basis of deleted edges
        rt = w_rk m <*> pure t -- maybe the rank of t
        new_rk = pure (\rk_ -> \rt_ -> \x -> rk_ (x `S.union` t) - rt_ ) <*> (w_rk m)    <*> rt
        new_indep = pure (\indep_ -> \bt_ -> \x -> indep_ (x `S.union` bt_))  <*> (w_indep m) <*> bt

        
{- | returns the contraction of a given matroid

  Note that this routine implements the correct routines provided that the prerequisite members
  in the input matroid are defined. Routines which have missing prerequisite members in the input
  matroid will be left to Nothing. Data.Matroid.Typeclass.wrapUp fills all AMatroid record members.
-}
a_dual :: (Show a, Ord a) => AMatroid a {- ^ input matroid -} -> AMatroid a
a_dual m  = a_namedDual name m 
            where name = "dual (" ++ m_name ++ ")"
                  m_name = maybe "M" id $ w_showName m
                  
{- | returns the contraction of a given matroid, named

  Note that this routine implements the correct routines provided that the prerequisite members
  in the input matroid are defined. Routines which have missing prerequisite members in the input
  matroid will be left to Nothing. Data.Matroid.Typeclass.wrapUp fills all AMatroid record members.
-}
a_namedDual :: Ord a => String {- ^ name -} -> AMatroid a {- ^ input matroid -} -> AMatroid a
a_namedDual name m = wrappedMatroid {
    w_groundset = e
  , w_showName = Just name
  , w_rk = new_rk
  , w_indep = new_indep
  , w_basis = pure (\indep_ -> D.basis indep_) <*> new_indep
  , w_cl =    pure (\rk_ -> D.cl rk_ e) <*> new_rk
  , w_loops = w_coloops m
  , w_coRk = w_rk m
  , w_coloops = w_loops m
  , w_dual = Just m -- bounce back to the original matroid
} where e = (w_groundset m)
        new_rk = (w_coRk m) `mplus` (pure (\rk_ -> D.coRk rk_ e) <*> w_rk m)
        new_indep = pure (\rk_ -> D.indep rk_) <*> new_rk
