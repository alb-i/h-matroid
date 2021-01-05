{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Ops.Unary
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides an implementation of matroid operations that
take a single matroid as argument; like dualization, contraction,
restriction.

-}

module Data.Matroid.Ops.Unary
  ( 
    UnaryDerivedMatroid
  ) where 

import Data.Matroid.Ops.Unary.Internal
  
import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Data.Matroid.Typeclass

instance (Show (m a), Show a) => Show (UnaryDerivedMatroid m a) where
  show (IdMatroid m) = "mopInject (" ++ show m ++ ")"
  show (RestrictedMatroid m e) = "( " ++ show m ++ " `restriction` " ++ show e ++")"
  
  
{--- This is a little bit of tedious busywork, if there's a better way, let me know! ---}
instance (Matroid m a) => Matroid (UnaryDerivedMatroid m) a where
  {--- I. ---}
  groundset (IdMatroid m) = groundset m
  groundset (RestrictedMatroid _ e) = e
  
  rk (IdMatroid m) = rk m
  rk (RestrictedMatroid m _) = rk m
  
  indep (IdMatroid m) = indep m
  indep (RestrictedMatroid m _) = indep m
  
  basis (IdMatroid m) = basis m
  basis (RestrictedMatroid m _) = basis m
  
  cl (IdMatroid m) = cl m
  cl (RestrictedMatroid m e) = (S.intersection e) . cl m
  
  {--- II. ---}
  
  {--- III. ---}
  loops (IdMatroid m) = loops m
  loops (RestrictedMatroid m e) = S.intersection e $ loops m
  
  coRk (IdMatroid m) x = coRk m x
  coRk (RestrictedMatroid m e) x = (rk m $ e `S.difference` x) + (length x) - (rk m x)
  
  coloops (IdMatroid m) = coloops m
  coloops (RestrictedMatroid m e) = isColoop `S.filter` e
      where rkM = rk m e
            isColoop x = -- a coloop c is in every basis, thus a basis of E\{c} cannot be a basis of E. 
             rk m (e `S.difference` S.singleton x) == rkM - 1
