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
import qualified Data.Matroid.Typeclass.Defaults as D

instance (Show (m a), Show a) => Show (UnaryDerivedMatroid m a) where
  show (IdMatroid m) = "mopInject (" ++ show m ++ ")"
  show (RestrictedMatroid m e) = "( " ++ show m ++ " `restriction` " ++ show e ++")"
  show (ContractedMatroid m e _ _ _) = "( " ++ show m ++ " `contraction` " ++ show e ++ ")"
  
  
{--- This is a little bit of tedious busywork, if there's a better way, let me know! ---}
instance (Matroid m a) => Matroid (UnaryDerivedMatroid m) a where
  {--- I. ---}
  groundset (IdMatroid m) = groundset m
  groundset (RestrictedMatroid _ e) = e
  groundset (ContractedMatroid _ e _ _ _) = e
  
  rk (IdMatroid m) = rk m
  rk (RestrictedMatroid m _) = rk m
  rk (ContractedMatroid m _ t rkt _) = flip (-) rkt . rk m . S.union t
  
  indep (IdMatroid m) = indep m
  indep (RestrictedMatroid m _) = indep m
  indep (ContractedMatroid m _ _ _ b) = indep m . S.union b
  
  basis (IdMatroid m) = basis m
  basis (RestrictedMatroid m _) = basis m
  
  cl (IdMatroid m) = cl m
  cl (RestrictedMatroid m e) = (S.intersection e) . cl m
  
  {--- II. ---}
  
  {--- III. ---}
  loops (IdMatroid m) = loops m
  loops (RestrictedMatroid m e) = S.intersection e $ loops m
  
  coRk (IdMatroid m) x = coRk m x
  coRk (RestrictedMatroid m e) x = D.coRk (rk m) e x
  
  coloops (IdMatroid m) = coloops m
  coloops (RestrictedMatroid m e) = D.coloops (rk m) e
