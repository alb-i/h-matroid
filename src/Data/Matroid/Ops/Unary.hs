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
    --RestrictedMatroid
  ) where 
  
import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Data.Matroid.Typeclass
import qualified Data.Matroid.Typeclass.Defaults as D
{-
data RestrictedMatroid m a = RMatroid (m a) {- ^ original matroid -} (Set a) {- ^ restriction of the ground set -}

instance Matroid m a => Matroid (RestrictedMatroid m) a where
  groundset (RMatroid _ e) = e
  rk (RMatroid m _) = rk m
  indep (RMatroid m _) = indep m
  basis (RMatroid m _) = basis m
  cl (RMatroid m e) = (S.intersection e) . cl m
  loops (RMatroid m e) = S.intersection e $ loops m
  coRk (RMatroid m e) x = D.coRk (rk m) e x
  coloops (RMatroid m e) = D.coloops (rk m) e
  
instance Show a, Show m a => Show (RestrictedMatroid m a) where
  show (RMatroid m e) = "(" ++ show m ++ ") `restriction` (" ++ show e ++ ")"
-}
{-
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
  basis m@(ContractedMatroid _ _ _ _ _) = 
    {- we have to use the default implementation, 
       because there is no guarantee that b is a subset of the filtered basis of (basis m x)
     -}
    D.basis (indep m) 
  
  cl (IdMatroid m) = cl m
  cl (RestrictedMatroid m e) = (S.intersection e) . cl m
  cl (ContractedMatroid m e t _ _) = (S.intersection e) . cl m . S.union t
  
  {--- II. ---}
  
  {--- III. ---}
  loops (IdMatroid m) = loops m
  loops (RestrictedMatroid m e) = S.intersection e $ loops m
  
  coRk (IdMatroid m) x = coRk m x
  coRk (RestrictedMatroid m e) x = D.coRk (rk m) e x
  
  coloops (IdMatroid m) = coloops m
  coloops (RestrictedMatroid m e) = D.coloops (rk m) e
-}