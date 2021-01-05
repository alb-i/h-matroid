{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Ops.Unary.Internal
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides the unary data type that is shared between
Data.Matroid.Ops.Unary and Data.Matroid.Typeclass.

-}

module Data.Matroid.Ops.Unary.Internal where
    
import Data.Set (Set)
import qualified Data.Set as S


-- | data type representing the matroid op(M), a matroid M of type (m a), for various operations.
data UnaryDerivedMatroid m a = 
    IdMatroid {- ^ Just the identity operation on (m a) -} (m a) {- ^ matroid -}
  | RestrictedMatroid {- ^ constructs M|X -} (m a) {- ^ the matroid -} (Set a) {- ^ restricted ground set -}
  | ContractedMatroid {- ^ constructs M|X -} (m a) {- ^ the matroid -} 
      (Set a) {- ^ contracted-onto ground set -} 
      (Set a) {- ^ contracted edges -}
      (Int) {- ^ rank of the contracted edges -}
      (Set a) {- ^ basis of contracted edges -}
  
-- | construct a representation of this matroid as the result of the unary identity operation
mopInject :: (m a) {- ^ the matroid -} -> UnaryDerivedMatroid m a
mopInject = IdMatroid 

-- | constructs the restriction of a matroid to a subset of its groundset
mopRestriction :: (m a) {- ^ the matroid -} -> (Set a) {- ^ restriction to this set -} -> UnaryDerivedMatroid m a
mopRestriction m x = RestrictedMatroid m x

-- | constructs the contraction of a matroid onto a subset of its groundset
mopContraction :: Ord a => (m a) {- ^ the matroid -} 
    -> (Set a) {- ^ the groundset of the matroid -}    
    -> (Set a -> Set a) {- ^ the basis filter of the matroid -} 
    -> (Set a) {- ^ contraction onto this set -} 
    -> UnaryDerivedMatroid m a
mopContraction m groundset_m basis_m x = ContractedMatroid m x t (length b) b
  where t = groundset_m `S.difference` x -- M.X =  M/T since E(M) = X+T
        b = basis_m t