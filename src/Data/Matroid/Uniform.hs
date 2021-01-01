{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Uniform
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of uniform matroids.

-}

module Data.Matroid.Uniform
    (
       UniformMatroid ( U )
     , uniform
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Matroid
import Data.Matroid.Internal

-- | data type representing a uniform matroid over a given ground set
data UniformMatroid a = U -- ^ uniform matroid constructor
                        (Set a) -- ^ ground set of the uniform matroid
                        Int -- ^ rank of the uniform matroid (provided that groundset is big enough), must be >= 0.   
                    deriving (Eq, Ord, Show)
                    
instance Ord a => Matroid UniformMatroid a where
    groundset (U e _) = e
    rk (U _ r) x = min (max 0 r) $ length x -- we enforce the rank to be at least 0
    indep (U _ r) x = length x <= (max 0 r) -- we enforce the rank to be at least 0
    basis (U _ r) = S.take (max 0 r) -- we enforce the rank to be at least 0
    cl (U e r) x
      | length x < (max 0 r) = x -- we enforce the rank to be at least 0
      | otherwise = e

-- | returns a uniform matroid on the intergers from 1..n of rank r
uniform :: 
 Int -- ^ size of the uniform matroid (number of edges, n)
 -> Int -- ^ rank of the uniform matroid (r)
 -> UniformMatroid Int
uniform n r
  | r < 0 = error "The rank of a matroid must be non-negative."
  | n < 0 = error "The cardinality of a matroid must be non-negative."
  | n < r = error "The cardinality of a matroid must be at least its rank."
  | otherwise = U (S.fromList [1..n]) r
