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
      UniformMatroid
     , uniformOn
     , uniform
     , FreeMatroid
     , freeOn
    ) where

import Data.Set (Set)
import qualified Data.Set as S

import Data.Matroid.Typeclass
import Data.Matroid.Internal

-- | data type representing a uniform matroid over a given ground set
data UniformMatroid a = U -- ^ uniform matroid constructor
                        (Set a) -- ^ ground set of the uniform matroid
                        Int -- ^ rank of the uniform matroid (provided that groundset is big enough), must be >= 0.   
                    deriving (Eq, Ord)
                    
instance Show a => Show (UniformMatroid a) where
  show (U e r) = "uniformOn (" ++ show e ++ ") " ++ show r
                    
instance Ord a => Matroid UniformMatroid a where
    groundset (U e _) = e
    rk (U _ r) x = min r $ length x 
    indep (U _ r) x = length x <= r 
    basis (U _ r) = S.take r 
    cl (U e r) x
      | length x < r = x 
      | otherwise = e

-- | returns a uniform matroid on the intergers from 1..n of rank r
uniform :: 
 Int -- ^ size of the uniform matroid (number of edges, n)
 -> Int -- ^ rank of the uniform matroid (r)
 -> UniformMatroid Int
uniform n r
  | n < 0 = error "The cardinality of a matroid must be non-negative."
  | otherwise = uniformOn (S.fromList [1..n]) r

-- | returns a uniform matroid on a given ground set of rank r
uniformOn :: Ord a => 
 Set a -- ^ ground set of the uniform matroid
 -> Int -- ^ rank of the uniform matroid (r)
 -> UniformMatroid a
uniformOn e r
  | r < 0 = error "The rank of a matroid must be non-negative."
  | n < r = error "The cardinality of the groundset of the matroid must be at least its rank."
  | otherwise = U e r
  where n = length e

-- | data type that represents a free matroid over a given set (free matroids are of course uniform)
data FreeMatroid a = Free (Set a) -- ^ ground set
                   deriving (Eq, Ord)
                   
instance Ord a => Matroid FreeMatroid a where
  groundset (Free e) = e
  rk _ = length
  indep _ _ = True
  basis _ = id
  cl _ = id
  
instance Show a => Show (FreeMatroid a) where
  show (Free e) = "freeOn (" ++ show e ++ ")"
  
-- | returns a free matroid on a given ground set (where every subset of the groundset is independent)
freeOn :: Ord a => 
 Set a -- ^ ground set of the free matroid
 -> FreeMatroid a
freeOn = Free
