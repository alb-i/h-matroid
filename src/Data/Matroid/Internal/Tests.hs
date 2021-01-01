{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Internal.Tests
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides internal helpers for the matroid package tests.

-}

module Data.Matroid.Internal.Tests where
    
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S

-- | Tests whether a given integer valued set function is indeed monotone increasing in at most unit steps
is_monotone_unit_increasing :: Ord a => (Set a -> Int) {- ^ the rank function (or similar) -} 
                                      -> [a] {- ^ sequence to check monotonicity with -} 
                                      -> Bool
is_monotone_unit_increasing rk e = result
   where  (_,result,_) = foldl' checkStep (S.empty :: Set a, True, 0 :: Int) e
          checkStep (x0,False,r) _ = (x0,False,r) -- propagate error
          checkStep (x0,True,r)  x = let x1 = S.insert x x0
                                         r1 = rk x1
                                      in (x1, (r <= r1) && (r1 <= r + 1), r1)

-- | Tests whether a given boolean valued set function indeed only flips from true to false once when adding elements to its argument
is_monotone_decreasing_bool :: Ord a => (Set a -> Bool) {- ^ the indep function (or similar) -}
                                    -> [a] {- ^ sequence to check monotonicity with -}
                                    -> Bool
is_monotone_decreasing_bool indep e = result
   where (_,result,_) = foldl' checkStep (S.empty :: Set a, True, True) e
         checkStep (x0,False,v)   _ = (x0,False,v) -- propagate error
         checkStep (x0,True,True) x = let x1 = S.insert x x0 
                                          v1 = indep x1
                                       in (x1,True,v1) -- we may flip here or later on
         checkStep (x0,True,False) x = let x1 = S.insert x x0
                                           v1 = indep x1 -- if this is True, then we have failed
                                        in (x1,v1 == False,False) 
                                        
-- | Tests the exchange property of the indep function of a matroid
has_exchange_property :: Ord a => (Set a -> Bool) {- ^ the indep function -}
                                  -> Set a {- ^ X -} 
                                  -> Set a {- ^ Y -} 
                                  -> Bool
has_exchange_property indep x y
    | (indep x) && (indep y) == False   = True -- vacuously true property
    | length x == length y              = True -- vacuously true property
    | length x > length y               = check y $ S.toList $ x `S.difference` y
    | otherwise                         = check x $ S.toList $ y `S.difference` x
    where check x_ (x0:xs)
            | indep $ x_ `S.union` S.singleton x0 = True -- we found a candidate from Y\X that can be used to augment X
            | otherwise                            = check x_ xs -- try the other candidates
          check _ _ = False -- no candidates left, property is not satisfied