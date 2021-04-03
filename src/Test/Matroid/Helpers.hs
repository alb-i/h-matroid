{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Test.Matroid.Helpers
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module contains helpers for the matroid unit tests.

-}

module Test.Matroid.Helpers where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S

-- | Tests whether a given integer valued set function is indeed monotone increasing in at most unit steps
isMonotoneUnitIncreasing :: Ord a => (Set a -> Int) {- ^ the rank function (or similar) -}
                                      -> [a] {- ^ sequence to check monotonicity with -}
                                      -> Bool
isMonotoneUnitIncreasing rk e = result
   where  (_,result,_) = foldl' checkStep (S.empty :: Set a, True, 0 :: Int) e
          checkStep (x0,False,r) _ = (x0,False,r) -- propagate error
          checkStep (x0,True,r)  x = let x1 = S.insert x x0
                                         r1 = rk x1
                                      in (x1, r <= r1 && r1 <= r + 1, r1)

-- | Tests whether a given boolean valued set function indeed only flips from true to false once when adding elements to its argument
isMonotoneDecreasingBool :: Ord a => (Set a -> Bool) {- ^ the indep function (or similar) -}
                                    -> [a] {- ^ sequence to check monotonicity with -}
                                    -> Bool
isMonotoneDecreasingBool indep e = result
   where (_,result,_) = foldl' checkStep (S.empty :: Set a, True, True) e
         checkStep (x0,False,v)   _ = (x0,False,v) -- propagate error
         checkStep (x0,True,True) x = let x1 = S.insert x x0
                                          v1 = indep x1
                                       in (x1,True,v1) -- we may flip here or later on
         checkStep (x0,True,False) x = let x1 = S.insert x x0
                                           v1 = indep x1 -- if this is True, then we have failed
                                        in (x1,not v1,False)

-- | Tests the exchange property of the indep function of a matroid
hasIndepExchangeProperty :: Ord a => (Set a -> Bool) {- ^ the indep function -}
                                  -> Set a {- ^ X -}
                                  -> Set a {- ^ Y -}
                                  -> Bool
hasIndepExchangeProperty indep x y
    | not (indep x && indep y)        = True -- vacuously true property
    | length x == length y                = True -- vacuously true property
    | length x > length y                 = check y $ S.toList $ x `S.difference` y
    | otherwise                           = check x $ S.toList $ y `S.difference` x
    where check x_ (x0:xs)
            | indep $ x_ `S.union` S.singleton x0 = True -- we found a candidate from Y\X that can be used to augment X
            | otherwise                            = check x_ xs -- try the other candidates
          check _ _ = False -- no candidates left, property is not satisfied

-- | Tests whether a given set valued set function is isotone in the set lattice
isIsotoneSetMap :: Ord a => (Set a -> Set a) {- ^ the cl function (or similar) -}
                                    -> [a] {- ^ sequence to check monotonicity with -}
                                    -> Bool
isIsotoneSetMap cl e = result
   where (_,result,_) = foldl' checkStep (S.empty :: Set a, True, S.empty :: Set a) e
         checkStep (x0,False,c0)   _ = (x0,False,c0) -- propagate error
         checkStep (x0, True,c0)   x = let x1 = S.insert x x0
                                           c1 = cl x1
                                           isSuperset = c0 `S.isSubsetOf` c1
                                        in (x1,isSuperset,c1)
