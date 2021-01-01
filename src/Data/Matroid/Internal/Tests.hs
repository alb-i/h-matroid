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
