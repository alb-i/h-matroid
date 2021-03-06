{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

{-|
Module      : Data.Matroid.Algorithms.Enumerate
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of algorithms that
enumerate things from matroids.

-}

module Data.Matroid.Algorithms.Enumerate where

import Data.Matroid.Typeclass

import Data.Set (Set)
import qualified Data.Set as S

{- | enumerates all bases of a matroid 
-}
enumerateBases :: Matroid m a => m a {- ^ matroid -} -> [Set a]
enumerateBases m = dfs (S.toList $ groundset m) S.empty
            where dfs es !x
                    | not $ indep m x = []
                    | length x == rankM = [x]
                    | (e:ex) <- es = dfs ex ( S.insert e x) ++ dfs ex x
                    | otherwise = [] 
                  rankM = rk m $ groundset m