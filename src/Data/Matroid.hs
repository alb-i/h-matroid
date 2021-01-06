{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides type classes and functionalities that allow you to work with matroids and related structures.

A [matroid](https://en.wikipedia.org/wiki/Matroid) is also called a combinatorial pre-geometry and is a structure 
that abstracts (linear) dependence. Matroids occur naturally in settings where the greedy algorithm works, although 
they usually stay under the radar.

-}
module Data.Matroid
  (
    module Data.Matroid.Typeclass
  , module Data.Matroid.Uniform
  , module Data.Matroid.Graphic
  , module Data.Matroid.Ops
  , fromRk
  , namedFromRk
  , fromIndep
  , namedFromIndep
  , fromBasisFilter
  , namedFromBasisFilter
  ) where
  
import Data.Matroid.Typeclass
import Data.Matroid.Internal 
import Data.Matroid.Uniform
import Data.Matroid.Graphic
import Data.Matroid.Ops