{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Algorithms
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of algorithms that work with
matroids.

-}

module Data.Matroid.Algorithms (
    module Data.Matroid.Algorithms.Greedy
) where 

import Data.Matroid.Algorithms.Greedy