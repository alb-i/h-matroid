{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

{-|
Module      : Data.Fields.Zp
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides the finite fields of te form Z/Zp for a prime p.

-}

module Data.Fields.Zp where

{-| determine the multiplicative inverse in Z/Zp (of x) -}
invModP :: Integer {- ^ characteristic p -} -> Integer {- ^ element to invert, != 0 mod p -} -> Integer
invModP p x = euclidStep p 0 x 1
  where euclidStep r0 t0 r1 t1 -- see: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
           | r1 == 0 && r0 > 1 = error "Input element not invertible!"
           | r1 == 0 && t0 < 0 = t0 + p
           | r1 == 0 = t0
           | otherwise = let (quotient,remainder) = r0 `divMod` r1
                         in euclidStep r1 t1 remainder (t0 - quotient*t1)