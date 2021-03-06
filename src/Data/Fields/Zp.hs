{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances, BangPatterns #-}

{-|
Module      : Data.Fields.Zp
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides the finite fields of the form Z\/Zp for a prime p.

-}

module Data.Fields.Zp (
  invModP
  , XModP 
  , getMod
  , getVal
  , prjModP
  , ratModP
  , ratModP'
  , isPrime
  , Ratio
  , (%)
  ) where
  
import Data.Fields.Typeclass
import Data.Ratio

import Data.Numbers.Primes (isPrime)

{-| Data type that represents an element x (mod p) of Z\/Zp where p is a prime.

    __WARNING__
    This data type may hold elements of different cyclic fields,
    which is not 100 percent correct with a pure type system in mind,
    but it also allows us to have a single type for any element of a
    cyclic prime field. On the downside, we also have to support 0 and 1,
    which must be the same object for Z\/Zp and Z\/Zp'. This creates elements
    of this type that have lost the information of their home module,
    so we have to carry over an /un-homed/ copy of Q.
    So beware that the __operations will fail__ if you try to use them on elements
    that live in __different__ modules.    
    Furthermore __@x == y@ implies @x \`eqF\` y@__, but not the other way around,
    for instance, the unhomed unit is different from 1 (mod p) wrt. '==' but not
    wrt. 'eqF'.
-}
data XModP = XModP Int {- ^ modulus p of x (mod p) or Nothing if canonical integer -} Int {- ^ value (x) of x (mod p) -}
           | Rat (Ratio Int)
           
 deriving (Ord, Eq) -- note that == is not the equality with respect to the field, since (XModP p 0) is different from ZeroModP
 
instance Show XModP where
  show (XModP p x) = (show x) ++ " (mod " ++ (show p) ++ ")"
  show (Rat x) = (show x) ++ " (mod p)"
  
instance Field XModP where
  unitF = Rat (1 % 1)
  zeroF = Rat (0 % 1)
  invF (Rat r) = Rat (1 / r)
  invF (XModP p x) = XModP p (invModP p x)
  negF (Rat r) = Rat (-r)
  negF (XModP p x) = XModP p (p - x)
  addF (Rat x) (Rat y) = Rat (x + y)
  addF (XModP p x) (XModP q y)
          | p == q && (x+y) < p = XModP p (x+y)
          | p == q = XModP p (x+y-p)
          | otherwise = error "cannot combine elements from different modules!"
  addF v@(XModP p _) (Rat r) = v `addF` ((XModP p x) `divF` (XModP p y))
                where x = numerator r
                      y = denominator r
  addF (Rat r) v@(XModP p _) = ((XModP p x) `divF` (XModP p y)) `addF` v
                where x = numerator r
                      y = denominator r
  
  mulF (Rat r) (Rat q) = Rat (r * q)
  mulF (XModP p x) (XModP q y)
            | p == q = XModP p ((x * y) `mod` p)
            | otherwise = error "cannot combine elements from different modules!"
  mulF v@(XModP p _) (Rat r) = v `mulF` ((XModP p x) `divF` (XModP p y))
                where x = numerator r
                      y = denominator r
  mulF (Rat r) v@(XModP p _) = ((XModP p x) `divF` (XModP p y)) `mulF` v
                where x = numerator r
                      y = denominator r
  
  isZeroF (XModP _ 0) = True
  isZeroF (XModP _ _) = False
  isZeroF (Rat r)     = r == 0
  
                           
  
{-| get the modulus m of an element x (mod p)
-}
getMod :: XModP {- ^ element of Z\/Zp -} -> Int
getMod (XModP m _) = m
getMod (Rat _) = error "canonical integral element has no modulus attached to it"

{-| get the normalized x of an element x (mod p),
i.e. x is between 0 and (p-1).
-}
getVal :: XModP {- ^ element of Z\/Zp -} -> Int
getVal (XModP _ x) = x
getVal (Rat _) = error "canonical integral element has no modulus attached to it"

{-| returns the natural projection from Z to Z\/Zp for
    a given prime p. 
    
    This routine checks for primality of p.
    
    @
      prjModP 141650939 :: Int -> XModP
    @
-}
prjModP :: Int {- ^ prime number p -} -> (Int -> XModP)
prjModP p | isPrime p = \x -> XModP p (x `mod` p)
          | otherwise = error "The given modulus p is not a prime!"
          
-- | convert a (Ratio Int) rational to the proper element of Z\/Zp
ratModP :: Int {- ^ prime number p -} -> (Ratio Int) -> XModP
ratModP p r | isPrime p =  (XModP p x) `divF` (XModP p y)
            | otherwise = error "The given modulus p is not a prime!"
            where x = numerator r
                  y = denominator r
                  
-- | convert a (Ratio Int) to an element of all Z\/Zp's, i.e. an un-homed rational
ratModP' :: (Ratio Int) {- ^ un-homed integral element -} -> XModP
ratModP' r = Rat r

{-| determine the multiplicative inverse in Z\/Zp (of x) -}
invModP :: Int {- ^ characteristic p -} -> Int {- ^ element to invert, != 0 mod p -} -> Int
invModP p x = euclidStep p 0 x 1
  where euclidStep r0 t0 !r1 !t1 -- see: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
           | r1 == 0 && r0 > 1 = error ("Input element not invertible! " ++ (show x) ++ " (mod " ++ (show p) ++ ")")
           | r1 == 0 && t0 < 0 = t0 + p
           | r1 == 0 = t0
           | otherwise = let (quotient,remainder) = r0 `divMod` r1
                         in euclidStep r1 t1 remainder (t0 - quotient*t1)
