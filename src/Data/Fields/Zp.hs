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
  , isPrime
  ) where
  
import Data.Fields.Typeclass

import Data.Numbers.Primes (isPrime)

{-| Data type that represents an element x (mod p) of Z\/Zp where p is a prime.

    __WARNING__
    This data type may hold elements of different cyclic fields,
    which is not 100 percent correct with a pure type system in mind,
    but it also allows us to have a single type for any element of a
    cyclic prime field. On the downside, we also have to support 0 and 1,
    which must be the same object for Z\/Zp and Z\/Zp'. This creates elements
    of this type that have lost the information of their home module,
    which also creates the necessity to allow at least some sort of arithmetic
    on these objects.
    So beware that the __operations will fail__ if you try to use them on elements
    that live in __different__ modules, or if you try to divide by an __integral__
    derived from __unitF and zeroF alone__.
    
    __Therefore you should always prefer creating 0 and 1 through 'prjModP'.__
    The unitF and zeroF objects come in handy mostly when checking equalities in unit tests.
    
    Furthermore __@x == y@ implies @x \`eqF\` y@__, but not the other way around!
-}
data XModP = XModP (Maybe Int) {- ^ modulus p of x (mod p) or Nothing if canonical integer -} Int {- ^ value (x) of x (mod p) -}
           
 deriving (Ord, Eq) -- note that == is not the equality with respect to the field, since (XModP p 0) is different from ZeroModP
 
instance Show XModP where
  show (XModP (Just p) x) = (show x) ++ " (mod " ++ (show p) ++ ")"
  show (XModP Nothing x) = (show x) ++ " (mod p)"
  
instance Field XModP where
  unitF = XModP Nothing 1
  zeroF = XModP Nothing 0
  invF (XModP Nothing 1) = XModP Nothing 1
  invF (XModP Nothing _) = error "canonical integral element without modulus cannot be inverted!"
  invF (XModP (Just p) x) = XModP (Just p) (invModP p x)
  negF (XModP Nothing x) = XModP Nothing (-x)
  negF (XModP (Just p) x) = XModP (Just p) (p - x)
  addF (XModP p x) (XModP q y) = XModP r s
     where r = combineP p q
           s = s0 r
           s0 Nothing  = x + y -- canonical integral addition
           s0 (Just m) = let  s1 = x + y
                              mp x0
                                  | x0 < m = x0
                                  | otherwise = x0 - m
                            in mp s1
  mulF (XModP p x) (XModP q y) = XModP r s
     where r = combineP p q
           s = s0 r
           s0 Nothing = x * y
           s0 (Just m)  = (x * y) `mod` m
  isZeroF (XModP _ 0) = True
  isZeroF _           = False
                           
  
{-| determine the combined modulus of two elements -}
combineP :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
combineP Nothing Nothing = Nothing
combineP Nothing p = p
combineP p Nothing = p
combineP p q | p == q = p
             | otherwise = error "moduli of the two operands do not match!"

{-| get the modulus m of an element x (mod p)
-}
getMod :: XModP {- ^ element of Z\/Zp -} -> Int
getMod (XModP (Just m) _) = m
getMod (XModP Nothing _) = error "canonical integral element has no modulus attached to it"

{-| get the normalized x of an element x (mod p),
i.e. x is between 0 and (p-1).
-}
getVal :: XModP {- ^ element of Z\/Zp -} -> Int
getVal (XModP _ x) = x

{-| returns the natural projection from Z to Z\/Zp for
    a given prime p. 
    
    This routine checks for primality of p.
    
    @
      prjModP 141650939 :: Int -> XModP
    @
-}
prjModP :: Int {- ^ prime number p -} -> (Int -> XModP)
prjModP p | isPrime p = \x -> XModP (Just p) (x `mod` p)
          | otherwise = error "The given modulus p is not a prime!"

{-| determine the multiplicative inverse in Z\/Zp (of x) -}
invModP :: Int {- ^ characteristic p -} -> Int {- ^ element to invert, != 0 mod p -} -> Int
invModP p x = euclidStep p 0 x 1
  where euclidStep r0 t0 !r1 !t1 -- see: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
           | r1 == 0 && r0 > 1 = error "Input element not invertible!"
           | r1 == 0 && t0 < 0 = t0 + p
           | r1 == 0 = t0
           | otherwise = let (quotient,remainder) = r0 `divMod` r1
                         in euclidStep r1 t1 remainder (t0 - quotient*t1)
