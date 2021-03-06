{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

{-|
Module      : Data.Fields.Typeclass
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides the Field typeclass.

-}

module Data.Fields.Typeclass where

{-| Typeclass that provides the interface to field elements
    used for linear matroids. I added this so it is easier to
    choose different libraries as the finite field backend.
-}

class (Ord f, Show f) => Field f where
    {-# MINIMAL zeroF, unitF, invF, negF, addF, mulF #-}
    infixl 7 `mulF`
    infixl 7 `divF`
    infixl 6 `addF`
    infixl 6 `subF`
    infixl 4 `eqF`
    -- | the zero element of the field
    zeroF :: f
    -- | the unit element of the field
    unitF :: f
    -- | gives the multiplicative inverse of the input element for non-zero elements
    invF :: f {- ^ element which shall be inverted, must be non-zero -} -> f
    -- | gives the additive inverse of the input element
    negF :: f {- ^ element which shall be negated -} -> f
    -- | multiplies two elements of the field
    mulF :: f -> f -> f
    -- | adds two elements of the field
    addF :: f -> f -> f
    -- | subtracts the second argument from the first in the field
    subF :: f -> f -> f
    subF x = (addF x) . negF
    -- | divides the first argument by the second in the field
    divF :: f -> f -> f
    divF x = (mulF x) . invF
    -- | tests whether a given element equals zero, override this if your field does not use canonical representants
    isZeroF :: f {- ^ element to test for zero -} -> Bool
    isZeroF = (==) zeroF
    -- | tests whether two given elements are equal
    eqF :: f {- ^ element to check -} -> f {- ^ element to check against -} -> Bool
    eqF x y = isZeroF (x `subF` y)
    

{-| The standard rational numbers are a field.
-}
instance Field Rational where
    zeroF = toRational (0 :: Integer)
    unitF = toRational (1 :: Integer)
    invF = (/) unitF
    negF = (-) zeroF 
    addF = (+)
    mulF = (*)