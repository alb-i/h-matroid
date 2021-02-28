{-|
Module      : Test.Matroid
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module exports routines that may be used to write test cases for
user written instances of the Matroid typeclass.

It is also used in the unit tests of the matroid library.

-}

module Test.Matroid (
        module Test.Matroid.Generators
    ,   module Test.Matroid.Suites
    ,   module Test.Matroid.Algorithms.Suites
) where
    
import Test.Matroid.Generators
import Test.Matroid.Suites
import Test.Matroid.Algorithms.Suites