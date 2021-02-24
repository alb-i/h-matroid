{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-|
Module      : Test.Matroid.Algorithms.Suites
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module contains hspec test suites that check certain properties of the algorithms

-}

module Test.Matroid.Algorithms.Suites where

import Data.Matroid

import qualified Data.Set as S

import Test.Matroid.Helpers
import Test.QuickCheck
import Test.Hspec

