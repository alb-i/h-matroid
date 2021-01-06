{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Internal.Helpers
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides internal helpers for the matroid package which fall into the 'general' category.
-}

module Data.Matroid.Internal.Helpers where



-- | little helper that either chooses the implementation of a typeclass member from the record, or uses the default implementation
defaultsTo :: (a0 -> Maybe a1) {- ^ record getter function -} 
            -> a0 {- ^ the matroid -}           
            -> (a1) {- ^ default implementation of the typeclass member -}
            -> a1
defaultsTo w_op m defImp = maybe defImp id $ w_op m