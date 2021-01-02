
{-|
Module      : Data.Matroid.Internal
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides internal helpers for the matroid package.

-}
module Data.Matroid.Internal
    (
        fromRk
    ) where

import Data.Matroid (Matroid)
    
data RkMatroid a = RkM -- ^ matroid from rank function constructor
                   { groundset :: (Set a) -- ^ ground set of the uniform matroid
                    ,rk :: (Set a -> Int) -- ^ rank of the uniform matroid (provided that groundset is big enough), must be >= 0.
                   }   
        deriving (Eq, Ord, Show)

-- | matroid constructor given groundset and rank function
fromRk :: Matroid m a => (Set a) {- ^ ground set -} -> (Set a -> Int) {- ^ rank function -} -> (m a)
