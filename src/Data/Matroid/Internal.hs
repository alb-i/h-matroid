{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Internal
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides internal helpers for the matroid package,
for instance data types that help converting different matroid
representations to Matroid . . -types.

Although it is exported, using anything from this module that is 
not re-exported by another module may (and eventually will) break 
client side code. The main reason for exporting this is so anyone
can inspect internals using haddock; it's a little bit like an
open door policy for code.

-}
module Data.Matroid.Internal
    (
        fromRk, fromIndep, fromBasisFilter,
        RkMatroid, IndepMatroid, BasisFilterMatroid
    ) where

import Data.Matroid.Typeclass 
import Data.Set (Set)
import qualified Data.Set as S

-- | we use this data type to combine a given rank function with the default implementations from the Matroid typeclass
data RkMatroid a = RkM -- ^ matroid from rank function constructor
                   (Set a) -- ^ ground set of the matroid
                   (Set a -> Int) -- ^ rank function
            
                   
instance Show a => Show (RkMatroid a) where
    show (RkM g _) = "fromRk (" ++ (show g) ++ ") (rk)"
    
instance Ord a => Matroid RkMatroid a where
    groundset (RkM e _) = e
    rk (RkM _ r) = r

-- | matroid constructor given groundset and rank function
fromRk :: (Set a) {- ^ ground set -} -> (Set a -> Int) {- ^ rank function -} -> (RkMatroid a)
fromRk = RkM


-- | we use this data type to combine a given independence test with the default implementations from the Matroid typeclass
data IndepMatroid a = IndepM -- ^ matroid from independence-test constructor
                   (Set a) -- ^ ground set of the matroid
                   (Set a ->  Bool) -- ^ independence test function
            
                   
instance Show a => Show (IndepMatroid a) where
    show (IndepM g _) = "fromIndep (" ++ (show g) ++ ") (indep)"
    
instance Ord a => Matroid IndepMatroid a where
    groundset (IndepM e _) = e
    indep (IndepM _ i) = i
    
-- | matroid constructor given groundset and test for independence
fromIndep :: (Set a) {- ^ ground set -} -> (Set a -> Bool) {- ^ independence test -} -> (IndepMatroid a)
fromIndep = IndepM


-- | we use this data type to combine a given a basis filter with the default implementations from the Matroid typeclass
data BasisFilterMatroid a = BasisM -- ^ matroid from basis-filter constructor
                   (Set a) -- ^ ground set of the matroid
                   (Set a -> Set a) -- ^ function that returns a maximal independent subset of its input
            
                   
instance Show a => Show (BasisFilterMatroid a) where
    show (BasisM g _) = "fromBasis (" ++ (show g) ++ ") (basis)"
    
instance Ord a => Matroid BasisFilterMatroid a where
    groundset (BasisM e _) = e
    basis (BasisM _ b) = b
    
-- | matroid constructor given groundset and set-basis filter
fromBasisFilter :: (Set a) {- ^ ground set -} -> (Set a -> Set a) {- ^ returns maximal independent subset -} -> (BasisFilterMatroid a)
fromBasisFilter = BasisM
