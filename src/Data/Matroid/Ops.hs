{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Ops
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of common operations on matroids
and an abstract matroid data type.

-}

module Data.Matroid.Ops where 

import qualified Data.Matroid.Typeclass.Defaults as D
  
import Data.Set (Set)
import qualified Data.Set as S
    

{- | abstract matroid data type with elements of a given type

  Its purpose is to hide the underlying type from the type system.
  The records resemble the typeclass Matroid, where everything except for the
  groundset has been placed inside the Maybe-Monad. A value of Nothing
  indicates that the default implementation of the typeclass should be used.
  
-}
data AMatroid a = WMatroid {
  {--- I. ---}
    w_groundset :: Set a 
  , w_showName :: Maybe (String)
  , w_rk :: Maybe (Set a -> Int)
  , w_indep :: Maybe (Set a -> Bool)
  , w_basis :: Maybe (Set a -> Set a)
  , w_cl :: Maybe (Set a -> Set a)
  {--- II. ---}
  , w_abstract :: Maybe (AMatroid a)
  , w_dual :: Maybe (AMatroid a)
  , w_restriction :: Maybe (Set a -> AMatroid a)
  , w_contraction :: Maybe (Set a -> AMatroid a)
  {--- III. ---}
  ,  w_loops ::  Maybe (Set a)
  ,  w_coRk :: Maybe (Set a -> Int)
  ,  w_coloops ::  Maybe (Set a)
}

instance Show a => Show (AMatroid a) where
  show x = maybe defShow id $ w_showName x
    where defShow = "WMatroid (" ++ show (w_groundset x) ++ ") (...)"
    


-- | defaults for WMatroid
wrappedMatroid = WMatroid {
  {--- I. ---}
    w_groundset = S.empty
  , w_showName = Nothing
  , w_rk = Nothing
  , w_indep = Nothing
  , w_basis = Nothing
  , w_cl = Nothing
  {--- II. ---}
  , w_abstract = Nothing
  , w_dual = Nothing
  , w_restriction = Nothing
  , w_contraction = Nothing
  {--- III. ---}
  , w_loops = Nothing
  , w_coRk = Nothing
  , w_coloops = Nothing
}

-- | returns the restriction of a given matroid
{-
restriction :: AMatroid a {- ^ input matroid -} -> Set a {- ^ restriction of ground set -} -> AMatroid a
restriction m x0 = WrappedMatroid {
  w_groundset = e
rk (RMatroid m _) = rk m
indep (RMatroid m _) = indep m
basis (RMatroid m _) = basis m
cl (RMatroid m e) = (S.intersection e) . cl m
loops (RMatroid m e) = S.intersection e $ loops m
coRk (RMatroid m e) x = D.coRk (rk m) e x
coloops (RMatroid m e) = D.coloops (rk m) e
  } where e = x0 `S.intersection` (w_groundset m)
  -}