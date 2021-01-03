{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Data.Matroid.Graphic
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides implementations of graphic matroids.

-}

module Data.Matroid.Graphic 
  ( GraphicMatroid
  , fromGraph
  ) where 

import Data.Set (Set)
import qualified Data.Set as S

import Data.Matroid.Typeclass
import Data.Matroid.Internal

-- | data type representing the cycle matroid (aka. polygon matroid) of a (multi-)graph
data GraphicMatroid v a = 
    MG (Set a) -- ^ ground set of M(G) = E for a (multi-)graph G = (V,E)
       (a -> Set v) -- ^ map that maps the edge e to the set of its incident vertices {u,v}; u == v is possible (loop)
    deriving (Eq, Ord)
    

instance Ord a => Matroid (GraphicMatroid v) a where
    groundset (MG e _) = e
       
-- | constructs a GraphicMatroid from a set of (abstract) edges and the incident-vertex map
fromGraph :: Ord a => Set a -- ^ set of edges of the (multi-)graph
                   -> (a -> Set v) -- ^ map that maps edges to their incident vertex set {u,v}
                   -> GraphicMatroid v a
fromGraph = MG
