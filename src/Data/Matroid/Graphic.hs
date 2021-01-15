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
  , namedFromGraph
  , fromGraph'
  , mK
  ) where 

import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Map as M

import Data.Matroid.Typeclass
import Data.Matroid.Graphic.Internal

-- | data type representing the cycle matroid (aka. polygon matroid) of a (multi-)graph
data GraphicMatroid v a = 
    MG String
       -- ^ given name of the matroid
       (Set a) 
       -- ^ ground set of M(G) = E for a (multi-)graph G = (V,E)
       (a -> (v,v)) 
       {- ^ map that maps the edge e to the set of its incident vertices {u,v}; 
            the order is ignored, and {v} is represented by (v,v) -}
         

instance Show a => Show (GraphicMatroid v a) where
    show (MG name _ _) = name

instance (Ord a, Ord v, Show a) => Matroid (GraphicMatroid v) a where
    groundset (MG _ e _) = e
    showName (MG name _ _) = name
    -- | A set of edges of G=(V,E) is independent, if it contains no cycle. 
    indep (MG _ _ inc) x = result
         where (result,_) = S.foldl' step (True, emptyForrest) x
               step (False, f) _ = (False, f) -- propagate failure
               step (True, f)  e = maybeContinue f $ insertEdgeOrGetCycleComponent f e $ inc e
               maybeContinue f (Left _) = (False, f) -- edge e closes a cycle
               maybeContinue _ (Right f) = (True, f) -- edge e added to the forrest f
    -- | determine a spanning forrest of the vertices incident with the edges x
    basis (MG _ _ inc) x = M.foldl' S.union S.empty component_map
         where F _ _ component_map = S.foldl' step emptyForrest x 
               step f e = doContinue f $ insertEdgeOrGetCycleComponent f e $ inc e
               doContinue f (Left _) = f -- edge e closes a cycle, continue with previous forrest
               doContinue _ (Right f) = f -- edge e added to the forrest f
    -- | count the size while determining the spanning forrest
    rk (MG _ _ inc) x = result
       where (_,result) = S.foldl' step (emptyForrest, 0) x 
             step (f,r) e = doContinue f r $ insertEdgeOrGetCycleComponent f e $ inc e
             doContinue f r (Left _) = (f,r) -- edge e closes a cycle, continue with previous forrest
             doContinue _ r (Right f) = (f,r+1) -- edge e added to the forrest f
    -- | determine a spanning forrest of x, then add all elements from e\x that are either loops or stay within a single component
    cl (MG _ e inc) x = x `S.union` cx
         where F _ component_map _ = S.foldl' step emptyForrest x 
               step f g = doContinue f $ insertEdgeOrGetCycleComponent f g $ inc g
               doContinue f (Left _) = f -- edge e closes a cycle, continue with previous forrest
               doContinue _ (Right f) = f -- edge e added to the forrest f
               cx = S.filter inClosure $ S.difference e x
               inClosure y = let (u,v) = inc y
                                 loop = u == v
                                 uc = M.lookup u component_map
                                 vc = M.lookup v component_map
                                 single_component = uc == vc && (uc /= Nothing)
                             in loop || single_component

-- | constructs a 'GraphicMatroid' from a set of (abstract) edges and the incident-vertex map
fromGraph :: (Ord a, Show a) => Set a -- ^ set of edges of the (multi-)graph
                   -> (a -> (v,v))
                   {- ^ map that maps the edge e to the set of its incident vertices {u,v}; 
                        the order is ignored, and {v} is represented by (v,v) -}
                   -> GraphicMatroid v a
fromGraph e = namedFromGraph ("fromGraph (" ++ show e ++ ") (incidence)") e

-- | constructs an unnamed 'GraphicMatroid' from a set of (abstract) edges and the incident-vertex map
fromGraph' :: (Ord a) => Set a -- ^ set of edges of the (multi-)graph
                   -> (a -> (v,v))
                   {- ^ map that maps the edge e to the set of its incident vertices {u,v}; 
                        the order is ignored, and {v} is represented by (v,v) -}
                   -> GraphicMatroid v a
fromGraph' = namedFromGraph "M(G)"

-- | constructs a named 'GraphicMatroid' from a set of (abstract) edges and the incident-vertex map
namedFromGraph :: Ord a => 
                      String
                   -- ^ name of the matroid  
                   -> Set a 
                   -- ^ set of edges of the (multi-)graph
                   -> (a -> (v,v))
                   {- ^ map that maps the edge e to the set of its incident vertices {u,v}; 
                        the order is ignored, and {v} is represented by (v,v) -}
                   -> GraphicMatroid v a
namedFromGraph = MG


-- | constructs the graphic matroid M(K_n) where K_n is the complete graph on n vertices
mK :: Int -- ^ n = number of vertices in K_n
      -> GraphicMatroid Int (Int,Int)
mK n = namedFromGraph name e id
  where e = S.fromList [(u,v) | u <- [1..n]
                              , v <- [1..n]
                              , u < v]
        name = "mK " ++ show n