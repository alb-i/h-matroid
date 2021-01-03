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

import Data.Map (Map)
import qualified Data.Map as M

import Data.Matroid.Typeclass

-- | data type representing the cycle matroid (aka. polygon matroid) of a (multi-)graph
data GraphicMatroid v a = 
    MG (Set a) 
       -- ^ ground set of M(G) = E for a (multi-)graph G = (V,E)
       (a -> (v,v)) 
       {- ^ map that maps the edge e to the set of its incident vertices {u,v}; 
            the order is ignored, and {v} is represented by (v,v) -}

-- | data type to keep track of forrests in a (multi-)graph
data Forrest v a = F Int {- ^ fresh component id counter -}
                     (Map v Int) {- ^ tracks which vertex belongs to which component -}
                     (Map Int (Set a)) {- ^ tracks which edges belong to which component -}
            deriving (Show, Eq, Ord)

-- | obtain an empty forrest
emptyForrest :: Forrest v a
emptyForrest = F 1 M.empty M.empty

{- | Takes a forrest and tries to add another edge to it.

 If possible (Right), then it returns the forrest with the edge added 
 otherwise (Left) returns the component with a cycle after adding e.
 Please note that for a result Left x, the set x contains a cycle, but it
 is not necessarily a cycle itself. (It's a cycle with trees on it)
-}
insertEdgeOrGetCycleComponent :: (Ord v, Ord a) => 
                        Forrest v a {- ^ forrest to insert into / find the cycle -} 
                     -> a {- ^ name of the edge -}
                     -> (v,v) {- ^ incidence tuple of the edge; (v,v) represents a loop -} 
                     -> Either (Set a) (Forrest v a)
insertEdgeOrGetCycleComponent (F n c t) e (u,v) -- e is a non-loop edge
           | u == v =  Left $ S.singleton e -- a loop is a single edge cycle
           | not (udef || vdef) =           -- e is a new single-edge tree component
                                  let n1 = n + 1 
                                      c1 = M.insert u n $ M.insert v n c
                                      t1 = M.insert n (S.singleton e) t
                                   in Right $ F n1 c1 t1
            -- at this point, at least udef or vdef is True
           | uc == vc =                     -- this edge closes a loop with the tree; udef==vdef==True
                         let Just cid = uc
                             Just comp = M.lookup cid t
                          in Left $ S.insert e comp
           | udef && vdef =                 -- the edge e connects two components of the forrest
                            let Just uid = uc
                                Just vid = vc
                                Just ut = M.lookup uid t
                                Just vt = M.lookup vid t
                                prj xid 
                                    | xid == vid = uid -- map the component id of v to u
                                    | otherwise = xid
                                c1 = M.map prj c 
                                uvt = S.insert e $ ut `S.union` vt
                                t1 = M.insert uid uvt $ M.delete vid t
                             in Right $ F n c1 t1
            -- at this point, either vdef or udef is True, the other is False
           | vdef = insertEdgeOrGetCycleComponent (F n c t) e (v,u) -- bounce to next case
           | otherwise =  -- e connects the component of u with the new vertex v
                    let Just uid = uc
                        Just ut = M.lookup uid t
                        c1 = M.insert v uid c
                        ut1 = S.insert e ut
                        t1 = M.insert uid ut1 t
                     in Right $ F n c1 t1
           where uc = M.lookup u c
                 vc = M.lookup v c
                 udef = uc /= Nothing
                 vdef = vc /= Nothing

instance (Ord a, Ord v) => Matroid (GraphicMatroid v) a where
    groundset (MG e _) = e
    -- | A set of edges of G=(V,E) is independent, if it contains no cycle. 
    indep (MG _ inc) x = result
         where (result,_) = S.foldl' step (True, emptyForrest) x
               step (False, f) _ = (False, f) -- propagate failure
               step (True, f)  e = maybeContinue f $ insertEdgeOrGetCycleComponent f e $ inc e
               maybeContinue f (Left _) = (False, f) -- edge e closes a cycle
               maybeContinue _ (Right f) = (True, f) -- edge e added to the forrest f
    -- | determine a spanning forrest of the vertices incident with the edges x
    basis (MG _ inc) x = M.foldl' S.union S.empty component_map
         where F _ _ component_map = S.foldl' step emptyForrest x 
               step f e = doContinue f $ insertEdgeOrGetCycleComponent f e $ inc e
               doContinue f (Left _) = f -- edge e closes a cycle, continue with previous forrest
               doContinue _ (Right f) = f -- edge e added to the forrest f
    -- | count the size while determining the spanning forrest
    rk (MG _ inc) x = result
       where (_,result) = S.foldl' step (emptyForrest, 0) x 
             step (f,r) e = doContinue f r $ insertEdgeOrGetCycleComponent f e $ inc e
             doContinue f r (Left _) = (f,r) -- edge e closes a cycle, continue with previous forrest
             doContinue _ r (Right f) = (f,r+1) -- edge e added to the forrest f
    -- | determine a spanning forrest of x, then add all elements from e\x that are either loops or stay within a single component
    cl (MG e inc) x = x `S.union` cx
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

-- | constructs a GraphicMatroid from a set of (abstract) edges and the incident-vertex map
fromGraph :: Ord a => Set a -- ^ set of edges of the (multi-)graph
                   -> (a -> (v,v))
                   {- ^ map that maps the edge e to the set of its incident vertices {u,v}; 
                        the order is ignored, and {v} is represented by (v,v) -}
                   -> GraphicMatroid v a
fromGraph = MG
