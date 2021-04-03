{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

{-|
Module      : Data.Matroid.Graphic.Internal
Description : 
Copyright   : (c) Immanuel Albrecht, 2020-202x
License     : BSD-3
Maintainer  : mail@immanuel-albrecht.de
Stability   : experimental
Portability : POSIX

This module provides internal helpers used by the graphic matroid
module.

Although it is exported, using anything from this module that is 
not re-exported by another module may (and eventually will) break 
client side code. The main reason for exporting this is so anyone
can inspect internals using haddock; it's a little bit like an
open door policy for code.

-}
module Data.Matroid.Graphic.Internal where

import Data.Set (Set)
import qualified Data.Set as S


import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Maybe

-- | data type to keep track of forrests in a (multi-)graph
data Forrest v a = F Int {- ^ fresh component id counter -}
                     (Map v Int) {- ^ tracks which vertex belongs to which component -}
                     (Map Int (Set a)) {- ^ tracks which edges belong to which component -}
            deriving (Show, Eq, Ord)

-- | obtain an empty forrest
emptyForrest :: Forrest v a
emptyForrest = F 1 M.empty M.empty

{- | Takes a forrest and tries to add another edge to it.

 If possible ('Right'), then it returns the forrest with the edge added 
 otherwise ('Left') returns the component with a cycle after adding @e@.
 Please note that for a result @Left x@, the set @x@ contains a cycle, but it
 is not necessarily a cycle itself. (It's a cycle with trees on it)
-}
insertEdgeOrGetCycleComponent :: (Ord v, Ord a) =>
                        Forrest v a {- ^ forrest to insert into / find the cycle -}
                     -> a {- ^ name of the edge -}
                     -> (v,v) {- ^ incidence tuple of the edge; @(x,x)@ represents a loop around the vertex @x@ -}
                     -> Either (Set a) (Forrest v a)
insertEdgeOrGetCycleComponent (F !n !c !t) e (u,v) -- e is a non-loop edge
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
                 udef = Data.Maybe.isJust uc
                 vdef = Data.Maybe.isJust vc
