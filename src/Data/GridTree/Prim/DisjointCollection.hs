{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Prim.DisjointCollection (
    DisjointHandle,
    DisjointCollection,
    makeDisjointCollection,
) where

import Data.GridTree.Geometry
import Data.GridTree.Prim.GridCollection (GridCollection(..), gridCollectionGrids)
import Data.GridTree.Prim.UnstructuredCollection


--------------------------------------------------------------------------------


newtype DisjointHandle = DisjointHandle UnstructuredHandle


newtype DisjointCollection = DisjointCollection UnstructuredCollection


makeDisjointCollection :: DisjointCollection
makeDisjointCollection = DisjointCollection makeUnstructuredCollection


instance GridCollection DisjointCollection DisjointHandle where
    gridCollectionAdd g (DisjointCollection uCollection) = let
        grids = gridCollectionGrids uCollection
        in case any (intersectsGrid g) grids of    
            True -> Nothing
            False -> case gridCollectionAdd g uCollection of
                Nothing -> Nothing
                Just (uCollection', uHandle) -> let
                    disjointCollection = DisjointCollection uCollection'
                    disjointHandle = DisjointHandle uHandle
                    in Just (disjointCollection, disjointHandle)
    gridCollectionGrid (DisjointCollection collection) (DisjointHandle handle) = gridCollectionGrid collection handle
    gridCollectionHandles (DisjointCollection collection) = map DisjointHandle $ gridCollectionHandles collection


