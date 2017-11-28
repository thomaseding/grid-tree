{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Prim.HierarchyCollection (
    HierarchyHandle,
    HierarchyCollection,
    makeHierarchyCollection,
    hierarchyRoot,
    getHierarchyGrid,
    addHierarchyGrid,
    getHierarchyChildrenOf,
) where

import Data.GridTree.Geometry
import Data.GridTree.Prim.GridCollection (GridCollection(..))
import Data.GridTree.Prim.BoundedCollection
import Data.Map (Map)

import qualified Data.Map as Map


--------------------------------------------------------------------------------


newtype HierarchyHandle = HierarchyHandle Int
    deriving (Eq, Ord)


data HierarchyCollection = HierarchyCollection {
    _hierarchyGrids :: Map HierarchyHandle BoundedCollection,
    _hierarchyGraph :: Map HierarchyHandle [HierarchyHandle]
}


hierarchyRoot :: HierarchyHandle
hierarchyRoot = HierarchyHandle 0


makeHierarchyCollection :: Grid 'Absolute -> HierarchyCollection
makeHierarchyCollection grid = let
    rootCollection = makeBoundedCollection grid
    graph = Map.empty
    grids = Map.singleton hierarchyRoot rootCollection
    hierarchyCollection = HierarchyCollection {
        _hierarchyGrids = grids,
        _hierarchyGraph = graph }
    in hierarchyCollection


getHierarchyGrid :: HierarchyCollection -> HierarchyHandle -> Maybe (Grid 'Absolute)
getHierarchyGrid hierarchyCollection handle = let
    grids = _hierarchyGrids hierarchyCollection
    in case Map.lookup handle grids of
        Nothing -> Nothing
        Just boundaryCollection -> Just $ getCollectionBoundary boundaryCollection


addHierarchyGrid :: Grid 'Absolute -> HierarchyHandle -> HierarchyCollection -> Maybe (HierarchyCollection, HierarchyHandle)
addHierarchyGrid childGrid parentHandle hierarchyCollection = let
    grids = _hierarchyGrids hierarchyCollection
    in case Map.lookup parentHandle grids of
        Nothing -> Nothing
        Just parentCollection -> case gridCollectionAdd childGrid parentCollection of
            Nothing -> Nothing
            Just (parentCollection', _) -> let
                childCollection = makeBoundedCollection childGrid
                childHandle = HierarchyHandle $ Map.size grids
                grids' = Map.insert parentHandle parentCollection' grids
                grids'' = Map.insert childHandle childCollection grids'
                graph = _hierarchyGraph hierarchyCollection
                graph' = Map.insert parentHandle children' graph
                children = getHierarchyChildrenOf hierarchyCollection parentHandle
                children' = childHandle : children
                hierarchyCollection' = hierarchyCollection { 
                    _hierarchyGrids = grids'',
                    _hierarchyGraph = graph' }
                in Just (hierarchyCollection', childHandle)


getHierarchyChildrenOf :: HierarchyCollection -> HierarchyHandle -> [HierarchyHandle]
getHierarchyChildrenOf coll handle = let
    graph = _hierarchyGraph coll
    children = maybe [] id $ Map.lookup handle graph
    in children


