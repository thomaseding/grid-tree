{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Prim.BoundedCollection (
    BoundedHandle,
    BoundedCollection,
    makeBoundedCollection,
    getCollectionBoundary,
) where

import Data.GridTree.Geometry
import Data.GridTree.Prim.GridCollection (GridCollection(..))
import Data.GridTree.Prim.DisjointCollection


--------------------------------------------------------------------------------


newtype BoundedHandle = BoundedHandle DisjointHandle


data BoundedCollection = BoundedCollection (Grid 'Absolute) DisjointCollection


makeBoundedCollection :: Grid 'Absolute -> BoundedCollection
makeBoundedCollection grid = BoundedCollection grid makeDisjointCollection


getCollectionBoundary :: BoundedCollection -> Grid 'Absolute
getCollectionBoundary (BoundedCollection boundary _) = boundary


instance GridCollection BoundedCollection BoundedHandle where
    gridCollectionAdd g (BoundedCollection boundary disjointCollection) = let
        Grid p1 p2 = g
        i1 = intersectsPoint boundary p1
        i2 = intersectsPoint boundary p2
        in case i1 && i2 of
            False -> Nothing
            True -> case gridCollectionAdd g disjointCollection of
                Nothing -> Nothing
                Just (disjointCollection', disjointHandle) -> let
                    boundedCollection = BoundedCollection boundary disjointCollection'
                    boundedHandle = BoundedHandle disjointHandle
                    in Just (boundedCollection, boundedHandle)
    gridCollectionGrid (BoundedCollection _ collection) (BoundedHandle handle) = gridCollectionGrid collection handle
    gridCollectionHandles (BoundedCollection _ collection) = map BoundedHandle $ gridCollectionHandles collection


