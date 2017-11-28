{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Prim.GridCollection (
    GridCollection(..),
    gridCollectionGrids,
) where


import Data.GridTree.Geometry
import Data.Maybe (maybeToList)


--------------------------------------------------------------------------------


class GridCollection collection handle | collection -> handle, handle -> collection where
    gridCollectionAdd :: Grid 'Absolute -> collection -> Maybe (collection, handle)
    gridCollectionGrid :: collection -> handle -> Maybe (Grid 'Absolute)
    gridCollectionHandles :: collection -> [handle]


gridCollectionGrids :: (GridCollection collection handle) => collection -> [Grid 'Absolute]
gridCollectionGrids c = maybeToList . gridCollectionGrid c =<< gridCollectionHandles c


