{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Prim.UnstructuredCollection (
    UnstructuredHandle,
    UnstructuredCollection,
    makeUnstructuredCollection,
) where

import Data.GridTree.Geometry
import Data.GridTree.Prim.GridCollection (GridCollection(..))


--------------------------------------------------------------------------------


newtype UnstructuredHandle = UnstructuredHandle Int


data UnstructuredCollection = UnstructuredCollection [Grid 'Absolute]


makeUnstructuredCollection :: UnstructuredCollection
makeUnstructuredCollection = UnstructuredCollection []


instance GridCollection UnstructuredCollection UnstructuredHandle where
    gridCollectionAdd g (UnstructuredCollection gs) = let
        collection = UnstructuredCollection (g : gs)
        handle = UnstructuredHandle $ length gs
        in Just (collection, handle)
    gridCollectionGrid (UnstructuredCollection gs) (UnstructuredHandle idx) = case drop idx gs of
        g : _ -> Just g
        [] -> Nothing
    gridCollectionHandles (UnstructuredCollection gs) = let
        indices = [0 .. length gs - 1]
        in map UnstructuredHandle indices


