{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Data.GridTree.GridTree (
    GridHandle,
    GridTree,
    runGridTree,
    getRootHandle,
    boundaryOf,
    childrenOf,
    AddChild(..),
    GridRender(..),
    CutDescription(..),
    CutContext(..),
    CutResult(..),
) where

import Control.Monad.State.Strict
import Data.GridTree.Geometry
import Data.GridTree.Prim.HierarchyCollection


--------------------------------------------------------------------------------


newtype GridHandle :: * -> * where
    GridHandle :: HierarchyHandle -> GridHandle s


newtype GridTree :: * -> * -> * where
    GridTree :: {
        unGridTree :: State HierarchyCollection a }
        -> GridTree s a
    deriving (Functor, Applicative, Monad)


runGridTree :: forall a. Grid 'Absolute -> (forall s. GridTree s a) -> a
runGridTree grid action = let
    st = makeHierarchyCollection grid
    in (`evalState` st) $ unGridTree action


gridTreePut :: HierarchyCollection -> GridTree s ()
gridTreePut = GridTree . put


gridTreeGet :: GridTree s HierarchyCollection
gridTreeGet = GridTree get


boundaryOf :: GridHandle s -> GridTree s (Grid 'Absolute)
boundaryOf (GridHandle handle) = do
    coll <- gridTreeGet
    case getHierarchyGrid coll handle of
        Nothing -> error "GridTree: Internal logic error."
        Just grid -> return grid


getRootHandle :: GridTree s (GridHandle s)
getRootHandle = return $ GridHandle hierarchyRoot


childrenOf :: GridHandle s -> GridTree s [GridHandle s]
childrenOf (GridHandle handle) = do
    coll <- gridTreeGet
    let children = getHierarchyChildrenOf coll handle
    return $ map GridHandle children


--------------------------------------------------------------------------------


addChildAbsolute :: GridHandle s -> Grid 'Absolute -> GridTree s (Maybe (GridHandle s))
addChildAbsolute (GridHandle parent) grid = case isGridEmpty grid of
    True -> return Nothing
    False -> do
        coll <- gridTreeGet
        case addHierarchyGrid grid parent coll of
            Nothing -> return Nothing
            Just (coll', childHandle) -> do
                gridTreePut coll'
                return $ Just $ GridHandle childHandle


addChildRelative :: GridHandle s -> Grid 'Relative -> GridTree s (Maybe (GridHandle s))
addChildRelative parent grid = do
    boundary <- boundaryOf parent
    let grid' = grid `inReferenceTo` boundary
    addChild parent grid'


addChildCut :: GridHandle s -> GridCut -> GridTree s (Maybe (GridHandle s, GridHandle s))
addChildCut parentHandle gridCut = do 
    parentGrid <- boundaryOf parentHandle
    let (lowGrid, highGrid) = gridCut `inReferenceTo` parentGrid
    addChild parentHandle lowGrid >>= \case
        Nothing -> return Nothing
        Just lowHandle -> addChild parentHandle highGrid >>= \case
            Nothing -> return Nothing
            Just highHandle -> return $ Just (lowHandle, highHandle)


class AddChild (s :: *) (gridLike :: *) (handleResult :: *) | s gridLike -> handleResult, handleResult -> s where
    addChild :: GridHandle s -> gridLike -> GridTree s handleResult


instance AddChild s (Grid 'Absolute) (Maybe (GridHandle s)) where
    addChild = addChildAbsolute


instance AddChild s (Grid 'Relative) (Maybe (GridHandle s)) where
    addChild = addChildRelative


instance AddChild s GridCut (Maybe (GridHandle s, GridHandle s)) where
    addChild = addChildCut


--------------------------------------------------------------------------------


data CutDescription :: * -> * where
    CutLeaf :: a -> CutDescription a
    CutBranch :: GridCut -> CutDescription a -> CutDescription a -> CutDescription a
    deriving (Show, Eq, Ord, Foldable, Functor, Traversable)


data CutContext :: * where
    CutContext :: {
        _cutContextGrid :: Grid 'Absolute,
        _cutContextCut :: GridCut }
        -> CutContext
    deriving (Show, Eq, Ord)


data CutResult :: * -> * where
    LeafGrid :: Grid 'Absolute -> a -> CutResult a
    InvalidCut :: CutContext -> CutResult a
    ValidCut :: CutContext -> CutResult a -> CutResult a -> CutResult a
    deriving (Show, Eq, Ord, Foldable, Functor, Traversable)


--------------------------------------------------------------------------------


renderCutDescM :: GridHandle s -> CutDescription a -> GridTree s (CutResult a)
renderCutDescM currHandle desc = do
    currGrid <- boundaryOf currHandle
    case desc of
        CutLeaf value -> return $ LeafGrid currGrid value
        CutBranch gridCut lowDesc highDesc -> let
            cutContext = CutContext {
                _cutContextGrid = currGrid,
                _cutContextCut = gridCut }
            in addChild currHandle gridCut >>= \case
                Nothing -> return $ InvalidCut cutContext
                Just (lowHandle, highHandle) -> do
                    lowResult <- renderCutDescM lowHandle lowDesc
                    highResult <- renderCutDescM highHandle highDesc
                    return $ ValidCut cutContext lowResult highResult


--------------------------------------------------------------------------------


class GridRender (gridIn :: *) (gridOut :: *) | gridIn -> gridOut where
    gridRender :: Grid 'Absolute -> gridIn -> gridOut


instance GridRender (CutDescription a) (CutResult a) where
    gridRender boundary cutDesc = runGridTree boundary $ do
        rootHandle <- getRootHandle
        renderCutDescM rootHandle cutDesc


