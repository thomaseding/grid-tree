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

module Data.GridTree.Render (
    CutDescription(..),
    CutContext(..),
    CutResult(..),
    GridRender(..),
) where

import Data.GridTree.Geometry
import Data.GridTree.Monad


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


renderCutDescM :: GridHandle s -> CutDescription a -> GridMonad s (CutResult a)
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
    gridRender boundary cutDesc = runGridMonad boundary $ do
        rootHandle <- getRootHandle
        renderCutDescM rootHandle cutDesc


