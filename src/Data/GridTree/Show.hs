{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Show (
    module Data.GridTree.Show,
) where


import Data.DList (DList)
import Data.Function (on)
import Data.GridTree.Geometry
import Data.GridTree.Monad
import Data.GridTree.Render (CutDescription(..), CutResult(..), gridRender)
import Data.List (foldl', intercalate)
import Data.Map (Map)

import qualified Data.DList as DList
import qualified Data.Map as Map


--------------------------------------------------------------------------------


type PointMap a = Map (Point 'Absolute) a


fillByAbsoluteGrid :: a -> PointMap a -> Grid 'Absolute -> PointMap a
fillByAbsoluteGrid v pointMap grid = let
    Grid (Point x0 y0) (Point x1 y1) = grid
    xs = [x0 .. x1 - 1]
    ys = [y0 .. y1 - 1]
    points = [Point x y | x <- xs, y <- ys]
    pointMap' = let
        f = \m p -> Map.insert p v m
        in foldl' f pointMap points
    in pointMap'


xComponent :: Point p -> Int
xComponent (Point x _) = x


yComponent :: Point p -> Int
yComponent (Point _ y) = y


showPointMap :: (Maybe a -> Char) -> PointMap a -> String
showPointMap toChar pointMap = let
    points = Map.keys pointMap
    symbols = Map.elems pointMap
    xs = map xComponent points
    ys = map yComponent points
    leastX = foldl' min 0 xs
    leastY = foldl' min 0 ys
    points' = flip map points $ \(Point x y) -> Point (x - leastX) (y - leastY)
    pointMap' = Map.fromList $ zip points' symbols
    in showPointMap' toChar pointMap'


showPointMap' :: (Maybe a -> Char) -> PointMap a -> String
showPointMap' toChar pointMap = let
    points = Map.keys pointMap
    xs = map xComponent points
    ys = map yComponent points
    greatestX = foldl' max 0 xs
    greatestY = foldl' max 0 ys
    cellChar p = toChar $ Map.lookup p pointMap
    rowString y = map (\x -> cellChar $ Point x y) [0 .. greatestX]
    totalString = intercalate "\n" $ map (\y -> rowString y) [0 .. greatestY]
    in totalString


class ShowGrid (index :: *) (grid :: *) where
    showGrid :: (Maybe index -> Char) -> grid -> String


instance ShowGrid () (Grid 'Absolute) where
    showGrid toChar grid = let
        pointMap = fillByAbsoluteGrid () Map.empty grid
        str = showPointMap toChar pointMap
        in str


instance ShowGrid () (Grid 'Relative) where
    showGrid toChar grid = let
        grid' = grid `inReferenceTo` (Point 0 0 :: Point 'Absolute)
        in showGrid toChar grid'


instance ShowGrid Char [Grid 'Absolute] where
    showGrid toChar grids = let
        fills = map flip $ map fillByAbsoluteGrid $ let
            num = ['0'..'9']
            upper = ['A'..'Z']
            lower = ['a'..'z']
            exhausted = repeat '?'
            in num ++ upper ++ lower ++ exhausted
        fills' = zipWith ($) fills grids
        pointMap = foldl' (\x f -> f x) Map.empty fills'
        str = showPointMap toChar pointMap
        in str


cutResultGrids :: CutResult a -> [Grid 'Absolute]
cutResultGrids = DList.toList . cutResultGrids'


cutResultGrids' :: CutResult a -> DList (Grid 'Absolute)
cutResultGrids' = \case
    LeafGrid g _ -> DList.singleton g
    InvalidCut _ -> DList.empty
    ValidCut _ l r -> (DList.append `on` cutResultGrids') l r


instance ShowGrid Char (CutResult a) where
    showGrid toChar cutResult = let
        grids = cutResultGrids cutResult
        in showGrid toChar grids


test1 :: IO ()
test1 = do
    let toChar = \case
            Nothing -> ' '
            Just c -> c
        lowerA = Point 0 0
        upperA = Point 3 3
        gridA = Grid lowerA upperA :: Grid 'Absolute
        lowerB = Point (-1) 0
        upperB = Point 1 5
        gridB = Grid lowerB upperB :: Grid 'Absolute
        grids = [gridA, gridB]
    putStrLn $ showGrid toChar grids


test2 :: IO ()
test2 = do
    let toChar = \case
            Nothing -> ' '
            Just () -> 'X'
        lower = Point 0 0
        upper = Point 3 3
        grid = Grid lower upper :: Grid 'Absolute
    putStrLn $ showGrid toChar grid


test3 :: IO ()
test3 = do
    let toChar = \case
            Nothing -> ' '
            Just c -> c
        lower = Point 0 0
        upper = Point 10 10
        grid = Grid lower upper :: Grid 'Absolute
        cut = GridCut {
            _cutAxis = CutY,
            _cutRounding = RoundDown,
            _cutRatio = 0.5,
            _cutMin = Nothing,
            _cutMax = Nothing }
        cutResult = cut `inReferenceTo` grid
        (lowGrid, highGrid) = cutResult
        gridsA = [lowGrid, highGrid]
        gridsB = reverse gridsA
    print cutResult
    putStrLn $ showGrid toChar gridsA
    putStrLn "--------------"
    putStrLn $ showGrid toChar gridsB


test4 :: IO ()
test4 = do
    let toChar = \case
            Nothing -> ' '
            Just c -> c
        lower = Point 0 0
        upper = Point 10 10
        grid = Grid lower upper :: Grid 'Absolute
        leaf = CutLeaf ()
        cutDesc = leaf
        cutResult = gridRender grid cutDesc
    print cutResult
    putStrLn $ showGrid toChar cutResult


test5 :: IO ()
test5 = do
    let lowerA = Point 0 0
        upperA = Point 10 10
        gridA = Grid lowerA upperA :: Grid 'Absolute
        lowerB = Point 0 0
        upperB = Point 10 9
        gridB = Grid lowerB upperB :: Grid 'Absolute
        grid = runGridMonad gridA $ do
            rootHandle <- getRootHandle
            mKid <- addChild rootHandle gridB
            case mKid of
                Nothing -> error "test failure"
                Just handle -> boundaryOf handle
    print grid


test6 :: IO ()
test6 = do
    let lower = Point 0 0
        upper = Point 10 10
        grid = Grid lower upper :: Grid 'Absolute
        cut = GridCut {
            _cutAxis = CutY,
            _cutRounding = RoundDown,
            _cutRatio = 0.5,
            _cutMin = Nothing,
            _cutMax = Nothing }
        grids = runGridMonad grid $ do
            rootHandle <- getRootHandle
            mKids <- addChild rootHandle cut
            case mKids of
                Nothing -> error "test failure"
                Just (lHandle, rHandle) -> do
                    lGrid <- boundaryOf lHandle
                    rGrid <- boundaryOf rHandle
                    return (lGrid, rGrid)
    print grids


test7 :: IO ()
test7 = do
    let toChar = \case
            Nothing -> ' '
            Just c -> c
        lower = Point 0 0
        upper = Point 10 10
        grid = Grid lower upper :: Grid 'Absolute
        leaf = CutLeaf ()
        cutDesc = let
            cut = GridCut {
                _cutAxis = CutY,
                _cutRounding = RoundDown,
                _cutRatio = 0.5,
                _cutMin = Nothing,
                _cutMax = Nothing }
            in CutBranch cut leaf leaf
        cutResult = gridRender grid cutDesc
    print cutResult
    putStrLn $ showGrid toChar cutResult


test8 :: IO ()
test8 = do
    let toChar = \case
            Nothing -> ' '
            Just c -> c
        lower = Point 0 0
        upper = Point 20 20
        grid = Grid lower upper :: Grid 'Absolute
        leaf = CutLeaf ()
        cutDesc = let
            cut0 = GridCut {
                _cutAxis = CutX,
                _cutRounding = RoundDown,
                _cutRatio = 0.66,
                _cutMin = Nothing,
                _cutMax = Nothing }
            left0 = let
                cut1 = GridCut {
                    _cutAxis = CutY,
                    _cutRounding = RoundDown,
                    _cutRatio = 0.3,
                    _cutMin = Nothing,
                    _cutMax = Nothing }
                left1 = leaf
                right1 = leaf
                in CutBranch cut1 left1 right1
            right0 = let
                cut1 = GridCut {
                    _cutAxis = CutX,
                    _cutRounding = RoundUp,
                    _cutRatio = 0.51,
                    _cutMin = Nothing,
                    _cutMax = Nothing }
                left1 = leaf
                right1 = leaf
                in CutBranch cut1 left1 right1
            in CutBranch cut0 left0 right0
        cutResult = gridRender grid cutDesc
    print cutResult
    putStrLn $ showGrid toChar cutResult


