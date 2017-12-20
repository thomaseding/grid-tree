{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Show (
    module Data.GridTree.Show,
) where


import Data.GridTree.Geometry (Position(..), Point(..), Grid(..), inReferenceTo)
import Data.List (foldl', intercalate)
import Data.Map (Map)

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


test :: IO ()
test = do
    let lower = Point (-2) (2)
        upper = Point 3 9
        grid = Grid lower upper :: Grid 'Relative
        toChar = \case
            Nothing -> ' '
            Just () -> 'X'
        str = showGrid toChar grid
    putStrLn str


