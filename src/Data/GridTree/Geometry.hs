{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.GridTree.Geometry (
    Position(..),
    Point(..),
    Grid(..),
    Rounding(..),
    CutAxis(..),
    GridCut(..),
    InReferenceTo(..),
    ToRelative(..),
    Move(..),
    isGridEmpty,
    intersectsPoint,
    intersectsGrid,
) where


import Data.Function (on)
import Data.Ratio


--------------------------------------------------------------------------------


data Position :: * where
    Absolute :: Position
    Relative :: Position
    deriving (Show, Eq, Ord)


data Point :: Position -> * where
    Point :: Int -> Int -> Point a
    deriving (Show, Eq, Ord)


data Grid :: Position -> * where
    Grid :: Point a -> Point a -> Grid a
    deriving (Show, Eq, Ord)


data Rounding :: * where
    RoundDown :: Rounding
    RoundUp :: Rounding
    deriving (Show, Eq, Ord)


data CutAxis :: * where
    CutX :: CutAxis
    CutY :: CutAxis
    deriving (Show, Eq, Ord)


data GridCut :: * where
    GridCut :: {
        _cutAxis :: CutAxis,
        _cutRounding :: Rounding,
        _cutRatio :: Rational,
        _cutMin :: Maybe Int,
        _cutMax :: Maybe Int }
        -> GridCut
    deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------


liftPoint2 :: (Int -> Int -> Int) -> Point p -> Point p -> Point p
liftPoint2 f (Point x y) (Point x' y') = Point (f x x') (f y y')


vectorAdd :: Point a -> Point a -> Point a
vectorAdd = liftPoint2 (+)


vectorSubtract :: Point a -> Point a -> Point a
vectorSubtract = liftPoint2 (-)


castPoint :: Point p -> Point p'
castPoint (Point x y) = Point x y


castGrid :: Grid p -> Grid p'
castGrid (Grid x y) = (Grid `on` castPoint) x y


intersectsPoint :: Grid p -> Point p -> Bool
intersectsPoint (Grid (Point x1 y1) (Point x2 y2)) (Point px py)
    =       x1 <= px && px < x2 
        &&  y1 <= py && py < y2


intersectsGrid :: Grid p -> Grid p -> Bool
intersectsGrid g (Grid p1 p2) = let
    Point x2 y2 = p2
    p3 = Point (x2 - 1) (y2 - 1)
    in intersectsPoint g p1 || intersectsPoint g p3


dimensionsOf :: (ToRelative (Grid p) Grid) => Grid p -> (Int, Int)
dimensionsOf grid = let
    Grid _ upper = toRelative grid
    Point w h = upper
    in (w, h)


isGridEmpty :: (ToRelative (Grid p) Grid) => Grid p -> Bool
isGridEmpty grid = let
    (w, h) = dimensionsOf grid
    in w > 0 && h > 0


roundImpl :: Int -> Rational -> Int
roundImpl offset x = let
    numer = fromInteger $ numerator x
    denom = fromInteger $ denominator x
    in case denom of
        1 -> numer
        _ -> (numer `div` denom) + offset


roundDown :: Rational -> Int
roundDown = roundImpl 0


roundUp :: Rational -> Int
roundUp = roundImpl 1


--------------------------------------------------------------------------------


class ToRelative (a :: *) (b :: Position -> *) | a -> b where
    toRelative :: a -> b 'Relative


instance ToRelative (Grid 'Relative) Grid where
    toRelative = id


instance ToRelative (Grid 'Absolute) Grid where
    toRelative (Grid lower upper) = let
        lower' = Point 0 0
        upper' = castPoint $ upper `vectorSubtract` lower
        in Grid lower' upper'


--------------------------------------------------------------------------------


class Move (grid :: *) (point :: *) where
    move :: grid -> point -> grid


instance Move (Grid 'Relative) (Point 'Relative) where
    move (Grid lower upper) delta = let
        lower' = lower `vectorAdd` delta
        upper' = upper `vectorAdd` delta
        in Grid lower' upper'


instance Move (Grid 'Absolute) (Point 'Relative) where
    move g = castGrid . move (castGrid g :: Grid 'Relative)


instance Move (Grid 'Absolute) (Point 'Absolute) where
    move g p = castGrid $ move (toRelative g) (castPoint p :: Point 'Relative)


--------------------------------------------------------------------------------


class InReferenceTo (a :: *) (b :: *) (c :: *) | a b -> c where
    inReferenceTo :: a -> b -> c


instance InReferenceTo (Point 'Relative) (Point 'Absolute) (Point 'Absolute) where
    inReferenceTo relativePoint absolutePoint = castPoint relativePoint `vectorAdd` absolutePoint


instance InReferenceTo (Grid 'Relative) (Point 'Absolute) (Grid 'Absolute) where
    inReferenceTo (Grid lowerR upperR) lowerA = let
        lowerR' = inReferenceTo lowerR lowerA
        upperR' = inReferenceTo upperR lowerA
        in Grid lowerR' upperR'


instance InReferenceTo (Grid 'Relative) (Grid 'Absolute) (Grid 'Absolute) where
    inReferenceTo gridR (Grid lowerA _) = inReferenceTo gridR lowerA


cutGridX :: Int -> Grid 'Absolute -> (Grid 'Absolute, Grid 'Absolute)
cutGridX cutIndex (Grid (Point xl yl) (Point xu yu)) = let
    gl = Grid (Point xl yl) (Point cutIndex yu)
    gu = Grid (Point cutIndex yl) (Point xu yu)
    in (gl, gu)


cutGridY :: Int -> Grid 'Absolute -> (Grid 'Absolute, Grid 'Absolute)
cutGridY cutIndex (Grid (Point xl yl) (Point xu yu)) = let
    gl = Grid (Point xl yl) (Point xu cutIndex)
    gu = Grid (Point xl cutIndex) (Point xu yu)
    in (gl, gu)


instance InReferenceTo GridCut (Grid 'Absolute) (Grid 'Absolute, Grid 'Absolute) where
    inReferenceTo gridCut absoluteProto = let
        Grid (Point xOffset yOffset) _ = absoluteProto
        (width, height) :: (Int, Int) = dimensionsOf $ toRelative absoluteProto
        (dim, offset, cutter) = case cutAxis of
            CutX -> (width, xOffset, cutGridX)
            CutY -> (height, yOffset, cutGridY)
        --
        cutAxis :: CutAxis = _cutAxis gridCut
        cutRounding :: Rounding = _cutRounding gridCut
        cutRatio :: Rational = max 0 $ min 1 $ _cutRatio gridCut
        cutMin :: Int = max 0 $ min dim $ maybe 0 id $ _cutMin gridCut
        cutMax :: Int = max 0 $ min dim $ maybe dim id $ _cutMax gridCut
        --
        cutIndex :: Int = let
            rounder = case cutRounding of
                RoundDown -> roundDown
                RoundUp -> roundUp
            cast = fromIntegral :: Int -> Rational
            dim' = cast dim
            index = rounder $ cutRatio * dim'
            in offset + (max cutMin $ min cutMax $ index)
        --
        in cutter cutIndex absoluteProto


