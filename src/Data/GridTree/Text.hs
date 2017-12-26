{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.GridTree.Text (
    module Data.GridTree.Text,
) where


import Data.GridTree.Geometry
import Data.GridTree.Render
import Data.Text
import Data.Text.Prettyprint.Doc


--------------------------------------------------------------------------------


-- XXX:
-- Perhaps a better way of doing all this is to handle the console offsets as annotations.
-- (Perhaps also the frame cutoffs should also be left for the renderer?)
-- Then the renderer can do all the offsetting (in addition to other ANSI stuff).
-- This is probably MUCH easier to implement and does what I need in the end anyway.


framedText :: Grid 'Absolute -> Doc ann -> SimpleDocStream ann
framedText frame doc = let
    (totalWidth, totalHeight) = dimensionsOf frame
    layoutOptions = LayoutOptions {
        layoutPageWidth = AvailablePerLine totalWidth 1 }
    docStream = layoutSmart layoutOptions doc
    docStream' = shiftVertical $ shiftHorizontal $ confine 0 0 docStream
    --
    confine !currWidth !currHeight = \case
        x @ SFail -> x
        x @ SEmpty -> x
        SChar c rest -> case currWidth == totalWidth of
            False -> SChar c $ confine (currWidth + 1) currHeight rest
            True -> confine currWidth currHeight rest
        SText !textLength text rest -> let
            availWidth = totalWidth - currWidth
            in case availWidth of
                0 -> confine totalWidth currHeight rest
                _ -> case textLength <= availWidth of
                    True -> SText textLength text $ confine (currWidth + textLength) currHeight rest
                    False -> let
                        text' = Data.Text.take availWidth text
                        in SText availWidth text' $ confine totalWidth currHeight rest
        -- TODO:
        -- If indent level is not guaranteed to be in spaces, map this to:
        --  (SLine 0 (SText indentLevel spaces rest))
        SLine !indentLevel rest -> case currHeight == totalHeight of
            True -> SEmpty
            False -> case indentLevel > totalWidth of
                True -> confine totalWidth (currHeight + 1) rest
                False -> SLine indentLevel $ confine indentLevel (currHeight + 1) rest
        SAnnPush ann rest -> SAnnPush ann $ confine currWidth currHeight rest
        SAnnPop rest -> SAnnPop $ confine currWidth currHeight rest
    --
    -- TODO:
    -- May need to do something special for the initial line.
    -- This might be something like injecting
    --  (SLine totalWidth rest)
    --  or
    --  (SText totalWidth spaces rest)
    shiftHorizontal = \case
        x @ SFail -> x
        x @ SEmpty -> x
        SChar c rest -> SChar c $ shiftHorzintal rest
        SText !textLength text rest -> SText textLength text $ shiftHorizontal rest
        SLine !indentLevel rest -> SLine (indentLevel + totalWidth) $ shiftHorizontal rest
        SAnnPush ann rest -> SAnnPush ann $ shiftHorizontal rest
        SAnnPop rest -> shiftHorizontal rest
    --
    shiftVertical = shiftVertical' totalHeight
    shiftVertical' rest = \case
        0 -> rest
        n -> SLine 0 $ shiftVertical' (n - 1) rest
    --
    in docStream'


zipStreamsMany :: [SimpleDocStream ann] -> SimpleDocStream ann
zipStreamsMany = xxx


tightenStream :: SimpleDocStream ann -> SimpleDocStream ann
tightenStream = \case
    x @ SFail -> x
    x @ SEmpty -> x
    SChar c rest -> tightenStream $ SText 1 (singleton c) rest
    SText !l1 t1 (SText !l2 t2 rest) = let
        l3 = l1 + l2
        t3 = t1 `append` t2
        in tightenStream l3 t3 rest
    SLine !indentLevel rest -> SLine indentLevel $ tightenStream rest
    SAnnPush ann -> SAnnPush ann $ tightenStream rest
    SAnnPop -> SAnnPop $ tightenStream rest


zipStreams :: SimpleDocStream ann -> SimpleDocStreamAnn -> SimpleDocStream ann
zipStreams = zipStreams' `on` tightenStream


zipStreams' :: SimpleDocStream ann -> SimpleDocStreamAnn -> SimpleDocStream ann
zipStreams' SFail _ = SFail
zipStreams' _ SFail = SFail
zipStreams' SEmpty _ = SEmpty
zipStreams' _ SEmpty = SEmpty
zipStreams' SChar{} _ = error "Internal logic error."
zipStreams' _ SChar{} = error "Internal logic error."
zipStreams' = undefined -- Some combination of Text.take and Text.append. Complicated by annotations.


multiFramedText :: CutResult (Doc ann) -> SimpleDocStream ann
multiFramedText = undefined -- Walk tree to obtiain streams and then zip them.




