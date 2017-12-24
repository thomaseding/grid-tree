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


framedText :: Grid 'Absolute -> Doc ann -> (SimpleDocStream ann -> Text) -> Text
framedText frame doc renderer = let
    (totalWidth, totalHeight) = dimensionsOf frame
    layoutOptions = LayoutOptions {
        layoutPageWidth = AvailablePerLine totalWidth 1 }
    docStream = layoutSmart layoutOptions doc
    docStream' = confine 0 0 docStream
    --
    confine !currWidth !currHeight = \case
        SFail -> SFail
        SEmpty -> SEmpty
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
            False -> case currWidth + indentLevel > totalWidth of
                True -> confine totalWidth (currHeight + 1) rest
                False -> SLine indentLevel $ confine (currWidth + indentLevel) (currHeight + 1) rest
        SAnnPush ann rest -> SAnnPush ann $ confine currWidth currHeight rest
        SAnnPop rest -> SAnnPop $ confine currWidth currHeight rest
    --
    in renderer docStream'


multiFramedText :: CutResult (Doc ann) -> (SimpleDocStream ann -> Text) -> Text
multiFramedText = undefined




