{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.ViewFrame
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Portable Rectangle type that works with older Gdk versions
--
---------------------------------------------------------------------------------

module Yi.UI.Pango.Rectangle (
    Rectangle(..)
,   newRectangle
,   rectangleWidth
,   rectangleHeight
,   rectangleX
,   rectangleY
,   rectangleReadWidth
,   rectangleReadHeight
,   rectangleReadX
,   rectangleReadY
) where

import Data.GI.Base (new, AttrOp)
import Data.GI.Base.Attributes (AttrOpTag(..))
#ifdef MIN_VERSION_GDK_3_18
import GI.Gdk (Rectangle(..), rectangleWidth, rectangleHeight, rectangleX, rectangleY, rectangleReadWidth, rectangleReadHeight, rectangleReadX, rectangleReadY)
#else
import Data.Int (Int32)
import GI.Cairo (RectangleInt(..), rectangleIntWidth, rectangleIntHeight, rectangleIntX, rectangleIntY, rectangleIntReadWidth, rectangleIntReadHeight, rectangleIntReadX, rectangleIntReadY)
#endif

#ifdef MIN_VERSION_GDK_3_18
newRectangle :: [AttrOp Rectangle AttrSet] -> IO Rectangle
newRectangle = new Rectangle
#else
type Rectangle      = RectangleInt
rectangleWidth      = rectangleIntWidth
rectangleHeight     = rectangleIntHeight
rectangleX          = rectangleIntX
rectangleY          = rectangleIntY
rectangleReadWidth :: Rectangle -> IO Int32
rectangleReadWidth  = rectangleIntReadWidth
rectangleReadHeight :: Rectangle -> IO Int32
rectangleReadHeight = rectangleIntReadHeight
rectangleReadX :: Rectangle -> IO Int32
rectangleReadX      = rectangleIntReadX
rectangleReadY :: Rectangle -> IO Int32
rectangleReadY      = rectangleIntReadY
newRectangle :: [AttrOp Rectangle AttrSet] -> IO Rectangle
newRectangle = new RectangleInt
#endif
