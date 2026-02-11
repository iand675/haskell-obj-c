{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableHeaderCell@.
module ObjC.AppKit.NSTableHeaderCell
  ( NSTableHeaderCell
  , IsNSTableHeaderCell(..)
  , drawSortIndicatorWithFrame_inView_ascending_priority
  , sortIndicatorRectForBounds
  , drawSortIndicatorWithFrame_inView_ascending_prioritySelector
  , sortIndicatorRectForBoundsSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- drawSortIndicatorWithFrame:inView:ascending:priority:@
drawSortIndicatorWithFrame_inView_ascending_priority :: (IsNSTableHeaderCell nsTableHeaderCell, IsNSView controlView) => nsTableHeaderCell -> NSRect -> controlView -> Bool -> CLong -> IO ()
drawSortIndicatorWithFrame_inView_ascending_priority nsTableHeaderCell  cellFrame controlView ascending priority =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsTableHeaderCell (mkSelector "drawSortIndicatorWithFrame:inView:ascending:priority:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ()), argCULong (if ascending then 1 else 0), argCLong (fromIntegral priority)]

-- | @- sortIndicatorRectForBounds:@
sortIndicatorRectForBounds :: IsNSTableHeaderCell nsTableHeaderCell => nsTableHeaderCell -> NSRect -> IO NSRect
sortIndicatorRectForBounds nsTableHeaderCell  rect =
  sendMsgStret nsTableHeaderCell (mkSelector "sortIndicatorRectForBounds:") retNSRect [argNSRect rect]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawSortIndicatorWithFrame:inView:ascending:priority:@
drawSortIndicatorWithFrame_inView_ascending_prioritySelector :: Selector
drawSortIndicatorWithFrame_inView_ascending_prioritySelector = mkSelector "drawSortIndicatorWithFrame:inView:ascending:priority:"

-- | @Selector@ for @sortIndicatorRectForBounds:@
sortIndicatorRectForBoundsSelector :: Selector
sortIndicatorRectForBoundsSelector = mkSelector "sortIndicatorRectForBounds:"

