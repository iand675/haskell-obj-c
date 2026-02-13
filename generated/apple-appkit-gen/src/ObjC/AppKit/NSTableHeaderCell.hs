{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- drawSortIndicatorWithFrame:inView:ascending:priority:@
drawSortIndicatorWithFrame_inView_ascending_priority :: (IsNSTableHeaderCell nsTableHeaderCell, IsNSView controlView) => nsTableHeaderCell -> NSRect -> controlView -> Bool -> CLong -> IO ()
drawSortIndicatorWithFrame_inView_ascending_priority nsTableHeaderCell cellFrame controlView ascending priority =
  sendMessage nsTableHeaderCell drawSortIndicatorWithFrame_inView_ascending_prioritySelector cellFrame (toNSView controlView) ascending priority

-- | @- sortIndicatorRectForBounds:@
sortIndicatorRectForBounds :: IsNSTableHeaderCell nsTableHeaderCell => nsTableHeaderCell -> NSRect -> IO NSRect
sortIndicatorRectForBounds nsTableHeaderCell rect =
  sendMessage nsTableHeaderCell sortIndicatorRectForBoundsSelector rect

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @drawSortIndicatorWithFrame:inView:ascending:priority:@
drawSortIndicatorWithFrame_inView_ascending_prioritySelector :: Selector '[NSRect, Id NSView, Bool, CLong] ()
drawSortIndicatorWithFrame_inView_ascending_prioritySelector = mkSelector "drawSortIndicatorWithFrame:inView:ascending:priority:"

-- | @Selector@ for @sortIndicatorRectForBounds:@
sortIndicatorRectForBoundsSelector :: Selector '[NSRect] NSRect
sortIndicatorRectForBoundsSelector = mkSelector "sortIndicatorRectForBounds:"

