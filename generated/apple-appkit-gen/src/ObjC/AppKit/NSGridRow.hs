{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGridRow@.
module ObjC.AppKit.NSGridRow
  ( NSGridRow
  , IsNSGridRow(..)
  , cellAtIndex
  , mergeCellsInRange
  , gridView
  , numberOfCells
  , yPlacement
  , setYPlacement
  , rowAlignment
  , setRowAlignment
  , height
  , setHeight
  , topPadding
  , setTopPadding
  , bottomPadding
  , setBottomPadding
  , hidden
  , setHidden
  , bottomPaddingSelector
  , cellAtIndexSelector
  , gridViewSelector
  , heightSelector
  , hiddenSelector
  , mergeCellsInRangeSelector
  , numberOfCellsSelector
  , rowAlignmentSelector
  , setBottomPaddingSelector
  , setHeightSelector
  , setHiddenSelector
  , setRowAlignmentSelector
  , setTopPaddingSelector
  , setYPlacementSelector
  , topPaddingSelector
  , yPlacementSelector

  -- * Enum types
  , NSGridCellPlacement(NSGridCellPlacement)
  , pattern NSGridCellPlacementInherited
  , pattern NSGridCellPlacementNone
  , pattern NSGridCellPlacementLeading
  , pattern NSGridCellPlacementTop
  , pattern NSGridCellPlacementTrailing
  , pattern NSGridCellPlacementBottom
  , pattern NSGridCellPlacementCenter
  , pattern NSGridCellPlacementFill
  , NSGridRowAlignment(NSGridRowAlignment)
  , pattern NSGridRowAlignmentInherited
  , pattern NSGridRowAlignmentNone
  , pattern NSGridRowAlignmentFirstBaseline
  , pattern NSGridRowAlignmentLastBaseline

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- cellAtIndex:@
cellAtIndex :: IsNSGridRow nsGridRow => nsGridRow -> CLong -> IO (Id NSGridCell)
cellAtIndex nsGridRow index =
  sendMessage nsGridRow cellAtIndexSelector index

-- | @- mergeCellsInRange:@
mergeCellsInRange :: IsNSGridRow nsGridRow => nsGridRow -> NSRange -> IO ()
mergeCellsInRange nsGridRow range =
  sendMessage nsGridRow mergeCellsInRangeSelector range

-- | @- gridView@
gridView :: IsNSGridRow nsGridRow => nsGridRow -> IO (Id NSGridView)
gridView nsGridRow =
  sendMessage nsGridRow gridViewSelector

-- | @- numberOfCells@
numberOfCells :: IsNSGridRow nsGridRow => nsGridRow -> IO CLong
numberOfCells nsGridRow =
  sendMessage nsGridRow numberOfCellsSelector

-- | @- yPlacement@
yPlacement :: IsNSGridRow nsGridRow => nsGridRow -> IO NSGridCellPlacement
yPlacement nsGridRow =
  sendMessage nsGridRow yPlacementSelector

-- | @- setYPlacement:@
setYPlacement :: IsNSGridRow nsGridRow => nsGridRow -> NSGridCellPlacement -> IO ()
setYPlacement nsGridRow value =
  sendMessage nsGridRow setYPlacementSelector value

-- | @- rowAlignment@
rowAlignment :: IsNSGridRow nsGridRow => nsGridRow -> IO NSGridRowAlignment
rowAlignment nsGridRow =
  sendMessage nsGridRow rowAlignmentSelector

-- | @- setRowAlignment:@
setRowAlignment :: IsNSGridRow nsGridRow => nsGridRow -> NSGridRowAlignment -> IO ()
setRowAlignment nsGridRow value =
  sendMessage nsGridRow setRowAlignmentSelector value

-- | @- height@
height :: IsNSGridRow nsGridRow => nsGridRow -> IO CDouble
height nsGridRow =
  sendMessage nsGridRow heightSelector

-- | @- setHeight:@
setHeight :: IsNSGridRow nsGridRow => nsGridRow -> CDouble -> IO ()
setHeight nsGridRow value =
  sendMessage nsGridRow setHeightSelector value

-- | @- topPadding@
topPadding :: IsNSGridRow nsGridRow => nsGridRow -> IO CDouble
topPadding nsGridRow =
  sendMessage nsGridRow topPaddingSelector

-- | @- setTopPadding:@
setTopPadding :: IsNSGridRow nsGridRow => nsGridRow -> CDouble -> IO ()
setTopPadding nsGridRow value =
  sendMessage nsGridRow setTopPaddingSelector value

-- | @- bottomPadding@
bottomPadding :: IsNSGridRow nsGridRow => nsGridRow -> IO CDouble
bottomPadding nsGridRow =
  sendMessage nsGridRow bottomPaddingSelector

-- | @- setBottomPadding:@
setBottomPadding :: IsNSGridRow nsGridRow => nsGridRow -> CDouble -> IO ()
setBottomPadding nsGridRow value =
  sendMessage nsGridRow setBottomPaddingSelector value

-- | @- hidden@
hidden :: IsNSGridRow nsGridRow => nsGridRow -> IO Bool
hidden nsGridRow =
  sendMessage nsGridRow hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSGridRow nsGridRow => nsGridRow -> Bool -> IO ()
setHidden nsGridRow value =
  sendMessage nsGridRow setHiddenSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cellAtIndex:@
cellAtIndexSelector :: Selector '[CLong] (Id NSGridCell)
cellAtIndexSelector = mkSelector "cellAtIndex:"

-- | @Selector@ for @mergeCellsInRange:@
mergeCellsInRangeSelector :: Selector '[NSRange] ()
mergeCellsInRangeSelector = mkSelector "mergeCellsInRange:"

-- | @Selector@ for @gridView@
gridViewSelector :: Selector '[] (Id NSGridView)
gridViewSelector = mkSelector "gridView"

-- | @Selector@ for @numberOfCells@
numberOfCellsSelector :: Selector '[] CLong
numberOfCellsSelector = mkSelector "numberOfCells"

-- | @Selector@ for @yPlacement@
yPlacementSelector :: Selector '[] NSGridCellPlacement
yPlacementSelector = mkSelector "yPlacement"

-- | @Selector@ for @setYPlacement:@
setYPlacementSelector :: Selector '[NSGridCellPlacement] ()
setYPlacementSelector = mkSelector "setYPlacement:"

-- | @Selector@ for @rowAlignment@
rowAlignmentSelector :: Selector '[] NSGridRowAlignment
rowAlignmentSelector = mkSelector "rowAlignment"

-- | @Selector@ for @setRowAlignment:@
setRowAlignmentSelector :: Selector '[NSGridRowAlignment] ()
setRowAlignmentSelector = mkSelector "setRowAlignment:"

-- | @Selector@ for @height@
heightSelector :: Selector '[] CDouble
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector '[CDouble] ()
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @topPadding@
topPaddingSelector :: Selector '[] CDouble
topPaddingSelector = mkSelector "topPadding"

-- | @Selector@ for @setTopPadding:@
setTopPaddingSelector :: Selector '[CDouble] ()
setTopPaddingSelector = mkSelector "setTopPadding:"

-- | @Selector@ for @bottomPadding@
bottomPaddingSelector :: Selector '[] CDouble
bottomPaddingSelector = mkSelector "bottomPadding"

-- | @Selector@ for @setBottomPadding:@
setBottomPaddingSelector :: Selector '[CDouble] ()
setBottomPaddingSelector = mkSelector "setBottomPadding:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

