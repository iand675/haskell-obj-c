{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGridColumn@.
module ObjC.AppKit.NSGridColumn
  ( NSGridColumn
  , IsNSGridColumn(..)
  , cellAtIndex
  , mergeCellsInRange
  , gridView
  , numberOfCells
  , xPlacement
  , setXPlacement
  , width
  , setWidth
  , leadingPadding
  , setLeadingPadding
  , trailingPadding
  , setTrailingPadding
  , hidden
  , setHidden
  , cellAtIndexSelector
  , gridViewSelector
  , hiddenSelector
  , leadingPaddingSelector
  , mergeCellsInRangeSelector
  , numberOfCellsSelector
  , setHiddenSelector
  , setLeadingPaddingSelector
  , setTrailingPaddingSelector
  , setWidthSelector
  , setXPlacementSelector
  , trailingPaddingSelector
  , widthSelector
  , xPlacementSelector

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
cellAtIndex :: IsNSGridColumn nsGridColumn => nsGridColumn -> CLong -> IO (Id NSGridCell)
cellAtIndex nsGridColumn index =
  sendMessage nsGridColumn cellAtIndexSelector index

-- | @- mergeCellsInRange:@
mergeCellsInRange :: IsNSGridColumn nsGridColumn => nsGridColumn -> NSRange -> IO ()
mergeCellsInRange nsGridColumn range =
  sendMessage nsGridColumn mergeCellsInRangeSelector range

-- | @- gridView@
gridView :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO (Id NSGridView)
gridView nsGridColumn =
  sendMessage nsGridColumn gridViewSelector

-- | @- numberOfCells@
numberOfCells :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CLong
numberOfCells nsGridColumn =
  sendMessage nsGridColumn numberOfCellsSelector

-- | @- xPlacement@
xPlacement :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO NSGridCellPlacement
xPlacement nsGridColumn =
  sendMessage nsGridColumn xPlacementSelector

-- | @- setXPlacement:@
setXPlacement :: IsNSGridColumn nsGridColumn => nsGridColumn -> NSGridCellPlacement -> IO ()
setXPlacement nsGridColumn value =
  sendMessage nsGridColumn setXPlacementSelector value

-- | @- width@
width :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CDouble
width nsGridColumn =
  sendMessage nsGridColumn widthSelector

-- | @- setWidth:@
setWidth :: IsNSGridColumn nsGridColumn => nsGridColumn -> CDouble -> IO ()
setWidth nsGridColumn value =
  sendMessage nsGridColumn setWidthSelector value

-- | @- leadingPadding@
leadingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CDouble
leadingPadding nsGridColumn =
  sendMessage nsGridColumn leadingPaddingSelector

-- | @- setLeadingPadding:@
setLeadingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> CDouble -> IO ()
setLeadingPadding nsGridColumn value =
  sendMessage nsGridColumn setLeadingPaddingSelector value

-- | @- trailingPadding@
trailingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CDouble
trailingPadding nsGridColumn =
  sendMessage nsGridColumn trailingPaddingSelector

-- | @- setTrailingPadding:@
setTrailingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> CDouble -> IO ()
setTrailingPadding nsGridColumn value =
  sendMessage nsGridColumn setTrailingPaddingSelector value

-- | @- hidden@
hidden :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO Bool
hidden nsGridColumn =
  sendMessage nsGridColumn hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSGridColumn nsGridColumn => nsGridColumn -> Bool -> IO ()
setHidden nsGridColumn value =
  sendMessage nsGridColumn setHiddenSelector value

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

-- | @Selector@ for @xPlacement@
xPlacementSelector :: Selector '[] NSGridCellPlacement
xPlacementSelector = mkSelector "xPlacement"

-- | @Selector@ for @setXPlacement:@
setXPlacementSelector :: Selector '[NSGridCellPlacement] ()
setXPlacementSelector = mkSelector "setXPlacement:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CDouble
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CDouble] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @leadingPadding@
leadingPaddingSelector :: Selector '[] CDouble
leadingPaddingSelector = mkSelector "leadingPadding"

-- | @Selector@ for @setLeadingPadding:@
setLeadingPaddingSelector :: Selector '[CDouble] ()
setLeadingPaddingSelector = mkSelector "setLeadingPadding:"

-- | @Selector@ for @trailingPadding@
trailingPaddingSelector :: Selector '[] CDouble
trailingPaddingSelector = mkSelector "trailingPadding"

-- | @Selector@ for @setTrailingPadding:@
setTrailingPaddingSelector :: Selector '[CDouble] ()
setTrailingPaddingSelector = mkSelector "setTrailingPadding:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

