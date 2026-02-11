{-# LANGUAGE PatternSynonyms #-}
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
  , mergeCellsInRangeSelector
  , gridViewSelector
  , numberOfCellsSelector
  , xPlacementSelector
  , setXPlacementSelector
  , widthSelector
  , setWidthSelector
  , leadingPaddingSelector
  , setLeadingPaddingSelector
  , trailingPaddingSelector
  , setTrailingPaddingSelector
  , hiddenSelector
  , setHiddenSelector

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

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- cellAtIndex:@
cellAtIndex :: IsNSGridColumn nsGridColumn => nsGridColumn -> CLong -> IO (Id NSGridCell)
cellAtIndex nsGridColumn  index =
  sendMsg nsGridColumn (mkSelector "cellAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- mergeCellsInRange:@
mergeCellsInRange :: IsNSGridColumn nsGridColumn => nsGridColumn -> NSRange -> IO ()
mergeCellsInRange nsGridColumn  range =
  sendMsg nsGridColumn (mkSelector "mergeCellsInRange:") retVoid [argNSRange range]

-- | @- gridView@
gridView :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO (Id NSGridView)
gridView nsGridColumn  =
  sendMsg nsGridColumn (mkSelector "gridView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfCells@
numberOfCells :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CLong
numberOfCells nsGridColumn  =
  sendMsg nsGridColumn (mkSelector "numberOfCells") retCLong []

-- | @- xPlacement@
xPlacement :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO NSGridCellPlacement
xPlacement nsGridColumn  =
  fmap (coerce :: CLong -> NSGridCellPlacement) $ sendMsg nsGridColumn (mkSelector "xPlacement") retCLong []

-- | @- setXPlacement:@
setXPlacement :: IsNSGridColumn nsGridColumn => nsGridColumn -> NSGridCellPlacement -> IO ()
setXPlacement nsGridColumn  value =
  sendMsg nsGridColumn (mkSelector "setXPlacement:") retVoid [argCLong (coerce value)]

-- | @- width@
width :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CDouble
width nsGridColumn  =
  sendMsg nsGridColumn (mkSelector "width") retCDouble []

-- | @- setWidth:@
setWidth :: IsNSGridColumn nsGridColumn => nsGridColumn -> CDouble -> IO ()
setWidth nsGridColumn  value =
  sendMsg nsGridColumn (mkSelector "setWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- leadingPadding@
leadingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CDouble
leadingPadding nsGridColumn  =
  sendMsg nsGridColumn (mkSelector "leadingPadding") retCDouble []

-- | @- setLeadingPadding:@
setLeadingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> CDouble -> IO ()
setLeadingPadding nsGridColumn  value =
  sendMsg nsGridColumn (mkSelector "setLeadingPadding:") retVoid [argCDouble (fromIntegral value)]

-- | @- trailingPadding@
trailingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO CDouble
trailingPadding nsGridColumn  =
  sendMsg nsGridColumn (mkSelector "trailingPadding") retCDouble []

-- | @- setTrailingPadding:@
setTrailingPadding :: IsNSGridColumn nsGridColumn => nsGridColumn -> CDouble -> IO ()
setTrailingPadding nsGridColumn  value =
  sendMsg nsGridColumn (mkSelector "setTrailingPadding:") retVoid [argCDouble (fromIntegral value)]

-- | @- hidden@
hidden :: IsNSGridColumn nsGridColumn => nsGridColumn -> IO Bool
hidden nsGridColumn  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGridColumn (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSGridColumn nsGridColumn => nsGridColumn -> Bool -> IO ()
setHidden nsGridColumn  value =
  sendMsg nsGridColumn (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cellAtIndex:@
cellAtIndexSelector :: Selector
cellAtIndexSelector = mkSelector "cellAtIndex:"

-- | @Selector@ for @mergeCellsInRange:@
mergeCellsInRangeSelector :: Selector
mergeCellsInRangeSelector = mkSelector "mergeCellsInRange:"

-- | @Selector@ for @gridView@
gridViewSelector :: Selector
gridViewSelector = mkSelector "gridView"

-- | @Selector@ for @numberOfCells@
numberOfCellsSelector :: Selector
numberOfCellsSelector = mkSelector "numberOfCells"

-- | @Selector@ for @xPlacement@
xPlacementSelector :: Selector
xPlacementSelector = mkSelector "xPlacement"

-- | @Selector@ for @setXPlacement:@
setXPlacementSelector :: Selector
setXPlacementSelector = mkSelector "setXPlacement:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @leadingPadding@
leadingPaddingSelector :: Selector
leadingPaddingSelector = mkSelector "leadingPadding"

-- | @Selector@ for @setLeadingPadding:@
setLeadingPaddingSelector :: Selector
setLeadingPaddingSelector = mkSelector "setLeadingPadding:"

-- | @Selector@ for @trailingPadding@
trailingPaddingSelector :: Selector
trailingPaddingSelector = mkSelector "trailingPadding"

-- | @Selector@ for @setTrailingPadding:@
setTrailingPaddingSelector :: Selector
setTrailingPaddingSelector = mkSelector "setTrailingPadding:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

