{-# LANGUAGE PatternSynonyms #-}
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
  , cellAtIndexSelector
  , mergeCellsInRangeSelector
  , gridViewSelector
  , numberOfCellsSelector
  , yPlacementSelector
  , setYPlacementSelector
  , rowAlignmentSelector
  , setRowAlignmentSelector
  , heightSelector
  , setHeightSelector
  , topPaddingSelector
  , setTopPaddingSelector
  , bottomPaddingSelector
  , setBottomPaddingSelector
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
  , NSGridRowAlignment(NSGridRowAlignment)
  , pattern NSGridRowAlignmentInherited
  , pattern NSGridRowAlignmentNone
  , pattern NSGridRowAlignmentFirstBaseline
  , pattern NSGridRowAlignmentLastBaseline

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
cellAtIndex :: IsNSGridRow nsGridRow => nsGridRow -> CLong -> IO (Id NSGridCell)
cellAtIndex nsGridRow  index =
  sendMsg nsGridRow (mkSelector "cellAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- mergeCellsInRange:@
mergeCellsInRange :: IsNSGridRow nsGridRow => nsGridRow -> NSRange -> IO ()
mergeCellsInRange nsGridRow  range =
  sendMsg nsGridRow (mkSelector "mergeCellsInRange:") retVoid [argNSRange range]

-- | @- gridView@
gridView :: IsNSGridRow nsGridRow => nsGridRow -> IO (Id NSGridView)
gridView nsGridRow  =
  sendMsg nsGridRow (mkSelector "gridView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfCells@
numberOfCells :: IsNSGridRow nsGridRow => nsGridRow -> IO CLong
numberOfCells nsGridRow  =
  sendMsg nsGridRow (mkSelector "numberOfCells") retCLong []

-- | @- yPlacement@
yPlacement :: IsNSGridRow nsGridRow => nsGridRow -> IO NSGridCellPlacement
yPlacement nsGridRow  =
  fmap (coerce :: CLong -> NSGridCellPlacement) $ sendMsg nsGridRow (mkSelector "yPlacement") retCLong []

-- | @- setYPlacement:@
setYPlacement :: IsNSGridRow nsGridRow => nsGridRow -> NSGridCellPlacement -> IO ()
setYPlacement nsGridRow  value =
  sendMsg nsGridRow (mkSelector "setYPlacement:") retVoid [argCLong (coerce value)]

-- | @- rowAlignment@
rowAlignment :: IsNSGridRow nsGridRow => nsGridRow -> IO NSGridRowAlignment
rowAlignment nsGridRow  =
  fmap (coerce :: CLong -> NSGridRowAlignment) $ sendMsg nsGridRow (mkSelector "rowAlignment") retCLong []

-- | @- setRowAlignment:@
setRowAlignment :: IsNSGridRow nsGridRow => nsGridRow -> NSGridRowAlignment -> IO ()
setRowAlignment nsGridRow  value =
  sendMsg nsGridRow (mkSelector "setRowAlignment:") retVoid [argCLong (coerce value)]

-- | @- height@
height :: IsNSGridRow nsGridRow => nsGridRow -> IO CDouble
height nsGridRow  =
  sendMsg nsGridRow (mkSelector "height") retCDouble []

-- | @- setHeight:@
setHeight :: IsNSGridRow nsGridRow => nsGridRow -> CDouble -> IO ()
setHeight nsGridRow  value =
  sendMsg nsGridRow (mkSelector "setHeight:") retVoid [argCDouble (fromIntegral value)]

-- | @- topPadding@
topPadding :: IsNSGridRow nsGridRow => nsGridRow -> IO CDouble
topPadding nsGridRow  =
  sendMsg nsGridRow (mkSelector "topPadding") retCDouble []

-- | @- setTopPadding:@
setTopPadding :: IsNSGridRow nsGridRow => nsGridRow -> CDouble -> IO ()
setTopPadding nsGridRow  value =
  sendMsg nsGridRow (mkSelector "setTopPadding:") retVoid [argCDouble (fromIntegral value)]

-- | @- bottomPadding@
bottomPadding :: IsNSGridRow nsGridRow => nsGridRow -> IO CDouble
bottomPadding nsGridRow  =
  sendMsg nsGridRow (mkSelector "bottomPadding") retCDouble []

-- | @- setBottomPadding:@
setBottomPadding :: IsNSGridRow nsGridRow => nsGridRow -> CDouble -> IO ()
setBottomPadding nsGridRow  value =
  sendMsg nsGridRow (mkSelector "setBottomPadding:") retVoid [argCDouble (fromIntegral value)]

-- | @- hidden@
hidden :: IsNSGridRow nsGridRow => nsGridRow -> IO Bool
hidden nsGridRow  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGridRow (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSGridRow nsGridRow => nsGridRow -> Bool -> IO ()
setHidden nsGridRow  value =
  sendMsg nsGridRow (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

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

-- | @Selector@ for @yPlacement@
yPlacementSelector :: Selector
yPlacementSelector = mkSelector "yPlacement"

-- | @Selector@ for @setYPlacement:@
setYPlacementSelector :: Selector
setYPlacementSelector = mkSelector "setYPlacement:"

-- | @Selector@ for @rowAlignment@
rowAlignmentSelector :: Selector
rowAlignmentSelector = mkSelector "rowAlignment"

-- | @Selector@ for @setRowAlignment:@
setRowAlignmentSelector :: Selector
setRowAlignmentSelector = mkSelector "setRowAlignment:"

-- | @Selector@ for @height@
heightSelector :: Selector
heightSelector = mkSelector "height"

-- | @Selector@ for @setHeight:@
setHeightSelector :: Selector
setHeightSelector = mkSelector "setHeight:"

-- | @Selector@ for @topPadding@
topPaddingSelector :: Selector
topPaddingSelector = mkSelector "topPadding"

-- | @Selector@ for @setTopPadding:@
setTopPaddingSelector :: Selector
setTopPaddingSelector = mkSelector "setTopPadding:"

-- | @Selector@ for @bottomPadding@
bottomPaddingSelector :: Selector
bottomPaddingSelector = mkSelector "bottomPadding"

-- | @Selector@ for @setBottomPadding:@
setBottomPaddingSelector :: Selector
setBottomPaddingSelector = mkSelector "setBottomPadding:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

