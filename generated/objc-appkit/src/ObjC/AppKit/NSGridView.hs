{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGridView@.
module ObjC.AppKit.NSGridView
  ( NSGridView
  , IsNSGridView(..)
  , initWithFrame
  , initWithCoder
  , gridViewWithNumberOfColumns_rows
  , gridViewWithViews
  , rowAtIndex
  , indexOfRow
  , columnAtIndex
  , indexOfColumn
  , cellAtColumnIndex_rowIndex
  , cellForView
  , addRowWithViews
  , insertRowAtIndex_withViews
  , moveRowAtIndex_toIndex
  , removeRowAtIndex
  , addColumnWithViews
  , insertColumnAtIndex_withViews
  , moveColumnAtIndex_toIndex
  , removeColumnAtIndex
  , mergeCellsInHorizontalRange_verticalRange
  , numberOfRows
  , numberOfColumns
  , xPlacement
  , setXPlacement
  , yPlacement
  , setYPlacement
  , rowAlignment
  , setRowAlignment
  , rowSpacing
  , setRowSpacing
  , columnSpacing
  , setColumnSpacing
  , initWithFrameSelector
  , initWithCoderSelector
  , gridViewWithNumberOfColumns_rowsSelector
  , gridViewWithViewsSelector
  , rowAtIndexSelector
  , indexOfRowSelector
  , columnAtIndexSelector
  , indexOfColumnSelector
  , cellAtColumnIndex_rowIndexSelector
  , cellForViewSelector
  , addRowWithViewsSelector
  , insertRowAtIndex_withViewsSelector
  , moveRowAtIndex_toIndexSelector
  , removeRowAtIndexSelector
  , addColumnWithViewsSelector
  , insertColumnAtIndex_withViewsSelector
  , moveColumnAtIndex_toIndexSelector
  , removeColumnAtIndexSelector
  , mergeCellsInHorizontalRange_verticalRangeSelector
  , numberOfRowsSelector
  , numberOfColumnsSelector
  , xPlacementSelector
  , setXPlacementSelector
  , yPlacementSelector
  , setYPlacementSelector
  , rowAlignmentSelector
  , setRowAlignmentSelector
  , rowSpacingSelector
  , setRowSpacingSelector
  , columnSpacingSelector
  , setColumnSpacingSelector

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

-- | @- initWithFrame:@
initWithFrame :: IsNSGridView nsGridView => nsGridView -> NSRect -> IO (Id NSGridView)
initWithFrame nsGridView  frameRect =
  sendMsg nsGridView (mkSelector "initWithFrame:") (retPtr retVoid) [argNSRect frameRect] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSGridView nsGridView, IsNSCoder coder) => nsGridView -> coder -> IO (Id NSGridView)
initWithCoder nsGridView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsGridView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ gridViewWithNumberOfColumns:rows:@
gridViewWithNumberOfColumns_rows :: CLong -> CLong -> IO (Id NSGridView)
gridViewWithNumberOfColumns_rows columnCount rowCount =
  do
    cls' <- getRequiredClass "NSGridView"
    sendClassMsg cls' (mkSelector "gridViewWithNumberOfColumns:rows:") (retPtr retVoid) [argCLong (fromIntegral columnCount), argCLong (fromIntegral rowCount)] >>= retainedObject . castPtr

-- | @+ gridViewWithViews:@
gridViewWithViews :: IsNSArray rows => rows -> IO (Id NSGridView)
gridViewWithViews rows =
  do
    cls' <- getRequiredClass "NSGridView"
    withObjCPtr rows $ \raw_rows ->
      sendClassMsg cls' (mkSelector "gridViewWithViews:") (retPtr retVoid) [argPtr (castPtr raw_rows :: Ptr ())] >>= retainedObject . castPtr

-- | @- rowAtIndex:@
rowAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO (Id NSGridRow)
rowAtIndex nsGridView  index =
  sendMsg nsGridView (mkSelector "rowAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- indexOfRow:@
indexOfRow :: (IsNSGridView nsGridView, IsNSGridRow row) => nsGridView -> row -> IO CLong
indexOfRow nsGridView  row =
withObjCPtr row $ \raw_row ->
    sendMsg nsGridView (mkSelector "indexOfRow:") retCLong [argPtr (castPtr raw_row :: Ptr ())]

-- | @- columnAtIndex:@
columnAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO (Id NSGridColumn)
columnAtIndex nsGridView  index =
  sendMsg nsGridView (mkSelector "columnAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- indexOfColumn:@
indexOfColumn :: (IsNSGridView nsGridView, IsNSGridColumn column) => nsGridView -> column -> IO CLong
indexOfColumn nsGridView  column =
withObjCPtr column $ \raw_column ->
    sendMsg nsGridView (mkSelector "indexOfColumn:") retCLong [argPtr (castPtr raw_column :: Ptr ())]

-- | @- cellAtColumnIndex:rowIndex:@
cellAtColumnIndex_rowIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> CLong -> IO (Id NSGridCell)
cellAtColumnIndex_rowIndex nsGridView  columnIndex rowIndex =
  sendMsg nsGridView (mkSelector "cellAtColumnIndex:rowIndex:") (retPtr retVoid) [argCLong (fromIntegral columnIndex), argCLong (fromIntegral rowIndex)] >>= retainedObject . castPtr

-- | @- cellForView:@
cellForView :: (IsNSGridView nsGridView, IsNSView view) => nsGridView -> view -> IO (Id NSGridCell)
cellForView nsGridView  view =
withObjCPtr view $ \raw_view ->
    sendMsg nsGridView (mkSelector "cellForView:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- addRowWithViews:@
addRowWithViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> views -> IO (Id NSGridRow)
addRowWithViews nsGridView  views =
withObjCPtr views $ \raw_views ->
    sendMsg nsGridView (mkSelector "addRowWithViews:") (retPtr retVoid) [argPtr (castPtr raw_views :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertRowAtIndex:withViews:@
insertRowAtIndex_withViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> CLong -> views -> IO (Id NSGridRow)
insertRowAtIndex_withViews nsGridView  index views =
withObjCPtr views $ \raw_views ->
    sendMsg nsGridView (mkSelector "insertRowAtIndex:withViews:") (retPtr retVoid) [argCLong (fromIntegral index), argPtr (castPtr raw_views :: Ptr ())] >>= retainedObject . castPtr

-- | @- moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> CLong -> IO ()
moveRowAtIndex_toIndex nsGridView  fromIndex toIndex =
  sendMsg nsGridView (mkSelector "moveRowAtIndex:toIndex:") retVoid [argCLong (fromIntegral fromIndex), argCLong (fromIntegral toIndex)]

-- | @- removeRowAtIndex:@
removeRowAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO ()
removeRowAtIndex nsGridView  index =
  sendMsg nsGridView (mkSelector "removeRowAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- addColumnWithViews:@
addColumnWithViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> views -> IO (Id NSGridColumn)
addColumnWithViews nsGridView  views =
withObjCPtr views $ \raw_views ->
    sendMsg nsGridView (mkSelector "addColumnWithViews:") (retPtr retVoid) [argPtr (castPtr raw_views :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertColumnAtIndex:withViews:@
insertColumnAtIndex_withViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> CLong -> views -> IO (Id NSGridColumn)
insertColumnAtIndex_withViews nsGridView  index views =
withObjCPtr views $ \raw_views ->
    sendMsg nsGridView (mkSelector "insertColumnAtIndex:withViews:") (retPtr retVoid) [argCLong (fromIntegral index), argPtr (castPtr raw_views :: Ptr ())] >>= retainedObject . castPtr

-- | @- moveColumnAtIndex:toIndex:@
moveColumnAtIndex_toIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> CLong -> IO ()
moveColumnAtIndex_toIndex nsGridView  fromIndex toIndex =
  sendMsg nsGridView (mkSelector "moveColumnAtIndex:toIndex:") retVoid [argCLong (fromIntegral fromIndex), argCLong (fromIntegral toIndex)]

-- | @- removeColumnAtIndex:@
removeColumnAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO ()
removeColumnAtIndex nsGridView  index =
  sendMsg nsGridView (mkSelector "removeColumnAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- mergeCellsInHorizontalRange:verticalRange:@
mergeCellsInHorizontalRange_verticalRange :: IsNSGridView nsGridView => nsGridView -> NSRange -> NSRange -> IO ()
mergeCellsInHorizontalRange_verticalRange nsGridView  hRange vRange =
  sendMsg nsGridView (mkSelector "mergeCellsInHorizontalRange:verticalRange:") retVoid [argNSRange hRange, argNSRange vRange]

-- | @- numberOfRows@
numberOfRows :: IsNSGridView nsGridView => nsGridView -> IO CLong
numberOfRows nsGridView  =
  sendMsg nsGridView (mkSelector "numberOfRows") retCLong []

-- | @- numberOfColumns@
numberOfColumns :: IsNSGridView nsGridView => nsGridView -> IO CLong
numberOfColumns nsGridView  =
  sendMsg nsGridView (mkSelector "numberOfColumns") retCLong []

-- | @- xPlacement@
xPlacement :: IsNSGridView nsGridView => nsGridView -> IO NSGridCellPlacement
xPlacement nsGridView  =
  fmap (coerce :: CLong -> NSGridCellPlacement) $ sendMsg nsGridView (mkSelector "xPlacement") retCLong []

-- | @- setXPlacement:@
setXPlacement :: IsNSGridView nsGridView => nsGridView -> NSGridCellPlacement -> IO ()
setXPlacement nsGridView  value =
  sendMsg nsGridView (mkSelector "setXPlacement:") retVoid [argCLong (coerce value)]

-- | @- yPlacement@
yPlacement :: IsNSGridView nsGridView => nsGridView -> IO NSGridCellPlacement
yPlacement nsGridView  =
  fmap (coerce :: CLong -> NSGridCellPlacement) $ sendMsg nsGridView (mkSelector "yPlacement") retCLong []

-- | @- setYPlacement:@
setYPlacement :: IsNSGridView nsGridView => nsGridView -> NSGridCellPlacement -> IO ()
setYPlacement nsGridView  value =
  sendMsg nsGridView (mkSelector "setYPlacement:") retVoid [argCLong (coerce value)]

-- | @- rowAlignment@
rowAlignment :: IsNSGridView nsGridView => nsGridView -> IO NSGridRowAlignment
rowAlignment nsGridView  =
  fmap (coerce :: CLong -> NSGridRowAlignment) $ sendMsg nsGridView (mkSelector "rowAlignment") retCLong []

-- | @- setRowAlignment:@
setRowAlignment :: IsNSGridView nsGridView => nsGridView -> NSGridRowAlignment -> IO ()
setRowAlignment nsGridView  value =
  sendMsg nsGridView (mkSelector "setRowAlignment:") retVoid [argCLong (coerce value)]

-- | @- rowSpacing@
rowSpacing :: IsNSGridView nsGridView => nsGridView -> IO CDouble
rowSpacing nsGridView  =
  sendMsg nsGridView (mkSelector "rowSpacing") retCDouble []

-- | @- setRowSpacing:@
setRowSpacing :: IsNSGridView nsGridView => nsGridView -> CDouble -> IO ()
setRowSpacing nsGridView  value =
  sendMsg nsGridView (mkSelector "setRowSpacing:") retVoid [argCDouble (fromIntegral value)]

-- | @- columnSpacing@
columnSpacing :: IsNSGridView nsGridView => nsGridView -> IO CDouble
columnSpacing nsGridView  =
  sendMsg nsGridView (mkSelector "columnSpacing") retCDouble []

-- | @- setColumnSpacing:@
setColumnSpacing :: IsNSGridView nsGridView => nsGridView -> CDouble -> IO ()
setColumnSpacing nsGridView  value =
  sendMsg nsGridView (mkSelector "setColumnSpacing:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @gridViewWithNumberOfColumns:rows:@
gridViewWithNumberOfColumns_rowsSelector :: Selector
gridViewWithNumberOfColumns_rowsSelector = mkSelector "gridViewWithNumberOfColumns:rows:"

-- | @Selector@ for @gridViewWithViews:@
gridViewWithViewsSelector :: Selector
gridViewWithViewsSelector = mkSelector "gridViewWithViews:"

-- | @Selector@ for @rowAtIndex:@
rowAtIndexSelector :: Selector
rowAtIndexSelector = mkSelector "rowAtIndex:"

-- | @Selector@ for @indexOfRow:@
indexOfRowSelector :: Selector
indexOfRowSelector = mkSelector "indexOfRow:"

-- | @Selector@ for @columnAtIndex:@
columnAtIndexSelector :: Selector
columnAtIndexSelector = mkSelector "columnAtIndex:"

-- | @Selector@ for @indexOfColumn:@
indexOfColumnSelector :: Selector
indexOfColumnSelector = mkSelector "indexOfColumn:"

-- | @Selector@ for @cellAtColumnIndex:rowIndex:@
cellAtColumnIndex_rowIndexSelector :: Selector
cellAtColumnIndex_rowIndexSelector = mkSelector "cellAtColumnIndex:rowIndex:"

-- | @Selector@ for @cellForView:@
cellForViewSelector :: Selector
cellForViewSelector = mkSelector "cellForView:"

-- | @Selector@ for @addRowWithViews:@
addRowWithViewsSelector :: Selector
addRowWithViewsSelector = mkSelector "addRowWithViews:"

-- | @Selector@ for @insertRowAtIndex:withViews:@
insertRowAtIndex_withViewsSelector :: Selector
insertRowAtIndex_withViewsSelector = mkSelector "insertRowAtIndex:withViews:"

-- | @Selector@ for @moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndexSelector :: Selector
moveRowAtIndex_toIndexSelector = mkSelector "moveRowAtIndex:toIndex:"

-- | @Selector@ for @removeRowAtIndex:@
removeRowAtIndexSelector :: Selector
removeRowAtIndexSelector = mkSelector "removeRowAtIndex:"

-- | @Selector@ for @addColumnWithViews:@
addColumnWithViewsSelector :: Selector
addColumnWithViewsSelector = mkSelector "addColumnWithViews:"

-- | @Selector@ for @insertColumnAtIndex:withViews:@
insertColumnAtIndex_withViewsSelector :: Selector
insertColumnAtIndex_withViewsSelector = mkSelector "insertColumnAtIndex:withViews:"

-- | @Selector@ for @moveColumnAtIndex:toIndex:@
moveColumnAtIndex_toIndexSelector :: Selector
moveColumnAtIndex_toIndexSelector = mkSelector "moveColumnAtIndex:toIndex:"

-- | @Selector@ for @removeColumnAtIndex:@
removeColumnAtIndexSelector :: Selector
removeColumnAtIndexSelector = mkSelector "removeColumnAtIndex:"

-- | @Selector@ for @mergeCellsInHorizontalRange:verticalRange:@
mergeCellsInHorizontalRange_verticalRangeSelector :: Selector
mergeCellsInHorizontalRange_verticalRangeSelector = mkSelector "mergeCellsInHorizontalRange:verticalRange:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @xPlacement@
xPlacementSelector :: Selector
xPlacementSelector = mkSelector "xPlacement"

-- | @Selector@ for @setXPlacement:@
setXPlacementSelector :: Selector
setXPlacementSelector = mkSelector "setXPlacement:"

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

-- | @Selector@ for @rowSpacing@
rowSpacingSelector :: Selector
rowSpacingSelector = mkSelector "rowSpacing"

-- | @Selector@ for @setRowSpacing:@
setRowSpacingSelector :: Selector
setRowSpacingSelector = mkSelector "setRowSpacing:"

-- | @Selector@ for @columnSpacing@
columnSpacingSelector :: Selector
columnSpacingSelector = mkSelector "columnSpacing"

-- | @Selector@ for @setColumnSpacing:@
setColumnSpacingSelector :: Selector
setColumnSpacingSelector = mkSelector "setColumnSpacing:"

