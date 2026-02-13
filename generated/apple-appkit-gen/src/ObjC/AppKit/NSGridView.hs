{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addColumnWithViewsSelector
  , addRowWithViewsSelector
  , cellAtColumnIndex_rowIndexSelector
  , cellForViewSelector
  , columnAtIndexSelector
  , columnSpacingSelector
  , gridViewWithNumberOfColumns_rowsSelector
  , gridViewWithViewsSelector
  , indexOfColumnSelector
  , indexOfRowSelector
  , initWithCoderSelector
  , initWithFrameSelector
  , insertColumnAtIndex_withViewsSelector
  , insertRowAtIndex_withViewsSelector
  , mergeCellsInHorizontalRange_verticalRangeSelector
  , moveColumnAtIndex_toIndexSelector
  , moveRowAtIndex_toIndexSelector
  , numberOfColumnsSelector
  , numberOfRowsSelector
  , removeColumnAtIndexSelector
  , removeRowAtIndexSelector
  , rowAlignmentSelector
  , rowAtIndexSelector
  , rowSpacingSelector
  , setColumnSpacingSelector
  , setRowAlignmentSelector
  , setRowSpacingSelector
  , setXPlacementSelector
  , setYPlacementSelector
  , xPlacementSelector
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

-- | @- initWithFrame:@
initWithFrame :: IsNSGridView nsGridView => nsGridView -> NSRect -> IO (Id NSGridView)
initWithFrame nsGridView frameRect =
  sendOwnedMessage nsGridView initWithFrameSelector frameRect

-- | @- initWithCoder:@
initWithCoder :: (IsNSGridView nsGridView, IsNSCoder coder) => nsGridView -> coder -> IO (Id NSGridView)
initWithCoder nsGridView coder =
  sendOwnedMessage nsGridView initWithCoderSelector (toNSCoder coder)

-- | @+ gridViewWithNumberOfColumns:rows:@
gridViewWithNumberOfColumns_rows :: CLong -> CLong -> IO (Id NSGridView)
gridViewWithNumberOfColumns_rows columnCount rowCount =
  do
    cls' <- getRequiredClass "NSGridView"
    sendClassMessage cls' gridViewWithNumberOfColumns_rowsSelector columnCount rowCount

-- | @+ gridViewWithViews:@
gridViewWithViews :: IsNSArray rows => rows -> IO (Id NSGridView)
gridViewWithViews rows =
  do
    cls' <- getRequiredClass "NSGridView"
    sendClassMessage cls' gridViewWithViewsSelector (toNSArray rows)

-- | @- rowAtIndex:@
rowAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO (Id NSGridRow)
rowAtIndex nsGridView index =
  sendMessage nsGridView rowAtIndexSelector index

-- | @- indexOfRow:@
indexOfRow :: (IsNSGridView nsGridView, IsNSGridRow row) => nsGridView -> row -> IO CLong
indexOfRow nsGridView row =
  sendMessage nsGridView indexOfRowSelector (toNSGridRow row)

-- | @- columnAtIndex:@
columnAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO (Id NSGridColumn)
columnAtIndex nsGridView index =
  sendMessage nsGridView columnAtIndexSelector index

-- | @- indexOfColumn:@
indexOfColumn :: (IsNSGridView nsGridView, IsNSGridColumn column) => nsGridView -> column -> IO CLong
indexOfColumn nsGridView column =
  sendMessage nsGridView indexOfColumnSelector (toNSGridColumn column)

-- | @- cellAtColumnIndex:rowIndex:@
cellAtColumnIndex_rowIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> CLong -> IO (Id NSGridCell)
cellAtColumnIndex_rowIndex nsGridView columnIndex rowIndex =
  sendMessage nsGridView cellAtColumnIndex_rowIndexSelector columnIndex rowIndex

-- | @- cellForView:@
cellForView :: (IsNSGridView nsGridView, IsNSView view) => nsGridView -> view -> IO (Id NSGridCell)
cellForView nsGridView view =
  sendMessage nsGridView cellForViewSelector (toNSView view)

-- | @- addRowWithViews:@
addRowWithViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> views -> IO (Id NSGridRow)
addRowWithViews nsGridView views =
  sendMessage nsGridView addRowWithViewsSelector (toNSArray views)

-- | @- insertRowAtIndex:withViews:@
insertRowAtIndex_withViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> CLong -> views -> IO (Id NSGridRow)
insertRowAtIndex_withViews nsGridView index views =
  sendMessage nsGridView insertRowAtIndex_withViewsSelector index (toNSArray views)

-- | @- moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> CLong -> IO ()
moveRowAtIndex_toIndex nsGridView fromIndex toIndex =
  sendMessage nsGridView moveRowAtIndex_toIndexSelector fromIndex toIndex

-- | @- removeRowAtIndex:@
removeRowAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO ()
removeRowAtIndex nsGridView index =
  sendMessage nsGridView removeRowAtIndexSelector index

-- | @- addColumnWithViews:@
addColumnWithViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> views -> IO (Id NSGridColumn)
addColumnWithViews nsGridView views =
  sendMessage nsGridView addColumnWithViewsSelector (toNSArray views)

-- | @- insertColumnAtIndex:withViews:@
insertColumnAtIndex_withViews :: (IsNSGridView nsGridView, IsNSArray views) => nsGridView -> CLong -> views -> IO (Id NSGridColumn)
insertColumnAtIndex_withViews nsGridView index views =
  sendMessage nsGridView insertColumnAtIndex_withViewsSelector index (toNSArray views)

-- | @- moveColumnAtIndex:toIndex:@
moveColumnAtIndex_toIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> CLong -> IO ()
moveColumnAtIndex_toIndex nsGridView fromIndex toIndex =
  sendMessage nsGridView moveColumnAtIndex_toIndexSelector fromIndex toIndex

-- | @- removeColumnAtIndex:@
removeColumnAtIndex :: IsNSGridView nsGridView => nsGridView -> CLong -> IO ()
removeColumnAtIndex nsGridView index =
  sendMessage nsGridView removeColumnAtIndexSelector index

-- | @- mergeCellsInHorizontalRange:verticalRange:@
mergeCellsInHorizontalRange_verticalRange :: IsNSGridView nsGridView => nsGridView -> NSRange -> NSRange -> IO ()
mergeCellsInHorizontalRange_verticalRange nsGridView hRange vRange =
  sendMessage nsGridView mergeCellsInHorizontalRange_verticalRangeSelector hRange vRange

-- | @- numberOfRows@
numberOfRows :: IsNSGridView nsGridView => nsGridView -> IO CLong
numberOfRows nsGridView =
  sendMessage nsGridView numberOfRowsSelector

-- | @- numberOfColumns@
numberOfColumns :: IsNSGridView nsGridView => nsGridView -> IO CLong
numberOfColumns nsGridView =
  sendMessage nsGridView numberOfColumnsSelector

-- | @- xPlacement@
xPlacement :: IsNSGridView nsGridView => nsGridView -> IO NSGridCellPlacement
xPlacement nsGridView =
  sendMessage nsGridView xPlacementSelector

-- | @- setXPlacement:@
setXPlacement :: IsNSGridView nsGridView => nsGridView -> NSGridCellPlacement -> IO ()
setXPlacement nsGridView value =
  sendMessage nsGridView setXPlacementSelector value

-- | @- yPlacement@
yPlacement :: IsNSGridView nsGridView => nsGridView -> IO NSGridCellPlacement
yPlacement nsGridView =
  sendMessage nsGridView yPlacementSelector

-- | @- setYPlacement:@
setYPlacement :: IsNSGridView nsGridView => nsGridView -> NSGridCellPlacement -> IO ()
setYPlacement nsGridView value =
  sendMessage nsGridView setYPlacementSelector value

-- | @- rowAlignment@
rowAlignment :: IsNSGridView nsGridView => nsGridView -> IO NSGridRowAlignment
rowAlignment nsGridView =
  sendMessage nsGridView rowAlignmentSelector

-- | @- setRowAlignment:@
setRowAlignment :: IsNSGridView nsGridView => nsGridView -> NSGridRowAlignment -> IO ()
setRowAlignment nsGridView value =
  sendMessage nsGridView setRowAlignmentSelector value

-- | @- rowSpacing@
rowSpacing :: IsNSGridView nsGridView => nsGridView -> IO CDouble
rowSpacing nsGridView =
  sendMessage nsGridView rowSpacingSelector

-- | @- setRowSpacing:@
setRowSpacing :: IsNSGridView nsGridView => nsGridView -> CDouble -> IO ()
setRowSpacing nsGridView value =
  sendMessage nsGridView setRowSpacingSelector value

-- | @- columnSpacing@
columnSpacing :: IsNSGridView nsGridView => nsGridView -> IO CDouble
columnSpacing nsGridView =
  sendMessage nsGridView columnSpacingSelector

-- | @- setColumnSpacing:@
setColumnSpacing :: IsNSGridView nsGridView => nsGridView -> CDouble -> IO ()
setColumnSpacing nsGridView value =
  sendMessage nsGridView setColumnSpacingSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithFrame:@
initWithFrameSelector :: Selector '[NSRect] (Id NSGridView)
initWithFrameSelector = mkSelector "initWithFrame:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSGridView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @gridViewWithNumberOfColumns:rows:@
gridViewWithNumberOfColumns_rowsSelector :: Selector '[CLong, CLong] (Id NSGridView)
gridViewWithNumberOfColumns_rowsSelector = mkSelector "gridViewWithNumberOfColumns:rows:"

-- | @Selector@ for @gridViewWithViews:@
gridViewWithViewsSelector :: Selector '[Id NSArray] (Id NSGridView)
gridViewWithViewsSelector = mkSelector "gridViewWithViews:"

-- | @Selector@ for @rowAtIndex:@
rowAtIndexSelector :: Selector '[CLong] (Id NSGridRow)
rowAtIndexSelector = mkSelector "rowAtIndex:"

-- | @Selector@ for @indexOfRow:@
indexOfRowSelector :: Selector '[Id NSGridRow] CLong
indexOfRowSelector = mkSelector "indexOfRow:"

-- | @Selector@ for @columnAtIndex:@
columnAtIndexSelector :: Selector '[CLong] (Id NSGridColumn)
columnAtIndexSelector = mkSelector "columnAtIndex:"

-- | @Selector@ for @indexOfColumn:@
indexOfColumnSelector :: Selector '[Id NSGridColumn] CLong
indexOfColumnSelector = mkSelector "indexOfColumn:"

-- | @Selector@ for @cellAtColumnIndex:rowIndex:@
cellAtColumnIndex_rowIndexSelector :: Selector '[CLong, CLong] (Id NSGridCell)
cellAtColumnIndex_rowIndexSelector = mkSelector "cellAtColumnIndex:rowIndex:"

-- | @Selector@ for @cellForView:@
cellForViewSelector :: Selector '[Id NSView] (Id NSGridCell)
cellForViewSelector = mkSelector "cellForView:"

-- | @Selector@ for @addRowWithViews:@
addRowWithViewsSelector :: Selector '[Id NSArray] (Id NSGridRow)
addRowWithViewsSelector = mkSelector "addRowWithViews:"

-- | @Selector@ for @insertRowAtIndex:withViews:@
insertRowAtIndex_withViewsSelector :: Selector '[CLong, Id NSArray] (Id NSGridRow)
insertRowAtIndex_withViewsSelector = mkSelector "insertRowAtIndex:withViews:"

-- | @Selector@ for @moveRowAtIndex:toIndex:@
moveRowAtIndex_toIndexSelector :: Selector '[CLong, CLong] ()
moveRowAtIndex_toIndexSelector = mkSelector "moveRowAtIndex:toIndex:"

-- | @Selector@ for @removeRowAtIndex:@
removeRowAtIndexSelector :: Selector '[CLong] ()
removeRowAtIndexSelector = mkSelector "removeRowAtIndex:"

-- | @Selector@ for @addColumnWithViews:@
addColumnWithViewsSelector :: Selector '[Id NSArray] (Id NSGridColumn)
addColumnWithViewsSelector = mkSelector "addColumnWithViews:"

-- | @Selector@ for @insertColumnAtIndex:withViews:@
insertColumnAtIndex_withViewsSelector :: Selector '[CLong, Id NSArray] (Id NSGridColumn)
insertColumnAtIndex_withViewsSelector = mkSelector "insertColumnAtIndex:withViews:"

-- | @Selector@ for @moveColumnAtIndex:toIndex:@
moveColumnAtIndex_toIndexSelector :: Selector '[CLong, CLong] ()
moveColumnAtIndex_toIndexSelector = mkSelector "moveColumnAtIndex:toIndex:"

-- | @Selector@ for @removeColumnAtIndex:@
removeColumnAtIndexSelector :: Selector '[CLong] ()
removeColumnAtIndexSelector = mkSelector "removeColumnAtIndex:"

-- | @Selector@ for @mergeCellsInHorizontalRange:verticalRange:@
mergeCellsInHorizontalRange_verticalRangeSelector :: Selector '[NSRange, NSRange] ()
mergeCellsInHorizontalRange_verticalRangeSelector = mkSelector "mergeCellsInHorizontalRange:verticalRange:"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CLong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @numberOfColumns@
numberOfColumnsSelector :: Selector '[] CLong
numberOfColumnsSelector = mkSelector "numberOfColumns"

-- | @Selector@ for @xPlacement@
xPlacementSelector :: Selector '[] NSGridCellPlacement
xPlacementSelector = mkSelector "xPlacement"

-- | @Selector@ for @setXPlacement:@
setXPlacementSelector :: Selector '[NSGridCellPlacement] ()
setXPlacementSelector = mkSelector "setXPlacement:"

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

-- | @Selector@ for @rowSpacing@
rowSpacingSelector :: Selector '[] CDouble
rowSpacingSelector = mkSelector "rowSpacing"

-- | @Selector@ for @setRowSpacing:@
setRowSpacingSelector :: Selector '[CDouble] ()
setRowSpacingSelector = mkSelector "setRowSpacing:"

-- | @Selector@ for @columnSpacing@
columnSpacingSelector :: Selector '[] CDouble
columnSpacingSelector = mkSelector "columnSpacing"

-- | @Selector@ for @setColumnSpacing:@
setColumnSpacingSelector :: Selector '[CDouble] ()
setColumnSpacingSelector = mkSelector "setColumnSpacing:"

