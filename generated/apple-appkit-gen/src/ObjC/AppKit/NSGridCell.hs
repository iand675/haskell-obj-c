{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGridCell@.
module ObjC.AppKit.NSGridCell
  ( NSGridCell
  , IsNSGridCell(..)
  , contentView
  , setContentView
  , emptyContentView
  , row
  , column
  , xPlacement
  , setXPlacement
  , yPlacement
  , setYPlacement
  , rowAlignment
  , setRowAlignment
  , customPlacementConstraints
  , setCustomPlacementConstraints
  , columnSelector
  , contentViewSelector
  , customPlacementConstraintsSelector
  , emptyContentViewSelector
  , rowAlignmentSelector
  , rowSelector
  , setContentViewSelector
  , setCustomPlacementConstraintsSelector
  , setRowAlignmentSelector
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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- contentView@
contentView :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSView)
contentView nsGridCell =
  sendMessage nsGridCell contentViewSelector

-- | @- setContentView:@
setContentView :: (IsNSGridCell nsGridCell, IsNSView value) => nsGridCell -> value -> IO ()
setContentView nsGridCell value =
  sendMessage nsGridCell setContentViewSelector (toNSView value)

-- | @+ emptyContentView@
emptyContentView :: IO (Id NSView)
emptyContentView  =
  do
    cls' <- getRequiredClass "NSGridCell"
    sendClassMessage cls' emptyContentViewSelector

-- | @- row@
row :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSGridRow)
row nsGridCell =
  sendMessage nsGridCell rowSelector

-- | @- column@
column :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSGridColumn)
column nsGridCell =
  sendMessage nsGridCell columnSelector

-- | @- xPlacement@
xPlacement :: IsNSGridCell nsGridCell => nsGridCell -> IO NSGridCellPlacement
xPlacement nsGridCell =
  sendMessage nsGridCell xPlacementSelector

-- | @- setXPlacement:@
setXPlacement :: IsNSGridCell nsGridCell => nsGridCell -> NSGridCellPlacement -> IO ()
setXPlacement nsGridCell value =
  sendMessage nsGridCell setXPlacementSelector value

-- | @- yPlacement@
yPlacement :: IsNSGridCell nsGridCell => nsGridCell -> IO NSGridCellPlacement
yPlacement nsGridCell =
  sendMessage nsGridCell yPlacementSelector

-- | @- setYPlacement:@
setYPlacement :: IsNSGridCell nsGridCell => nsGridCell -> NSGridCellPlacement -> IO ()
setYPlacement nsGridCell value =
  sendMessage nsGridCell setYPlacementSelector value

-- | @- rowAlignment@
rowAlignment :: IsNSGridCell nsGridCell => nsGridCell -> IO NSGridRowAlignment
rowAlignment nsGridCell =
  sendMessage nsGridCell rowAlignmentSelector

-- | @- setRowAlignment:@
setRowAlignment :: IsNSGridCell nsGridCell => nsGridCell -> NSGridRowAlignment -> IO ()
setRowAlignment nsGridCell value =
  sendMessage nsGridCell setRowAlignmentSelector value

-- | @- customPlacementConstraints@
customPlacementConstraints :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSArray)
customPlacementConstraints nsGridCell =
  sendMessage nsGridCell customPlacementConstraintsSelector

-- | @- setCustomPlacementConstraints:@
setCustomPlacementConstraints :: (IsNSGridCell nsGridCell, IsNSArray value) => nsGridCell -> value -> IO ()
setCustomPlacementConstraints nsGridCell value =
  sendMessage nsGridCell setCustomPlacementConstraintsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector '[] (Id NSView)
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector '[Id NSView] ()
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @emptyContentView@
emptyContentViewSelector :: Selector '[] (Id NSView)
emptyContentViewSelector = mkSelector "emptyContentView"

-- | @Selector@ for @row@
rowSelector :: Selector '[] (Id NSGridRow)
rowSelector = mkSelector "row"

-- | @Selector@ for @column@
columnSelector :: Selector '[] (Id NSGridColumn)
columnSelector = mkSelector "column"

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

-- | @Selector@ for @customPlacementConstraints@
customPlacementConstraintsSelector :: Selector '[] (Id NSArray)
customPlacementConstraintsSelector = mkSelector "customPlacementConstraints"

-- | @Selector@ for @setCustomPlacementConstraints:@
setCustomPlacementConstraintsSelector :: Selector '[Id NSArray] ()
setCustomPlacementConstraintsSelector = mkSelector "setCustomPlacementConstraints:"

