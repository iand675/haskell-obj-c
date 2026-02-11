{-# LANGUAGE PatternSynonyms #-}
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
  , contentViewSelector
  , setContentViewSelector
  , emptyContentViewSelector
  , rowSelector
  , columnSelector
  , xPlacementSelector
  , setXPlacementSelector
  , yPlacementSelector
  , setYPlacementSelector
  , rowAlignmentSelector
  , setRowAlignmentSelector
  , customPlacementConstraintsSelector
  , setCustomPlacementConstraintsSelector

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- contentView@
contentView :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSView)
contentView nsGridCell  =
  sendMsg nsGridCell (mkSelector "contentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContentView:@
setContentView :: (IsNSGridCell nsGridCell, IsNSView value) => nsGridCell -> value -> IO ()
setContentView nsGridCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGridCell (mkSelector "setContentView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ emptyContentView@
emptyContentView :: IO (Id NSView)
emptyContentView  =
  do
    cls' <- getRequiredClass "NSGridCell"
    sendClassMsg cls' (mkSelector "emptyContentView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- row@
row :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSGridRow)
row nsGridCell  =
  sendMsg nsGridCell (mkSelector "row") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- column@
column :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSGridColumn)
column nsGridCell  =
  sendMsg nsGridCell (mkSelector "column") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- xPlacement@
xPlacement :: IsNSGridCell nsGridCell => nsGridCell -> IO NSGridCellPlacement
xPlacement nsGridCell  =
  fmap (coerce :: CLong -> NSGridCellPlacement) $ sendMsg nsGridCell (mkSelector "xPlacement") retCLong []

-- | @- setXPlacement:@
setXPlacement :: IsNSGridCell nsGridCell => nsGridCell -> NSGridCellPlacement -> IO ()
setXPlacement nsGridCell  value =
  sendMsg nsGridCell (mkSelector "setXPlacement:") retVoid [argCLong (coerce value)]

-- | @- yPlacement@
yPlacement :: IsNSGridCell nsGridCell => nsGridCell -> IO NSGridCellPlacement
yPlacement nsGridCell  =
  fmap (coerce :: CLong -> NSGridCellPlacement) $ sendMsg nsGridCell (mkSelector "yPlacement") retCLong []

-- | @- setYPlacement:@
setYPlacement :: IsNSGridCell nsGridCell => nsGridCell -> NSGridCellPlacement -> IO ()
setYPlacement nsGridCell  value =
  sendMsg nsGridCell (mkSelector "setYPlacement:") retVoid [argCLong (coerce value)]

-- | @- rowAlignment@
rowAlignment :: IsNSGridCell nsGridCell => nsGridCell -> IO NSGridRowAlignment
rowAlignment nsGridCell  =
  fmap (coerce :: CLong -> NSGridRowAlignment) $ sendMsg nsGridCell (mkSelector "rowAlignment") retCLong []

-- | @- setRowAlignment:@
setRowAlignment :: IsNSGridCell nsGridCell => nsGridCell -> NSGridRowAlignment -> IO ()
setRowAlignment nsGridCell  value =
  sendMsg nsGridCell (mkSelector "setRowAlignment:") retVoid [argCLong (coerce value)]

-- | @- customPlacementConstraints@
customPlacementConstraints :: IsNSGridCell nsGridCell => nsGridCell -> IO (Id NSArray)
customPlacementConstraints nsGridCell  =
  sendMsg nsGridCell (mkSelector "customPlacementConstraints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCustomPlacementConstraints:@
setCustomPlacementConstraints :: (IsNSGridCell nsGridCell, IsNSArray value) => nsGridCell -> value -> IO ()
setCustomPlacementConstraints nsGridCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsGridCell (mkSelector "setCustomPlacementConstraints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @contentView@
contentViewSelector :: Selector
contentViewSelector = mkSelector "contentView"

-- | @Selector@ for @setContentView:@
setContentViewSelector :: Selector
setContentViewSelector = mkSelector "setContentView:"

-- | @Selector@ for @emptyContentView@
emptyContentViewSelector :: Selector
emptyContentViewSelector = mkSelector "emptyContentView"

-- | @Selector@ for @row@
rowSelector :: Selector
rowSelector = mkSelector "row"

-- | @Selector@ for @column@
columnSelector :: Selector
columnSelector = mkSelector "column"

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

-- | @Selector@ for @customPlacementConstraints@
customPlacementConstraintsSelector :: Selector
customPlacementConstraintsSelector = mkSelector "customPlacementConstraints"

-- | @Selector@ for @setCustomPlacementConstraints:@
setCustomPlacementConstraintsSelector :: Selector
setCustomPlacementConstraintsSelector = mkSelector "setCustomPlacementConstraints:"

