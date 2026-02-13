{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTextTableBlock@.
module ObjC.AppKit.NSTextTableBlock
  ( NSTextTableBlock
  , IsNSTextTableBlock(..)
  , initWithTable_startingRow_rowSpan_startingColumn_columnSpan
  , table
  , startingRow
  , rowSpan
  , startingColumn
  , columnSpan
  , columnSpanSelector
  , initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector
  , rowSpanSelector
  , startingColumnSelector
  , startingRowSelector
  , tableSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithTable:startingRow:rowSpan:startingColumn:columnSpan:@
initWithTable_startingRow_rowSpan_startingColumn_columnSpan :: (IsNSTextTableBlock nsTextTableBlock, IsNSTextTable table) => nsTextTableBlock -> table -> CLong -> CLong -> CLong -> CLong -> IO (Id NSTextTableBlock)
initWithTable_startingRow_rowSpan_startingColumn_columnSpan nsTextTableBlock table row rowSpan col colSpan =
  sendOwnedMessage nsTextTableBlock initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector (toNSTextTable table) row rowSpan col colSpan

-- | @- table@
table :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO (Id NSTextTable)
table nsTextTableBlock =
  sendMessage nsTextTableBlock tableSelector

-- | @- startingRow@
startingRow :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
startingRow nsTextTableBlock =
  sendMessage nsTextTableBlock startingRowSelector

-- | @- rowSpan@
rowSpan :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
rowSpan nsTextTableBlock =
  sendMessage nsTextTableBlock rowSpanSelector

-- | @- startingColumn@
startingColumn :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
startingColumn nsTextTableBlock =
  sendMessage nsTextTableBlock startingColumnSelector

-- | @- columnSpan@
columnSpan :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
columnSpan nsTextTableBlock =
  sendMessage nsTextTableBlock columnSpanSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTable:startingRow:rowSpan:startingColumn:columnSpan:@
initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector :: Selector '[Id NSTextTable, CLong, CLong, CLong, CLong] (Id NSTextTableBlock)
initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector = mkSelector "initWithTable:startingRow:rowSpan:startingColumn:columnSpan:"

-- | @Selector@ for @table@
tableSelector :: Selector '[] (Id NSTextTable)
tableSelector = mkSelector "table"

-- | @Selector@ for @startingRow@
startingRowSelector :: Selector '[] CLong
startingRowSelector = mkSelector "startingRow"

-- | @Selector@ for @rowSpan@
rowSpanSelector :: Selector '[] CLong
rowSpanSelector = mkSelector "rowSpan"

-- | @Selector@ for @startingColumn@
startingColumnSelector :: Selector '[] CLong
startingColumnSelector = mkSelector "startingColumn"

-- | @Selector@ for @columnSpan@
columnSpanSelector :: Selector '[] CLong
columnSpanSelector = mkSelector "columnSpan"

