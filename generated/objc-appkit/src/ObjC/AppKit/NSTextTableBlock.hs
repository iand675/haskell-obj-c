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
  , initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector
  , tableSelector
  , startingRowSelector
  , rowSpanSelector
  , startingColumnSelector
  , columnSpanSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithTable:startingRow:rowSpan:startingColumn:columnSpan:@
initWithTable_startingRow_rowSpan_startingColumn_columnSpan :: (IsNSTextTableBlock nsTextTableBlock, IsNSTextTable table) => nsTextTableBlock -> table -> CLong -> CLong -> CLong -> CLong -> IO (Id NSTextTableBlock)
initWithTable_startingRow_rowSpan_startingColumn_columnSpan nsTextTableBlock  table row rowSpan col colSpan =
withObjCPtr table $ \raw_table ->
    sendMsg nsTextTableBlock (mkSelector "initWithTable:startingRow:rowSpan:startingColumn:columnSpan:") (retPtr retVoid) [argPtr (castPtr raw_table :: Ptr ()), argCLong (fromIntegral row), argCLong (fromIntegral rowSpan), argCLong (fromIntegral col), argCLong (fromIntegral colSpan)] >>= ownedObject . castPtr

-- | @- table@
table :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO (Id NSTextTable)
table nsTextTableBlock  =
  sendMsg nsTextTableBlock (mkSelector "table") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- startingRow@
startingRow :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
startingRow nsTextTableBlock  =
  sendMsg nsTextTableBlock (mkSelector "startingRow") retCLong []

-- | @- rowSpan@
rowSpan :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
rowSpan nsTextTableBlock  =
  sendMsg nsTextTableBlock (mkSelector "rowSpan") retCLong []

-- | @- startingColumn@
startingColumn :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
startingColumn nsTextTableBlock  =
  sendMsg nsTextTableBlock (mkSelector "startingColumn") retCLong []

-- | @- columnSpan@
columnSpan :: IsNSTextTableBlock nsTextTableBlock => nsTextTableBlock -> IO CLong
columnSpan nsTextTableBlock  =
  sendMsg nsTextTableBlock (mkSelector "columnSpan") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTable:startingRow:rowSpan:startingColumn:columnSpan:@
initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector :: Selector
initWithTable_startingRow_rowSpan_startingColumn_columnSpanSelector = mkSelector "initWithTable:startingRow:rowSpan:startingColumn:columnSpan:"

-- | @Selector@ for @table@
tableSelector :: Selector
tableSelector = mkSelector "table"

-- | @Selector@ for @startingRow@
startingRowSelector :: Selector
startingRowSelector = mkSelector "startingRow"

-- | @Selector@ for @rowSpan@
rowSpanSelector :: Selector
rowSpanSelector = mkSelector "rowSpan"

-- | @Selector@ for @startingColumn@
startingColumnSelector :: Selector
startingColumnSelector = mkSelector "startingColumn"

-- | @Selector@ for @columnSpan@
columnSpanSelector :: Selector
columnSpanSelector = mkSelector "columnSpan"

