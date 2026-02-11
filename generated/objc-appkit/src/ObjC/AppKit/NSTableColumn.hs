{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSTableColumn@.
module ObjC.AppKit.NSTableColumn
  ( NSTableColumn
  , IsNSTableColumn(..)
  , initWithIdentifier
  , initWithCoder
  , sizeToFit
  , setResizable
  , isResizable
  , dataCellForRow
  , identifier
  , setIdentifier
  , tableView
  , setTableView
  , width
  , setWidth
  , minWidth
  , setMinWidth
  , maxWidth
  , setMaxWidth
  , title
  , setTitle
  , headerCell
  , setHeaderCell
  , editable
  , setEditable
  , sortDescriptorPrototype
  , setSortDescriptorPrototype
  , resizingMask
  , setResizingMask
  , hidden
  , setHidden
  , dataCell
  , setDataCell
  , initWithIdentifierSelector
  , initWithCoderSelector
  , sizeToFitSelector
  , setResizableSelector
  , isResizableSelector
  , dataCellForRowSelector
  , identifierSelector
  , setIdentifierSelector
  , tableViewSelector
  , setTableViewSelector
  , widthSelector
  , setWidthSelector
  , minWidthSelector
  , setMinWidthSelector
  , maxWidthSelector
  , setMaxWidthSelector
  , titleSelector
  , setTitleSelector
  , headerCellSelector
  , setHeaderCellSelector
  , editableSelector
  , setEditableSelector
  , sortDescriptorPrototypeSelector
  , setSortDescriptorPrototypeSelector
  , resizingMaskSelector
  , setResizingMaskSelector
  , hiddenSelector
  , setHiddenSelector
  , dataCellSelector
  , setDataCellSelector

  -- * Enum types
  , NSTableColumnResizingOptions(NSTableColumnResizingOptions)
  , pattern NSTableColumnNoResizing
  , pattern NSTableColumnAutoresizingMask
  , pattern NSTableColumnUserResizingMask

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

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSTableColumn nsTableColumn, IsNSString identifier) => nsTableColumn -> identifier -> IO (Id NSTableColumn)
initWithIdentifier nsTableColumn  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsTableColumn (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSTableColumn nsTableColumn, IsNSCoder coder) => nsTableColumn -> coder -> IO (Id NSTableColumn)
initWithCoder nsTableColumn  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsTableColumn (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- sizeToFit@
sizeToFit :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO ()
sizeToFit nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "sizeToFit") retVoid []

-- | @- setResizable:@
setResizable :: IsNSTableColumn nsTableColumn => nsTableColumn -> Bool -> IO ()
setResizable nsTableColumn  flag =
  sendMsg nsTableColumn (mkSelector "setResizable:") retVoid [argCULong (if flag then 1 else 0)]

-- | @- isResizable@
isResizable :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO Bool
isResizable nsTableColumn  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableColumn (mkSelector "isResizable") retCULong []

-- | @- dataCellForRow:@
dataCellForRow :: IsNSTableColumn nsTableColumn => nsTableColumn -> CLong -> IO RawId
dataCellForRow nsTableColumn  row =
  fmap (RawId . castPtr) $ sendMsg nsTableColumn (mkSelector "dataCellForRow:") (retPtr retVoid) [argCLong (fromIntegral row)]

-- | @- identifier@
identifier :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSString)
identifier nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIdentifier:@
setIdentifier :: (IsNSTableColumn nsTableColumn, IsNSString value) => nsTableColumn -> value -> IO ()
setIdentifier nsTableColumn  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableColumn (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- tableView@
tableView :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSTableView)
tableView nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "tableView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTableView:@
setTableView :: (IsNSTableColumn nsTableColumn, IsNSTableView value) => nsTableColumn -> value -> IO ()
setTableView nsTableColumn  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableColumn (mkSelector "setTableView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- width@
width :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO CDouble
width nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "width") retCDouble []

-- | @- setWidth:@
setWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> CDouble -> IO ()
setWidth nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- minWidth@
minWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO CDouble
minWidth nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "minWidth") retCDouble []

-- | @- setMinWidth:@
setMinWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> CDouble -> IO ()
setMinWidth nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setMinWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxWidth@
maxWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO CDouble
maxWidth nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "maxWidth") retCDouble []

-- | @- setMaxWidth:@
setMaxWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> CDouble -> IO ()
setMaxWidth nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setMaxWidth:") retVoid [argCDouble (fromIntegral value)]

-- | @- title@
title :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSString)
title nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSTableColumn nsTableColumn, IsNSString value) => nsTableColumn -> value -> IO ()
setTitle nsTableColumn  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableColumn (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- headerCell@
headerCell :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSTableHeaderCell)
headerCell nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "headerCell") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHeaderCell:@
setHeaderCell :: (IsNSTableColumn nsTableColumn, IsNSTableHeaderCell value) => nsTableColumn -> value -> IO ()
setHeaderCell nsTableColumn  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableColumn (mkSelector "setHeaderCell:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- editable@
editable :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO Bool
editable nsTableColumn  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableColumn (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSTableColumn nsTableColumn => nsTableColumn -> Bool -> IO ()
setEditable nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- sortDescriptorPrototype@
sortDescriptorPrototype :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSSortDescriptor)
sortDescriptorPrototype nsTableColumn  =
  sendMsg nsTableColumn (mkSelector "sortDescriptorPrototype") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptorPrototype:@
setSortDescriptorPrototype :: (IsNSTableColumn nsTableColumn, IsNSSortDescriptor value) => nsTableColumn -> value -> IO ()
setSortDescriptorPrototype nsTableColumn  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsTableColumn (mkSelector "setSortDescriptorPrototype:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resizingMask@
resizingMask :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO NSTableColumnResizingOptions
resizingMask nsTableColumn  =
  fmap (coerce :: CULong -> NSTableColumnResizingOptions) $ sendMsg nsTableColumn (mkSelector "resizingMask") retCULong []

-- | @- setResizingMask:@
setResizingMask :: IsNSTableColumn nsTableColumn => nsTableColumn -> NSTableColumnResizingOptions -> IO ()
setResizingMask nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setResizingMask:") retVoid [argCULong (coerce value)]

-- | @- hidden@
hidden :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO Bool
hidden nsTableColumn  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsTableColumn (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSTableColumn nsTableColumn => nsTableColumn -> Bool -> IO ()
setHidden nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- dataCell@
dataCell :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO RawId
dataCell nsTableColumn  =
  fmap (RawId . castPtr) $ sendMsg nsTableColumn (mkSelector "dataCell") (retPtr retVoid) []

-- | @- setDataCell:@
setDataCell :: IsNSTableColumn nsTableColumn => nsTableColumn -> RawId -> IO ()
setDataCell nsTableColumn  value =
  sendMsg nsTableColumn (mkSelector "setDataCell:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @setResizable:@
setResizableSelector :: Selector
setResizableSelector = mkSelector "setResizable:"

-- | @Selector@ for @isResizable@
isResizableSelector :: Selector
isResizableSelector = mkSelector "isResizable"

-- | @Selector@ for @dataCellForRow:@
dataCellForRowSelector :: Selector
dataCellForRowSelector = mkSelector "dataCellForRow:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @tableView@
tableViewSelector :: Selector
tableViewSelector = mkSelector "tableView"

-- | @Selector@ for @setTableView:@
setTableViewSelector :: Selector
setTableViewSelector = mkSelector "setTableView:"

-- | @Selector@ for @width@
widthSelector :: Selector
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @minWidth@
minWidthSelector :: Selector
minWidthSelector = mkSelector "minWidth"

-- | @Selector@ for @setMinWidth:@
setMinWidthSelector :: Selector
setMinWidthSelector = mkSelector "setMinWidth:"

-- | @Selector@ for @maxWidth@
maxWidthSelector :: Selector
maxWidthSelector = mkSelector "maxWidth"

-- | @Selector@ for @setMaxWidth:@
setMaxWidthSelector :: Selector
setMaxWidthSelector = mkSelector "setMaxWidth:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @headerCell@
headerCellSelector :: Selector
headerCellSelector = mkSelector "headerCell"

-- | @Selector@ for @setHeaderCell:@
setHeaderCellSelector :: Selector
setHeaderCellSelector = mkSelector "setHeaderCell:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @sortDescriptorPrototype@
sortDescriptorPrototypeSelector :: Selector
sortDescriptorPrototypeSelector = mkSelector "sortDescriptorPrototype"

-- | @Selector@ for @setSortDescriptorPrototype:@
setSortDescriptorPrototypeSelector :: Selector
setSortDescriptorPrototypeSelector = mkSelector "setSortDescriptorPrototype:"

-- | @Selector@ for @resizingMask@
resizingMaskSelector :: Selector
resizingMaskSelector = mkSelector "resizingMask"

-- | @Selector@ for @setResizingMask:@
setResizingMaskSelector :: Selector
setResizingMaskSelector = mkSelector "setResizingMask:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @dataCell@
dataCellSelector :: Selector
dataCellSelector = mkSelector "dataCell"

-- | @Selector@ for @setDataCell:@
setDataCellSelector :: Selector
setDataCellSelector = mkSelector "setDataCell:"

