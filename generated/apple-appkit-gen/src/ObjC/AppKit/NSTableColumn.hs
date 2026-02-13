{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , headerToolTip
  , setHeaderToolTip
  , hidden
  , setHidden
  , dataCell
  , setDataCell
  , dataCellForRowSelector
  , dataCellSelector
  , editableSelector
  , headerCellSelector
  , headerToolTipSelector
  , hiddenSelector
  , identifierSelector
  , initWithCoderSelector
  , initWithIdentifierSelector
  , isResizableSelector
  , maxWidthSelector
  , minWidthSelector
  , resizingMaskSelector
  , setDataCellSelector
  , setEditableSelector
  , setHeaderCellSelector
  , setHeaderToolTipSelector
  , setHiddenSelector
  , setIdentifierSelector
  , setMaxWidthSelector
  , setMinWidthSelector
  , setResizableSelector
  , setResizingMaskSelector
  , setSortDescriptorPrototypeSelector
  , setTableViewSelector
  , setTitleSelector
  , setWidthSelector
  , sizeToFitSelector
  , sortDescriptorPrototypeSelector
  , tableViewSelector
  , titleSelector
  , widthSelector

  -- * Enum types
  , NSTableColumnResizingOptions(NSTableColumnResizingOptions)
  , pattern NSTableColumnNoResizing
  , pattern NSTableColumnAutoresizingMask
  , pattern NSTableColumnUserResizingMask

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

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSTableColumn nsTableColumn, IsNSString identifier) => nsTableColumn -> identifier -> IO (Id NSTableColumn)
initWithIdentifier nsTableColumn identifier =
  sendOwnedMessage nsTableColumn initWithIdentifierSelector (toNSString identifier)

-- | @- initWithCoder:@
initWithCoder :: (IsNSTableColumn nsTableColumn, IsNSCoder coder) => nsTableColumn -> coder -> IO (Id NSTableColumn)
initWithCoder nsTableColumn coder =
  sendOwnedMessage nsTableColumn initWithCoderSelector (toNSCoder coder)

-- | @- sizeToFit@
sizeToFit :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO ()
sizeToFit nsTableColumn =
  sendMessage nsTableColumn sizeToFitSelector

-- | @- setResizable:@
setResizable :: IsNSTableColumn nsTableColumn => nsTableColumn -> Bool -> IO ()
setResizable nsTableColumn flag =
  sendMessage nsTableColumn setResizableSelector flag

-- | @- isResizable@
isResizable :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO Bool
isResizable nsTableColumn =
  sendMessage nsTableColumn isResizableSelector

-- | @- dataCellForRow:@
dataCellForRow :: IsNSTableColumn nsTableColumn => nsTableColumn -> CLong -> IO RawId
dataCellForRow nsTableColumn row =
  sendMessage nsTableColumn dataCellForRowSelector row

-- | @- identifier@
identifier :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSString)
identifier nsTableColumn =
  sendMessage nsTableColumn identifierSelector

-- | @- setIdentifier:@
setIdentifier :: (IsNSTableColumn nsTableColumn, IsNSString value) => nsTableColumn -> value -> IO ()
setIdentifier nsTableColumn value =
  sendMessage nsTableColumn setIdentifierSelector (toNSString value)

-- | @- tableView@
tableView :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSTableView)
tableView nsTableColumn =
  sendMessage nsTableColumn tableViewSelector

-- | @- setTableView:@
setTableView :: (IsNSTableColumn nsTableColumn, IsNSTableView value) => nsTableColumn -> value -> IO ()
setTableView nsTableColumn value =
  sendMessage nsTableColumn setTableViewSelector (toNSTableView value)

-- | @- width@
width :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO CDouble
width nsTableColumn =
  sendMessage nsTableColumn widthSelector

-- | @- setWidth:@
setWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> CDouble -> IO ()
setWidth nsTableColumn value =
  sendMessage nsTableColumn setWidthSelector value

-- | @- minWidth@
minWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO CDouble
minWidth nsTableColumn =
  sendMessage nsTableColumn minWidthSelector

-- | @- setMinWidth:@
setMinWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> CDouble -> IO ()
setMinWidth nsTableColumn value =
  sendMessage nsTableColumn setMinWidthSelector value

-- | @- maxWidth@
maxWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO CDouble
maxWidth nsTableColumn =
  sendMessage nsTableColumn maxWidthSelector

-- | @- setMaxWidth:@
setMaxWidth :: IsNSTableColumn nsTableColumn => nsTableColumn -> CDouble -> IO ()
setMaxWidth nsTableColumn value =
  sendMessage nsTableColumn setMaxWidthSelector value

-- | @- title@
title :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSString)
title nsTableColumn =
  sendMessage nsTableColumn titleSelector

-- | @- setTitle:@
setTitle :: (IsNSTableColumn nsTableColumn, IsNSString value) => nsTableColumn -> value -> IO ()
setTitle nsTableColumn value =
  sendMessage nsTableColumn setTitleSelector (toNSString value)

-- | @- headerCell@
headerCell :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSTableHeaderCell)
headerCell nsTableColumn =
  sendMessage nsTableColumn headerCellSelector

-- | @- setHeaderCell:@
setHeaderCell :: (IsNSTableColumn nsTableColumn, IsNSTableHeaderCell value) => nsTableColumn -> value -> IO ()
setHeaderCell nsTableColumn value =
  sendMessage nsTableColumn setHeaderCellSelector (toNSTableHeaderCell value)

-- | @- editable@
editable :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO Bool
editable nsTableColumn =
  sendMessage nsTableColumn editableSelector

-- | @- setEditable:@
setEditable :: IsNSTableColumn nsTableColumn => nsTableColumn -> Bool -> IO ()
setEditable nsTableColumn value =
  sendMessage nsTableColumn setEditableSelector value

-- | @- sortDescriptorPrototype@
sortDescriptorPrototype :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSSortDescriptor)
sortDescriptorPrototype nsTableColumn =
  sendMessage nsTableColumn sortDescriptorPrototypeSelector

-- | @- setSortDescriptorPrototype:@
setSortDescriptorPrototype :: (IsNSTableColumn nsTableColumn, IsNSSortDescriptor value) => nsTableColumn -> value -> IO ()
setSortDescriptorPrototype nsTableColumn value =
  sendMessage nsTableColumn setSortDescriptorPrototypeSelector (toNSSortDescriptor value)

-- | @- resizingMask@
resizingMask :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO NSTableColumnResizingOptions
resizingMask nsTableColumn =
  sendMessage nsTableColumn resizingMaskSelector

-- | @- setResizingMask:@
setResizingMask :: IsNSTableColumn nsTableColumn => nsTableColumn -> NSTableColumnResizingOptions -> IO ()
setResizingMask nsTableColumn value =
  sendMessage nsTableColumn setResizingMaskSelector value

-- | @- headerToolTip@
headerToolTip :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO (Id NSString)
headerToolTip nsTableColumn =
  sendMessage nsTableColumn headerToolTipSelector

-- | @- setHeaderToolTip:@
setHeaderToolTip :: (IsNSTableColumn nsTableColumn, IsNSString value) => nsTableColumn -> value -> IO ()
setHeaderToolTip nsTableColumn value =
  sendMessage nsTableColumn setHeaderToolTipSelector (toNSString value)

-- | @- hidden@
hidden :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO Bool
hidden nsTableColumn =
  sendMessage nsTableColumn hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSTableColumn nsTableColumn => nsTableColumn -> Bool -> IO ()
setHidden nsTableColumn value =
  sendMessage nsTableColumn setHiddenSelector value

-- | @- dataCell@
dataCell :: IsNSTableColumn nsTableColumn => nsTableColumn -> IO RawId
dataCell nsTableColumn =
  sendMessage nsTableColumn dataCellSelector

-- | @- setDataCell:@
setDataCell :: IsNSTableColumn nsTableColumn => nsTableColumn -> RawId -> IO ()
setDataCell nsTableColumn value =
  sendMessage nsTableColumn setDataCellSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id NSTableColumn)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSTableColumn)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @setResizable:@
setResizableSelector :: Selector '[Bool] ()
setResizableSelector = mkSelector "setResizable:"

-- | @Selector@ for @isResizable@
isResizableSelector :: Selector '[] Bool
isResizableSelector = mkSelector "isResizable"

-- | @Selector@ for @dataCellForRow:@
dataCellForRowSelector :: Selector '[CLong] RawId
dataCellForRowSelector = mkSelector "dataCellForRow:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @tableView@
tableViewSelector :: Selector '[] (Id NSTableView)
tableViewSelector = mkSelector "tableView"

-- | @Selector@ for @setTableView:@
setTableViewSelector :: Selector '[Id NSTableView] ()
setTableViewSelector = mkSelector "setTableView:"

-- | @Selector@ for @width@
widthSelector :: Selector '[] CDouble
widthSelector = mkSelector "width"

-- | @Selector@ for @setWidth:@
setWidthSelector :: Selector '[CDouble] ()
setWidthSelector = mkSelector "setWidth:"

-- | @Selector@ for @minWidth@
minWidthSelector :: Selector '[] CDouble
minWidthSelector = mkSelector "minWidth"

-- | @Selector@ for @setMinWidth:@
setMinWidthSelector :: Selector '[CDouble] ()
setMinWidthSelector = mkSelector "setMinWidth:"

-- | @Selector@ for @maxWidth@
maxWidthSelector :: Selector '[] CDouble
maxWidthSelector = mkSelector "maxWidth"

-- | @Selector@ for @setMaxWidth:@
setMaxWidthSelector :: Selector '[CDouble] ()
setMaxWidthSelector = mkSelector "setMaxWidth:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @headerCell@
headerCellSelector :: Selector '[] (Id NSTableHeaderCell)
headerCellSelector = mkSelector "headerCell"

-- | @Selector@ for @setHeaderCell:@
setHeaderCellSelector :: Selector '[Id NSTableHeaderCell] ()
setHeaderCellSelector = mkSelector "setHeaderCell:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @sortDescriptorPrototype@
sortDescriptorPrototypeSelector :: Selector '[] (Id NSSortDescriptor)
sortDescriptorPrototypeSelector = mkSelector "sortDescriptorPrototype"

-- | @Selector@ for @setSortDescriptorPrototype:@
setSortDescriptorPrototypeSelector :: Selector '[Id NSSortDescriptor] ()
setSortDescriptorPrototypeSelector = mkSelector "setSortDescriptorPrototype:"

-- | @Selector@ for @resizingMask@
resizingMaskSelector :: Selector '[] NSTableColumnResizingOptions
resizingMaskSelector = mkSelector "resizingMask"

-- | @Selector@ for @setResizingMask:@
setResizingMaskSelector :: Selector '[NSTableColumnResizingOptions] ()
setResizingMaskSelector = mkSelector "setResizingMask:"

-- | @Selector@ for @headerToolTip@
headerToolTipSelector :: Selector '[] (Id NSString)
headerToolTipSelector = mkSelector "headerToolTip"

-- | @Selector@ for @setHeaderToolTip:@
setHeaderToolTipSelector :: Selector '[Id NSString] ()
setHeaderToolTipSelector = mkSelector "setHeaderToolTip:"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @dataCell@
dataCellSelector :: Selector '[] RawId
dataCellSelector = mkSelector "dataCell"

-- | @Selector@ for @setDataCell:@
setDataCellSelector :: Selector '[RawId] ()
setDataCellSelector = mkSelector "setDataCell:"

