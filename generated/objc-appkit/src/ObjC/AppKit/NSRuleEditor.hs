{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRuleEditor@.
module ObjC.AppKit.NSRuleEditor
  ( NSRuleEditor
  , IsNSRuleEditor(..)
  , reloadCriteria
  , reloadPredicate
  , predicateForRow
  , subrowIndexesForRow
  , criteriaForRow
  , displayValuesForRow
  , rowForDisplayValue
  , rowTypeForRow
  , parentRowForRow
  , addRow
  , insertRowAtIndex_withType_asSubrowOfRow_animate
  , setCriteria_andDisplayValues_forRowAtIndex
  , removeRowAtIndex
  , removeRowsAtIndexes_includeSubrows
  , selectRowIndexes_byExtendingSelection
  , formattingStringsFilename
  , setFormattingStringsFilename
  , formattingDictionary
  , setFormattingDictionary
  , nestingMode
  , setNestingMode
  , rowHeight
  , setRowHeight
  , editable
  , setEditable
  , canRemoveAllRows
  , setCanRemoveAllRows
  , predicate
  , numberOfRows
  , selectedRowIndexes
  , rowClass
  , setRowClass
  , rowTypeKeyPath
  , setRowTypeKeyPath
  , subrowsKeyPath
  , setSubrowsKeyPath
  , criteriaKeyPath
  , setCriteriaKeyPath
  , displayValuesKeyPath
  , setDisplayValuesKeyPath
  , reloadCriteriaSelector
  , reloadPredicateSelector
  , predicateForRowSelector
  , subrowIndexesForRowSelector
  , criteriaForRowSelector
  , displayValuesForRowSelector
  , rowForDisplayValueSelector
  , rowTypeForRowSelector
  , parentRowForRowSelector
  , addRowSelector
  , insertRowAtIndex_withType_asSubrowOfRow_animateSelector
  , setCriteria_andDisplayValues_forRowAtIndexSelector
  , removeRowAtIndexSelector
  , removeRowsAtIndexes_includeSubrowsSelector
  , selectRowIndexes_byExtendingSelectionSelector
  , formattingStringsFilenameSelector
  , setFormattingStringsFilenameSelector
  , formattingDictionarySelector
  , setFormattingDictionarySelector
  , nestingModeSelector
  , setNestingModeSelector
  , rowHeightSelector
  , setRowHeightSelector
  , editableSelector
  , setEditableSelector
  , canRemoveAllRowsSelector
  , setCanRemoveAllRowsSelector
  , predicateSelector
  , numberOfRowsSelector
  , selectedRowIndexesSelector
  , rowClassSelector
  , setRowClassSelector
  , rowTypeKeyPathSelector
  , setRowTypeKeyPathSelector
  , subrowsKeyPathSelector
  , setSubrowsKeyPathSelector
  , criteriaKeyPathSelector
  , setCriteriaKeyPathSelector
  , displayValuesKeyPathSelector
  , setDisplayValuesKeyPathSelector

  -- * Enum types
  , NSRuleEditorNestingMode(NSRuleEditorNestingMode)
  , pattern NSRuleEditorNestingModeSingle
  , pattern NSRuleEditorNestingModeList
  , pattern NSRuleEditorNestingModeCompound
  , pattern NSRuleEditorNestingModeSimple
  , NSRuleEditorRowType(NSRuleEditorRowType)
  , pattern NSRuleEditorRowTypeSimple
  , pattern NSRuleEditorRowTypeCompound

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

-- | @- reloadCriteria@
reloadCriteria :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO ()
reloadCriteria nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "reloadCriteria") retVoid []

-- | @- reloadPredicate@
reloadPredicate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO ()
reloadPredicate nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "reloadPredicate") retVoid []

-- | @- predicateForRow:@
predicateForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSPredicate)
predicateForRow nsRuleEditor  row =
  sendMsg nsRuleEditor (mkSelector "predicateForRow:") (retPtr retVoid) [argCLong (fromIntegral row)] >>= retainedObject . castPtr

-- | @- subrowIndexesForRow:@
subrowIndexesForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSIndexSet)
subrowIndexesForRow nsRuleEditor  rowIndex =
  sendMsg nsRuleEditor (mkSelector "subrowIndexesForRow:") (retPtr retVoid) [argCLong (fromIntegral rowIndex)] >>= retainedObject . castPtr

-- | @- criteriaForRow:@
criteriaForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSArray)
criteriaForRow nsRuleEditor  row =
  sendMsg nsRuleEditor (mkSelector "criteriaForRow:") (retPtr retVoid) [argCLong (fromIntegral row)] >>= retainedObject . castPtr

-- | @- displayValuesForRow:@
displayValuesForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSArray)
displayValuesForRow nsRuleEditor  row =
  sendMsg nsRuleEditor (mkSelector "displayValuesForRow:") (retPtr retVoid) [argCLong (fromIntegral row)] >>= retainedObject . castPtr

-- | @- rowForDisplayValue:@
rowForDisplayValue :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> RawId -> IO CLong
rowForDisplayValue nsRuleEditor  displayValue =
  sendMsg nsRuleEditor (mkSelector "rowForDisplayValue:") retCLong [argPtr (castPtr (unRawId displayValue) :: Ptr ())]

-- | @- rowTypeForRow:@
rowTypeForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO NSRuleEditorRowType
rowTypeForRow nsRuleEditor  rowIndex =
  fmap (coerce :: CULong -> NSRuleEditorRowType) $ sendMsg nsRuleEditor (mkSelector "rowTypeForRow:") retCULong [argCLong (fromIntegral rowIndex)]

-- | @- parentRowForRow:@
parentRowForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO CLong
parentRowForRow nsRuleEditor  rowIndex =
  sendMsg nsRuleEditor (mkSelector "parentRowForRow:") retCLong [argCLong (fromIntegral rowIndex)]

-- | @- addRow:@
addRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> RawId -> IO ()
addRow nsRuleEditor  sender =
  sendMsg nsRuleEditor (mkSelector "addRow:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- insertRowAtIndex:withType:asSubrowOfRow:animate:@
insertRowAtIndex_withType_asSubrowOfRow_animate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> NSRuleEditorRowType -> CLong -> Bool -> IO ()
insertRowAtIndex_withType_asSubrowOfRow_animate nsRuleEditor  rowIndex rowType parentRow shouldAnimate =
  sendMsg nsRuleEditor (mkSelector "insertRowAtIndex:withType:asSubrowOfRow:animate:") retVoid [argCLong (fromIntegral rowIndex), argCULong (coerce rowType), argCLong (fromIntegral parentRow), argCULong (if shouldAnimate then 1 else 0)]

-- | @- setCriteria:andDisplayValues:forRowAtIndex:@
setCriteria_andDisplayValues_forRowAtIndex :: (IsNSRuleEditor nsRuleEditor, IsNSArray criteria, IsNSArray values) => nsRuleEditor -> criteria -> values -> CLong -> IO ()
setCriteria_andDisplayValues_forRowAtIndex nsRuleEditor  criteria values rowIndex =
withObjCPtr criteria $ \raw_criteria ->
  withObjCPtr values $ \raw_values ->
      sendMsg nsRuleEditor (mkSelector "setCriteria:andDisplayValues:forRowAtIndex:") retVoid [argPtr (castPtr raw_criteria :: Ptr ()), argPtr (castPtr raw_values :: Ptr ()), argCLong (fromIntegral rowIndex)]

-- | @- removeRowAtIndex:@
removeRowAtIndex :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO ()
removeRowAtIndex nsRuleEditor  rowIndex =
  sendMsg nsRuleEditor (mkSelector "removeRowAtIndex:") retVoid [argCLong (fromIntegral rowIndex)]

-- | @- removeRowsAtIndexes:includeSubrows:@
removeRowsAtIndexes_includeSubrows :: (IsNSRuleEditor nsRuleEditor, IsNSIndexSet rowIndexes) => nsRuleEditor -> rowIndexes -> Bool -> IO ()
removeRowsAtIndexes_includeSubrows nsRuleEditor  rowIndexes includeSubrows =
withObjCPtr rowIndexes $ \raw_rowIndexes ->
    sendMsg nsRuleEditor (mkSelector "removeRowsAtIndexes:includeSubrows:") retVoid [argPtr (castPtr raw_rowIndexes :: Ptr ()), argCULong (if includeSubrows then 1 else 0)]

-- | @- selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelection :: (IsNSRuleEditor nsRuleEditor, IsNSIndexSet indexes) => nsRuleEditor -> indexes -> Bool -> IO ()
selectRowIndexes_byExtendingSelection nsRuleEditor  indexes extend =
withObjCPtr indexes $ \raw_indexes ->
    sendMsg nsRuleEditor (mkSelector "selectRowIndexes:byExtendingSelection:") retVoid [argPtr (castPtr raw_indexes :: Ptr ()), argCULong (if extend then 1 else 0)]

-- | @- formattingStringsFilename@
formattingStringsFilename :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
formattingStringsFilename nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "formattingStringsFilename") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFormattingStringsFilename:@
setFormattingStringsFilename :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setFormattingStringsFilename nsRuleEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRuleEditor (mkSelector "setFormattingStringsFilename:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- formattingDictionary@
formattingDictionary :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSDictionary)
formattingDictionary nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "formattingDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFormattingDictionary:@
setFormattingDictionary :: (IsNSRuleEditor nsRuleEditor, IsNSDictionary value) => nsRuleEditor -> value -> IO ()
setFormattingDictionary nsRuleEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRuleEditor (mkSelector "setFormattingDictionary:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- nestingMode@
nestingMode :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO NSRuleEditorNestingMode
nestingMode nsRuleEditor  =
  fmap (coerce :: CULong -> NSRuleEditorNestingMode) $ sendMsg nsRuleEditor (mkSelector "nestingMode") retCULong []

-- | @- setNestingMode:@
setNestingMode :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> NSRuleEditorNestingMode -> IO ()
setNestingMode nsRuleEditor  value =
  sendMsg nsRuleEditor (mkSelector "setNestingMode:") retVoid [argCULong (coerce value)]

-- | @- rowHeight@
rowHeight :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO CDouble
rowHeight nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "rowHeight") retCDouble []

-- | @- setRowHeight:@
setRowHeight :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CDouble -> IO ()
setRowHeight nsRuleEditor  value =
  sendMsg nsRuleEditor (mkSelector "setRowHeight:") retVoid [argCDouble (fromIntegral value)]

-- | @- editable@
editable :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO Bool
editable nsRuleEditor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRuleEditor (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> Bool -> IO ()
setEditable nsRuleEditor  value =
  sendMsg nsRuleEditor (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- canRemoveAllRows@
canRemoveAllRows :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO Bool
canRemoveAllRows nsRuleEditor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRuleEditor (mkSelector "canRemoveAllRows") retCULong []

-- | @- setCanRemoveAllRows:@
setCanRemoveAllRows :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> Bool -> IO ()
setCanRemoveAllRows nsRuleEditor  value =
  sendMsg nsRuleEditor (mkSelector "setCanRemoveAllRows:") retVoid [argCULong (if value then 1 else 0)]

-- | @- predicate@
predicate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSPredicate)
predicate nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfRows@
numberOfRows :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO CLong
numberOfRows nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "numberOfRows") retCLong []

-- | @- selectedRowIndexes@
selectedRowIndexes :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSIndexSet)
selectedRowIndexes nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "selectedRowIndexes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- rowClass@
rowClass :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO Class
rowClass nsRuleEditor  =
  fmap (Class . castPtr) $ sendMsg nsRuleEditor (mkSelector "rowClass") (retPtr retVoid) []

-- | @- setRowClass:@
setRowClass :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> Class -> IO ()
setRowClass nsRuleEditor  value =
  sendMsg nsRuleEditor (mkSelector "setRowClass:") retVoid [argPtr (unClass value)]

-- | @- rowTypeKeyPath@
rowTypeKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
rowTypeKeyPath nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "rowTypeKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRowTypeKeyPath:@
setRowTypeKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setRowTypeKeyPath nsRuleEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRuleEditor (mkSelector "setRowTypeKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subrowsKeyPath@
subrowsKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
subrowsKeyPath nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "subrowsKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubrowsKeyPath:@
setSubrowsKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setSubrowsKeyPath nsRuleEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRuleEditor (mkSelector "setSubrowsKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- criteriaKeyPath@
criteriaKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
criteriaKeyPath nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "criteriaKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCriteriaKeyPath:@
setCriteriaKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setCriteriaKeyPath nsRuleEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRuleEditor (mkSelector "setCriteriaKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- displayValuesKeyPath@
displayValuesKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
displayValuesKeyPath nsRuleEditor  =
  sendMsg nsRuleEditor (mkSelector "displayValuesKeyPath") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDisplayValuesKeyPath:@
setDisplayValuesKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setDisplayValuesKeyPath nsRuleEditor  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRuleEditor (mkSelector "setDisplayValuesKeyPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadCriteria@
reloadCriteriaSelector :: Selector
reloadCriteriaSelector = mkSelector "reloadCriteria"

-- | @Selector@ for @reloadPredicate@
reloadPredicateSelector :: Selector
reloadPredicateSelector = mkSelector "reloadPredicate"

-- | @Selector@ for @predicateForRow:@
predicateForRowSelector :: Selector
predicateForRowSelector = mkSelector "predicateForRow:"

-- | @Selector@ for @subrowIndexesForRow:@
subrowIndexesForRowSelector :: Selector
subrowIndexesForRowSelector = mkSelector "subrowIndexesForRow:"

-- | @Selector@ for @criteriaForRow:@
criteriaForRowSelector :: Selector
criteriaForRowSelector = mkSelector "criteriaForRow:"

-- | @Selector@ for @displayValuesForRow:@
displayValuesForRowSelector :: Selector
displayValuesForRowSelector = mkSelector "displayValuesForRow:"

-- | @Selector@ for @rowForDisplayValue:@
rowForDisplayValueSelector :: Selector
rowForDisplayValueSelector = mkSelector "rowForDisplayValue:"

-- | @Selector@ for @rowTypeForRow:@
rowTypeForRowSelector :: Selector
rowTypeForRowSelector = mkSelector "rowTypeForRow:"

-- | @Selector@ for @parentRowForRow:@
parentRowForRowSelector :: Selector
parentRowForRowSelector = mkSelector "parentRowForRow:"

-- | @Selector@ for @addRow:@
addRowSelector :: Selector
addRowSelector = mkSelector "addRow:"

-- | @Selector@ for @insertRowAtIndex:withType:asSubrowOfRow:animate:@
insertRowAtIndex_withType_asSubrowOfRow_animateSelector :: Selector
insertRowAtIndex_withType_asSubrowOfRow_animateSelector = mkSelector "insertRowAtIndex:withType:asSubrowOfRow:animate:"

-- | @Selector@ for @setCriteria:andDisplayValues:forRowAtIndex:@
setCriteria_andDisplayValues_forRowAtIndexSelector :: Selector
setCriteria_andDisplayValues_forRowAtIndexSelector = mkSelector "setCriteria:andDisplayValues:forRowAtIndex:"

-- | @Selector@ for @removeRowAtIndex:@
removeRowAtIndexSelector :: Selector
removeRowAtIndexSelector = mkSelector "removeRowAtIndex:"

-- | @Selector@ for @removeRowsAtIndexes:includeSubrows:@
removeRowsAtIndexes_includeSubrowsSelector :: Selector
removeRowsAtIndexes_includeSubrowsSelector = mkSelector "removeRowsAtIndexes:includeSubrows:"

-- | @Selector@ for @selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelectionSelector :: Selector
selectRowIndexes_byExtendingSelectionSelector = mkSelector "selectRowIndexes:byExtendingSelection:"

-- | @Selector@ for @formattingStringsFilename@
formattingStringsFilenameSelector :: Selector
formattingStringsFilenameSelector = mkSelector "formattingStringsFilename"

-- | @Selector@ for @setFormattingStringsFilename:@
setFormattingStringsFilenameSelector :: Selector
setFormattingStringsFilenameSelector = mkSelector "setFormattingStringsFilename:"

-- | @Selector@ for @formattingDictionary@
formattingDictionarySelector :: Selector
formattingDictionarySelector = mkSelector "formattingDictionary"

-- | @Selector@ for @setFormattingDictionary:@
setFormattingDictionarySelector :: Selector
setFormattingDictionarySelector = mkSelector "setFormattingDictionary:"

-- | @Selector@ for @nestingMode@
nestingModeSelector :: Selector
nestingModeSelector = mkSelector "nestingMode"

-- | @Selector@ for @setNestingMode:@
setNestingModeSelector :: Selector
setNestingModeSelector = mkSelector "setNestingMode:"

-- | @Selector@ for @rowHeight@
rowHeightSelector :: Selector
rowHeightSelector = mkSelector "rowHeight"

-- | @Selector@ for @setRowHeight:@
setRowHeightSelector :: Selector
setRowHeightSelector = mkSelector "setRowHeight:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @canRemoveAllRows@
canRemoveAllRowsSelector :: Selector
canRemoveAllRowsSelector = mkSelector "canRemoveAllRows"

-- | @Selector@ for @setCanRemoveAllRows:@
setCanRemoveAllRowsSelector :: Selector
setCanRemoveAllRowsSelector = mkSelector "setCanRemoveAllRows:"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @selectedRowIndexes@
selectedRowIndexesSelector :: Selector
selectedRowIndexesSelector = mkSelector "selectedRowIndexes"

-- | @Selector@ for @rowClass@
rowClassSelector :: Selector
rowClassSelector = mkSelector "rowClass"

-- | @Selector@ for @setRowClass:@
setRowClassSelector :: Selector
setRowClassSelector = mkSelector "setRowClass:"

-- | @Selector@ for @rowTypeKeyPath@
rowTypeKeyPathSelector :: Selector
rowTypeKeyPathSelector = mkSelector "rowTypeKeyPath"

-- | @Selector@ for @setRowTypeKeyPath:@
setRowTypeKeyPathSelector :: Selector
setRowTypeKeyPathSelector = mkSelector "setRowTypeKeyPath:"

-- | @Selector@ for @subrowsKeyPath@
subrowsKeyPathSelector :: Selector
subrowsKeyPathSelector = mkSelector "subrowsKeyPath"

-- | @Selector@ for @setSubrowsKeyPath:@
setSubrowsKeyPathSelector :: Selector
setSubrowsKeyPathSelector = mkSelector "setSubrowsKeyPath:"

-- | @Selector@ for @criteriaKeyPath@
criteriaKeyPathSelector :: Selector
criteriaKeyPathSelector = mkSelector "criteriaKeyPath"

-- | @Selector@ for @setCriteriaKeyPath:@
setCriteriaKeyPathSelector :: Selector
setCriteriaKeyPathSelector = mkSelector "setCriteriaKeyPath:"

-- | @Selector@ for @displayValuesKeyPath@
displayValuesKeyPathSelector :: Selector
displayValuesKeyPathSelector = mkSelector "displayValuesKeyPath"

-- | @Selector@ for @setDisplayValuesKeyPath:@
setDisplayValuesKeyPathSelector :: Selector
setDisplayValuesKeyPathSelector = mkSelector "setDisplayValuesKeyPath:"

