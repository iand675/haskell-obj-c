{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , delegate
  , setDelegate
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
  , addRowSelector
  , canRemoveAllRowsSelector
  , criteriaForRowSelector
  , criteriaKeyPathSelector
  , delegateSelector
  , displayValuesForRowSelector
  , displayValuesKeyPathSelector
  , editableSelector
  , formattingDictionarySelector
  , formattingStringsFilenameSelector
  , insertRowAtIndex_withType_asSubrowOfRow_animateSelector
  , nestingModeSelector
  , numberOfRowsSelector
  , parentRowForRowSelector
  , predicateForRowSelector
  , predicateSelector
  , reloadCriteriaSelector
  , reloadPredicateSelector
  , removeRowAtIndexSelector
  , removeRowsAtIndexes_includeSubrowsSelector
  , rowClassSelector
  , rowForDisplayValueSelector
  , rowHeightSelector
  , rowTypeForRowSelector
  , rowTypeKeyPathSelector
  , selectRowIndexes_byExtendingSelectionSelector
  , selectedRowIndexesSelector
  , setCanRemoveAllRowsSelector
  , setCriteriaKeyPathSelector
  , setCriteria_andDisplayValues_forRowAtIndexSelector
  , setDelegateSelector
  , setDisplayValuesKeyPathSelector
  , setEditableSelector
  , setFormattingDictionarySelector
  , setFormattingStringsFilenameSelector
  , setNestingModeSelector
  , setRowClassSelector
  , setRowHeightSelector
  , setRowTypeKeyPathSelector
  , setSubrowsKeyPathSelector
  , subrowIndexesForRowSelector
  , subrowsKeyPathSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- reloadCriteria@
reloadCriteria :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO ()
reloadCriteria nsRuleEditor =
  sendMessage nsRuleEditor reloadCriteriaSelector

-- | @- reloadPredicate@
reloadPredicate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO ()
reloadPredicate nsRuleEditor =
  sendMessage nsRuleEditor reloadPredicateSelector

-- | @- predicateForRow:@
predicateForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSPredicate)
predicateForRow nsRuleEditor row =
  sendMessage nsRuleEditor predicateForRowSelector row

-- | @- subrowIndexesForRow:@
subrowIndexesForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSIndexSet)
subrowIndexesForRow nsRuleEditor rowIndex =
  sendMessage nsRuleEditor subrowIndexesForRowSelector rowIndex

-- | @- criteriaForRow:@
criteriaForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSArray)
criteriaForRow nsRuleEditor row =
  sendMessage nsRuleEditor criteriaForRowSelector row

-- | @- displayValuesForRow:@
displayValuesForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO (Id NSArray)
displayValuesForRow nsRuleEditor row =
  sendMessage nsRuleEditor displayValuesForRowSelector row

-- | @- rowForDisplayValue:@
rowForDisplayValue :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> RawId -> IO CLong
rowForDisplayValue nsRuleEditor displayValue =
  sendMessage nsRuleEditor rowForDisplayValueSelector displayValue

-- | @- rowTypeForRow:@
rowTypeForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO NSRuleEditorRowType
rowTypeForRow nsRuleEditor rowIndex =
  sendMessage nsRuleEditor rowTypeForRowSelector rowIndex

-- | @- parentRowForRow:@
parentRowForRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO CLong
parentRowForRow nsRuleEditor rowIndex =
  sendMessage nsRuleEditor parentRowForRowSelector rowIndex

-- | @- addRow:@
addRow :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> RawId -> IO ()
addRow nsRuleEditor sender =
  sendMessage nsRuleEditor addRowSelector sender

-- | @- insertRowAtIndex:withType:asSubrowOfRow:animate:@
insertRowAtIndex_withType_asSubrowOfRow_animate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> NSRuleEditorRowType -> CLong -> Bool -> IO ()
insertRowAtIndex_withType_asSubrowOfRow_animate nsRuleEditor rowIndex rowType parentRow shouldAnimate =
  sendMessage nsRuleEditor insertRowAtIndex_withType_asSubrowOfRow_animateSelector rowIndex rowType parentRow shouldAnimate

-- | @- setCriteria:andDisplayValues:forRowAtIndex:@
setCriteria_andDisplayValues_forRowAtIndex :: (IsNSRuleEditor nsRuleEditor, IsNSArray criteria, IsNSArray values) => nsRuleEditor -> criteria -> values -> CLong -> IO ()
setCriteria_andDisplayValues_forRowAtIndex nsRuleEditor criteria values rowIndex =
  sendMessage nsRuleEditor setCriteria_andDisplayValues_forRowAtIndexSelector (toNSArray criteria) (toNSArray values) rowIndex

-- | @- removeRowAtIndex:@
removeRowAtIndex :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CLong -> IO ()
removeRowAtIndex nsRuleEditor rowIndex =
  sendMessage nsRuleEditor removeRowAtIndexSelector rowIndex

-- | @- removeRowsAtIndexes:includeSubrows:@
removeRowsAtIndexes_includeSubrows :: (IsNSRuleEditor nsRuleEditor, IsNSIndexSet rowIndexes) => nsRuleEditor -> rowIndexes -> Bool -> IO ()
removeRowsAtIndexes_includeSubrows nsRuleEditor rowIndexes includeSubrows =
  sendMessage nsRuleEditor removeRowsAtIndexes_includeSubrowsSelector (toNSIndexSet rowIndexes) includeSubrows

-- | @- selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelection :: (IsNSRuleEditor nsRuleEditor, IsNSIndexSet indexes) => nsRuleEditor -> indexes -> Bool -> IO ()
selectRowIndexes_byExtendingSelection nsRuleEditor indexes extend =
  sendMessage nsRuleEditor selectRowIndexes_byExtendingSelectionSelector (toNSIndexSet indexes) extend

-- | @- delegate@
delegate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO RawId
delegate nsRuleEditor =
  sendMessage nsRuleEditor delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> RawId -> IO ()
setDelegate nsRuleEditor value =
  sendMessage nsRuleEditor setDelegateSelector value

-- | @- formattingStringsFilename@
formattingStringsFilename :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
formattingStringsFilename nsRuleEditor =
  sendMessage nsRuleEditor formattingStringsFilenameSelector

-- | @- setFormattingStringsFilename:@
setFormattingStringsFilename :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setFormattingStringsFilename nsRuleEditor value =
  sendMessage nsRuleEditor setFormattingStringsFilenameSelector (toNSString value)

-- | @- formattingDictionary@
formattingDictionary :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSDictionary)
formattingDictionary nsRuleEditor =
  sendMessage nsRuleEditor formattingDictionarySelector

-- | @- setFormattingDictionary:@
setFormattingDictionary :: (IsNSRuleEditor nsRuleEditor, IsNSDictionary value) => nsRuleEditor -> value -> IO ()
setFormattingDictionary nsRuleEditor value =
  sendMessage nsRuleEditor setFormattingDictionarySelector (toNSDictionary value)

-- | @- nestingMode@
nestingMode :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO NSRuleEditorNestingMode
nestingMode nsRuleEditor =
  sendMessage nsRuleEditor nestingModeSelector

-- | @- setNestingMode:@
setNestingMode :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> NSRuleEditorNestingMode -> IO ()
setNestingMode nsRuleEditor value =
  sendMessage nsRuleEditor setNestingModeSelector value

-- | @- rowHeight@
rowHeight :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO CDouble
rowHeight nsRuleEditor =
  sendMessage nsRuleEditor rowHeightSelector

-- | @- setRowHeight:@
setRowHeight :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> CDouble -> IO ()
setRowHeight nsRuleEditor value =
  sendMessage nsRuleEditor setRowHeightSelector value

-- | @- editable@
editable :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO Bool
editable nsRuleEditor =
  sendMessage nsRuleEditor editableSelector

-- | @- setEditable:@
setEditable :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> Bool -> IO ()
setEditable nsRuleEditor value =
  sendMessage nsRuleEditor setEditableSelector value

-- | @- canRemoveAllRows@
canRemoveAllRows :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO Bool
canRemoveAllRows nsRuleEditor =
  sendMessage nsRuleEditor canRemoveAllRowsSelector

-- | @- setCanRemoveAllRows:@
setCanRemoveAllRows :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> Bool -> IO ()
setCanRemoveAllRows nsRuleEditor value =
  sendMessage nsRuleEditor setCanRemoveAllRowsSelector value

-- | @- predicate@
predicate :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSPredicate)
predicate nsRuleEditor =
  sendMessage nsRuleEditor predicateSelector

-- | @- numberOfRows@
numberOfRows :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO CLong
numberOfRows nsRuleEditor =
  sendMessage nsRuleEditor numberOfRowsSelector

-- | @- selectedRowIndexes@
selectedRowIndexes :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSIndexSet)
selectedRowIndexes nsRuleEditor =
  sendMessage nsRuleEditor selectedRowIndexesSelector

-- | @- rowClass@
rowClass :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO Class
rowClass nsRuleEditor =
  sendMessage nsRuleEditor rowClassSelector

-- | @- setRowClass:@
setRowClass :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> Class -> IO ()
setRowClass nsRuleEditor value =
  sendMessage nsRuleEditor setRowClassSelector value

-- | @- rowTypeKeyPath@
rowTypeKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
rowTypeKeyPath nsRuleEditor =
  sendMessage nsRuleEditor rowTypeKeyPathSelector

-- | @- setRowTypeKeyPath:@
setRowTypeKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setRowTypeKeyPath nsRuleEditor value =
  sendMessage nsRuleEditor setRowTypeKeyPathSelector (toNSString value)

-- | @- subrowsKeyPath@
subrowsKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
subrowsKeyPath nsRuleEditor =
  sendMessage nsRuleEditor subrowsKeyPathSelector

-- | @- setSubrowsKeyPath:@
setSubrowsKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setSubrowsKeyPath nsRuleEditor value =
  sendMessage nsRuleEditor setSubrowsKeyPathSelector (toNSString value)

-- | @- criteriaKeyPath@
criteriaKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
criteriaKeyPath nsRuleEditor =
  sendMessage nsRuleEditor criteriaKeyPathSelector

-- | @- setCriteriaKeyPath:@
setCriteriaKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setCriteriaKeyPath nsRuleEditor value =
  sendMessage nsRuleEditor setCriteriaKeyPathSelector (toNSString value)

-- | @- displayValuesKeyPath@
displayValuesKeyPath :: IsNSRuleEditor nsRuleEditor => nsRuleEditor -> IO (Id NSString)
displayValuesKeyPath nsRuleEditor =
  sendMessage nsRuleEditor displayValuesKeyPathSelector

-- | @- setDisplayValuesKeyPath:@
setDisplayValuesKeyPath :: (IsNSRuleEditor nsRuleEditor, IsNSString value) => nsRuleEditor -> value -> IO ()
setDisplayValuesKeyPath nsRuleEditor value =
  sendMessage nsRuleEditor setDisplayValuesKeyPathSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadCriteria@
reloadCriteriaSelector :: Selector '[] ()
reloadCriteriaSelector = mkSelector "reloadCriteria"

-- | @Selector@ for @reloadPredicate@
reloadPredicateSelector :: Selector '[] ()
reloadPredicateSelector = mkSelector "reloadPredicate"

-- | @Selector@ for @predicateForRow:@
predicateForRowSelector :: Selector '[CLong] (Id NSPredicate)
predicateForRowSelector = mkSelector "predicateForRow:"

-- | @Selector@ for @subrowIndexesForRow:@
subrowIndexesForRowSelector :: Selector '[CLong] (Id NSIndexSet)
subrowIndexesForRowSelector = mkSelector "subrowIndexesForRow:"

-- | @Selector@ for @criteriaForRow:@
criteriaForRowSelector :: Selector '[CLong] (Id NSArray)
criteriaForRowSelector = mkSelector "criteriaForRow:"

-- | @Selector@ for @displayValuesForRow:@
displayValuesForRowSelector :: Selector '[CLong] (Id NSArray)
displayValuesForRowSelector = mkSelector "displayValuesForRow:"

-- | @Selector@ for @rowForDisplayValue:@
rowForDisplayValueSelector :: Selector '[RawId] CLong
rowForDisplayValueSelector = mkSelector "rowForDisplayValue:"

-- | @Selector@ for @rowTypeForRow:@
rowTypeForRowSelector :: Selector '[CLong] NSRuleEditorRowType
rowTypeForRowSelector = mkSelector "rowTypeForRow:"

-- | @Selector@ for @parentRowForRow:@
parentRowForRowSelector :: Selector '[CLong] CLong
parentRowForRowSelector = mkSelector "parentRowForRow:"

-- | @Selector@ for @addRow:@
addRowSelector :: Selector '[RawId] ()
addRowSelector = mkSelector "addRow:"

-- | @Selector@ for @insertRowAtIndex:withType:asSubrowOfRow:animate:@
insertRowAtIndex_withType_asSubrowOfRow_animateSelector :: Selector '[CLong, NSRuleEditorRowType, CLong, Bool] ()
insertRowAtIndex_withType_asSubrowOfRow_animateSelector = mkSelector "insertRowAtIndex:withType:asSubrowOfRow:animate:"

-- | @Selector@ for @setCriteria:andDisplayValues:forRowAtIndex:@
setCriteria_andDisplayValues_forRowAtIndexSelector :: Selector '[Id NSArray, Id NSArray, CLong] ()
setCriteria_andDisplayValues_forRowAtIndexSelector = mkSelector "setCriteria:andDisplayValues:forRowAtIndex:"

-- | @Selector@ for @removeRowAtIndex:@
removeRowAtIndexSelector :: Selector '[CLong] ()
removeRowAtIndexSelector = mkSelector "removeRowAtIndex:"

-- | @Selector@ for @removeRowsAtIndexes:includeSubrows:@
removeRowsAtIndexes_includeSubrowsSelector :: Selector '[Id NSIndexSet, Bool] ()
removeRowsAtIndexes_includeSubrowsSelector = mkSelector "removeRowsAtIndexes:includeSubrows:"

-- | @Selector@ for @selectRowIndexes:byExtendingSelection:@
selectRowIndexes_byExtendingSelectionSelector :: Selector '[Id NSIndexSet, Bool] ()
selectRowIndexes_byExtendingSelectionSelector = mkSelector "selectRowIndexes:byExtendingSelection:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @formattingStringsFilename@
formattingStringsFilenameSelector :: Selector '[] (Id NSString)
formattingStringsFilenameSelector = mkSelector "formattingStringsFilename"

-- | @Selector@ for @setFormattingStringsFilename:@
setFormattingStringsFilenameSelector :: Selector '[Id NSString] ()
setFormattingStringsFilenameSelector = mkSelector "setFormattingStringsFilename:"

-- | @Selector@ for @formattingDictionary@
formattingDictionarySelector :: Selector '[] (Id NSDictionary)
formattingDictionarySelector = mkSelector "formattingDictionary"

-- | @Selector@ for @setFormattingDictionary:@
setFormattingDictionarySelector :: Selector '[Id NSDictionary] ()
setFormattingDictionarySelector = mkSelector "setFormattingDictionary:"

-- | @Selector@ for @nestingMode@
nestingModeSelector :: Selector '[] NSRuleEditorNestingMode
nestingModeSelector = mkSelector "nestingMode"

-- | @Selector@ for @setNestingMode:@
setNestingModeSelector :: Selector '[NSRuleEditorNestingMode] ()
setNestingModeSelector = mkSelector "setNestingMode:"

-- | @Selector@ for @rowHeight@
rowHeightSelector :: Selector '[] CDouble
rowHeightSelector = mkSelector "rowHeight"

-- | @Selector@ for @setRowHeight:@
setRowHeightSelector :: Selector '[CDouble] ()
setRowHeightSelector = mkSelector "setRowHeight:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @canRemoveAllRows@
canRemoveAllRowsSelector :: Selector '[] Bool
canRemoveAllRowsSelector = mkSelector "canRemoveAllRows"

-- | @Selector@ for @setCanRemoveAllRows:@
setCanRemoveAllRowsSelector :: Selector '[Bool] ()
setCanRemoveAllRowsSelector = mkSelector "setCanRemoveAllRows:"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @numberOfRows@
numberOfRowsSelector :: Selector '[] CLong
numberOfRowsSelector = mkSelector "numberOfRows"

-- | @Selector@ for @selectedRowIndexes@
selectedRowIndexesSelector :: Selector '[] (Id NSIndexSet)
selectedRowIndexesSelector = mkSelector "selectedRowIndexes"

-- | @Selector@ for @rowClass@
rowClassSelector :: Selector '[] Class
rowClassSelector = mkSelector "rowClass"

-- | @Selector@ for @setRowClass:@
setRowClassSelector :: Selector '[Class] ()
setRowClassSelector = mkSelector "setRowClass:"

-- | @Selector@ for @rowTypeKeyPath@
rowTypeKeyPathSelector :: Selector '[] (Id NSString)
rowTypeKeyPathSelector = mkSelector "rowTypeKeyPath"

-- | @Selector@ for @setRowTypeKeyPath:@
setRowTypeKeyPathSelector :: Selector '[Id NSString] ()
setRowTypeKeyPathSelector = mkSelector "setRowTypeKeyPath:"

-- | @Selector@ for @subrowsKeyPath@
subrowsKeyPathSelector :: Selector '[] (Id NSString)
subrowsKeyPathSelector = mkSelector "subrowsKeyPath"

-- | @Selector@ for @setSubrowsKeyPath:@
setSubrowsKeyPathSelector :: Selector '[Id NSString] ()
setSubrowsKeyPathSelector = mkSelector "setSubrowsKeyPath:"

-- | @Selector@ for @criteriaKeyPath@
criteriaKeyPathSelector :: Selector '[] (Id NSString)
criteriaKeyPathSelector = mkSelector "criteriaKeyPath"

-- | @Selector@ for @setCriteriaKeyPath:@
setCriteriaKeyPathSelector :: Selector '[Id NSString] ()
setCriteriaKeyPathSelector = mkSelector "setCriteriaKeyPath:"

-- | @Selector@ for @displayValuesKeyPath@
displayValuesKeyPathSelector :: Selector '[] (Id NSString)
displayValuesKeyPathSelector = mkSelector "displayValuesKeyPath"

-- | @Selector@ for @setDisplayValuesKeyPath:@
setDisplayValuesKeyPathSelector :: Selector '[Id NSString] ()
setDisplayValuesKeyPathSelector = mkSelector "setDisplayValuesKeyPath:"

