{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSComboBoxCell@.
module ObjC.AppKit.NSComboBoxCell
  ( NSComboBoxCell
  , IsNSComboBoxCell(..)
  , reloadData
  , noteNumberOfItemsChanged
  , scrollItemAtIndexToTop
  , scrollItemAtIndexToVisible
  , selectItemAtIndex
  , deselectItemAtIndex
  , completedString
  , addItemWithObjectValue
  , addItemsWithObjectValues
  , insertItemWithObjectValue_atIndex
  , removeItemWithObjectValue
  , removeItemAtIndex
  , removeAllItems
  , selectItemWithObjectValue
  , itemObjectValueAtIndex
  , indexOfItemWithObjectValue
  , hasVerticalScroller
  , setHasVerticalScroller
  , intercellSpacing
  , setIntercellSpacing
  , itemHeight
  , setItemHeight
  , numberOfVisibleItems
  , setNumberOfVisibleItems
  , buttonBordered
  , setButtonBordered
  , usesDataSource
  , setUsesDataSource
  , indexOfSelectedItem
  , numberOfItems
  , completes
  , setCompletes
  , dataSource
  , setDataSource
  , objectValueOfSelectedItem
  , objectValues
  , addItemWithObjectValueSelector
  , addItemsWithObjectValuesSelector
  , buttonBorderedSelector
  , completedStringSelector
  , completesSelector
  , dataSourceSelector
  , deselectItemAtIndexSelector
  , hasVerticalScrollerSelector
  , indexOfItemWithObjectValueSelector
  , indexOfSelectedItemSelector
  , insertItemWithObjectValue_atIndexSelector
  , intercellSpacingSelector
  , itemHeightSelector
  , itemObjectValueAtIndexSelector
  , noteNumberOfItemsChangedSelector
  , numberOfItemsSelector
  , numberOfVisibleItemsSelector
  , objectValueOfSelectedItemSelector
  , objectValuesSelector
  , reloadDataSelector
  , removeAllItemsSelector
  , removeItemAtIndexSelector
  , removeItemWithObjectValueSelector
  , scrollItemAtIndexToTopSelector
  , scrollItemAtIndexToVisibleSelector
  , selectItemAtIndexSelector
  , selectItemWithObjectValueSelector
  , setButtonBorderedSelector
  , setCompletesSelector
  , setDataSourceSelector
  , setHasVerticalScrollerSelector
  , setIntercellSpacingSelector
  , setItemHeightSelector
  , setNumberOfVisibleItemsSelector
  , setUsesDataSourceSelector
  , usesDataSourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- reloadData@
reloadData :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO ()
reloadData nsComboBoxCell =
  sendMessage nsComboBoxCell reloadDataSelector

-- | @- noteNumberOfItemsChanged@
noteNumberOfItemsChanged :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO ()
noteNumberOfItemsChanged nsComboBoxCell =
  sendMessage nsComboBoxCell noteNumberOfItemsChangedSelector

-- | @- scrollItemAtIndexToTop:@
scrollItemAtIndexToTop :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
scrollItemAtIndexToTop nsComboBoxCell index =
  sendMessage nsComboBoxCell scrollItemAtIndexToTopSelector index

-- | @- scrollItemAtIndexToVisible:@
scrollItemAtIndexToVisible :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
scrollItemAtIndexToVisible nsComboBoxCell index =
  sendMessage nsComboBoxCell scrollItemAtIndexToVisibleSelector index

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
selectItemAtIndex nsComboBoxCell index =
  sendMessage nsComboBoxCell selectItemAtIndexSelector index

-- | @- deselectItemAtIndex:@
deselectItemAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
deselectItemAtIndex nsComboBoxCell index =
  sendMessage nsComboBoxCell deselectItemAtIndexSelector index

-- | @- completedString:@
completedString :: (IsNSComboBoxCell nsComboBoxCell, IsNSString string) => nsComboBoxCell -> string -> IO (Id NSString)
completedString nsComboBoxCell string =
  sendMessage nsComboBoxCell completedStringSelector (toNSString string)

-- | @- addItemWithObjectValue:@
addItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
addItemWithObjectValue nsComboBoxCell object =
  sendMessage nsComboBoxCell addItemWithObjectValueSelector object

-- | @- addItemsWithObjectValues:@
addItemsWithObjectValues :: (IsNSComboBoxCell nsComboBoxCell, IsNSArray objects) => nsComboBoxCell -> objects -> IO ()
addItemsWithObjectValues nsComboBoxCell objects =
  sendMessage nsComboBoxCell addItemsWithObjectValuesSelector (toNSArray objects)

-- | @- insertItemWithObjectValue:atIndex:@
insertItemWithObjectValue_atIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> CLong -> IO ()
insertItemWithObjectValue_atIndex nsComboBoxCell object index =
  sendMessage nsComboBoxCell insertItemWithObjectValue_atIndexSelector object index

-- | @- removeItemWithObjectValue:@
removeItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
removeItemWithObjectValue nsComboBoxCell object =
  sendMessage nsComboBoxCell removeItemWithObjectValueSelector object

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
removeItemAtIndex nsComboBoxCell index =
  sendMessage nsComboBoxCell removeItemAtIndexSelector index

-- | @- removeAllItems@
removeAllItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO ()
removeAllItems nsComboBoxCell =
  sendMessage nsComboBoxCell removeAllItemsSelector

-- | @- selectItemWithObjectValue:@
selectItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
selectItemWithObjectValue nsComboBoxCell object =
  sendMessage nsComboBoxCell selectItemWithObjectValueSelector object

-- | @- itemObjectValueAtIndex:@
itemObjectValueAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO RawId
itemObjectValueAtIndex nsComboBoxCell index =
  sendMessage nsComboBoxCell itemObjectValueAtIndexSelector index

-- | @- indexOfItemWithObjectValue:@
indexOfItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO CLong
indexOfItemWithObjectValue nsComboBoxCell object =
  sendMessage nsComboBoxCell indexOfItemWithObjectValueSelector object

-- | @- hasVerticalScroller@
hasVerticalScroller :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
hasVerticalScroller nsComboBoxCell =
  sendMessage nsComboBoxCell hasVerticalScrollerSelector

-- | @- setHasVerticalScroller:@
setHasVerticalScroller :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setHasVerticalScroller nsComboBoxCell value =
  sendMessage nsComboBoxCell setHasVerticalScrollerSelector value

-- | @- intercellSpacing@
intercellSpacing :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO NSSize
intercellSpacing nsComboBoxCell =
  sendMessage nsComboBoxCell intercellSpacingSelector

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> NSSize -> IO ()
setIntercellSpacing nsComboBoxCell value =
  sendMessage nsComboBoxCell setIntercellSpacingSelector value

-- | @- itemHeight@
itemHeight :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CDouble
itemHeight nsComboBoxCell =
  sendMessage nsComboBoxCell itemHeightSelector

-- | @- setItemHeight:@
setItemHeight :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CDouble -> IO ()
setItemHeight nsComboBoxCell value =
  sendMessage nsComboBoxCell setItemHeightSelector value

-- | @- numberOfVisibleItems@
numberOfVisibleItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CLong
numberOfVisibleItems nsComboBoxCell =
  sendMessage nsComboBoxCell numberOfVisibleItemsSelector

-- | @- setNumberOfVisibleItems:@
setNumberOfVisibleItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
setNumberOfVisibleItems nsComboBoxCell value =
  sendMessage nsComboBoxCell setNumberOfVisibleItemsSelector value

-- | @- buttonBordered@
buttonBordered :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
buttonBordered nsComboBoxCell =
  sendMessage nsComboBoxCell buttonBorderedSelector

-- | @- setButtonBordered:@
setButtonBordered :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setButtonBordered nsComboBoxCell value =
  sendMessage nsComboBoxCell setButtonBorderedSelector value

-- | @- usesDataSource@
usesDataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
usesDataSource nsComboBoxCell =
  sendMessage nsComboBoxCell usesDataSourceSelector

-- | @- setUsesDataSource:@
setUsesDataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setUsesDataSource nsComboBoxCell value =
  sendMessage nsComboBoxCell setUsesDataSourceSelector value

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CLong
indexOfSelectedItem nsComboBoxCell =
  sendMessage nsComboBoxCell indexOfSelectedItemSelector

-- | @- numberOfItems@
numberOfItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CLong
numberOfItems nsComboBoxCell =
  sendMessage nsComboBoxCell numberOfItemsSelector

-- | @- completes@
completes :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
completes nsComboBoxCell =
  sendMessage nsComboBoxCell completesSelector

-- | @- setCompletes:@
setCompletes :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setCompletes nsComboBoxCell value =
  sendMessage nsComboBoxCell setCompletesSelector value

-- | @- dataSource@
dataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO RawId
dataSource nsComboBoxCell =
  sendMessage nsComboBoxCell dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
setDataSource nsComboBoxCell value =
  sendMessage nsComboBoxCell setDataSourceSelector value

-- | @- objectValueOfSelectedItem@
objectValueOfSelectedItem :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO RawId
objectValueOfSelectedItem nsComboBoxCell =
  sendMessage nsComboBoxCell objectValueOfSelectedItemSelector

-- | @- objectValues@
objectValues :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO (Id NSArray)
objectValues nsComboBoxCell =
  sendMessage nsComboBoxCell objectValuesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector '[] ()
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @noteNumberOfItemsChanged@
noteNumberOfItemsChangedSelector :: Selector '[] ()
noteNumberOfItemsChangedSelector = mkSelector "noteNumberOfItemsChanged"

-- | @Selector@ for @scrollItemAtIndexToTop:@
scrollItemAtIndexToTopSelector :: Selector '[CLong] ()
scrollItemAtIndexToTopSelector = mkSelector "scrollItemAtIndexToTop:"

-- | @Selector@ for @scrollItemAtIndexToVisible:@
scrollItemAtIndexToVisibleSelector :: Selector '[CLong] ()
scrollItemAtIndexToVisibleSelector = mkSelector "scrollItemAtIndexToVisible:"

-- | @Selector@ for @selectItemAtIndex:@
selectItemAtIndexSelector :: Selector '[CLong] ()
selectItemAtIndexSelector = mkSelector "selectItemAtIndex:"

-- | @Selector@ for @deselectItemAtIndex:@
deselectItemAtIndexSelector :: Selector '[CLong] ()
deselectItemAtIndexSelector = mkSelector "deselectItemAtIndex:"

-- | @Selector@ for @completedString:@
completedStringSelector :: Selector '[Id NSString] (Id NSString)
completedStringSelector = mkSelector "completedString:"

-- | @Selector@ for @addItemWithObjectValue:@
addItemWithObjectValueSelector :: Selector '[RawId] ()
addItemWithObjectValueSelector = mkSelector "addItemWithObjectValue:"

-- | @Selector@ for @addItemsWithObjectValues:@
addItemsWithObjectValuesSelector :: Selector '[Id NSArray] ()
addItemsWithObjectValuesSelector = mkSelector "addItemsWithObjectValues:"

-- | @Selector@ for @insertItemWithObjectValue:atIndex:@
insertItemWithObjectValue_atIndexSelector :: Selector '[RawId, CLong] ()
insertItemWithObjectValue_atIndexSelector = mkSelector "insertItemWithObjectValue:atIndex:"

-- | @Selector@ for @removeItemWithObjectValue:@
removeItemWithObjectValueSelector :: Selector '[RawId] ()
removeItemWithObjectValueSelector = mkSelector "removeItemWithObjectValue:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector '[CLong] ()
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector '[] ()
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @selectItemWithObjectValue:@
selectItemWithObjectValueSelector :: Selector '[RawId] ()
selectItemWithObjectValueSelector = mkSelector "selectItemWithObjectValue:"

-- | @Selector@ for @itemObjectValueAtIndex:@
itemObjectValueAtIndexSelector :: Selector '[CLong] RawId
itemObjectValueAtIndexSelector = mkSelector "itemObjectValueAtIndex:"

-- | @Selector@ for @indexOfItemWithObjectValue:@
indexOfItemWithObjectValueSelector :: Selector '[RawId] CLong
indexOfItemWithObjectValueSelector = mkSelector "indexOfItemWithObjectValue:"

-- | @Selector@ for @hasVerticalScroller@
hasVerticalScrollerSelector :: Selector '[] Bool
hasVerticalScrollerSelector = mkSelector "hasVerticalScroller"

-- | @Selector@ for @setHasVerticalScroller:@
setHasVerticalScrollerSelector :: Selector '[Bool] ()
setHasVerticalScrollerSelector = mkSelector "setHasVerticalScroller:"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector '[] NSSize
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector '[NSSize] ()
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @itemHeight@
itemHeightSelector :: Selector '[] CDouble
itemHeightSelector = mkSelector "itemHeight"

-- | @Selector@ for @setItemHeight:@
setItemHeightSelector :: Selector '[CDouble] ()
setItemHeightSelector = mkSelector "setItemHeight:"

-- | @Selector@ for @numberOfVisibleItems@
numberOfVisibleItemsSelector :: Selector '[] CLong
numberOfVisibleItemsSelector = mkSelector "numberOfVisibleItems"

-- | @Selector@ for @setNumberOfVisibleItems:@
setNumberOfVisibleItemsSelector :: Selector '[CLong] ()
setNumberOfVisibleItemsSelector = mkSelector "setNumberOfVisibleItems:"

-- | @Selector@ for @buttonBordered@
buttonBorderedSelector :: Selector '[] Bool
buttonBorderedSelector = mkSelector "buttonBordered"

-- | @Selector@ for @setButtonBordered:@
setButtonBorderedSelector :: Selector '[Bool] ()
setButtonBorderedSelector = mkSelector "setButtonBordered:"

-- | @Selector@ for @usesDataSource@
usesDataSourceSelector :: Selector '[] Bool
usesDataSourceSelector = mkSelector "usesDataSource"

-- | @Selector@ for @setUsesDataSource:@
setUsesDataSourceSelector :: Selector '[Bool] ()
setUsesDataSourceSelector = mkSelector "setUsesDataSource:"

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector '[] CLong
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector '[] CLong
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @completes@
completesSelector :: Selector '[] Bool
completesSelector = mkSelector "completes"

-- | @Selector@ for @setCompletes:@
setCompletesSelector :: Selector '[Bool] ()
setCompletesSelector = mkSelector "setCompletes:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector '[] RawId
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector '[RawId] ()
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @objectValueOfSelectedItem@
objectValueOfSelectedItemSelector :: Selector '[] RawId
objectValueOfSelectedItemSelector = mkSelector "objectValueOfSelectedItem"

-- | @Selector@ for @objectValues@
objectValuesSelector :: Selector '[] (Id NSArray)
objectValuesSelector = mkSelector "objectValues"

