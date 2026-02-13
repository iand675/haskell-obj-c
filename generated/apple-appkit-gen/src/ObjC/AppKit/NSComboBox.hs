{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSComboBox@.
module ObjC.AppKit.NSComboBox
  ( NSComboBox
  , IsNSComboBox(..)
  , reloadData
  , noteNumberOfItemsChanged
  , scrollItemAtIndexToTop
  , scrollItemAtIndexToVisible
  , selectItemAtIndex
  , deselectItemAtIndex
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
  , delegate
  , setDelegate
  , dataSource
  , setDataSource
  , objectValueOfSelectedItem
  , objectValues
  , addItemWithObjectValueSelector
  , addItemsWithObjectValuesSelector
  , buttonBorderedSelector
  , completesSelector
  , dataSourceSelector
  , delegateSelector
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
  , setDelegateSelector
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
reloadData :: IsNSComboBox nsComboBox => nsComboBox -> IO ()
reloadData nsComboBox =
  sendMessage nsComboBox reloadDataSelector

-- | @- noteNumberOfItemsChanged@
noteNumberOfItemsChanged :: IsNSComboBox nsComboBox => nsComboBox -> IO ()
noteNumberOfItemsChanged nsComboBox =
  sendMessage nsComboBox noteNumberOfItemsChangedSelector

-- | @- scrollItemAtIndexToTop:@
scrollItemAtIndexToTop :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
scrollItemAtIndexToTop nsComboBox index =
  sendMessage nsComboBox scrollItemAtIndexToTopSelector index

-- | @- scrollItemAtIndexToVisible:@
scrollItemAtIndexToVisible :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
scrollItemAtIndexToVisible nsComboBox index =
  sendMessage nsComboBox scrollItemAtIndexToVisibleSelector index

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
selectItemAtIndex nsComboBox index =
  sendMessage nsComboBox selectItemAtIndexSelector index

-- | @- deselectItemAtIndex:@
deselectItemAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
deselectItemAtIndex nsComboBox index =
  sendMessage nsComboBox deselectItemAtIndexSelector index

-- | @- addItemWithObjectValue:@
addItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
addItemWithObjectValue nsComboBox object =
  sendMessage nsComboBox addItemWithObjectValueSelector object

-- | @- addItemsWithObjectValues:@
addItemsWithObjectValues :: (IsNSComboBox nsComboBox, IsNSArray objects) => nsComboBox -> objects -> IO ()
addItemsWithObjectValues nsComboBox objects =
  sendMessage nsComboBox addItemsWithObjectValuesSelector (toNSArray objects)

-- | @- insertItemWithObjectValue:atIndex:@
insertItemWithObjectValue_atIndex :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> CLong -> IO ()
insertItemWithObjectValue_atIndex nsComboBox object index =
  sendMessage nsComboBox insertItemWithObjectValue_atIndexSelector object index

-- | @- removeItemWithObjectValue:@
removeItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
removeItemWithObjectValue nsComboBox object =
  sendMessage nsComboBox removeItemWithObjectValueSelector object

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
removeItemAtIndex nsComboBox index =
  sendMessage nsComboBox removeItemAtIndexSelector index

-- | @- removeAllItems@
removeAllItems :: IsNSComboBox nsComboBox => nsComboBox -> IO ()
removeAllItems nsComboBox =
  sendMessage nsComboBox removeAllItemsSelector

-- | @- selectItemWithObjectValue:@
selectItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
selectItemWithObjectValue nsComboBox object =
  sendMessage nsComboBox selectItemWithObjectValueSelector object

-- | @- itemObjectValueAtIndex:@
itemObjectValueAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO RawId
itemObjectValueAtIndex nsComboBox index =
  sendMessage nsComboBox itemObjectValueAtIndexSelector index

-- | @- indexOfItemWithObjectValue:@
indexOfItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO CLong
indexOfItemWithObjectValue nsComboBox object =
  sendMessage nsComboBox indexOfItemWithObjectValueSelector object

-- | @- hasVerticalScroller@
hasVerticalScroller :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
hasVerticalScroller nsComboBox =
  sendMessage nsComboBox hasVerticalScrollerSelector

-- | @- setHasVerticalScroller:@
setHasVerticalScroller :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setHasVerticalScroller nsComboBox value =
  sendMessage nsComboBox setHasVerticalScrollerSelector value

-- | @- intercellSpacing@
intercellSpacing :: IsNSComboBox nsComboBox => nsComboBox -> IO NSSize
intercellSpacing nsComboBox =
  sendMessage nsComboBox intercellSpacingSelector

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSComboBox nsComboBox => nsComboBox -> NSSize -> IO ()
setIntercellSpacing nsComboBox value =
  sendMessage nsComboBox setIntercellSpacingSelector value

-- | @- itemHeight@
itemHeight :: IsNSComboBox nsComboBox => nsComboBox -> IO CDouble
itemHeight nsComboBox =
  sendMessage nsComboBox itemHeightSelector

-- | @- setItemHeight:@
setItemHeight :: IsNSComboBox nsComboBox => nsComboBox -> CDouble -> IO ()
setItemHeight nsComboBox value =
  sendMessage nsComboBox setItemHeightSelector value

-- | @- numberOfVisibleItems@
numberOfVisibleItems :: IsNSComboBox nsComboBox => nsComboBox -> IO CLong
numberOfVisibleItems nsComboBox =
  sendMessage nsComboBox numberOfVisibleItemsSelector

-- | @- setNumberOfVisibleItems:@
setNumberOfVisibleItems :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
setNumberOfVisibleItems nsComboBox value =
  sendMessage nsComboBox setNumberOfVisibleItemsSelector value

-- | @- buttonBordered@
buttonBordered :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
buttonBordered nsComboBox =
  sendMessage nsComboBox buttonBorderedSelector

-- | @- setButtonBordered:@
setButtonBordered :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setButtonBordered nsComboBox value =
  sendMessage nsComboBox setButtonBorderedSelector value

-- | @- usesDataSource@
usesDataSource :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
usesDataSource nsComboBox =
  sendMessage nsComboBox usesDataSourceSelector

-- | @- setUsesDataSource:@
setUsesDataSource :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setUsesDataSource nsComboBox value =
  sendMessage nsComboBox setUsesDataSourceSelector value

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSComboBox nsComboBox => nsComboBox -> IO CLong
indexOfSelectedItem nsComboBox =
  sendMessage nsComboBox indexOfSelectedItemSelector

-- | @- numberOfItems@
numberOfItems :: IsNSComboBox nsComboBox => nsComboBox -> IO CLong
numberOfItems nsComboBox =
  sendMessage nsComboBox numberOfItemsSelector

-- | @- completes@
completes :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
completes nsComboBox =
  sendMessage nsComboBox completesSelector

-- | @- setCompletes:@
setCompletes :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setCompletes nsComboBox value =
  sendMessage nsComboBox setCompletesSelector value

-- | @- delegate@
delegate :: IsNSComboBox nsComboBox => nsComboBox -> IO RawId
delegate nsComboBox =
  sendMessage nsComboBox delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
setDelegate nsComboBox value =
  sendMessage nsComboBox setDelegateSelector value

-- | @- dataSource@
dataSource :: IsNSComboBox nsComboBox => nsComboBox -> IO RawId
dataSource nsComboBox =
  sendMessage nsComboBox dataSourceSelector

-- | @- setDataSource:@
setDataSource :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
setDataSource nsComboBox value =
  sendMessage nsComboBox setDataSourceSelector value

-- | @- objectValueOfSelectedItem@
objectValueOfSelectedItem :: IsNSComboBox nsComboBox => nsComboBox -> IO RawId
objectValueOfSelectedItem nsComboBox =
  sendMessage nsComboBox objectValueOfSelectedItemSelector

-- | @- objectValues@
objectValues :: IsNSComboBox nsComboBox => nsComboBox -> IO (Id NSArray)
objectValues nsComboBox =
  sendMessage nsComboBox objectValuesSelector

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

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

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

