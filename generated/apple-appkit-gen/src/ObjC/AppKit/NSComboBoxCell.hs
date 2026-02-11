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
  , reloadDataSelector
  , noteNumberOfItemsChangedSelector
  , scrollItemAtIndexToTopSelector
  , scrollItemAtIndexToVisibleSelector
  , selectItemAtIndexSelector
  , deselectItemAtIndexSelector
  , completedStringSelector
  , addItemWithObjectValueSelector
  , addItemsWithObjectValuesSelector
  , insertItemWithObjectValue_atIndexSelector
  , removeItemWithObjectValueSelector
  , removeItemAtIndexSelector
  , removeAllItemsSelector
  , selectItemWithObjectValueSelector
  , itemObjectValueAtIndexSelector
  , indexOfItemWithObjectValueSelector
  , hasVerticalScrollerSelector
  , setHasVerticalScrollerSelector
  , intercellSpacingSelector
  , setIntercellSpacingSelector
  , itemHeightSelector
  , setItemHeightSelector
  , numberOfVisibleItemsSelector
  , setNumberOfVisibleItemsSelector
  , buttonBorderedSelector
  , setButtonBorderedSelector
  , usesDataSourceSelector
  , setUsesDataSourceSelector
  , indexOfSelectedItemSelector
  , numberOfItemsSelector
  , completesSelector
  , setCompletesSelector
  , dataSourceSelector
  , setDataSourceSelector
  , objectValueOfSelectedItemSelector
  , objectValuesSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- reloadData@
reloadData :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO ()
reloadData nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "reloadData") retVoid []

-- | @- noteNumberOfItemsChanged@
noteNumberOfItemsChanged :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO ()
noteNumberOfItemsChanged nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "noteNumberOfItemsChanged") retVoid []

-- | @- scrollItemAtIndexToTop:@
scrollItemAtIndexToTop :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
scrollItemAtIndexToTop nsComboBoxCell  index =
    sendMsg nsComboBoxCell (mkSelector "scrollItemAtIndexToTop:") retVoid [argCLong index]

-- | @- scrollItemAtIndexToVisible:@
scrollItemAtIndexToVisible :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
scrollItemAtIndexToVisible nsComboBoxCell  index =
    sendMsg nsComboBoxCell (mkSelector "scrollItemAtIndexToVisible:") retVoid [argCLong index]

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
selectItemAtIndex nsComboBoxCell  index =
    sendMsg nsComboBoxCell (mkSelector "selectItemAtIndex:") retVoid [argCLong index]

-- | @- deselectItemAtIndex:@
deselectItemAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
deselectItemAtIndex nsComboBoxCell  index =
    sendMsg nsComboBoxCell (mkSelector "deselectItemAtIndex:") retVoid [argCLong index]

-- | @- completedString:@
completedString :: (IsNSComboBoxCell nsComboBoxCell, IsNSString string) => nsComboBoxCell -> string -> IO (Id NSString)
completedString nsComboBoxCell  string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsComboBoxCell (mkSelector "completedString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @- addItemWithObjectValue:@
addItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
addItemWithObjectValue nsComboBoxCell  object =
    sendMsg nsComboBoxCell (mkSelector "addItemWithObjectValue:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- addItemsWithObjectValues:@
addItemsWithObjectValues :: (IsNSComboBoxCell nsComboBoxCell, IsNSArray objects) => nsComboBoxCell -> objects -> IO ()
addItemsWithObjectValues nsComboBoxCell  objects =
  withObjCPtr objects $ \raw_objects ->
      sendMsg nsComboBoxCell (mkSelector "addItemsWithObjectValues:") retVoid [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- insertItemWithObjectValue:atIndex:@
insertItemWithObjectValue_atIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> CLong -> IO ()
insertItemWithObjectValue_atIndex nsComboBoxCell  object index =
    sendMsg nsComboBoxCell (mkSelector "insertItemWithObjectValue:atIndex:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argCLong index]

-- | @- removeItemWithObjectValue:@
removeItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
removeItemWithObjectValue nsComboBoxCell  object =
    sendMsg nsComboBoxCell (mkSelector "removeItemWithObjectValue:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
removeItemAtIndex nsComboBoxCell  index =
    sendMsg nsComboBoxCell (mkSelector "removeItemAtIndex:") retVoid [argCLong index]

-- | @- removeAllItems@
removeAllItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO ()
removeAllItems nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "removeAllItems") retVoid []

-- | @- selectItemWithObjectValue:@
selectItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
selectItemWithObjectValue nsComboBoxCell  object =
    sendMsg nsComboBoxCell (mkSelector "selectItemWithObjectValue:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- itemObjectValueAtIndex:@
itemObjectValueAtIndex :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO RawId
itemObjectValueAtIndex nsComboBoxCell  index =
    fmap (RawId . castPtr) $ sendMsg nsComboBoxCell (mkSelector "itemObjectValueAtIndex:") (retPtr retVoid) [argCLong index]

-- | @- indexOfItemWithObjectValue:@
indexOfItemWithObjectValue :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO CLong
indexOfItemWithObjectValue nsComboBoxCell  object =
    sendMsg nsComboBoxCell (mkSelector "indexOfItemWithObjectValue:") retCLong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- hasVerticalScroller@
hasVerticalScroller :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
hasVerticalScroller nsComboBoxCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBoxCell (mkSelector "hasVerticalScroller") retCULong []

-- | @- setHasVerticalScroller:@
setHasVerticalScroller :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setHasVerticalScroller nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setHasVerticalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | @- intercellSpacing@
intercellSpacing :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO NSSize
intercellSpacing nsComboBoxCell  =
    sendMsgStret nsComboBoxCell (mkSelector "intercellSpacing") retNSSize []

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> NSSize -> IO ()
setIntercellSpacing nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setIntercellSpacing:") retVoid [argNSSize value]

-- | @- itemHeight@
itemHeight :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CDouble
itemHeight nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "itemHeight") retCDouble []

-- | @- setItemHeight:@
setItemHeight :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CDouble -> IO ()
setItemHeight nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setItemHeight:") retVoid [argCDouble value]

-- | @- numberOfVisibleItems@
numberOfVisibleItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CLong
numberOfVisibleItems nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "numberOfVisibleItems") retCLong []

-- | @- setNumberOfVisibleItems:@
setNumberOfVisibleItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> CLong -> IO ()
setNumberOfVisibleItems nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setNumberOfVisibleItems:") retVoid [argCLong value]

-- | @- buttonBordered@
buttonBordered :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
buttonBordered nsComboBoxCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBoxCell (mkSelector "buttonBordered") retCULong []

-- | @- setButtonBordered:@
setButtonBordered :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setButtonBordered nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setButtonBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesDataSource@
usesDataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
usesDataSource nsComboBoxCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBoxCell (mkSelector "usesDataSource") retCULong []

-- | @- setUsesDataSource:@
setUsesDataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setUsesDataSource nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setUsesDataSource:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CLong
indexOfSelectedItem nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "indexOfSelectedItem") retCLong []

-- | @- numberOfItems@
numberOfItems :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO CLong
numberOfItems nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "numberOfItems") retCLong []

-- | @- completes@
completes :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO Bool
completes nsComboBoxCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBoxCell (mkSelector "completes") retCULong []

-- | @- setCompletes:@
setCompletes :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> Bool -> IO ()
setCompletes nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setCompletes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- dataSource@
dataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO RawId
dataSource nsComboBoxCell  =
    fmap (RawId . castPtr) $ sendMsg nsComboBoxCell (mkSelector "dataSource") (retPtr retVoid) []

-- | @- setDataSource:@
setDataSource :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> RawId -> IO ()
setDataSource nsComboBoxCell  value =
    sendMsg nsComboBoxCell (mkSelector "setDataSource:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- objectValueOfSelectedItem@
objectValueOfSelectedItem :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO RawId
objectValueOfSelectedItem nsComboBoxCell  =
    fmap (RawId . castPtr) $ sendMsg nsComboBoxCell (mkSelector "objectValueOfSelectedItem") (retPtr retVoid) []

-- | @- objectValues@
objectValues :: IsNSComboBoxCell nsComboBoxCell => nsComboBoxCell -> IO (Id NSArray)
objectValues nsComboBoxCell  =
    sendMsg nsComboBoxCell (mkSelector "objectValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @reloadData@
reloadDataSelector :: Selector
reloadDataSelector = mkSelector "reloadData"

-- | @Selector@ for @noteNumberOfItemsChanged@
noteNumberOfItemsChangedSelector :: Selector
noteNumberOfItemsChangedSelector = mkSelector "noteNumberOfItemsChanged"

-- | @Selector@ for @scrollItemAtIndexToTop:@
scrollItemAtIndexToTopSelector :: Selector
scrollItemAtIndexToTopSelector = mkSelector "scrollItemAtIndexToTop:"

-- | @Selector@ for @scrollItemAtIndexToVisible:@
scrollItemAtIndexToVisibleSelector :: Selector
scrollItemAtIndexToVisibleSelector = mkSelector "scrollItemAtIndexToVisible:"

-- | @Selector@ for @selectItemAtIndex:@
selectItemAtIndexSelector :: Selector
selectItemAtIndexSelector = mkSelector "selectItemAtIndex:"

-- | @Selector@ for @deselectItemAtIndex:@
deselectItemAtIndexSelector :: Selector
deselectItemAtIndexSelector = mkSelector "deselectItemAtIndex:"

-- | @Selector@ for @completedString:@
completedStringSelector :: Selector
completedStringSelector = mkSelector "completedString:"

-- | @Selector@ for @addItemWithObjectValue:@
addItemWithObjectValueSelector :: Selector
addItemWithObjectValueSelector = mkSelector "addItemWithObjectValue:"

-- | @Selector@ for @addItemsWithObjectValues:@
addItemsWithObjectValuesSelector :: Selector
addItemsWithObjectValuesSelector = mkSelector "addItemsWithObjectValues:"

-- | @Selector@ for @insertItemWithObjectValue:atIndex:@
insertItemWithObjectValue_atIndexSelector :: Selector
insertItemWithObjectValue_atIndexSelector = mkSelector "insertItemWithObjectValue:atIndex:"

-- | @Selector@ for @removeItemWithObjectValue:@
removeItemWithObjectValueSelector :: Selector
removeItemWithObjectValueSelector = mkSelector "removeItemWithObjectValue:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @selectItemWithObjectValue:@
selectItemWithObjectValueSelector :: Selector
selectItemWithObjectValueSelector = mkSelector "selectItemWithObjectValue:"

-- | @Selector@ for @itemObjectValueAtIndex:@
itemObjectValueAtIndexSelector :: Selector
itemObjectValueAtIndexSelector = mkSelector "itemObjectValueAtIndex:"

-- | @Selector@ for @indexOfItemWithObjectValue:@
indexOfItemWithObjectValueSelector :: Selector
indexOfItemWithObjectValueSelector = mkSelector "indexOfItemWithObjectValue:"

-- | @Selector@ for @hasVerticalScroller@
hasVerticalScrollerSelector :: Selector
hasVerticalScrollerSelector = mkSelector "hasVerticalScroller"

-- | @Selector@ for @setHasVerticalScroller:@
setHasVerticalScrollerSelector :: Selector
setHasVerticalScrollerSelector = mkSelector "setHasVerticalScroller:"

-- | @Selector@ for @intercellSpacing@
intercellSpacingSelector :: Selector
intercellSpacingSelector = mkSelector "intercellSpacing"

-- | @Selector@ for @setIntercellSpacing:@
setIntercellSpacingSelector :: Selector
setIntercellSpacingSelector = mkSelector "setIntercellSpacing:"

-- | @Selector@ for @itemHeight@
itemHeightSelector :: Selector
itemHeightSelector = mkSelector "itemHeight"

-- | @Selector@ for @setItemHeight:@
setItemHeightSelector :: Selector
setItemHeightSelector = mkSelector "setItemHeight:"

-- | @Selector@ for @numberOfVisibleItems@
numberOfVisibleItemsSelector :: Selector
numberOfVisibleItemsSelector = mkSelector "numberOfVisibleItems"

-- | @Selector@ for @setNumberOfVisibleItems:@
setNumberOfVisibleItemsSelector :: Selector
setNumberOfVisibleItemsSelector = mkSelector "setNumberOfVisibleItems:"

-- | @Selector@ for @buttonBordered@
buttonBorderedSelector :: Selector
buttonBorderedSelector = mkSelector "buttonBordered"

-- | @Selector@ for @setButtonBordered:@
setButtonBorderedSelector :: Selector
setButtonBorderedSelector = mkSelector "setButtonBordered:"

-- | @Selector@ for @usesDataSource@
usesDataSourceSelector :: Selector
usesDataSourceSelector = mkSelector "usesDataSource"

-- | @Selector@ for @setUsesDataSource:@
setUsesDataSourceSelector :: Selector
setUsesDataSourceSelector = mkSelector "setUsesDataSource:"

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @completes@
completesSelector :: Selector
completesSelector = mkSelector "completes"

-- | @Selector@ for @setCompletes:@
setCompletesSelector :: Selector
setCompletesSelector = mkSelector "setCompletes:"

-- | @Selector@ for @dataSource@
dataSourceSelector :: Selector
dataSourceSelector = mkSelector "dataSource"

-- | @Selector@ for @setDataSource:@
setDataSourceSelector :: Selector
setDataSourceSelector = mkSelector "setDataSource:"

-- | @Selector@ for @objectValueOfSelectedItem@
objectValueOfSelectedItemSelector :: Selector
objectValueOfSelectedItemSelector = mkSelector "objectValueOfSelectedItem"

-- | @Selector@ for @objectValues@
objectValuesSelector :: Selector
objectValuesSelector = mkSelector "objectValues"

