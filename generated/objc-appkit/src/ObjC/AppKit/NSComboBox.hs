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
  , objectValueOfSelectedItem
  , objectValues
  , reloadDataSelector
  , noteNumberOfItemsChangedSelector
  , scrollItemAtIndexToTopSelector
  , scrollItemAtIndexToVisibleSelector
  , selectItemAtIndexSelector
  , deselectItemAtIndexSelector
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
reloadData :: IsNSComboBox nsComboBox => nsComboBox -> IO ()
reloadData nsComboBox  =
  sendMsg nsComboBox (mkSelector "reloadData") retVoid []

-- | @- noteNumberOfItemsChanged@
noteNumberOfItemsChanged :: IsNSComboBox nsComboBox => nsComboBox -> IO ()
noteNumberOfItemsChanged nsComboBox  =
  sendMsg nsComboBox (mkSelector "noteNumberOfItemsChanged") retVoid []

-- | @- scrollItemAtIndexToTop:@
scrollItemAtIndexToTop :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
scrollItemAtIndexToTop nsComboBox  index =
  sendMsg nsComboBox (mkSelector "scrollItemAtIndexToTop:") retVoid [argCLong (fromIntegral index)]

-- | @- scrollItemAtIndexToVisible:@
scrollItemAtIndexToVisible :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
scrollItemAtIndexToVisible nsComboBox  index =
  sendMsg nsComboBox (mkSelector "scrollItemAtIndexToVisible:") retVoid [argCLong (fromIntegral index)]

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
selectItemAtIndex nsComboBox  index =
  sendMsg nsComboBox (mkSelector "selectItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- deselectItemAtIndex:@
deselectItemAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
deselectItemAtIndex nsComboBox  index =
  sendMsg nsComboBox (mkSelector "deselectItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- addItemWithObjectValue:@
addItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
addItemWithObjectValue nsComboBox  object =
  sendMsg nsComboBox (mkSelector "addItemWithObjectValue:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- addItemsWithObjectValues:@
addItemsWithObjectValues :: (IsNSComboBox nsComboBox, IsNSArray objects) => nsComboBox -> objects -> IO ()
addItemsWithObjectValues nsComboBox  objects =
withObjCPtr objects $ \raw_objects ->
    sendMsg nsComboBox (mkSelector "addItemsWithObjectValues:") retVoid [argPtr (castPtr raw_objects :: Ptr ())]

-- | @- insertItemWithObjectValue:atIndex:@
insertItemWithObjectValue_atIndex :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> CLong -> IO ()
insertItemWithObjectValue_atIndex nsComboBox  object index =
  sendMsg nsComboBox (mkSelector "insertItemWithObjectValue:atIndex:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argCLong (fromIntegral index)]

-- | @- removeItemWithObjectValue:@
removeItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
removeItemWithObjectValue nsComboBox  object =
  sendMsg nsComboBox (mkSelector "removeItemWithObjectValue:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
removeItemAtIndex nsComboBox  index =
  sendMsg nsComboBox (mkSelector "removeItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- removeAllItems@
removeAllItems :: IsNSComboBox nsComboBox => nsComboBox -> IO ()
removeAllItems nsComboBox  =
  sendMsg nsComboBox (mkSelector "removeAllItems") retVoid []

-- | @- selectItemWithObjectValue:@
selectItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO ()
selectItemWithObjectValue nsComboBox  object =
  sendMsg nsComboBox (mkSelector "selectItemWithObjectValue:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- itemObjectValueAtIndex:@
itemObjectValueAtIndex :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO RawId
itemObjectValueAtIndex nsComboBox  index =
  fmap (RawId . castPtr) $ sendMsg nsComboBox (mkSelector "itemObjectValueAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)]

-- | @- indexOfItemWithObjectValue:@
indexOfItemWithObjectValue :: IsNSComboBox nsComboBox => nsComboBox -> RawId -> IO CLong
indexOfItemWithObjectValue nsComboBox  object =
  sendMsg nsComboBox (mkSelector "indexOfItemWithObjectValue:") retCLong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- hasVerticalScroller@
hasVerticalScroller :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
hasVerticalScroller nsComboBox  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBox (mkSelector "hasVerticalScroller") retCULong []

-- | @- setHasVerticalScroller:@
setHasVerticalScroller :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setHasVerticalScroller nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setHasVerticalScroller:") retVoid [argCULong (if value then 1 else 0)]

-- | @- intercellSpacing@
intercellSpacing :: IsNSComboBox nsComboBox => nsComboBox -> IO NSSize
intercellSpacing nsComboBox  =
  sendMsgStret nsComboBox (mkSelector "intercellSpacing") retNSSize []

-- | @- setIntercellSpacing:@
setIntercellSpacing :: IsNSComboBox nsComboBox => nsComboBox -> NSSize -> IO ()
setIntercellSpacing nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setIntercellSpacing:") retVoid [argNSSize value]

-- | @- itemHeight@
itemHeight :: IsNSComboBox nsComboBox => nsComboBox -> IO CDouble
itemHeight nsComboBox  =
  sendMsg nsComboBox (mkSelector "itemHeight") retCDouble []

-- | @- setItemHeight:@
setItemHeight :: IsNSComboBox nsComboBox => nsComboBox -> CDouble -> IO ()
setItemHeight nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setItemHeight:") retVoid [argCDouble (fromIntegral value)]

-- | @- numberOfVisibleItems@
numberOfVisibleItems :: IsNSComboBox nsComboBox => nsComboBox -> IO CLong
numberOfVisibleItems nsComboBox  =
  sendMsg nsComboBox (mkSelector "numberOfVisibleItems") retCLong []

-- | @- setNumberOfVisibleItems:@
setNumberOfVisibleItems :: IsNSComboBox nsComboBox => nsComboBox -> CLong -> IO ()
setNumberOfVisibleItems nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setNumberOfVisibleItems:") retVoid [argCLong (fromIntegral value)]

-- | @- buttonBordered@
buttonBordered :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
buttonBordered nsComboBox  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBox (mkSelector "buttonBordered") retCULong []

-- | @- setButtonBordered:@
setButtonBordered :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setButtonBordered nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setButtonBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | @- usesDataSource@
usesDataSource :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
usesDataSource nsComboBox  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBox (mkSelector "usesDataSource") retCULong []

-- | @- setUsesDataSource:@
setUsesDataSource :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setUsesDataSource nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setUsesDataSource:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSComboBox nsComboBox => nsComboBox -> IO CLong
indexOfSelectedItem nsComboBox  =
  sendMsg nsComboBox (mkSelector "indexOfSelectedItem") retCLong []

-- | @- numberOfItems@
numberOfItems :: IsNSComboBox nsComboBox => nsComboBox -> IO CLong
numberOfItems nsComboBox  =
  sendMsg nsComboBox (mkSelector "numberOfItems") retCLong []

-- | @- completes@
completes :: IsNSComboBox nsComboBox => nsComboBox -> IO Bool
completes nsComboBox  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsComboBox (mkSelector "completes") retCULong []

-- | @- setCompletes:@
setCompletes :: IsNSComboBox nsComboBox => nsComboBox -> Bool -> IO ()
setCompletes nsComboBox  value =
  sendMsg nsComboBox (mkSelector "setCompletes:") retVoid [argCULong (if value then 1 else 0)]

-- | @- objectValueOfSelectedItem@
objectValueOfSelectedItem :: IsNSComboBox nsComboBox => nsComboBox -> IO RawId
objectValueOfSelectedItem nsComboBox  =
  fmap (RawId . castPtr) $ sendMsg nsComboBox (mkSelector "objectValueOfSelectedItem") (retPtr retVoid) []

-- | @- objectValues@
objectValues :: IsNSComboBox nsComboBox => nsComboBox -> IO (Id NSArray)
objectValues nsComboBox  =
  sendMsg nsComboBox (mkSelector "objectValues") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @objectValueOfSelectedItem@
objectValueOfSelectedItemSelector :: Selector
objectValueOfSelectedItemSelector = mkSelector "objectValueOfSelectedItem"

-- | @Selector@ for @objectValues@
objectValuesSelector :: Selector
objectValuesSelector = mkSelector "objectValues"

