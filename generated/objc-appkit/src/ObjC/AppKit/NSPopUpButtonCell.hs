{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPopUpButtonCell@.
module ObjC.AppKit.NSPopUpButtonCell
  ( NSPopUpButtonCell
  , IsNSPopUpButtonCell(..)
  , initTextCell_pullsDown
  , initWithCoder
  , addItemWithTitle
  , addItemsWithTitles
  , insertItemWithTitle_atIndex
  , removeItemWithTitle
  , removeItemAtIndex
  , removeAllItems
  , indexOfItem
  , indexOfItemWithTitle
  , indexOfItemWithTag
  , indexOfItemWithRepresentedObject
  , indexOfItemWithTarget_andAction
  , itemAtIndex
  , itemWithTitle
  , selectItem
  , selectItemAtIndex
  , selectItemWithTitle
  , selectItemWithTag
  , setTitle
  , synchronizeTitleAndSelectedItem
  , itemTitleAtIndex
  , attachPopUpWithFrame_inView
  , dismissPopUp
  , performClickWithFrame_inView
  , menu
  , setMenu
  , pullsDown
  , setPullsDown
  , autoenablesItems
  , setAutoenablesItems
  , preferredEdge
  , setPreferredEdge
  , usesItemFromMenu
  , setUsesItemFromMenu
  , altersStateOfSelectedItem
  , setAltersStateOfSelectedItem
  , itemArray
  , numberOfItems
  , lastItem
  , selectedItem
  , indexOfSelectedItem
  , itemTitles
  , titleOfSelectedItem
  , arrowPosition
  , setArrowPosition
  , initTextCell_pullsDownSelector
  , initWithCoderSelector
  , addItemWithTitleSelector
  , addItemsWithTitlesSelector
  , insertItemWithTitle_atIndexSelector
  , removeItemWithTitleSelector
  , removeItemAtIndexSelector
  , removeAllItemsSelector
  , indexOfItemSelector
  , indexOfItemWithTitleSelector
  , indexOfItemWithTagSelector
  , indexOfItemWithRepresentedObjectSelector
  , indexOfItemWithTarget_andActionSelector
  , itemAtIndexSelector
  , itemWithTitleSelector
  , selectItemSelector
  , selectItemAtIndexSelector
  , selectItemWithTitleSelector
  , selectItemWithTagSelector
  , setTitleSelector
  , synchronizeTitleAndSelectedItemSelector
  , itemTitleAtIndexSelector
  , attachPopUpWithFrame_inViewSelector
  , dismissPopUpSelector
  , performClickWithFrame_inViewSelector
  , menuSelector
  , setMenuSelector
  , pullsDownSelector
  , setPullsDownSelector
  , autoenablesItemsSelector
  , setAutoenablesItemsSelector
  , preferredEdgeSelector
  , setPreferredEdgeSelector
  , usesItemFromMenuSelector
  , setUsesItemFromMenuSelector
  , altersStateOfSelectedItemSelector
  , setAltersStateOfSelectedItemSelector
  , itemArraySelector
  , numberOfItemsSelector
  , lastItemSelector
  , selectedItemSelector
  , indexOfSelectedItemSelector
  , itemTitlesSelector
  , titleOfSelectedItemSelector
  , arrowPositionSelector
  , setArrowPositionSelector

  -- * Enum types
  , NSPopUpArrowPosition(NSPopUpArrowPosition)
  , pattern NSPopUpNoArrow
  , pattern NSPopUpArrowAtCenter
  , pattern NSPopUpArrowAtBottom
  , NSRectEdge(NSRectEdge)
  , pattern NSRectEdgeMinX
  , pattern NSRectEdgeMinY
  , pattern NSRectEdgeMaxX
  , pattern NSRectEdgeMaxY
  , pattern NSMinXEdge
  , pattern NSMinYEdge
  , pattern NSMaxXEdge
  , pattern NSMaxYEdge

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:pullsDown:@
initTextCell_pullsDown :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString stringValue) => nsPopUpButtonCell -> stringValue -> Bool -> IO (Id NSPopUpButtonCell)
initTextCell_pullsDown nsPopUpButtonCell  stringValue pullDown =
withObjCPtr stringValue $ \raw_stringValue ->
    sendMsg nsPopUpButtonCell (mkSelector "initTextCell:pullsDown:") (retPtr retVoid) [argPtr (castPtr raw_stringValue :: Ptr ()), argCULong (if pullDown then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSCoder coder) => nsPopUpButtonCell -> coder -> IO (Id NSPopUpButtonCell)
initWithCoder nsPopUpButtonCell  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsPopUpButtonCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- addItemWithTitle:@
addItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO ()
addItemWithTitle nsPopUpButtonCell  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButtonCell (mkSelector "addItemWithTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- addItemsWithTitles:@
addItemsWithTitles :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSArray itemTitles) => nsPopUpButtonCell -> itemTitles -> IO ()
addItemsWithTitles nsPopUpButtonCell  itemTitles =
withObjCPtr itemTitles $ \raw_itemTitles ->
    sendMsg nsPopUpButtonCell (mkSelector "addItemsWithTitles:") retVoid [argPtr (castPtr raw_itemTitles :: Ptr ())]

-- | @- insertItemWithTitle:atIndex:@
insertItemWithTitle_atIndex :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> CLong -> IO ()
insertItemWithTitle_atIndex nsPopUpButtonCell  title index =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButtonCell (mkSelector "insertItemWithTitle:atIndex:") retVoid [argPtr (castPtr raw_title :: Ptr ()), argCLong (fromIntegral index)]

-- | @- removeItemWithTitle:@
removeItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO ()
removeItemWithTitle nsPopUpButtonCell  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButtonCell (mkSelector "removeItemWithTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO ()
removeItemAtIndex nsPopUpButtonCell  index =
  sendMsg nsPopUpButtonCell (mkSelector "removeItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- removeAllItems@
removeAllItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO ()
removeAllItems nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "removeAllItems") retVoid []

-- | @- indexOfItem:@
indexOfItem :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSMenuItem item) => nsPopUpButtonCell -> item -> IO CLong
indexOfItem nsPopUpButtonCell  item =
withObjCPtr item $ \raw_item ->
    sendMsg nsPopUpButtonCell (mkSelector "indexOfItem:") retCLong [argPtr (castPtr raw_item :: Ptr ())]

-- | @- indexOfItemWithTitle:@
indexOfItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO CLong
indexOfItemWithTitle nsPopUpButtonCell  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButtonCell (mkSelector "indexOfItemWithTitle:") retCLong [argPtr (castPtr raw_title :: Ptr ())]

-- | @- indexOfItemWithTag:@
indexOfItemWithTag :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO CLong
indexOfItemWithTag nsPopUpButtonCell  tag =
  sendMsg nsPopUpButtonCell (mkSelector "indexOfItemWithTag:") retCLong [argCLong (fromIntegral tag)]

-- | @- indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObject :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> RawId -> IO CLong
indexOfItemWithRepresentedObject nsPopUpButtonCell  obj_ =
  sendMsg nsPopUpButtonCell (mkSelector "indexOfItemWithRepresentedObject:") retCLong [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @- indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andAction :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> RawId -> Selector -> IO CLong
indexOfItemWithTarget_andAction nsPopUpButtonCell  target actionSelector =
  sendMsg nsPopUpButtonCell (mkSelector "indexOfItemWithTarget:andAction:") retCLong [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector actionSelector)]

-- | @- itemAtIndex:@
itemAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO (Id NSMenuItem)
itemAtIndex nsPopUpButtonCell  index =
  sendMsg nsPopUpButtonCell (mkSelector "itemAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- itemWithTitle:@
itemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO (Id NSMenuItem)
itemWithTitle nsPopUpButtonCell  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButtonCell (mkSelector "itemWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- selectItem:@
selectItem :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSMenuItem item) => nsPopUpButtonCell -> item -> IO ()
selectItem nsPopUpButtonCell  item =
withObjCPtr item $ \raw_item ->
    sendMsg nsPopUpButtonCell (mkSelector "selectItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO ()
selectItemAtIndex nsPopUpButtonCell  index =
  sendMsg nsPopUpButtonCell (mkSelector "selectItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- selectItemWithTitle:@
selectItemWithTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString title) => nsPopUpButtonCell -> title -> IO ()
selectItemWithTitle nsPopUpButtonCell  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButtonCell (mkSelector "selectItemWithTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- selectItemWithTag:@
selectItemWithTag :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO Bool
selectItemWithTag nsPopUpButtonCell  tag =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButtonCell (mkSelector "selectItemWithTag:") retCULong [argCLong (fromIntegral tag)]

-- | @- setTitle:@
setTitle :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSString string) => nsPopUpButtonCell -> string -> IO ()
setTitle nsPopUpButtonCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsPopUpButtonCell (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- synchronizeTitleAndSelectedItem@
synchronizeTitleAndSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO ()
synchronizeTitleAndSelectedItem nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "synchronizeTitleAndSelectedItem") retVoid []

-- | @- itemTitleAtIndex:@
itemTitleAtIndex :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> CLong -> IO (Id NSString)
itemTitleAtIndex nsPopUpButtonCell  index =
  sendMsg nsPopUpButtonCell (mkSelector "itemTitleAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- attachPopUpWithFrame:inView:@
attachPopUpWithFrame_inView :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSView controlView) => nsPopUpButtonCell -> NSRect -> controlView -> IO ()
attachPopUpWithFrame_inView nsPopUpButtonCell  cellFrame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsPopUpButtonCell (mkSelector "attachPopUpWithFrame:inView:") retVoid [argNSRect cellFrame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- dismissPopUp@
dismissPopUp :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO ()
dismissPopUp nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "dismissPopUp") retVoid []

-- | @- performClickWithFrame:inView:@
performClickWithFrame_inView :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSView controlView) => nsPopUpButtonCell -> NSRect -> controlView -> IO ()
performClickWithFrame_inView nsPopUpButtonCell  frame controlView =
withObjCPtr controlView $ \raw_controlView ->
    sendMsg nsPopUpButtonCell (mkSelector "performClickWithFrame:inView:") retVoid [argNSRect frame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- menu@
menu :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSMenu)
menu nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMenu:@
setMenu :: (IsNSPopUpButtonCell nsPopUpButtonCell, IsNSMenu value) => nsPopUpButtonCell -> value -> IO ()
setMenu nsPopUpButtonCell  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPopUpButtonCell (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pullsDown@
pullsDown :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
pullsDown nsPopUpButtonCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButtonCell (mkSelector "pullsDown") retCULong []

-- | @- setPullsDown:@
setPullsDown :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setPullsDown nsPopUpButtonCell  value =
  sendMsg nsPopUpButtonCell (mkSelector "setPullsDown:") retVoid [argCULong (if value then 1 else 0)]

-- | @- autoenablesItems@
autoenablesItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
autoenablesItems nsPopUpButtonCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButtonCell (mkSelector "autoenablesItems") retCULong []

-- | @- setAutoenablesItems:@
setAutoenablesItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setAutoenablesItems nsPopUpButtonCell  value =
  sendMsg nsPopUpButtonCell (mkSelector "setAutoenablesItems:") retVoid [argCULong (if value then 1 else 0)]

-- | @- preferredEdge@
preferredEdge :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO NSRectEdge
preferredEdge nsPopUpButtonCell  =
  fmap (coerce :: CULong -> NSRectEdge) $ sendMsg nsPopUpButtonCell (mkSelector "preferredEdge") retCULong []

-- | @- setPreferredEdge:@
setPreferredEdge :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> NSRectEdge -> IO ()
setPreferredEdge nsPopUpButtonCell  value =
  sendMsg nsPopUpButtonCell (mkSelector "setPreferredEdge:") retVoid [argCULong (coerce value)]

-- | @- usesItemFromMenu@
usesItemFromMenu :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
usesItemFromMenu nsPopUpButtonCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButtonCell (mkSelector "usesItemFromMenu") retCULong []

-- | @- setUsesItemFromMenu:@
setUsesItemFromMenu :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setUsesItemFromMenu nsPopUpButtonCell  value =
  sendMsg nsPopUpButtonCell (mkSelector "setUsesItemFromMenu:") retVoid [argCULong (if value then 1 else 0)]

-- | @- altersStateOfSelectedItem@
altersStateOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO Bool
altersStateOfSelectedItem nsPopUpButtonCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButtonCell (mkSelector "altersStateOfSelectedItem") retCULong []

-- | @- setAltersStateOfSelectedItem:@
setAltersStateOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> Bool -> IO ()
setAltersStateOfSelectedItem nsPopUpButtonCell  value =
  sendMsg nsPopUpButtonCell (mkSelector "setAltersStateOfSelectedItem:") retVoid [argCULong (if value then 1 else 0)]

-- | @- itemArray@
itemArray :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSArray)
itemArray nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "itemArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfItems@
numberOfItems :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO CLong
numberOfItems nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "numberOfItems") retCLong []

-- | @- lastItem@
lastItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSMenuItem)
lastItem nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "lastItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedItem@
selectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSMenuItem)
selectedItem nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "selectedItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO CLong
indexOfSelectedItem nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "indexOfSelectedItem") retCLong []

-- | @- itemTitles@
itemTitles :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSArray)
itemTitles nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "itemTitles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- titleOfSelectedItem@
titleOfSelectedItem :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO (Id NSString)
titleOfSelectedItem nsPopUpButtonCell  =
  sendMsg nsPopUpButtonCell (mkSelector "titleOfSelectedItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- arrowPosition@
arrowPosition :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> IO NSPopUpArrowPosition
arrowPosition nsPopUpButtonCell  =
  fmap (coerce :: CULong -> NSPopUpArrowPosition) $ sendMsg nsPopUpButtonCell (mkSelector "arrowPosition") retCULong []

-- | @- setArrowPosition:@
setArrowPosition :: IsNSPopUpButtonCell nsPopUpButtonCell => nsPopUpButtonCell -> NSPopUpArrowPosition -> IO ()
setArrowPosition nsPopUpButtonCell  value =
  sendMsg nsPopUpButtonCell (mkSelector "setArrowPosition:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:pullsDown:@
initTextCell_pullsDownSelector :: Selector
initTextCell_pullsDownSelector = mkSelector "initTextCell:pullsDown:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @addItemWithTitle:@
addItemWithTitleSelector :: Selector
addItemWithTitleSelector = mkSelector "addItemWithTitle:"

-- | @Selector@ for @addItemsWithTitles:@
addItemsWithTitlesSelector :: Selector
addItemsWithTitlesSelector = mkSelector "addItemsWithTitles:"

-- | @Selector@ for @insertItemWithTitle:atIndex:@
insertItemWithTitle_atIndexSelector :: Selector
insertItemWithTitle_atIndexSelector = mkSelector "insertItemWithTitle:atIndex:"

-- | @Selector@ for @removeItemWithTitle:@
removeItemWithTitleSelector :: Selector
removeItemWithTitleSelector = mkSelector "removeItemWithTitle:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @indexOfItem:@
indexOfItemSelector :: Selector
indexOfItemSelector = mkSelector "indexOfItem:"

-- | @Selector@ for @indexOfItemWithTitle:@
indexOfItemWithTitleSelector :: Selector
indexOfItemWithTitleSelector = mkSelector "indexOfItemWithTitle:"

-- | @Selector@ for @indexOfItemWithTag:@
indexOfItemWithTagSelector :: Selector
indexOfItemWithTagSelector = mkSelector "indexOfItemWithTag:"

-- | @Selector@ for @indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObjectSelector :: Selector
indexOfItemWithRepresentedObjectSelector = mkSelector "indexOfItemWithRepresentedObject:"

-- | @Selector@ for @indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andActionSelector :: Selector
indexOfItemWithTarget_andActionSelector = mkSelector "indexOfItemWithTarget:andAction:"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @itemWithTitle:@
itemWithTitleSelector :: Selector
itemWithTitleSelector = mkSelector "itemWithTitle:"

-- | @Selector@ for @selectItem:@
selectItemSelector :: Selector
selectItemSelector = mkSelector "selectItem:"

-- | @Selector@ for @selectItemAtIndex:@
selectItemAtIndexSelector :: Selector
selectItemAtIndexSelector = mkSelector "selectItemAtIndex:"

-- | @Selector@ for @selectItemWithTitle:@
selectItemWithTitleSelector :: Selector
selectItemWithTitleSelector = mkSelector "selectItemWithTitle:"

-- | @Selector@ for @selectItemWithTag:@
selectItemWithTagSelector :: Selector
selectItemWithTagSelector = mkSelector "selectItemWithTag:"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @synchronizeTitleAndSelectedItem@
synchronizeTitleAndSelectedItemSelector :: Selector
synchronizeTitleAndSelectedItemSelector = mkSelector "synchronizeTitleAndSelectedItem"

-- | @Selector@ for @itemTitleAtIndex:@
itemTitleAtIndexSelector :: Selector
itemTitleAtIndexSelector = mkSelector "itemTitleAtIndex:"

-- | @Selector@ for @attachPopUpWithFrame:inView:@
attachPopUpWithFrame_inViewSelector :: Selector
attachPopUpWithFrame_inViewSelector = mkSelector "attachPopUpWithFrame:inView:"

-- | @Selector@ for @dismissPopUp@
dismissPopUpSelector :: Selector
dismissPopUpSelector = mkSelector "dismissPopUp"

-- | @Selector@ for @performClickWithFrame:inView:@
performClickWithFrame_inViewSelector :: Selector
performClickWithFrame_inViewSelector = mkSelector "performClickWithFrame:inView:"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @pullsDown@
pullsDownSelector :: Selector
pullsDownSelector = mkSelector "pullsDown"

-- | @Selector@ for @setPullsDown:@
setPullsDownSelector :: Selector
setPullsDownSelector = mkSelector "setPullsDown:"

-- | @Selector@ for @autoenablesItems@
autoenablesItemsSelector :: Selector
autoenablesItemsSelector = mkSelector "autoenablesItems"

-- | @Selector@ for @setAutoenablesItems:@
setAutoenablesItemsSelector :: Selector
setAutoenablesItemsSelector = mkSelector "setAutoenablesItems:"

-- | @Selector@ for @preferredEdge@
preferredEdgeSelector :: Selector
preferredEdgeSelector = mkSelector "preferredEdge"

-- | @Selector@ for @setPreferredEdge:@
setPreferredEdgeSelector :: Selector
setPreferredEdgeSelector = mkSelector "setPreferredEdge:"

-- | @Selector@ for @usesItemFromMenu@
usesItemFromMenuSelector :: Selector
usesItemFromMenuSelector = mkSelector "usesItemFromMenu"

-- | @Selector@ for @setUsesItemFromMenu:@
setUsesItemFromMenuSelector :: Selector
setUsesItemFromMenuSelector = mkSelector "setUsesItemFromMenu:"

-- | @Selector@ for @altersStateOfSelectedItem@
altersStateOfSelectedItemSelector :: Selector
altersStateOfSelectedItemSelector = mkSelector "altersStateOfSelectedItem"

-- | @Selector@ for @setAltersStateOfSelectedItem:@
setAltersStateOfSelectedItemSelector :: Selector
setAltersStateOfSelectedItemSelector = mkSelector "setAltersStateOfSelectedItem:"

-- | @Selector@ for @itemArray@
itemArraySelector :: Selector
itemArraySelector = mkSelector "itemArray"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @lastItem@
lastItemSelector :: Selector
lastItemSelector = mkSelector "lastItem"

-- | @Selector@ for @selectedItem@
selectedItemSelector :: Selector
selectedItemSelector = mkSelector "selectedItem"

-- | @Selector@ for @indexOfSelectedItem@
indexOfSelectedItemSelector :: Selector
indexOfSelectedItemSelector = mkSelector "indexOfSelectedItem"

-- | @Selector@ for @itemTitles@
itemTitlesSelector :: Selector
itemTitlesSelector = mkSelector "itemTitles"

-- | @Selector@ for @titleOfSelectedItem@
titleOfSelectedItemSelector :: Selector
titleOfSelectedItemSelector = mkSelector "titleOfSelectedItem"

-- | @Selector@ for @arrowPosition@
arrowPositionSelector :: Selector
arrowPositionSelector = mkSelector "arrowPosition"

-- | @Selector@ for @setArrowPosition:@
setArrowPositionSelector :: Selector
setArrowPositionSelector = mkSelector "setArrowPosition:"

