{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPopUpButton@.
module ObjC.AppKit.NSPopUpButton
  ( NSPopUpButton
  , IsNSPopUpButton(..)
  , popUpButtonWithMenu_target_action
  , pullDownButtonWithTitle_menu
  , pullDownButtonWithImage_menu
  , pullDownButtonWithTitle_image_menu
  , initWithFrame_pullsDown
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
  , selectedTag
  , itemTitles
  , titleOfSelectedItem
  , popUpButtonWithMenu_target_actionSelector
  , pullDownButtonWithTitle_menuSelector
  , pullDownButtonWithImage_menuSelector
  , pullDownButtonWithTitle_image_menuSelector
  , initWithFrame_pullsDownSelector
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
  , selectedTagSelector
  , itemTitlesSelector
  , titleOfSelectedItemSelector

  -- * Enum types
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
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a standard pop-up button with a menu, target, and action.
--
-- @menu@ — A menu presented by the pop-up button, containing items that the user can choose between.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- If @menu@ is non-empty, the pop-up button uses the first item for its initial selection.
--
-- Returns: An initialized pop-up button object.
--
-- ObjC selector: @+ popUpButtonWithMenu:target:action:@
popUpButtonWithMenu_target_action :: IsNSMenu menu => menu -> RawId -> Selector -> IO (Id NSPopUpButton)
popUpButtonWithMenu_target_action menu target action =
  do
    cls' <- getRequiredClass "NSPopUpButton"
    withObjCPtr menu $ \raw_menu ->
      sendClassMsg cls' (mkSelector "popUpButtonWithMenu:target:action:") (retPtr retVoid) [argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Creates a standard pull-down button with a title and menu.
--
-- @title@ — The localized title string that is displayed on the button.
--
-- @menu@ — The pull-down menu to present when interacting with the button.
--
-- Pull-down buttons created using this method have the @usesItemFromMenu@ property set to @NO@.
--
-- Returns: An initialized pull-down button object.
--
-- ObjC selector: @+ pullDownButtonWithTitle:menu:@
pullDownButtonWithTitle_menu :: (IsNSString title, IsNSMenu menu) => title -> menu -> IO (Id NSPopUpButton)
pullDownButtonWithTitle_menu title menu =
  do
    cls' <- getRequiredClass "NSPopUpButton"
    withObjCPtr title $ \raw_title ->
      withObjCPtr menu $ \raw_menu ->
        sendClassMsg cls' (mkSelector "pullDownButtonWithTitle:menu:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_menu :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a standard pull-down button with an image and menu.
--
-- @image@ — The icon that is displayed on the button.
--
-- @menu@ — The pull-down menu to present when interacting with the button.
--
-- Pull-down buttons created using this method have the @usesItemFromMenu@ property set to @NO@.
--
-- Returns: An initialized pull-down button object.
--
-- ObjC selector: @+ pullDownButtonWithImage:menu:@
pullDownButtonWithImage_menu :: (IsNSImage image, IsNSMenu menu) => image -> menu -> IO (Id NSPopUpButton)
pullDownButtonWithImage_menu image menu =
  do
    cls' <- getRequiredClass "NSPopUpButton"
    withObjCPtr image $ \raw_image ->
      withObjCPtr menu $ \raw_menu ->
        sendClassMsg cls' (mkSelector "pullDownButtonWithImage:menu:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_menu :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a standard pull-down button with a title, image, and menu.
--
-- @title@ — The localized title string that is displayed on the button.
--
-- @image@ — The icon that is displayed on the button.
--
-- @menu@ — The pull-down menu to present when interacting with the button.
--
-- Pull-down buttons created using this method have the @usesItemFromMenu@ property set to @NO@.
--
-- Returns: An initialized pull-down button object.
--
-- ObjC selector: @+ pullDownButtonWithTitle:image:menu:@
pullDownButtonWithTitle_image_menu :: (IsNSString title, IsNSImage image, IsNSMenu menu) => title -> image -> menu -> IO (Id NSPopUpButton)
pullDownButtonWithTitle_image_menu title image menu =
  do
    cls' <- getRequiredClass "NSPopUpButton"
    withObjCPtr title $ \raw_title ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr menu $ \raw_menu ->
          sendClassMsg cls' (mkSelector "pullDownButtonWithTitle:image:menu:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_menu :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithFrame:pullsDown:@
initWithFrame_pullsDown :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> NSRect -> Bool -> IO (Id NSPopUpButton)
initWithFrame_pullsDown nsPopUpButton  buttonFrame flag =
  sendMsg nsPopUpButton (mkSelector "initWithFrame:pullsDown:") (retPtr retVoid) [argNSRect buttonFrame, argCULong (if flag then 1 else 0)] >>= ownedObject . castPtr

-- | @- addItemWithTitle:@
addItemWithTitle :: (IsNSPopUpButton nsPopUpButton, IsNSString title) => nsPopUpButton -> title -> IO ()
addItemWithTitle nsPopUpButton  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButton (mkSelector "addItemWithTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- addItemsWithTitles:@
addItemsWithTitles :: (IsNSPopUpButton nsPopUpButton, IsNSArray itemTitles) => nsPopUpButton -> itemTitles -> IO ()
addItemsWithTitles nsPopUpButton  itemTitles =
withObjCPtr itemTitles $ \raw_itemTitles ->
    sendMsg nsPopUpButton (mkSelector "addItemsWithTitles:") retVoid [argPtr (castPtr raw_itemTitles :: Ptr ())]

-- | @- insertItemWithTitle:atIndex:@
insertItemWithTitle_atIndex :: (IsNSPopUpButton nsPopUpButton, IsNSString title) => nsPopUpButton -> title -> CLong -> IO ()
insertItemWithTitle_atIndex nsPopUpButton  title index =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButton (mkSelector "insertItemWithTitle:atIndex:") retVoid [argPtr (castPtr raw_title :: Ptr ()), argCLong (fromIntegral index)]

-- | @- removeItemWithTitle:@
removeItemWithTitle :: (IsNSPopUpButton nsPopUpButton, IsNSString title) => nsPopUpButton -> title -> IO ()
removeItemWithTitle nsPopUpButton  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButton (mkSelector "removeItemWithTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> CLong -> IO ()
removeItemAtIndex nsPopUpButton  index =
  sendMsg nsPopUpButton (mkSelector "removeItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- removeAllItems@
removeAllItems :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO ()
removeAllItems nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "removeAllItems") retVoid []

-- | @- indexOfItem:@
indexOfItem :: (IsNSPopUpButton nsPopUpButton, IsNSMenuItem item) => nsPopUpButton -> item -> IO CLong
indexOfItem nsPopUpButton  item =
withObjCPtr item $ \raw_item ->
    sendMsg nsPopUpButton (mkSelector "indexOfItem:") retCLong [argPtr (castPtr raw_item :: Ptr ())]

-- | @- indexOfItemWithTitle:@
indexOfItemWithTitle :: (IsNSPopUpButton nsPopUpButton, IsNSString title) => nsPopUpButton -> title -> IO CLong
indexOfItemWithTitle nsPopUpButton  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButton (mkSelector "indexOfItemWithTitle:") retCLong [argPtr (castPtr raw_title :: Ptr ())]

-- | @- indexOfItemWithTag:@
indexOfItemWithTag :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> CLong -> IO CLong
indexOfItemWithTag nsPopUpButton  tag =
  sendMsg nsPopUpButton (mkSelector "indexOfItemWithTag:") retCLong [argCLong (fromIntegral tag)]

-- | @- indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObject :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> RawId -> IO CLong
indexOfItemWithRepresentedObject nsPopUpButton  obj_ =
  sendMsg nsPopUpButton (mkSelector "indexOfItemWithRepresentedObject:") retCLong [argPtr (castPtr (unRawId obj_) :: Ptr ())]

-- | @- indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andAction :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> RawId -> Selector -> IO CLong
indexOfItemWithTarget_andAction nsPopUpButton  target actionSelector =
  sendMsg nsPopUpButton (mkSelector "indexOfItemWithTarget:andAction:") retCLong [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector actionSelector)]

-- | @- itemAtIndex:@
itemAtIndex :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> CLong -> IO (Id NSMenuItem)
itemAtIndex nsPopUpButton  index =
  sendMsg nsPopUpButton (mkSelector "itemAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | @- itemWithTitle:@
itemWithTitle :: (IsNSPopUpButton nsPopUpButton, IsNSString title) => nsPopUpButton -> title -> IO (Id NSMenuItem)
itemWithTitle nsPopUpButton  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButton (mkSelector "itemWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- selectItem:@
selectItem :: (IsNSPopUpButton nsPopUpButton, IsNSMenuItem item) => nsPopUpButton -> item -> IO ()
selectItem nsPopUpButton  item =
withObjCPtr item $ \raw_item ->
    sendMsg nsPopUpButton (mkSelector "selectItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | @- selectItemAtIndex:@
selectItemAtIndex :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> CLong -> IO ()
selectItemAtIndex nsPopUpButton  index =
  sendMsg nsPopUpButton (mkSelector "selectItemAtIndex:") retVoid [argCLong (fromIntegral index)]

-- | @- selectItemWithTitle:@
selectItemWithTitle :: (IsNSPopUpButton nsPopUpButton, IsNSString title) => nsPopUpButton -> title -> IO ()
selectItemWithTitle nsPopUpButton  title =
withObjCPtr title $ \raw_title ->
    sendMsg nsPopUpButton (mkSelector "selectItemWithTitle:") retVoid [argPtr (castPtr raw_title :: Ptr ())]

-- | @- selectItemWithTag:@
selectItemWithTag :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> CLong -> IO Bool
selectItemWithTag nsPopUpButton  tag =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButton (mkSelector "selectItemWithTag:") retCULong [argCLong (fromIntegral tag)]

-- | @- setTitle:@
setTitle :: (IsNSPopUpButton nsPopUpButton, IsNSString string) => nsPopUpButton -> string -> IO ()
setTitle nsPopUpButton  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsPopUpButton (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- synchronizeTitleAndSelectedItem@
synchronizeTitleAndSelectedItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO ()
synchronizeTitleAndSelectedItem nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "synchronizeTitleAndSelectedItem") retVoid []

-- | @- itemTitleAtIndex:@
itemTitleAtIndex :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> CLong -> IO (Id NSString)
itemTitleAtIndex nsPopUpButton  index =
  sendMsg nsPopUpButton (mkSelector "itemTitleAtIndex:") (retPtr retVoid) [argCLong (fromIntegral index)] >>= retainedObject . castPtr

-- | The menu that is presented by the popup button. This overrides the inherited NSView property and replaces NSView's standard context menu behavior.
--
-- ObjC selector: @- menu@
menu :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO (Id NSMenu)
menu nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The menu that is presented by the popup button. This overrides the inherited NSView property and replaces NSView's standard context menu behavior.
--
-- ObjC selector: @- setMenu:@
setMenu :: (IsNSPopUpButton nsPopUpButton, IsNSMenu value) => nsPopUpButton -> value -> IO ()
setMenu nsPopUpButton  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPopUpButton (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | When the value of this property is @YES@ the button adopts 'pull-down' behavior, displaying static button contents and presenting its menu at the edge of the button. When the value of this property is @NO@ the button behaves as a popup, displaying the currently-selected menu item and presenting its menu above the button, positioning the selected menu item to match the button's contents.
--
-- ObjC selector: @- pullsDown@
pullsDown :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO Bool
pullsDown nsPopUpButton  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButton (mkSelector "pullsDown") retCULong []

-- | When the value of this property is @YES@ the button adopts 'pull-down' behavior, displaying static button contents and presenting its menu at the edge of the button. When the value of this property is @NO@ the button behaves as a popup, displaying the currently-selected menu item and presenting its menu above the button, positioning the selected menu item to match the button's contents.
--
-- ObjC selector: @- setPullsDown:@
setPullsDown :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> Bool -> IO ()
setPullsDown nsPopUpButton  value =
  sendMsg nsPopUpButton (mkSelector "setPullsDown:") retVoid [argCULong (if value then 1 else 0)]

-- | When the value of this property is @YES@, the popup button automatically enables and disables its menu items according to the @NSMenuValidation@ protocol prior to user interaction.
--
-- ObjC selector: @- autoenablesItems@
autoenablesItems :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO Bool
autoenablesItems nsPopUpButton  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButton (mkSelector "autoenablesItems") retCULong []

-- | When the value of this property is @YES@, the popup button automatically enables and disables its menu items according to the @NSMenuValidation@ protocol prior to user interaction.
--
-- ObjC selector: @- setAutoenablesItems:@
setAutoenablesItems :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> Bool -> IO ()
setAutoenablesItems nsPopUpButton  value =
  sendMsg nsPopUpButton (mkSelector "setAutoenablesItems:") retVoid [argCULong (if value then 1 else 0)]

-- | For pull-down buttons and for popups under severe screen position restrictions, this property specifies the edge of the control that the menu should present from.
--
-- ObjC selector: @- preferredEdge@
preferredEdge :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO NSRectEdge
preferredEdge nsPopUpButton  =
  fmap (coerce :: CULong -> NSRectEdge) $ sendMsg nsPopUpButton (mkSelector "preferredEdge") retCULong []

-- | For pull-down buttons and for popups under severe screen position restrictions, this property specifies the edge of the control that the menu should present from.
--
-- ObjC selector: @- setPreferredEdge:@
setPreferredEdge :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> NSRectEdge -> IO ()
setPreferredEdge nsPopUpButton  value =
  sendMsg nsPopUpButton (mkSelector "setPreferredEdge:") retVoid [argCULong (coerce value)]

-- | When @usesItemFromMenu@ is @YES@, a pull-down button uses the title of the first menu item and hides the first menu item. A pop-up button uses the title of the currently selected menu. The default value is @YES@.
--
-- ObjC selector: @- usesItemFromMenu@
usesItemFromMenu :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO Bool
usesItemFromMenu nsPopUpButton  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButton (mkSelector "usesItemFromMenu") retCULong []

-- | When @usesItemFromMenu@ is @YES@, a pull-down button uses the title of the first menu item and hides the first menu item. A pop-up button uses the title of the currently selected menu. The default value is @YES@.
--
-- ObjC selector: @- setUsesItemFromMenu:@
setUsesItemFromMenu :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> Bool -> IO ()
setUsesItemFromMenu nsPopUpButton  value =
  sendMsg nsPopUpButton (mkSelector "setUsesItemFromMenu:") retVoid [argCULong (if value then 1 else 0)]

-- | When the value of this property is @YES@, the selected menu item's @state@ is set to @NSControlStateValueOn@. When the value of this property is @NO@, the menu item's @state@ is not changed. When this property changes, the @state@ of the currently selected item is updated appropriately. This property is ignored for pull-down buttons.
--
-- ObjC selector: @- altersStateOfSelectedItem@
altersStateOfSelectedItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO Bool
altersStateOfSelectedItem nsPopUpButton  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPopUpButton (mkSelector "altersStateOfSelectedItem") retCULong []

-- | When the value of this property is @YES@, the selected menu item's @state@ is set to @NSControlStateValueOn@. When the value of this property is @NO@, the menu item's @state@ is not changed. When this property changes, the @state@ of the currently selected item is updated appropriately. This property is ignored for pull-down buttons.
--
-- ObjC selector: @- setAltersStateOfSelectedItem:@
setAltersStateOfSelectedItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> Bool -> IO ()
setAltersStateOfSelectedItem nsPopUpButton  value =
  sendMsg nsPopUpButton (mkSelector "setAltersStateOfSelectedItem:") retVoid [argCULong (if value then 1 else 0)]

-- | @- itemArray@
itemArray :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO (Id NSArray)
itemArray nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "itemArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfItems@
numberOfItems :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO CLong
numberOfItems nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "numberOfItems") retCLong []

-- | @- lastItem@
lastItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO (Id NSMenuItem)
lastItem nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "lastItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- selectedItem@
selectedItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO (Id NSMenuItem)
selectedItem nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "selectedItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- indexOfSelectedItem@
indexOfSelectedItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO CLong
indexOfSelectedItem nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "indexOfSelectedItem") retCLong []

-- | @- selectedTag@
selectedTag :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO CLong
selectedTag nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "selectedTag") retCLong []

-- | @- itemTitles@
itemTitles :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO (Id NSArray)
itemTitles nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "itemTitles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- titleOfSelectedItem@
titleOfSelectedItem :: IsNSPopUpButton nsPopUpButton => nsPopUpButton -> IO (Id NSString)
titleOfSelectedItem nsPopUpButton  =
  sendMsg nsPopUpButton (mkSelector "titleOfSelectedItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @popUpButtonWithMenu:target:action:@
popUpButtonWithMenu_target_actionSelector :: Selector
popUpButtonWithMenu_target_actionSelector = mkSelector "popUpButtonWithMenu:target:action:"

-- | @Selector@ for @pullDownButtonWithTitle:menu:@
pullDownButtonWithTitle_menuSelector :: Selector
pullDownButtonWithTitle_menuSelector = mkSelector "pullDownButtonWithTitle:menu:"

-- | @Selector@ for @pullDownButtonWithImage:menu:@
pullDownButtonWithImage_menuSelector :: Selector
pullDownButtonWithImage_menuSelector = mkSelector "pullDownButtonWithImage:menu:"

-- | @Selector@ for @pullDownButtonWithTitle:image:menu:@
pullDownButtonWithTitle_image_menuSelector :: Selector
pullDownButtonWithTitle_image_menuSelector = mkSelector "pullDownButtonWithTitle:image:menu:"

-- | @Selector@ for @initWithFrame:pullsDown:@
initWithFrame_pullsDownSelector :: Selector
initWithFrame_pullsDownSelector = mkSelector "initWithFrame:pullsDown:"

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

-- | @Selector@ for @selectedTag@
selectedTagSelector :: Selector
selectedTagSelector = mkSelector "selectedTag"

-- | @Selector@ for @itemTitles@
itemTitlesSelector :: Selector
itemTitlesSelector = mkSelector "itemTitles"

-- | @Selector@ for @titleOfSelectedItem@
titleOfSelectedItemSelector :: Selector
titleOfSelectedItemSelector = mkSelector "titleOfSelectedItem"

