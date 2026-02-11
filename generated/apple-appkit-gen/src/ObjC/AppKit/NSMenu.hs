{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenu@.
module ObjC.AppKit.NSMenu
  ( NSMenu
  , IsNSMenu(..)
  , initWithTitle
  , initWithCoder
  , popUpContextMenu_withEvent_forView
  , popUpContextMenu_withEvent_forView_withFont
  , popUpMenuPositioningItem_atLocation_inView
  , setMenuBarVisible
  , menuBarVisible
  , insertItem_atIndex
  , addItem
  , insertItemWithTitle_action_keyEquivalent_atIndex
  , addItemWithTitle_action_keyEquivalent
  , removeItemAtIndex
  , removeItem
  , setSubmenu_forItem
  , removeAllItems
  , itemAtIndex
  , indexOfItem
  , indexOfItemWithTitle
  , indexOfItemWithTag
  , indexOfItemWithRepresentedObject
  , indexOfItemWithSubmenu
  , indexOfItemWithTarget_andAction
  , itemWithTitle
  , itemWithTag
  , update
  , performKeyEquivalent
  , itemChanged
  , performActionForItemAtIndex
  , cancelTracking
  , cancelTrackingWithoutAnimation
  , setMenuRepresentation
  , menuRepresentation
  , setContextMenuRepresentation
  , contextMenuRepresentation
  , setTearOffMenuRepresentation
  , tearOffMenuRepresentation
  , menuZone
  , setMenuZone
  , attachedMenu
  , isAttached
  , sizeToFit
  , locationForSubmenu
  , helpRequested
  , submenuAction
  , paletteMenuWithColors_titles_selectionHandler
  , paletteMenuWithColors_titles_templateImage_selectionHandler
  , title
  , setTitle
  , supermenu
  , setSupermenu
  , itemArray
  , setItemArray
  , numberOfItems
  , autoenablesItems
  , setAutoenablesItems
  , delegate
  , setDelegate
  , menuBarHeight
  , highlightedItem
  , minimumWidth
  , setMinimumWidth
  , size
  , font
  , setFont
  , allowsContextMenuPlugIns
  , setAllowsContextMenuPlugIns
  , automaticallyInsertsWritingToolsItems
  , setAutomaticallyInsertsWritingToolsItems
  , showsStateColumn
  , setShowsStateColumn
  , userInterfaceLayoutDirection
  , setUserInterfaceLayoutDirection
  , menuChangedMessagesEnabled
  , setMenuChangedMessagesEnabled
  , tornOff
  , propertiesToUpdate
  , presentationStyle
  , setPresentationStyle
  , selectionMode
  , setSelectionMode
  , selectedItems
  , setSelectedItems
  , initWithTitleSelector
  , initWithCoderSelector
  , popUpContextMenu_withEvent_forViewSelector
  , popUpContextMenu_withEvent_forView_withFontSelector
  , popUpMenuPositioningItem_atLocation_inViewSelector
  , setMenuBarVisibleSelector
  , menuBarVisibleSelector
  , insertItem_atIndexSelector
  , addItemSelector
  , insertItemWithTitle_action_keyEquivalent_atIndexSelector
  , addItemWithTitle_action_keyEquivalentSelector
  , removeItemAtIndexSelector
  , removeItemSelector
  , setSubmenu_forItemSelector
  , removeAllItemsSelector
  , itemAtIndexSelector
  , indexOfItemSelector
  , indexOfItemWithTitleSelector
  , indexOfItemWithTagSelector
  , indexOfItemWithRepresentedObjectSelector
  , indexOfItemWithSubmenuSelector
  , indexOfItemWithTarget_andActionSelector
  , itemWithTitleSelector
  , itemWithTagSelector
  , updateSelector
  , performKeyEquivalentSelector
  , itemChangedSelector
  , performActionForItemAtIndexSelector
  , cancelTrackingSelector
  , cancelTrackingWithoutAnimationSelector
  , setMenuRepresentationSelector
  , menuRepresentationSelector
  , setContextMenuRepresentationSelector
  , contextMenuRepresentationSelector
  , setTearOffMenuRepresentationSelector
  , tearOffMenuRepresentationSelector
  , menuZoneSelector
  , setMenuZoneSelector
  , attachedMenuSelector
  , isAttachedSelector
  , sizeToFitSelector
  , locationForSubmenuSelector
  , helpRequestedSelector
  , submenuActionSelector
  , paletteMenuWithColors_titles_selectionHandlerSelector
  , paletteMenuWithColors_titles_templateImage_selectionHandlerSelector
  , titleSelector
  , setTitleSelector
  , supermenuSelector
  , setSupermenuSelector
  , itemArraySelector
  , setItemArraySelector
  , numberOfItemsSelector
  , autoenablesItemsSelector
  , setAutoenablesItemsSelector
  , delegateSelector
  , setDelegateSelector
  , menuBarHeightSelector
  , highlightedItemSelector
  , minimumWidthSelector
  , setMinimumWidthSelector
  , sizeSelector
  , fontSelector
  , setFontSelector
  , allowsContextMenuPlugInsSelector
  , setAllowsContextMenuPlugInsSelector
  , automaticallyInsertsWritingToolsItemsSelector
  , setAutomaticallyInsertsWritingToolsItemsSelector
  , showsStateColumnSelector
  , setShowsStateColumnSelector
  , userInterfaceLayoutDirectionSelector
  , setUserInterfaceLayoutDirectionSelector
  , menuChangedMessagesEnabledSelector
  , setMenuChangedMessagesEnabledSelector
  , tornOffSelector
  , propertiesToUpdateSelector
  , presentationStyleSelector
  , setPresentationStyleSelector
  , selectionModeSelector
  , setSelectionModeSelector
  , selectedItemsSelector
  , setSelectedItemsSelector

  -- * Enum types
  , NSMenuPresentationStyle(NSMenuPresentationStyle)
  , pattern NSMenuPresentationStyleRegular
  , pattern NSMenuPresentationStylePalette
  , NSMenuProperties(NSMenuProperties)
  , pattern NSMenuPropertyItemTitle
  , pattern NSMenuPropertyItemAttributedTitle
  , pattern NSMenuPropertyItemKeyEquivalent
  , pattern NSMenuPropertyItemImage
  , pattern NSMenuPropertyItemEnabled
  , pattern NSMenuPropertyItemAccessibilityDescription
  , NSMenuSelectionMode(NSMenuSelectionMode)
  , pattern NSMenuSelectionModeAutomatic
  , pattern NSMenuSelectionModeSelectOne
  , pattern NSMenuSelectionModeSelectAny
  , NSUserInterfaceLayoutDirection(NSUserInterfaceLayoutDirection)
  , pattern NSUserInterfaceLayoutDirectionLeftToRight
  , pattern NSUserInterfaceLayoutDirectionRightToLeft

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
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:@
initWithTitle :: (IsNSMenu nsMenu, IsNSString title) => nsMenu -> title -> IO (Id NSMenu)
initWithTitle nsMenu  title =
  withObjCPtr title $ \raw_title ->
      sendMsg nsMenu (mkSelector "initWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSMenu nsMenu, IsNSCoder coder) => nsMenu -> coder -> IO (Id NSMenu)
initWithCoder nsMenu  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsMenu (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ popUpContextMenu:withEvent:forView:@
popUpContextMenu_withEvent_forView :: (IsNSMenu menu, IsNSEvent event, IsNSView view) => menu -> event -> view -> IO ()
popUpContextMenu_withEvent_forView menu event view =
  do
    cls' <- getRequiredClass "NSMenu"
    withObjCPtr menu $ \raw_menu ->
      withObjCPtr event $ \raw_event ->
        withObjCPtr view $ \raw_view ->
          sendClassMsg cls' (mkSelector "popUpContextMenu:withEvent:forView:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_view :: Ptr ())]

-- | @+ popUpContextMenu:withEvent:forView:withFont:@
popUpContextMenu_withEvent_forView_withFont :: (IsNSMenu menu, IsNSEvent event, IsNSView view, IsNSFont font) => menu -> event -> view -> font -> IO ()
popUpContextMenu_withEvent_forView_withFont menu event view font =
  do
    cls' <- getRequiredClass "NSMenu"
    withObjCPtr menu $ \raw_menu ->
      withObjCPtr event $ \raw_event ->
        withObjCPtr view $ \raw_view ->
          withObjCPtr font $ \raw_font ->
            sendClassMsg cls' (mkSelector "popUpContextMenu:withEvent:forView:withFont:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr raw_event :: Ptr ()), argPtr (castPtr raw_view :: Ptr ()), argPtr (castPtr raw_font :: Ptr ())]

-- | @- popUpMenuPositioningItem:atLocation:inView:@
popUpMenuPositioningItem_atLocation_inView :: (IsNSMenu nsMenu, IsNSMenuItem item, IsNSView view) => nsMenu -> item -> NSPoint -> view -> IO Bool
popUpMenuPositioningItem_atLocation_inView nsMenu  item location view =
  withObjCPtr item $ \raw_item ->
    withObjCPtr view $ \raw_view ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "popUpMenuPositioningItem:atLocation:inView:") retCULong [argPtr (castPtr raw_item :: Ptr ()), argNSPoint location, argPtr (castPtr raw_view :: Ptr ())]

-- | @+ setMenuBarVisible:@
setMenuBarVisible :: Bool -> IO ()
setMenuBarVisible visible =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMsg cls' (mkSelector "setMenuBarVisible:") retVoid [argCULong (if visible then 1 else 0)]

-- | @+ menuBarVisible@
menuBarVisible :: IO Bool
menuBarVisible  =
  do
    cls' <- getRequiredClass "NSMenu"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "menuBarVisible") retCULong []

-- | @- insertItem:atIndex:@
insertItem_atIndex :: (IsNSMenu nsMenu, IsNSMenuItem newItem) => nsMenu -> newItem -> CLong -> IO ()
insertItem_atIndex nsMenu  newItem index =
  withObjCPtr newItem $ \raw_newItem ->
      sendMsg nsMenu (mkSelector "insertItem:atIndex:") retVoid [argPtr (castPtr raw_newItem :: Ptr ()), argCLong index]

-- | @- addItem:@
addItem :: (IsNSMenu nsMenu, IsNSMenuItem newItem) => nsMenu -> newItem -> IO ()
addItem nsMenu  newItem =
  withObjCPtr newItem $ \raw_newItem ->
      sendMsg nsMenu (mkSelector "addItem:") retVoid [argPtr (castPtr raw_newItem :: Ptr ())]

-- | @- insertItemWithTitle:action:keyEquivalent:atIndex:@
insertItemWithTitle_action_keyEquivalent_atIndex :: (IsNSMenu nsMenu, IsNSString string, IsNSString charCode) => nsMenu -> string -> Selector -> charCode -> CLong -> IO (Id NSMenuItem)
insertItemWithTitle_action_keyEquivalent_atIndex nsMenu  string selector charCode index =
  withObjCPtr string $ \raw_string ->
    withObjCPtr charCode $ \raw_charCode ->
        sendMsg nsMenu (mkSelector "insertItemWithTitle:action:keyEquivalent:atIndex:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (unSelector selector), argPtr (castPtr raw_charCode :: Ptr ()), argCLong index] >>= retainedObject . castPtr

-- | @- addItemWithTitle:action:keyEquivalent:@
addItemWithTitle_action_keyEquivalent :: (IsNSMenu nsMenu, IsNSString string, IsNSString charCode) => nsMenu -> string -> Selector -> charCode -> IO (Id NSMenuItem)
addItemWithTitle_action_keyEquivalent nsMenu  string selector charCode =
  withObjCPtr string $ \raw_string ->
    withObjCPtr charCode $ \raw_charCode ->
        sendMsg nsMenu (mkSelector "addItemWithTitle:action:keyEquivalent:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (unSelector selector), argPtr (castPtr raw_charCode :: Ptr ())] >>= retainedObject . castPtr

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSMenu nsMenu => nsMenu -> CLong -> IO ()
removeItemAtIndex nsMenu  index =
    sendMsg nsMenu (mkSelector "removeItemAtIndex:") retVoid [argCLong index]

-- | @- removeItem:@
removeItem :: (IsNSMenu nsMenu, IsNSMenuItem item) => nsMenu -> item -> IO ()
removeItem nsMenu  item =
  withObjCPtr item $ \raw_item ->
      sendMsg nsMenu (mkSelector "removeItem:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | @- setSubmenu:forItem:@
setSubmenu_forItem :: (IsNSMenu nsMenu, IsNSMenu menu, IsNSMenuItem item) => nsMenu -> menu -> item -> IO ()
setSubmenu_forItem nsMenu  menu item =
  withObjCPtr menu $ \raw_menu ->
    withObjCPtr item $ \raw_item ->
        sendMsg nsMenu (mkSelector "setSubmenu:forItem:") retVoid [argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr raw_item :: Ptr ())]

-- | @- removeAllItems@
removeAllItems :: IsNSMenu nsMenu => nsMenu -> IO ()
removeAllItems nsMenu  =
    sendMsg nsMenu (mkSelector "removeAllItems") retVoid []

-- | @- itemAtIndex:@
itemAtIndex :: IsNSMenu nsMenu => nsMenu -> CLong -> IO (Id NSMenuItem)
itemAtIndex nsMenu  index =
    sendMsg nsMenu (mkSelector "itemAtIndex:") (retPtr retVoid) [argCLong index] >>= retainedObject . castPtr

-- | @- indexOfItem:@
indexOfItem :: (IsNSMenu nsMenu, IsNSMenuItem item) => nsMenu -> item -> IO CLong
indexOfItem nsMenu  item =
  withObjCPtr item $ \raw_item ->
      sendMsg nsMenu (mkSelector "indexOfItem:") retCLong [argPtr (castPtr raw_item :: Ptr ())]

-- | @- indexOfItemWithTitle:@
indexOfItemWithTitle :: (IsNSMenu nsMenu, IsNSString title) => nsMenu -> title -> IO CLong
indexOfItemWithTitle nsMenu  title =
  withObjCPtr title $ \raw_title ->
      sendMsg nsMenu (mkSelector "indexOfItemWithTitle:") retCLong [argPtr (castPtr raw_title :: Ptr ())]

-- | @- indexOfItemWithTag:@
indexOfItemWithTag :: IsNSMenu nsMenu => nsMenu -> CLong -> IO CLong
indexOfItemWithTag nsMenu  tag =
    sendMsg nsMenu (mkSelector "indexOfItemWithTag:") retCLong [argCLong tag]

-- | @- indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObject :: IsNSMenu nsMenu => nsMenu -> RawId -> IO CLong
indexOfItemWithRepresentedObject nsMenu  object =
    sendMsg nsMenu (mkSelector "indexOfItemWithRepresentedObject:") retCLong [argPtr (castPtr (unRawId object) :: Ptr ())]

-- | @- indexOfItemWithSubmenu:@
indexOfItemWithSubmenu :: (IsNSMenu nsMenu, IsNSMenu submenu) => nsMenu -> submenu -> IO CLong
indexOfItemWithSubmenu nsMenu  submenu =
  withObjCPtr submenu $ \raw_submenu ->
      sendMsg nsMenu (mkSelector "indexOfItemWithSubmenu:") retCLong [argPtr (castPtr raw_submenu :: Ptr ())]

-- | @- indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andAction :: IsNSMenu nsMenu => nsMenu -> RawId -> Selector -> IO CLong
indexOfItemWithTarget_andAction nsMenu  target actionSelector =
    sendMsg nsMenu (mkSelector "indexOfItemWithTarget:andAction:") retCLong [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector actionSelector)]

-- | @- itemWithTitle:@
itemWithTitle :: (IsNSMenu nsMenu, IsNSString title) => nsMenu -> title -> IO (Id NSMenuItem)
itemWithTitle nsMenu  title =
  withObjCPtr title $ \raw_title ->
      sendMsg nsMenu (mkSelector "itemWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- itemWithTag:@
itemWithTag :: IsNSMenu nsMenu => nsMenu -> CLong -> IO (Id NSMenuItem)
itemWithTag nsMenu  tag =
    sendMsg nsMenu (mkSelector "itemWithTag:") (retPtr retVoid) [argCLong tag] >>= retainedObject . castPtr

-- | @- update@
update :: IsNSMenu nsMenu => nsMenu -> IO ()
update nsMenu  =
    sendMsg nsMenu (mkSelector "update") retVoid []

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSMenu nsMenu, IsNSEvent event) => nsMenu -> event -> IO Bool
performKeyEquivalent nsMenu  event =
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "performKeyEquivalent:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- itemChanged:@
itemChanged :: (IsNSMenu nsMenu, IsNSMenuItem item) => nsMenu -> item -> IO ()
itemChanged nsMenu  item =
  withObjCPtr item $ \raw_item ->
      sendMsg nsMenu (mkSelector "itemChanged:") retVoid [argPtr (castPtr raw_item :: Ptr ())]

-- | @- performActionForItemAtIndex:@
performActionForItemAtIndex :: IsNSMenu nsMenu => nsMenu -> CLong -> IO ()
performActionForItemAtIndex nsMenu  index =
    sendMsg nsMenu (mkSelector "performActionForItemAtIndex:") retVoid [argCLong index]

-- | @- cancelTracking@
cancelTracking :: IsNSMenu nsMenu => nsMenu -> IO ()
cancelTracking nsMenu  =
    sendMsg nsMenu (mkSelector "cancelTracking") retVoid []

-- | @- cancelTrackingWithoutAnimation@
cancelTrackingWithoutAnimation :: IsNSMenu nsMenu => nsMenu -> IO ()
cancelTrackingWithoutAnimation nsMenu  =
    sendMsg nsMenu (mkSelector "cancelTrackingWithoutAnimation") retVoid []

-- | @- setMenuRepresentation:@
setMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setMenuRepresentation nsMenu  menuRep =
    sendMsg nsMenu (mkSelector "setMenuRepresentation:") retVoid [argPtr (castPtr (unRawId menuRep) :: Ptr ())]

-- | @- menuRepresentation@
menuRepresentation :: IsNSMenu nsMenu => nsMenu -> IO RawId
menuRepresentation nsMenu  =
    fmap (RawId . castPtr) $ sendMsg nsMenu (mkSelector "menuRepresentation") (retPtr retVoid) []

-- | @- setContextMenuRepresentation:@
setContextMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setContextMenuRepresentation nsMenu  menuRep =
    sendMsg nsMenu (mkSelector "setContextMenuRepresentation:") retVoid [argPtr (castPtr (unRawId menuRep) :: Ptr ())]

-- | @- contextMenuRepresentation@
contextMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> IO RawId
contextMenuRepresentation nsMenu  =
    fmap (RawId . castPtr) $ sendMsg nsMenu (mkSelector "contextMenuRepresentation") (retPtr retVoid) []

-- | @- setTearOffMenuRepresentation:@
setTearOffMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setTearOffMenuRepresentation nsMenu  menuRep =
    sendMsg nsMenu (mkSelector "setTearOffMenuRepresentation:") retVoid [argPtr (castPtr (unRawId menuRep) :: Ptr ())]

-- | @- tearOffMenuRepresentation@
tearOffMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> IO RawId
tearOffMenuRepresentation nsMenu  =
    fmap (RawId . castPtr) $ sendMsg nsMenu (mkSelector "tearOffMenuRepresentation") (retPtr retVoid) []

-- | @+ menuZone@
menuZone :: IO (Ptr ())
menuZone  =
  do
    cls' <- getRequiredClass "NSMenu"
    fmap castPtr $ sendClassMsg cls' (mkSelector "menuZone") (retPtr retVoid) []

-- | @+ setMenuZone:@
setMenuZone :: Ptr () -> IO ()
setMenuZone zone =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMsg cls' (mkSelector "setMenuZone:") retVoid [argPtr zone]

-- | @- attachedMenu@
attachedMenu :: IsNSMenu nsMenu => nsMenu -> IO (Id NSMenu)
attachedMenu nsMenu  =
    sendMsg nsMenu (mkSelector "attachedMenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- isAttached@
isAttached :: IsNSMenu nsMenu => nsMenu -> IO Bool
isAttached nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "isAttached") retCULong []

-- | @- sizeToFit@
sizeToFit :: IsNSMenu nsMenu => nsMenu -> IO ()
sizeToFit nsMenu  =
    sendMsg nsMenu (mkSelector "sizeToFit") retVoid []

-- | @- locationForSubmenu:@
locationForSubmenu :: (IsNSMenu nsMenu, IsNSMenu submenu) => nsMenu -> submenu -> IO NSPoint
locationForSubmenu nsMenu  submenu =
  withObjCPtr submenu $ \raw_submenu ->
      sendMsgStret nsMenu (mkSelector "locationForSubmenu:") retNSPoint [argPtr (castPtr raw_submenu :: Ptr ())]

-- | @- helpRequested:@
helpRequested :: (IsNSMenu nsMenu, IsNSEvent eventPtr) => nsMenu -> eventPtr -> IO ()
helpRequested nsMenu  eventPtr =
  withObjCPtr eventPtr $ \raw_eventPtr ->
      sendMsg nsMenu (mkSelector "helpRequested:") retVoid [argPtr (castPtr raw_eventPtr :: Ptr ())]

-- | @- submenuAction:@
submenuAction :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
submenuAction nsMenu  sender =
    sendMsg nsMenu (mkSelector "submenuAction:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Creates a palette menu displaying user-selectable color tags using the provided array of colors and optional titles.
--
-- Note that the palette menu is configured for display as an inline menu; you must set it as the submenu of another menu item, contained in a standard menu. The palette menu cannot be used to invoke the @popUpMenuPositioningItem@ method, or attached directly to a popup button or toolbar item.
--
-- Returns: An autoconfigured palette menu.
--
-- ObjC selector: @+ paletteMenuWithColors:titles:selectionHandler:@
paletteMenuWithColors_titles_selectionHandler :: (IsNSArray colors, IsNSArray itemTitles) => colors -> itemTitles -> Ptr () -> IO (Id NSMenu)
paletteMenuWithColors_titles_selectionHandler colors itemTitles onSelectionChange =
  do
    cls' <- getRequiredClass "NSMenu"
    withObjCPtr colors $ \raw_colors ->
      withObjCPtr itemTitles $ \raw_itemTitles ->
        sendClassMsg cls' (mkSelector "paletteMenuWithColors:titles:selectionHandler:") (retPtr retVoid) [argPtr (castPtr raw_colors :: Ptr ()), argPtr (castPtr raw_itemTitles :: Ptr ()), argPtr (castPtr onSelectionChange :: Ptr ())] >>= retainedObject . castPtr

-- | Creates a palette menu displaying user-selectable color tags using the provided template image, tinted using the specified array of colors.
--
-- Optionally allows observing changes to the selection state in the compact menu. The block is invoked after the selection has been updated. Currently selected items can be retrieved from the @selectedItems@ property.
--
-- Note that the palette menu is configured for display as an inline menu; you must set it as the submenu of another menu item, contained in a standard menu. The palette menu cannot be used to invoke the @popUpMenuPositioningItem@ method, or attached directly to a popup button or toolbar item.
--
-- Returns: An autoconfigured palette menu.
--
-- ObjC selector: @+ paletteMenuWithColors:titles:templateImage:selectionHandler:@
paletteMenuWithColors_titles_templateImage_selectionHandler :: (IsNSArray colors, IsNSArray itemTitles, IsNSImage image) => colors -> itemTitles -> image -> Ptr () -> IO (Id NSMenu)
paletteMenuWithColors_titles_templateImage_selectionHandler colors itemTitles image onSelectionChange =
  do
    cls' <- getRequiredClass "NSMenu"
    withObjCPtr colors $ \raw_colors ->
      withObjCPtr itemTitles $ \raw_itemTitles ->
        withObjCPtr image $ \raw_image ->
          sendClassMsg cls' (mkSelector "paletteMenuWithColors:titles:templateImage:selectionHandler:") (retPtr retVoid) [argPtr (castPtr raw_colors :: Ptr ()), argPtr (castPtr raw_itemTitles :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr onSelectionChange :: Ptr ())] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSMenu nsMenu => nsMenu -> IO (Id NSString)
title nsMenu  =
    sendMsg nsMenu (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSMenu nsMenu, IsNSString value) => nsMenu -> value -> IO ()
setTitle nsMenu  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenu (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- supermenu@
supermenu :: IsNSMenu nsMenu => nsMenu -> IO (Id NSMenu)
supermenu nsMenu  =
    sendMsg nsMenu (mkSelector "supermenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSupermenu:@
setSupermenu :: (IsNSMenu nsMenu, IsNSMenu value) => nsMenu -> value -> IO ()
setSupermenu nsMenu  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenu (mkSelector "setSupermenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- itemArray@
itemArray :: IsNSMenu nsMenu => nsMenu -> IO (Id NSArray)
itemArray nsMenu  =
    sendMsg nsMenu (mkSelector "itemArray") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setItemArray:@
setItemArray :: (IsNSMenu nsMenu, IsNSArray value) => nsMenu -> value -> IO ()
setItemArray nsMenu  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenu (mkSelector "setItemArray:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- numberOfItems@
numberOfItems :: IsNSMenu nsMenu => nsMenu -> IO CLong
numberOfItems nsMenu  =
    sendMsg nsMenu (mkSelector "numberOfItems") retCLong []

-- | @- autoenablesItems@
autoenablesItems :: IsNSMenu nsMenu => nsMenu -> IO Bool
autoenablesItems nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "autoenablesItems") retCULong []

-- | @- setAutoenablesItems:@
setAutoenablesItems :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setAutoenablesItems nsMenu  value =
    sendMsg nsMenu (mkSelector "setAutoenablesItems:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delegate@
delegate :: IsNSMenu nsMenu => nsMenu -> IO RawId
delegate nsMenu  =
    fmap (RawId . castPtr) $ sendMsg nsMenu (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setDelegate nsMenu  value =
    sendMsg nsMenu (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- menuBarHeight@
menuBarHeight :: IsNSMenu nsMenu => nsMenu -> IO CDouble
menuBarHeight nsMenu  =
    sendMsg nsMenu (mkSelector "menuBarHeight") retCDouble []

-- | @- highlightedItem@
highlightedItem :: IsNSMenu nsMenu => nsMenu -> IO (Id NSMenuItem)
highlightedItem nsMenu  =
    sendMsg nsMenu (mkSelector "highlightedItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- minimumWidth@
minimumWidth :: IsNSMenu nsMenu => nsMenu -> IO CDouble
minimumWidth nsMenu  =
    sendMsg nsMenu (mkSelector "minimumWidth") retCDouble []

-- | @- setMinimumWidth:@
setMinimumWidth :: IsNSMenu nsMenu => nsMenu -> CDouble -> IO ()
setMinimumWidth nsMenu  value =
    sendMsg nsMenu (mkSelector "setMinimumWidth:") retVoid [argCDouble value]

-- | @- size@
size :: IsNSMenu nsMenu => nsMenu -> IO NSSize
size nsMenu  =
    sendMsgStret nsMenu (mkSelector "size") retNSSize []

-- | @- font@
font :: IsNSMenu nsMenu => nsMenu -> IO (Id NSFont)
font nsMenu  =
    sendMsg nsMenu (mkSelector "font") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFont:@
setFont :: (IsNSMenu nsMenu, IsNSFont value) => nsMenu -> value -> IO ()
setFont nsMenu  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenu (mkSelector "setFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- allowsContextMenuPlugIns@
allowsContextMenuPlugIns :: IsNSMenu nsMenu => nsMenu -> IO Bool
allowsContextMenuPlugIns nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "allowsContextMenuPlugIns") retCULong []

-- | @- setAllowsContextMenuPlugIns:@
setAllowsContextMenuPlugIns :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setAllowsContextMenuPlugIns nsMenu  value =
    sendMsg nsMenu (mkSelector "setAllowsContextMenuPlugIns:") retVoid [argCULong (if value then 1 else 0)]

-- | @- automaticallyInsertsWritingToolsItems@
automaticallyInsertsWritingToolsItems :: IsNSMenu nsMenu => nsMenu -> IO Bool
automaticallyInsertsWritingToolsItems nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "automaticallyInsertsWritingToolsItems") retCULong []

-- | @- setAutomaticallyInsertsWritingToolsItems:@
setAutomaticallyInsertsWritingToolsItems :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setAutomaticallyInsertsWritingToolsItems nsMenu  value =
    sendMsg nsMenu (mkSelector "setAutomaticallyInsertsWritingToolsItems:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsStateColumn@
showsStateColumn :: IsNSMenu nsMenu => nsMenu -> IO Bool
showsStateColumn nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "showsStateColumn") retCULong []

-- | @- setShowsStateColumn:@
setShowsStateColumn :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setShowsStateColumn nsMenu  value =
    sendMsg nsMenu (mkSelector "setShowsStateColumn:") retVoid [argCULong (if value then 1 else 0)]

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSMenu nsMenu => nsMenu -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsMenu  =
    fmap (coerce :: CLong -> NSUserInterfaceLayoutDirection) $ sendMsg nsMenu (mkSelector "userInterfaceLayoutDirection") retCLong []

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSMenu nsMenu => nsMenu -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsMenu  value =
    sendMsg nsMenu (mkSelector "setUserInterfaceLayoutDirection:") retVoid [argCLong (coerce value)]

-- | @- menuChangedMessagesEnabled@
menuChangedMessagesEnabled :: IsNSMenu nsMenu => nsMenu -> IO Bool
menuChangedMessagesEnabled nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "menuChangedMessagesEnabled") retCULong []

-- | @- setMenuChangedMessagesEnabled:@
setMenuChangedMessagesEnabled :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setMenuChangedMessagesEnabled nsMenu  value =
    sendMsg nsMenu (mkSelector "setMenuChangedMessagesEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- tornOff@
tornOff :: IsNSMenu nsMenu => nsMenu -> IO Bool
tornOff nsMenu  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenu (mkSelector "tornOff") retCULong []

-- | @- propertiesToUpdate@
propertiesToUpdate :: IsNSMenu nsMenu => nsMenu -> IO NSMenuProperties
propertiesToUpdate nsMenu  =
    fmap (coerce :: CULong -> NSMenuProperties) $ sendMsg nsMenu (mkSelector "propertiesToUpdate") retCULong []

-- | The presentation style of the menu.
--
-- Note: This property is not respected if the menu is the main menu of the app.
--
-- ObjC selector: @- presentationStyle@
presentationStyle :: IsNSMenu nsMenu => nsMenu -> IO NSMenuPresentationStyle
presentationStyle nsMenu  =
    fmap (coerce :: CLong -> NSMenuPresentationStyle) $ sendMsg nsMenu (mkSelector "presentationStyle") retCLong []

-- | The presentation style of the menu.
--
-- Note: This property is not respected if the menu is the main menu of the app.
--
-- ObjC selector: @- setPresentationStyle:@
setPresentationStyle :: IsNSMenu nsMenu => nsMenu -> NSMenuPresentationStyle -> IO ()
setPresentationStyle nsMenu  value =
    sendMsg nsMenu (mkSelector "setPresentationStyle:") retVoid [argCLong (coerce value)]

-- | The selection mode of the menu.
--
-- Note the selection mode only has effect on menu items that belong to the same selection group. A selection group consists of the items with the same target/action.
--
-- ObjC selector: @- selectionMode@
selectionMode :: IsNSMenu nsMenu => nsMenu -> IO NSMenuSelectionMode
selectionMode nsMenu  =
    fmap (coerce :: CLong -> NSMenuSelectionMode) $ sendMsg nsMenu (mkSelector "selectionMode") retCLong []

-- | The selection mode of the menu.
--
-- Note the selection mode only has effect on menu items that belong to the same selection group. A selection group consists of the items with the same target/action.
--
-- ObjC selector: @- setSelectionMode:@
setSelectionMode :: IsNSMenu nsMenu => nsMenu -> NSMenuSelectionMode -> IO ()
setSelectionMode nsMenu  value =
    sendMsg nsMenu (mkSelector "setSelectionMode:") retVoid [argCLong (coerce value)]

-- | The menu items that are selected.
--
-- An item is selected when its state is @NSControl.StateValue.on@.
--
-- Note: This property is settable. Setting @selectedItems@ will select any items that are contained in the provided array, and deselect any previously selected items that are not in the array.
--
-- ObjC selector: @- selectedItems@
selectedItems :: IsNSMenu nsMenu => nsMenu -> IO (Id NSArray)
selectedItems nsMenu  =
    sendMsg nsMenu (mkSelector "selectedItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The menu items that are selected.
--
-- An item is selected when its state is @NSControl.StateValue.on@.
--
-- Note: This property is settable. Setting @selectedItems@ will select any items that are contained in the provided array, and deselect any previously selected items that are not in the array.
--
-- ObjC selector: @- setSelectedItems:@
setSelectedItems :: (IsNSMenu nsMenu, IsNSArray value) => nsMenu -> value -> IO ()
setSelectedItems nsMenu  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenu (mkSelector "setSelectedItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:@
initWithTitleSelector :: Selector
initWithTitleSelector = mkSelector "initWithTitle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @popUpContextMenu:withEvent:forView:@
popUpContextMenu_withEvent_forViewSelector :: Selector
popUpContextMenu_withEvent_forViewSelector = mkSelector "popUpContextMenu:withEvent:forView:"

-- | @Selector@ for @popUpContextMenu:withEvent:forView:withFont:@
popUpContextMenu_withEvent_forView_withFontSelector :: Selector
popUpContextMenu_withEvent_forView_withFontSelector = mkSelector "popUpContextMenu:withEvent:forView:withFont:"

-- | @Selector@ for @popUpMenuPositioningItem:atLocation:inView:@
popUpMenuPositioningItem_atLocation_inViewSelector :: Selector
popUpMenuPositioningItem_atLocation_inViewSelector = mkSelector "popUpMenuPositioningItem:atLocation:inView:"

-- | @Selector@ for @setMenuBarVisible:@
setMenuBarVisibleSelector :: Selector
setMenuBarVisibleSelector = mkSelector "setMenuBarVisible:"

-- | @Selector@ for @menuBarVisible@
menuBarVisibleSelector :: Selector
menuBarVisibleSelector = mkSelector "menuBarVisible"

-- | @Selector@ for @insertItem:atIndex:@
insertItem_atIndexSelector :: Selector
insertItem_atIndexSelector = mkSelector "insertItem:atIndex:"

-- | @Selector@ for @addItem:@
addItemSelector :: Selector
addItemSelector = mkSelector "addItem:"

-- | @Selector@ for @insertItemWithTitle:action:keyEquivalent:atIndex:@
insertItemWithTitle_action_keyEquivalent_atIndexSelector :: Selector
insertItemWithTitle_action_keyEquivalent_atIndexSelector = mkSelector "insertItemWithTitle:action:keyEquivalent:atIndex:"

-- | @Selector@ for @addItemWithTitle:action:keyEquivalent:@
addItemWithTitle_action_keyEquivalentSelector :: Selector
addItemWithTitle_action_keyEquivalentSelector = mkSelector "addItemWithTitle:action:keyEquivalent:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector
removeItemSelector = mkSelector "removeItem:"

-- | @Selector@ for @setSubmenu:forItem:@
setSubmenu_forItemSelector :: Selector
setSubmenu_forItemSelector = mkSelector "setSubmenu:forItem:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector
itemAtIndexSelector = mkSelector "itemAtIndex:"

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

-- | @Selector@ for @indexOfItemWithSubmenu:@
indexOfItemWithSubmenuSelector :: Selector
indexOfItemWithSubmenuSelector = mkSelector "indexOfItemWithSubmenu:"

-- | @Selector@ for @indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andActionSelector :: Selector
indexOfItemWithTarget_andActionSelector = mkSelector "indexOfItemWithTarget:andAction:"

-- | @Selector@ for @itemWithTitle:@
itemWithTitleSelector :: Selector
itemWithTitleSelector = mkSelector "itemWithTitle:"

-- | @Selector@ for @itemWithTag:@
itemWithTagSelector :: Selector
itemWithTagSelector = mkSelector "itemWithTag:"

-- | @Selector@ for @update@
updateSelector :: Selector
updateSelector = mkSelector "update"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @itemChanged:@
itemChangedSelector :: Selector
itemChangedSelector = mkSelector "itemChanged:"

-- | @Selector@ for @performActionForItemAtIndex:@
performActionForItemAtIndexSelector :: Selector
performActionForItemAtIndexSelector = mkSelector "performActionForItemAtIndex:"

-- | @Selector@ for @cancelTracking@
cancelTrackingSelector :: Selector
cancelTrackingSelector = mkSelector "cancelTracking"

-- | @Selector@ for @cancelTrackingWithoutAnimation@
cancelTrackingWithoutAnimationSelector :: Selector
cancelTrackingWithoutAnimationSelector = mkSelector "cancelTrackingWithoutAnimation"

-- | @Selector@ for @setMenuRepresentation:@
setMenuRepresentationSelector :: Selector
setMenuRepresentationSelector = mkSelector "setMenuRepresentation:"

-- | @Selector@ for @menuRepresentation@
menuRepresentationSelector :: Selector
menuRepresentationSelector = mkSelector "menuRepresentation"

-- | @Selector@ for @setContextMenuRepresentation:@
setContextMenuRepresentationSelector :: Selector
setContextMenuRepresentationSelector = mkSelector "setContextMenuRepresentation:"

-- | @Selector@ for @contextMenuRepresentation@
contextMenuRepresentationSelector :: Selector
contextMenuRepresentationSelector = mkSelector "contextMenuRepresentation"

-- | @Selector@ for @setTearOffMenuRepresentation:@
setTearOffMenuRepresentationSelector :: Selector
setTearOffMenuRepresentationSelector = mkSelector "setTearOffMenuRepresentation:"

-- | @Selector@ for @tearOffMenuRepresentation@
tearOffMenuRepresentationSelector :: Selector
tearOffMenuRepresentationSelector = mkSelector "tearOffMenuRepresentation"

-- | @Selector@ for @menuZone@
menuZoneSelector :: Selector
menuZoneSelector = mkSelector "menuZone"

-- | @Selector@ for @setMenuZone:@
setMenuZoneSelector :: Selector
setMenuZoneSelector = mkSelector "setMenuZone:"

-- | @Selector@ for @attachedMenu@
attachedMenuSelector :: Selector
attachedMenuSelector = mkSelector "attachedMenu"

-- | @Selector@ for @isAttached@
isAttachedSelector :: Selector
isAttachedSelector = mkSelector "isAttached"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @locationForSubmenu:@
locationForSubmenuSelector :: Selector
locationForSubmenuSelector = mkSelector "locationForSubmenu:"

-- | @Selector@ for @helpRequested:@
helpRequestedSelector :: Selector
helpRequestedSelector = mkSelector "helpRequested:"

-- | @Selector@ for @submenuAction:@
submenuActionSelector :: Selector
submenuActionSelector = mkSelector "submenuAction:"

-- | @Selector@ for @paletteMenuWithColors:titles:selectionHandler:@
paletteMenuWithColors_titles_selectionHandlerSelector :: Selector
paletteMenuWithColors_titles_selectionHandlerSelector = mkSelector "paletteMenuWithColors:titles:selectionHandler:"

-- | @Selector@ for @paletteMenuWithColors:titles:templateImage:selectionHandler:@
paletteMenuWithColors_titles_templateImage_selectionHandlerSelector :: Selector
paletteMenuWithColors_titles_templateImage_selectionHandlerSelector = mkSelector "paletteMenuWithColors:titles:templateImage:selectionHandler:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @supermenu@
supermenuSelector :: Selector
supermenuSelector = mkSelector "supermenu"

-- | @Selector@ for @setSupermenu:@
setSupermenuSelector :: Selector
setSupermenuSelector = mkSelector "setSupermenu:"

-- | @Selector@ for @itemArray@
itemArraySelector :: Selector
itemArraySelector = mkSelector "itemArray"

-- | @Selector@ for @setItemArray:@
setItemArraySelector :: Selector
setItemArraySelector = mkSelector "setItemArray:"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @autoenablesItems@
autoenablesItemsSelector :: Selector
autoenablesItemsSelector = mkSelector "autoenablesItems"

-- | @Selector@ for @setAutoenablesItems:@
setAutoenablesItemsSelector :: Selector
setAutoenablesItemsSelector = mkSelector "setAutoenablesItems:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @menuBarHeight@
menuBarHeightSelector :: Selector
menuBarHeightSelector = mkSelector "menuBarHeight"

-- | @Selector@ for @highlightedItem@
highlightedItemSelector :: Selector
highlightedItemSelector = mkSelector "highlightedItem"

-- | @Selector@ for @minimumWidth@
minimumWidthSelector :: Selector
minimumWidthSelector = mkSelector "minimumWidth"

-- | @Selector@ for @setMinimumWidth:@
setMinimumWidthSelector :: Selector
setMinimumWidthSelector = mkSelector "setMinimumWidth:"

-- | @Selector@ for @size@
sizeSelector :: Selector
sizeSelector = mkSelector "size"

-- | @Selector@ for @font@
fontSelector :: Selector
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @allowsContextMenuPlugIns@
allowsContextMenuPlugInsSelector :: Selector
allowsContextMenuPlugInsSelector = mkSelector "allowsContextMenuPlugIns"

-- | @Selector@ for @setAllowsContextMenuPlugIns:@
setAllowsContextMenuPlugInsSelector :: Selector
setAllowsContextMenuPlugInsSelector = mkSelector "setAllowsContextMenuPlugIns:"

-- | @Selector@ for @automaticallyInsertsWritingToolsItems@
automaticallyInsertsWritingToolsItemsSelector :: Selector
automaticallyInsertsWritingToolsItemsSelector = mkSelector "automaticallyInsertsWritingToolsItems"

-- | @Selector@ for @setAutomaticallyInsertsWritingToolsItems:@
setAutomaticallyInsertsWritingToolsItemsSelector :: Selector
setAutomaticallyInsertsWritingToolsItemsSelector = mkSelector "setAutomaticallyInsertsWritingToolsItems:"

-- | @Selector@ for @showsStateColumn@
showsStateColumnSelector :: Selector
showsStateColumnSelector = mkSelector "showsStateColumn"

-- | @Selector@ for @setShowsStateColumn:@
setShowsStateColumnSelector :: Selector
setShowsStateColumnSelector = mkSelector "setShowsStateColumn:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @menuChangedMessagesEnabled@
menuChangedMessagesEnabledSelector :: Selector
menuChangedMessagesEnabledSelector = mkSelector "menuChangedMessagesEnabled"

-- | @Selector@ for @setMenuChangedMessagesEnabled:@
setMenuChangedMessagesEnabledSelector :: Selector
setMenuChangedMessagesEnabledSelector = mkSelector "setMenuChangedMessagesEnabled:"

-- | @Selector@ for @tornOff@
tornOffSelector :: Selector
tornOffSelector = mkSelector "tornOff"

-- | @Selector@ for @propertiesToUpdate@
propertiesToUpdateSelector :: Selector
propertiesToUpdateSelector = mkSelector "propertiesToUpdate"

-- | @Selector@ for @presentationStyle@
presentationStyleSelector :: Selector
presentationStyleSelector = mkSelector "presentationStyle"

-- | @Selector@ for @setPresentationStyle:@
setPresentationStyleSelector :: Selector
setPresentationStyleSelector = mkSelector "setPresentationStyle:"

-- | @Selector@ for @selectionMode@
selectionModeSelector :: Selector
selectionModeSelector = mkSelector "selectionMode"

-- | @Selector@ for @setSelectionMode:@
setSelectionModeSelector :: Selector
setSelectionModeSelector = mkSelector "setSelectionMode:"

-- | @Selector@ for @selectedItems@
selectedItemsSelector :: Selector
selectedItemsSelector = mkSelector "selectedItems"

-- | @Selector@ for @setSelectedItems:@
setSelectedItemsSelector :: Selector
setSelectedItemsSelector = mkSelector "setSelectedItems:"

