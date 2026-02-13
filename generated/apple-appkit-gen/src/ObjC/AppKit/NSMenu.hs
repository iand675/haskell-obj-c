{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addItemSelector
  , addItemWithTitle_action_keyEquivalentSelector
  , allowsContextMenuPlugInsSelector
  , attachedMenuSelector
  , autoenablesItemsSelector
  , automaticallyInsertsWritingToolsItemsSelector
  , cancelTrackingSelector
  , cancelTrackingWithoutAnimationSelector
  , contextMenuRepresentationSelector
  , delegateSelector
  , fontSelector
  , helpRequestedSelector
  , highlightedItemSelector
  , indexOfItemSelector
  , indexOfItemWithRepresentedObjectSelector
  , indexOfItemWithSubmenuSelector
  , indexOfItemWithTagSelector
  , indexOfItemWithTarget_andActionSelector
  , indexOfItemWithTitleSelector
  , initWithCoderSelector
  , initWithTitleSelector
  , insertItemWithTitle_action_keyEquivalent_atIndexSelector
  , insertItem_atIndexSelector
  , isAttachedSelector
  , itemArraySelector
  , itemAtIndexSelector
  , itemChangedSelector
  , itemWithTagSelector
  , itemWithTitleSelector
  , locationForSubmenuSelector
  , menuBarHeightSelector
  , menuBarVisibleSelector
  , menuChangedMessagesEnabledSelector
  , menuRepresentationSelector
  , menuZoneSelector
  , minimumWidthSelector
  , numberOfItemsSelector
  , paletteMenuWithColors_titles_selectionHandlerSelector
  , paletteMenuWithColors_titles_templateImage_selectionHandlerSelector
  , performActionForItemAtIndexSelector
  , performKeyEquivalentSelector
  , popUpContextMenu_withEvent_forViewSelector
  , popUpContextMenu_withEvent_forView_withFontSelector
  , popUpMenuPositioningItem_atLocation_inViewSelector
  , presentationStyleSelector
  , propertiesToUpdateSelector
  , removeAllItemsSelector
  , removeItemAtIndexSelector
  , removeItemSelector
  , selectedItemsSelector
  , selectionModeSelector
  , setAllowsContextMenuPlugInsSelector
  , setAutoenablesItemsSelector
  , setAutomaticallyInsertsWritingToolsItemsSelector
  , setContextMenuRepresentationSelector
  , setDelegateSelector
  , setFontSelector
  , setItemArraySelector
  , setMenuBarVisibleSelector
  , setMenuChangedMessagesEnabledSelector
  , setMenuRepresentationSelector
  , setMenuZoneSelector
  , setMinimumWidthSelector
  , setPresentationStyleSelector
  , setSelectedItemsSelector
  , setSelectionModeSelector
  , setShowsStateColumnSelector
  , setSubmenu_forItemSelector
  , setSupermenuSelector
  , setTearOffMenuRepresentationSelector
  , setTitleSelector
  , setUserInterfaceLayoutDirectionSelector
  , showsStateColumnSelector
  , sizeSelector
  , sizeToFitSelector
  , submenuActionSelector
  , supermenuSelector
  , tearOffMenuRepresentationSelector
  , titleSelector
  , tornOffSelector
  , updateSelector
  , userInterfaceLayoutDirectionSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initWithTitle:@
initWithTitle :: (IsNSMenu nsMenu, IsNSString title) => nsMenu -> title -> IO (Id NSMenu)
initWithTitle nsMenu title =
  sendOwnedMessage nsMenu initWithTitleSelector (toNSString title)

-- | @- initWithCoder:@
initWithCoder :: (IsNSMenu nsMenu, IsNSCoder coder) => nsMenu -> coder -> IO (Id NSMenu)
initWithCoder nsMenu coder =
  sendOwnedMessage nsMenu initWithCoderSelector (toNSCoder coder)

-- | @+ popUpContextMenu:withEvent:forView:@
popUpContextMenu_withEvent_forView :: (IsNSMenu menu, IsNSEvent event, IsNSView view) => menu -> event -> view -> IO ()
popUpContextMenu_withEvent_forView menu event view =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' popUpContextMenu_withEvent_forViewSelector (toNSMenu menu) (toNSEvent event) (toNSView view)

-- | @+ popUpContextMenu:withEvent:forView:withFont:@
popUpContextMenu_withEvent_forView_withFont :: (IsNSMenu menu, IsNSEvent event, IsNSView view, IsNSFont font) => menu -> event -> view -> font -> IO ()
popUpContextMenu_withEvent_forView_withFont menu event view font =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' popUpContextMenu_withEvent_forView_withFontSelector (toNSMenu menu) (toNSEvent event) (toNSView view) (toNSFont font)

-- | @- popUpMenuPositioningItem:atLocation:inView:@
popUpMenuPositioningItem_atLocation_inView :: (IsNSMenu nsMenu, IsNSMenuItem item, IsNSView view) => nsMenu -> item -> NSPoint -> view -> IO Bool
popUpMenuPositioningItem_atLocation_inView nsMenu item location view =
  sendMessage nsMenu popUpMenuPositioningItem_atLocation_inViewSelector (toNSMenuItem item) location (toNSView view)

-- | @+ setMenuBarVisible:@
setMenuBarVisible :: Bool -> IO ()
setMenuBarVisible visible =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' setMenuBarVisibleSelector visible

-- | @+ menuBarVisible@
menuBarVisible :: IO Bool
menuBarVisible  =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' menuBarVisibleSelector

-- | @- insertItem:atIndex:@
insertItem_atIndex :: (IsNSMenu nsMenu, IsNSMenuItem newItem) => nsMenu -> newItem -> CLong -> IO ()
insertItem_atIndex nsMenu newItem index =
  sendMessage nsMenu insertItem_atIndexSelector (toNSMenuItem newItem) index

-- | @- addItem:@
addItem :: (IsNSMenu nsMenu, IsNSMenuItem newItem) => nsMenu -> newItem -> IO ()
addItem nsMenu newItem =
  sendMessage nsMenu addItemSelector (toNSMenuItem newItem)

-- | @- insertItemWithTitle:action:keyEquivalent:atIndex:@
insertItemWithTitle_action_keyEquivalent_atIndex :: (IsNSMenu nsMenu, IsNSString string, IsNSString charCode) => nsMenu -> string -> Sel -> charCode -> CLong -> IO (Id NSMenuItem)
insertItemWithTitle_action_keyEquivalent_atIndex nsMenu string selector charCode index =
  sendMessage nsMenu insertItemWithTitle_action_keyEquivalent_atIndexSelector (toNSString string) selector (toNSString charCode) index

-- | @- addItemWithTitle:action:keyEquivalent:@
addItemWithTitle_action_keyEquivalent :: (IsNSMenu nsMenu, IsNSString string, IsNSString charCode) => nsMenu -> string -> Sel -> charCode -> IO (Id NSMenuItem)
addItemWithTitle_action_keyEquivalent nsMenu string selector charCode =
  sendMessage nsMenu addItemWithTitle_action_keyEquivalentSelector (toNSString string) selector (toNSString charCode)

-- | @- removeItemAtIndex:@
removeItemAtIndex :: IsNSMenu nsMenu => nsMenu -> CLong -> IO ()
removeItemAtIndex nsMenu index =
  sendMessage nsMenu removeItemAtIndexSelector index

-- | @- removeItem:@
removeItem :: (IsNSMenu nsMenu, IsNSMenuItem item) => nsMenu -> item -> IO ()
removeItem nsMenu item =
  sendMessage nsMenu removeItemSelector (toNSMenuItem item)

-- | @- setSubmenu:forItem:@
setSubmenu_forItem :: (IsNSMenu nsMenu, IsNSMenu menu, IsNSMenuItem item) => nsMenu -> menu -> item -> IO ()
setSubmenu_forItem nsMenu menu item =
  sendMessage nsMenu setSubmenu_forItemSelector (toNSMenu menu) (toNSMenuItem item)

-- | @- removeAllItems@
removeAllItems :: IsNSMenu nsMenu => nsMenu -> IO ()
removeAllItems nsMenu =
  sendMessage nsMenu removeAllItemsSelector

-- | @- itemAtIndex:@
itemAtIndex :: IsNSMenu nsMenu => nsMenu -> CLong -> IO (Id NSMenuItem)
itemAtIndex nsMenu index =
  sendMessage nsMenu itemAtIndexSelector index

-- | @- indexOfItem:@
indexOfItem :: (IsNSMenu nsMenu, IsNSMenuItem item) => nsMenu -> item -> IO CLong
indexOfItem nsMenu item =
  sendMessage nsMenu indexOfItemSelector (toNSMenuItem item)

-- | @- indexOfItemWithTitle:@
indexOfItemWithTitle :: (IsNSMenu nsMenu, IsNSString title) => nsMenu -> title -> IO CLong
indexOfItemWithTitle nsMenu title =
  sendMessage nsMenu indexOfItemWithTitleSelector (toNSString title)

-- | @- indexOfItemWithTag:@
indexOfItemWithTag :: IsNSMenu nsMenu => nsMenu -> CLong -> IO CLong
indexOfItemWithTag nsMenu tag =
  sendMessage nsMenu indexOfItemWithTagSelector tag

-- | @- indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObject :: IsNSMenu nsMenu => nsMenu -> RawId -> IO CLong
indexOfItemWithRepresentedObject nsMenu object =
  sendMessage nsMenu indexOfItemWithRepresentedObjectSelector object

-- | @- indexOfItemWithSubmenu:@
indexOfItemWithSubmenu :: (IsNSMenu nsMenu, IsNSMenu submenu) => nsMenu -> submenu -> IO CLong
indexOfItemWithSubmenu nsMenu submenu =
  sendMessage nsMenu indexOfItemWithSubmenuSelector (toNSMenu submenu)

-- | @- indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andAction :: IsNSMenu nsMenu => nsMenu -> RawId -> Sel -> IO CLong
indexOfItemWithTarget_andAction nsMenu target actionSelector =
  sendMessage nsMenu indexOfItemWithTarget_andActionSelector target actionSelector

-- | @- itemWithTitle:@
itemWithTitle :: (IsNSMenu nsMenu, IsNSString title) => nsMenu -> title -> IO (Id NSMenuItem)
itemWithTitle nsMenu title =
  sendMessage nsMenu itemWithTitleSelector (toNSString title)

-- | @- itemWithTag:@
itemWithTag :: IsNSMenu nsMenu => nsMenu -> CLong -> IO (Id NSMenuItem)
itemWithTag nsMenu tag =
  sendMessage nsMenu itemWithTagSelector tag

-- | @- update@
update :: IsNSMenu nsMenu => nsMenu -> IO ()
update nsMenu =
  sendMessage nsMenu updateSelector

-- | @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSMenu nsMenu, IsNSEvent event) => nsMenu -> event -> IO Bool
performKeyEquivalent nsMenu event =
  sendMessage nsMenu performKeyEquivalentSelector (toNSEvent event)

-- | @- itemChanged:@
itemChanged :: (IsNSMenu nsMenu, IsNSMenuItem item) => nsMenu -> item -> IO ()
itemChanged nsMenu item =
  sendMessage nsMenu itemChangedSelector (toNSMenuItem item)

-- | @- performActionForItemAtIndex:@
performActionForItemAtIndex :: IsNSMenu nsMenu => nsMenu -> CLong -> IO ()
performActionForItemAtIndex nsMenu index =
  sendMessage nsMenu performActionForItemAtIndexSelector index

-- | @- cancelTracking@
cancelTracking :: IsNSMenu nsMenu => nsMenu -> IO ()
cancelTracking nsMenu =
  sendMessage nsMenu cancelTrackingSelector

-- | @- cancelTrackingWithoutAnimation@
cancelTrackingWithoutAnimation :: IsNSMenu nsMenu => nsMenu -> IO ()
cancelTrackingWithoutAnimation nsMenu =
  sendMessage nsMenu cancelTrackingWithoutAnimationSelector

-- | @- setMenuRepresentation:@
setMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setMenuRepresentation nsMenu menuRep =
  sendMessage nsMenu setMenuRepresentationSelector menuRep

-- | @- menuRepresentation@
menuRepresentation :: IsNSMenu nsMenu => nsMenu -> IO RawId
menuRepresentation nsMenu =
  sendMessage nsMenu menuRepresentationSelector

-- | @- setContextMenuRepresentation:@
setContextMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setContextMenuRepresentation nsMenu menuRep =
  sendMessage nsMenu setContextMenuRepresentationSelector menuRep

-- | @- contextMenuRepresentation@
contextMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> IO RawId
contextMenuRepresentation nsMenu =
  sendMessage nsMenu contextMenuRepresentationSelector

-- | @- setTearOffMenuRepresentation:@
setTearOffMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setTearOffMenuRepresentation nsMenu menuRep =
  sendMessage nsMenu setTearOffMenuRepresentationSelector menuRep

-- | @- tearOffMenuRepresentation@
tearOffMenuRepresentation :: IsNSMenu nsMenu => nsMenu -> IO RawId
tearOffMenuRepresentation nsMenu =
  sendMessage nsMenu tearOffMenuRepresentationSelector

-- | @+ menuZone@
menuZone :: IO (Ptr ())
menuZone  =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' menuZoneSelector

-- | @+ setMenuZone:@
setMenuZone :: Ptr () -> IO ()
setMenuZone zone =
  do
    cls' <- getRequiredClass "NSMenu"
    sendClassMessage cls' setMenuZoneSelector zone

-- | @- attachedMenu@
attachedMenu :: IsNSMenu nsMenu => nsMenu -> IO (Id NSMenu)
attachedMenu nsMenu =
  sendMessage nsMenu attachedMenuSelector

-- | @- isAttached@
isAttached :: IsNSMenu nsMenu => nsMenu -> IO Bool
isAttached nsMenu =
  sendMessage nsMenu isAttachedSelector

-- | @- sizeToFit@
sizeToFit :: IsNSMenu nsMenu => nsMenu -> IO ()
sizeToFit nsMenu =
  sendMessage nsMenu sizeToFitSelector

-- | @- locationForSubmenu:@
locationForSubmenu :: (IsNSMenu nsMenu, IsNSMenu submenu) => nsMenu -> submenu -> IO NSPoint
locationForSubmenu nsMenu submenu =
  sendMessage nsMenu locationForSubmenuSelector (toNSMenu submenu)

-- | @- helpRequested:@
helpRequested :: (IsNSMenu nsMenu, IsNSEvent eventPtr) => nsMenu -> eventPtr -> IO ()
helpRequested nsMenu eventPtr =
  sendMessage nsMenu helpRequestedSelector (toNSEvent eventPtr)

-- | @- submenuAction:@
submenuAction :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
submenuAction nsMenu sender =
  sendMessage nsMenu submenuActionSelector sender

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
    sendClassMessage cls' paletteMenuWithColors_titles_selectionHandlerSelector (toNSArray colors) (toNSArray itemTitles) onSelectionChange

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
    sendClassMessage cls' paletteMenuWithColors_titles_templateImage_selectionHandlerSelector (toNSArray colors) (toNSArray itemTitles) (toNSImage image) onSelectionChange

-- | @- title@
title :: IsNSMenu nsMenu => nsMenu -> IO (Id NSString)
title nsMenu =
  sendMessage nsMenu titleSelector

-- | @- setTitle:@
setTitle :: (IsNSMenu nsMenu, IsNSString value) => nsMenu -> value -> IO ()
setTitle nsMenu value =
  sendMessage nsMenu setTitleSelector (toNSString value)

-- | @- supermenu@
supermenu :: IsNSMenu nsMenu => nsMenu -> IO (Id NSMenu)
supermenu nsMenu =
  sendMessage nsMenu supermenuSelector

-- | @- setSupermenu:@
setSupermenu :: (IsNSMenu nsMenu, IsNSMenu value) => nsMenu -> value -> IO ()
setSupermenu nsMenu value =
  sendMessage nsMenu setSupermenuSelector (toNSMenu value)

-- | @- itemArray@
itemArray :: IsNSMenu nsMenu => nsMenu -> IO (Id NSArray)
itemArray nsMenu =
  sendMessage nsMenu itemArraySelector

-- | @- setItemArray:@
setItemArray :: (IsNSMenu nsMenu, IsNSArray value) => nsMenu -> value -> IO ()
setItemArray nsMenu value =
  sendMessage nsMenu setItemArraySelector (toNSArray value)

-- | @- numberOfItems@
numberOfItems :: IsNSMenu nsMenu => nsMenu -> IO CLong
numberOfItems nsMenu =
  sendMessage nsMenu numberOfItemsSelector

-- | @- autoenablesItems@
autoenablesItems :: IsNSMenu nsMenu => nsMenu -> IO Bool
autoenablesItems nsMenu =
  sendMessage nsMenu autoenablesItemsSelector

-- | @- setAutoenablesItems:@
setAutoenablesItems :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setAutoenablesItems nsMenu value =
  sendMessage nsMenu setAutoenablesItemsSelector value

-- | @- delegate@
delegate :: IsNSMenu nsMenu => nsMenu -> IO RawId
delegate nsMenu =
  sendMessage nsMenu delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSMenu nsMenu => nsMenu -> RawId -> IO ()
setDelegate nsMenu value =
  sendMessage nsMenu setDelegateSelector value

-- | @- menuBarHeight@
menuBarHeight :: IsNSMenu nsMenu => nsMenu -> IO CDouble
menuBarHeight nsMenu =
  sendMessage nsMenu menuBarHeightSelector

-- | @- highlightedItem@
highlightedItem :: IsNSMenu nsMenu => nsMenu -> IO (Id NSMenuItem)
highlightedItem nsMenu =
  sendMessage nsMenu highlightedItemSelector

-- | @- minimumWidth@
minimumWidth :: IsNSMenu nsMenu => nsMenu -> IO CDouble
minimumWidth nsMenu =
  sendMessage nsMenu minimumWidthSelector

-- | @- setMinimumWidth:@
setMinimumWidth :: IsNSMenu nsMenu => nsMenu -> CDouble -> IO ()
setMinimumWidth nsMenu value =
  sendMessage nsMenu setMinimumWidthSelector value

-- | @- size@
size :: IsNSMenu nsMenu => nsMenu -> IO NSSize
size nsMenu =
  sendMessage nsMenu sizeSelector

-- | @- font@
font :: IsNSMenu nsMenu => nsMenu -> IO (Id NSFont)
font nsMenu =
  sendMessage nsMenu fontSelector

-- | @- setFont:@
setFont :: (IsNSMenu nsMenu, IsNSFont value) => nsMenu -> value -> IO ()
setFont nsMenu value =
  sendMessage nsMenu setFontSelector (toNSFont value)

-- | @- allowsContextMenuPlugIns@
allowsContextMenuPlugIns :: IsNSMenu nsMenu => nsMenu -> IO Bool
allowsContextMenuPlugIns nsMenu =
  sendMessage nsMenu allowsContextMenuPlugInsSelector

-- | @- setAllowsContextMenuPlugIns:@
setAllowsContextMenuPlugIns :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setAllowsContextMenuPlugIns nsMenu value =
  sendMessage nsMenu setAllowsContextMenuPlugInsSelector value

-- | @- automaticallyInsertsWritingToolsItems@
automaticallyInsertsWritingToolsItems :: IsNSMenu nsMenu => nsMenu -> IO Bool
automaticallyInsertsWritingToolsItems nsMenu =
  sendMessage nsMenu automaticallyInsertsWritingToolsItemsSelector

-- | @- setAutomaticallyInsertsWritingToolsItems:@
setAutomaticallyInsertsWritingToolsItems :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setAutomaticallyInsertsWritingToolsItems nsMenu value =
  sendMessage nsMenu setAutomaticallyInsertsWritingToolsItemsSelector value

-- | @- showsStateColumn@
showsStateColumn :: IsNSMenu nsMenu => nsMenu -> IO Bool
showsStateColumn nsMenu =
  sendMessage nsMenu showsStateColumnSelector

-- | @- setShowsStateColumn:@
setShowsStateColumn :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setShowsStateColumn nsMenu value =
  sendMessage nsMenu setShowsStateColumnSelector value

-- | @- userInterfaceLayoutDirection@
userInterfaceLayoutDirection :: IsNSMenu nsMenu => nsMenu -> IO NSUserInterfaceLayoutDirection
userInterfaceLayoutDirection nsMenu =
  sendMessage nsMenu userInterfaceLayoutDirectionSelector

-- | @- setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirection :: IsNSMenu nsMenu => nsMenu -> NSUserInterfaceLayoutDirection -> IO ()
setUserInterfaceLayoutDirection nsMenu value =
  sendMessage nsMenu setUserInterfaceLayoutDirectionSelector value

-- | @- menuChangedMessagesEnabled@
menuChangedMessagesEnabled :: IsNSMenu nsMenu => nsMenu -> IO Bool
menuChangedMessagesEnabled nsMenu =
  sendMessage nsMenu menuChangedMessagesEnabledSelector

-- | @- setMenuChangedMessagesEnabled:@
setMenuChangedMessagesEnabled :: IsNSMenu nsMenu => nsMenu -> Bool -> IO ()
setMenuChangedMessagesEnabled nsMenu value =
  sendMessage nsMenu setMenuChangedMessagesEnabledSelector value

-- | @- tornOff@
tornOff :: IsNSMenu nsMenu => nsMenu -> IO Bool
tornOff nsMenu =
  sendMessage nsMenu tornOffSelector

-- | @- propertiesToUpdate@
propertiesToUpdate :: IsNSMenu nsMenu => nsMenu -> IO NSMenuProperties
propertiesToUpdate nsMenu =
  sendMessage nsMenu propertiesToUpdateSelector

-- | The presentation style of the menu.
--
-- Note: This property is not respected if the menu is the main menu of the app.
--
-- ObjC selector: @- presentationStyle@
presentationStyle :: IsNSMenu nsMenu => nsMenu -> IO NSMenuPresentationStyle
presentationStyle nsMenu =
  sendMessage nsMenu presentationStyleSelector

-- | The presentation style of the menu.
--
-- Note: This property is not respected if the menu is the main menu of the app.
--
-- ObjC selector: @- setPresentationStyle:@
setPresentationStyle :: IsNSMenu nsMenu => nsMenu -> NSMenuPresentationStyle -> IO ()
setPresentationStyle nsMenu value =
  sendMessage nsMenu setPresentationStyleSelector value

-- | The selection mode of the menu.
--
-- Note the selection mode only has effect on menu items that belong to the same selection group. A selection group consists of the items with the same target/action.
--
-- ObjC selector: @- selectionMode@
selectionMode :: IsNSMenu nsMenu => nsMenu -> IO NSMenuSelectionMode
selectionMode nsMenu =
  sendMessage nsMenu selectionModeSelector

-- | The selection mode of the menu.
--
-- Note the selection mode only has effect on menu items that belong to the same selection group. A selection group consists of the items with the same target/action.
--
-- ObjC selector: @- setSelectionMode:@
setSelectionMode :: IsNSMenu nsMenu => nsMenu -> NSMenuSelectionMode -> IO ()
setSelectionMode nsMenu value =
  sendMessage nsMenu setSelectionModeSelector value

-- | The menu items that are selected.
--
-- An item is selected when its state is @NSControl.StateValue.on@.
--
-- Note: This property is settable. Setting @selectedItems@ will select any items that are contained in the provided array, and deselect any previously selected items that are not in the array.
--
-- ObjC selector: @- selectedItems@
selectedItems :: IsNSMenu nsMenu => nsMenu -> IO (Id NSArray)
selectedItems nsMenu =
  sendMessage nsMenu selectedItemsSelector

-- | The menu items that are selected.
--
-- An item is selected when its state is @NSControl.StateValue.on@.
--
-- Note: This property is settable. Setting @selectedItems@ will select any items that are contained in the provided array, and deselect any previously selected items that are not in the array.
--
-- ObjC selector: @- setSelectedItems:@
setSelectedItems :: (IsNSMenu nsMenu, IsNSArray value) => nsMenu -> value -> IO ()
setSelectedItems nsMenu value =
  sendMessage nsMenu setSelectedItemsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTitle:@
initWithTitleSelector :: Selector '[Id NSString] (Id NSMenu)
initWithTitleSelector = mkSelector "initWithTitle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMenu)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @popUpContextMenu:withEvent:forView:@
popUpContextMenu_withEvent_forViewSelector :: Selector '[Id NSMenu, Id NSEvent, Id NSView] ()
popUpContextMenu_withEvent_forViewSelector = mkSelector "popUpContextMenu:withEvent:forView:"

-- | @Selector@ for @popUpContextMenu:withEvent:forView:withFont:@
popUpContextMenu_withEvent_forView_withFontSelector :: Selector '[Id NSMenu, Id NSEvent, Id NSView, Id NSFont] ()
popUpContextMenu_withEvent_forView_withFontSelector = mkSelector "popUpContextMenu:withEvent:forView:withFont:"

-- | @Selector@ for @popUpMenuPositioningItem:atLocation:inView:@
popUpMenuPositioningItem_atLocation_inViewSelector :: Selector '[Id NSMenuItem, NSPoint, Id NSView] Bool
popUpMenuPositioningItem_atLocation_inViewSelector = mkSelector "popUpMenuPositioningItem:atLocation:inView:"

-- | @Selector@ for @setMenuBarVisible:@
setMenuBarVisibleSelector :: Selector '[Bool] ()
setMenuBarVisibleSelector = mkSelector "setMenuBarVisible:"

-- | @Selector@ for @menuBarVisible@
menuBarVisibleSelector :: Selector '[] Bool
menuBarVisibleSelector = mkSelector "menuBarVisible"

-- | @Selector@ for @insertItem:atIndex:@
insertItem_atIndexSelector :: Selector '[Id NSMenuItem, CLong] ()
insertItem_atIndexSelector = mkSelector "insertItem:atIndex:"

-- | @Selector@ for @addItem:@
addItemSelector :: Selector '[Id NSMenuItem] ()
addItemSelector = mkSelector "addItem:"

-- | @Selector@ for @insertItemWithTitle:action:keyEquivalent:atIndex:@
insertItemWithTitle_action_keyEquivalent_atIndexSelector :: Selector '[Id NSString, Sel, Id NSString, CLong] (Id NSMenuItem)
insertItemWithTitle_action_keyEquivalent_atIndexSelector = mkSelector "insertItemWithTitle:action:keyEquivalent:atIndex:"

-- | @Selector@ for @addItemWithTitle:action:keyEquivalent:@
addItemWithTitle_action_keyEquivalentSelector :: Selector '[Id NSString, Sel, Id NSString] (Id NSMenuItem)
addItemWithTitle_action_keyEquivalentSelector = mkSelector "addItemWithTitle:action:keyEquivalent:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector '[CLong] ()
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeItem:@
removeItemSelector :: Selector '[Id NSMenuItem] ()
removeItemSelector = mkSelector "removeItem:"

-- | @Selector@ for @setSubmenu:forItem:@
setSubmenu_forItemSelector :: Selector '[Id NSMenu, Id NSMenuItem] ()
setSubmenu_forItemSelector = mkSelector "setSubmenu:forItem:"

-- | @Selector@ for @removeAllItems@
removeAllItemsSelector :: Selector '[] ()
removeAllItemsSelector = mkSelector "removeAllItems"

-- | @Selector@ for @itemAtIndex:@
itemAtIndexSelector :: Selector '[CLong] (Id NSMenuItem)
itemAtIndexSelector = mkSelector "itemAtIndex:"

-- | @Selector@ for @indexOfItem:@
indexOfItemSelector :: Selector '[Id NSMenuItem] CLong
indexOfItemSelector = mkSelector "indexOfItem:"

-- | @Selector@ for @indexOfItemWithTitle:@
indexOfItemWithTitleSelector :: Selector '[Id NSString] CLong
indexOfItemWithTitleSelector = mkSelector "indexOfItemWithTitle:"

-- | @Selector@ for @indexOfItemWithTag:@
indexOfItemWithTagSelector :: Selector '[CLong] CLong
indexOfItemWithTagSelector = mkSelector "indexOfItemWithTag:"

-- | @Selector@ for @indexOfItemWithRepresentedObject:@
indexOfItemWithRepresentedObjectSelector :: Selector '[RawId] CLong
indexOfItemWithRepresentedObjectSelector = mkSelector "indexOfItemWithRepresentedObject:"

-- | @Selector@ for @indexOfItemWithSubmenu:@
indexOfItemWithSubmenuSelector :: Selector '[Id NSMenu] CLong
indexOfItemWithSubmenuSelector = mkSelector "indexOfItemWithSubmenu:"

-- | @Selector@ for @indexOfItemWithTarget:andAction:@
indexOfItemWithTarget_andActionSelector :: Selector '[RawId, Sel] CLong
indexOfItemWithTarget_andActionSelector = mkSelector "indexOfItemWithTarget:andAction:"

-- | @Selector@ for @itemWithTitle:@
itemWithTitleSelector :: Selector '[Id NSString] (Id NSMenuItem)
itemWithTitleSelector = mkSelector "itemWithTitle:"

-- | @Selector@ for @itemWithTag:@
itemWithTagSelector :: Selector '[CLong] (Id NSMenuItem)
itemWithTagSelector = mkSelector "itemWithTag:"

-- | @Selector@ for @update@
updateSelector :: Selector '[] ()
updateSelector = mkSelector "update"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector '[Id NSEvent] Bool
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @itemChanged:@
itemChangedSelector :: Selector '[Id NSMenuItem] ()
itemChangedSelector = mkSelector "itemChanged:"

-- | @Selector@ for @performActionForItemAtIndex:@
performActionForItemAtIndexSelector :: Selector '[CLong] ()
performActionForItemAtIndexSelector = mkSelector "performActionForItemAtIndex:"

-- | @Selector@ for @cancelTracking@
cancelTrackingSelector :: Selector '[] ()
cancelTrackingSelector = mkSelector "cancelTracking"

-- | @Selector@ for @cancelTrackingWithoutAnimation@
cancelTrackingWithoutAnimationSelector :: Selector '[] ()
cancelTrackingWithoutAnimationSelector = mkSelector "cancelTrackingWithoutAnimation"

-- | @Selector@ for @setMenuRepresentation:@
setMenuRepresentationSelector :: Selector '[RawId] ()
setMenuRepresentationSelector = mkSelector "setMenuRepresentation:"

-- | @Selector@ for @menuRepresentation@
menuRepresentationSelector :: Selector '[] RawId
menuRepresentationSelector = mkSelector "menuRepresentation"

-- | @Selector@ for @setContextMenuRepresentation:@
setContextMenuRepresentationSelector :: Selector '[RawId] ()
setContextMenuRepresentationSelector = mkSelector "setContextMenuRepresentation:"

-- | @Selector@ for @contextMenuRepresentation@
contextMenuRepresentationSelector :: Selector '[] RawId
contextMenuRepresentationSelector = mkSelector "contextMenuRepresentation"

-- | @Selector@ for @setTearOffMenuRepresentation:@
setTearOffMenuRepresentationSelector :: Selector '[RawId] ()
setTearOffMenuRepresentationSelector = mkSelector "setTearOffMenuRepresentation:"

-- | @Selector@ for @tearOffMenuRepresentation@
tearOffMenuRepresentationSelector :: Selector '[] RawId
tearOffMenuRepresentationSelector = mkSelector "tearOffMenuRepresentation"

-- | @Selector@ for @menuZone@
menuZoneSelector :: Selector '[] (Ptr ())
menuZoneSelector = mkSelector "menuZone"

-- | @Selector@ for @setMenuZone:@
setMenuZoneSelector :: Selector '[Ptr ()] ()
setMenuZoneSelector = mkSelector "setMenuZone:"

-- | @Selector@ for @attachedMenu@
attachedMenuSelector :: Selector '[] (Id NSMenu)
attachedMenuSelector = mkSelector "attachedMenu"

-- | @Selector@ for @isAttached@
isAttachedSelector :: Selector '[] Bool
isAttachedSelector = mkSelector "isAttached"

-- | @Selector@ for @sizeToFit@
sizeToFitSelector :: Selector '[] ()
sizeToFitSelector = mkSelector "sizeToFit"

-- | @Selector@ for @locationForSubmenu:@
locationForSubmenuSelector :: Selector '[Id NSMenu] NSPoint
locationForSubmenuSelector = mkSelector "locationForSubmenu:"

-- | @Selector@ for @helpRequested:@
helpRequestedSelector :: Selector '[Id NSEvent] ()
helpRequestedSelector = mkSelector "helpRequested:"

-- | @Selector@ for @submenuAction:@
submenuActionSelector :: Selector '[RawId] ()
submenuActionSelector = mkSelector "submenuAction:"

-- | @Selector@ for @paletteMenuWithColors:titles:selectionHandler:@
paletteMenuWithColors_titles_selectionHandlerSelector :: Selector '[Id NSArray, Id NSArray, Ptr ()] (Id NSMenu)
paletteMenuWithColors_titles_selectionHandlerSelector = mkSelector "paletteMenuWithColors:titles:selectionHandler:"

-- | @Selector@ for @paletteMenuWithColors:titles:templateImage:selectionHandler:@
paletteMenuWithColors_titles_templateImage_selectionHandlerSelector :: Selector '[Id NSArray, Id NSArray, Id NSImage, Ptr ()] (Id NSMenu)
paletteMenuWithColors_titles_templateImage_selectionHandlerSelector = mkSelector "paletteMenuWithColors:titles:templateImage:selectionHandler:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @supermenu@
supermenuSelector :: Selector '[] (Id NSMenu)
supermenuSelector = mkSelector "supermenu"

-- | @Selector@ for @setSupermenu:@
setSupermenuSelector :: Selector '[Id NSMenu] ()
setSupermenuSelector = mkSelector "setSupermenu:"

-- | @Selector@ for @itemArray@
itemArraySelector :: Selector '[] (Id NSArray)
itemArraySelector = mkSelector "itemArray"

-- | @Selector@ for @setItemArray:@
setItemArraySelector :: Selector '[Id NSArray] ()
setItemArraySelector = mkSelector "setItemArray:"

-- | @Selector@ for @numberOfItems@
numberOfItemsSelector :: Selector '[] CLong
numberOfItemsSelector = mkSelector "numberOfItems"

-- | @Selector@ for @autoenablesItems@
autoenablesItemsSelector :: Selector '[] Bool
autoenablesItemsSelector = mkSelector "autoenablesItems"

-- | @Selector@ for @setAutoenablesItems:@
setAutoenablesItemsSelector :: Selector '[Bool] ()
setAutoenablesItemsSelector = mkSelector "setAutoenablesItems:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @menuBarHeight@
menuBarHeightSelector :: Selector '[] CDouble
menuBarHeightSelector = mkSelector "menuBarHeight"

-- | @Selector@ for @highlightedItem@
highlightedItemSelector :: Selector '[] (Id NSMenuItem)
highlightedItemSelector = mkSelector "highlightedItem"

-- | @Selector@ for @minimumWidth@
minimumWidthSelector :: Selector '[] CDouble
minimumWidthSelector = mkSelector "minimumWidth"

-- | @Selector@ for @setMinimumWidth:@
setMinimumWidthSelector :: Selector '[CDouble] ()
setMinimumWidthSelector = mkSelector "setMinimumWidth:"

-- | @Selector@ for @size@
sizeSelector :: Selector '[] NSSize
sizeSelector = mkSelector "size"

-- | @Selector@ for @font@
fontSelector :: Selector '[] (Id NSFont)
fontSelector = mkSelector "font"

-- | @Selector@ for @setFont:@
setFontSelector :: Selector '[Id NSFont] ()
setFontSelector = mkSelector "setFont:"

-- | @Selector@ for @allowsContextMenuPlugIns@
allowsContextMenuPlugInsSelector :: Selector '[] Bool
allowsContextMenuPlugInsSelector = mkSelector "allowsContextMenuPlugIns"

-- | @Selector@ for @setAllowsContextMenuPlugIns:@
setAllowsContextMenuPlugInsSelector :: Selector '[Bool] ()
setAllowsContextMenuPlugInsSelector = mkSelector "setAllowsContextMenuPlugIns:"

-- | @Selector@ for @automaticallyInsertsWritingToolsItems@
automaticallyInsertsWritingToolsItemsSelector :: Selector '[] Bool
automaticallyInsertsWritingToolsItemsSelector = mkSelector "automaticallyInsertsWritingToolsItems"

-- | @Selector@ for @setAutomaticallyInsertsWritingToolsItems:@
setAutomaticallyInsertsWritingToolsItemsSelector :: Selector '[Bool] ()
setAutomaticallyInsertsWritingToolsItemsSelector = mkSelector "setAutomaticallyInsertsWritingToolsItems:"

-- | @Selector@ for @showsStateColumn@
showsStateColumnSelector :: Selector '[] Bool
showsStateColumnSelector = mkSelector "showsStateColumn"

-- | @Selector@ for @setShowsStateColumn:@
setShowsStateColumnSelector :: Selector '[Bool] ()
setShowsStateColumnSelector = mkSelector "setShowsStateColumn:"

-- | @Selector@ for @userInterfaceLayoutDirection@
userInterfaceLayoutDirectionSelector :: Selector '[] NSUserInterfaceLayoutDirection
userInterfaceLayoutDirectionSelector = mkSelector "userInterfaceLayoutDirection"

-- | @Selector@ for @setUserInterfaceLayoutDirection:@
setUserInterfaceLayoutDirectionSelector :: Selector '[NSUserInterfaceLayoutDirection] ()
setUserInterfaceLayoutDirectionSelector = mkSelector "setUserInterfaceLayoutDirection:"

-- | @Selector@ for @menuChangedMessagesEnabled@
menuChangedMessagesEnabledSelector :: Selector '[] Bool
menuChangedMessagesEnabledSelector = mkSelector "menuChangedMessagesEnabled"

-- | @Selector@ for @setMenuChangedMessagesEnabled:@
setMenuChangedMessagesEnabledSelector :: Selector '[Bool] ()
setMenuChangedMessagesEnabledSelector = mkSelector "setMenuChangedMessagesEnabled:"

-- | @Selector@ for @tornOff@
tornOffSelector :: Selector '[] Bool
tornOffSelector = mkSelector "tornOff"

-- | @Selector@ for @propertiesToUpdate@
propertiesToUpdateSelector :: Selector '[] NSMenuProperties
propertiesToUpdateSelector = mkSelector "propertiesToUpdate"

-- | @Selector@ for @presentationStyle@
presentationStyleSelector :: Selector '[] NSMenuPresentationStyle
presentationStyleSelector = mkSelector "presentationStyle"

-- | @Selector@ for @setPresentationStyle:@
setPresentationStyleSelector :: Selector '[NSMenuPresentationStyle] ()
setPresentationStyleSelector = mkSelector "setPresentationStyle:"

-- | @Selector@ for @selectionMode@
selectionModeSelector :: Selector '[] NSMenuSelectionMode
selectionModeSelector = mkSelector "selectionMode"

-- | @Selector@ for @setSelectionMode:@
setSelectionModeSelector :: Selector '[NSMenuSelectionMode] ()
setSelectionModeSelector = mkSelector "setSelectionMode:"

-- | @Selector@ for @selectedItems@
selectedItemsSelector :: Selector '[] (Id NSArray)
selectedItemsSelector = mkSelector "selectedItems"

-- | @Selector@ for @setSelectedItems:@
setSelectedItemsSelector :: Selector '[Id NSArray] ()
setSelectedItemsSelector = mkSelector "setSelectedItems:"

