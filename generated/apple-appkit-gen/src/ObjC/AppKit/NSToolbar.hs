{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSToolbar@.
module ObjC.AppKit.NSToolbar
  ( NSToolbar
  , IsNSToolbar(..)
  , initWithIdentifier
  , init_
  , insertItemWithItemIdentifier_atIndex
  , removeItemAtIndex
  , removeItemWithItemIdentifier
  , runCustomizationPalette
  , validateVisibleItems
  , setConfigurationFromDictionary
  , delegate
  , setDelegate
  , visible
  , setVisible
  , customizationPaletteIsRunning
  , displayMode
  , setDisplayMode
  , selectedItemIdentifier
  , setSelectedItemIdentifier
  , allowsUserCustomization
  , setAllowsUserCustomization
  , allowsDisplayModeCustomization
  , setAllowsDisplayModeCustomization
  , identifier
  , items
  , visibleItems
  , itemIdentifiers
  , setItemIdentifiers
  , centeredItemIdentifiers
  , setCenteredItemIdentifiers
  , autosavesConfiguration
  , setAutosavesConfiguration
  , allowsExtensionItems
  , setAllowsExtensionItems
  , sizeMode
  , setSizeMode
  , centeredItemIdentifier
  , setCenteredItemIdentifier
  , fullScreenAccessoryView
  , setFullScreenAccessoryView
  , fullScreenAccessoryViewMinHeight
  , setFullScreenAccessoryViewMinHeight
  , fullScreenAccessoryViewMaxHeight
  , setFullScreenAccessoryViewMaxHeight
  , showsBaselineSeparator
  , setShowsBaselineSeparator
  , configurationDictionary
  , initWithIdentifierSelector
  , initSelector
  , insertItemWithItemIdentifier_atIndexSelector
  , removeItemAtIndexSelector
  , removeItemWithItemIdentifierSelector
  , runCustomizationPaletteSelector
  , validateVisibleItemsSelector
  , setConfigurationFromDictionarySelector
  , delegateSelector
  , setDelegateSelector
  , visibleSelector
  , setVisibleSelector
  , customizationPaletteIsRunningSelector
  , displayModeSelector
  , setDisplayModeSelector
  , selectedItemIdentifierSelector
  , setSelectedItemIdentifierSelector
  , allowsUserCustomizationSelector
  , setAllowsUserCustomizationSelector
  , allowsDisplayModeCustomizationSelector
  , setAllowsDisplayModeCustomizationSelector
  , identifierSelector
  , itemsSelector
  , visibleItemsSelector
  , itemIdentifiersSelector
  , setItemIdentifiersSelector
  , centeredItemIdentifiersSelector
  , setCenteredItemIdentifiersSelector
  , autosavesConfigurationSelector
  , setAutosavesConfigurationSelector
  , allowsExtensionItemsSelector
  , setAllowsExtensionItemsSelector
  , sizeModeSelector
  , setSizeModeSelector
  , centeredItemIdentifierSelector
  , setCenteredItemIdentifierSelector
  , fullScreenAccessoryViewSelector
  , setFullScreenAccessoryViewSelector
  , fullScreenAccessoryViewMinHeightSelector
  , setFullScreenAccessoryViewMinHeightSelector
  , fullScreenAccessoryViewMaxHeightSelector
  , setFullScreenAccessoryViewMaxHeightSelector
  , showsBaselineSeparatorSelector
  , setShowsBaselineSeparatorSelector
  , configurationDictionarySelector

  -- * Enum types
  , NSToolbarDisplayMode(NSToolbarDisplayMode)
  , pattern NSToolbarDisplayModeDefault
  , pattern NSToolbarDisplayModeIconAndLabel
  , pattern NSToolbarDisplayModeIconOnly
  , pattern NSToolbarDisplayModeLabelOnly
  , NSToolbarSizeMode(NSToolbarSizeMode)
  , pattern NSToolbarSizeModeDefault
  , pattern NSToolbarSizeModeRegular
  , pattern NSToolbarSizeModeSmall

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

-- | The identifier is used to form the toolbar's autosave name. Toolbars with the same identifier are implicitly synchronized so that they maintain the same state.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsNSToolbar nsToolbar, IsNSString identifier) => nsToolbar -> identifier -> IO (Id NSToolbar)
initWithIdentifier nsToolbar  identifier =
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg nsToolbar (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | Calls through to -initWithIdentifier: with an empty string identifier. Customizable toolbars should use @-initWithIdentifier:@ with a unique identifier instead.
--
-- ObjC selector: @- init@
init_ :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSToolbar)
init_ nsToolbar  =
    sendMsg nsToolbar (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Inserts an item with the specified identifier in the receiving toolbar at the specified index.
--
-- Any change made will be propagated immediately to all other toolbars with the same identifier.
--
-- ObjC selector: @- insertItemWithItemIdentifier:atIndex:@
insertItemWithItemIdentifier_atIndex :: (IsNSToolbar nsToolbar, IsNSString itemIdentifier) => nsToolbar -> itemIdentifier -> CLong -> IO ()
insertItemWithItemIdentifier_atIndex nsToolbar  itemIdentifier index =
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsToolbar (mkSelector "insertItemWithItemIdentifier:atIndex:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ()), argCLong index]

-- | Removes the item at the specified index in the receiving toolbar.
--
-- Any change made will be propagated immediately to all other toolbars with the same identifier.
--
-- ObjC selector: @- removeItemAtIndex:@
removeItemAtIndex :: IsNSToolbar nsToolbar => nsToolbar -> CLong -> IO ()
removeItemAtIndex nsToolbar  index =
    sendMsg nsToolbar (mkSelector "removeItemAtIndex:") retVoid [argCLong index]

-- | Removes the item with matching @itemIdentifier@ in the receiving toolbar. If multiple items share the same identifier (as is the case with space items) all matching items will be removed. To remove only a single space item, use @-removeItemAtIndex:@ instead.
--
-- Any change made will be propagated immediately to all other toolbars with the same identifier.
--
-- ObjC selector: @- removeItemWithItemIdentifier:@
removeItemWithItemIdentifier :: (IsNSToolbar nsToolbar, IsNSString itemIdentifier) => nsToolbar -> itemIdentifier -> IO ()
removeItemWithItemIdentifier nsToolbar  itemIdentifier =
  withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
      sendMsg nsToolbar (mkSelector "removeItemWithItemIdentifier:") retVoid [argPtr (castPtr raw_itemIdentifier :: Ptr ())]

-- | Customizable toolbars (those with delegates) can show a palette which allows users to populate the toolbar with individual items or to reset the toolbar to some default set of items. The items and item sets in the palette are specified by the delegate (@-toolbarAllowedItemIdentifiers:@ and @-toolbarDefaultItemIdentifiers:@). When the user is done configuring, they will dismiss the palette.
--
-- ObjC selector: @- runCustomizationPalette:@
runCustomizationPalette :: IsNSToolbar nsToolbar => nsToolbar -> RawId -> IO ()
runCustomizationPalette nsToolbar  sender =
    sendMsg nsToolbar (mkSelector "runCustomizationPalette:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | Typically you should not invoke this method. This method is called on window updates with the purpose of validating each of the visible items. The toolbar will iterate through the list of visible items, sending each a @-validate@ message. If this method is invoked directly, all visible items including those with @autovalidates@ disabled will get a @-validate@ message.
--
-- ObjC selector: @- validateVisibleItems@
validateVisibleItems :: IsNSToolbar nsToolbar => nsToolbar -> IO ()
validateVisibleItems nsToolbar  =
    sendMsg nsToolbar (mkSelector "validateVisibleItems") retVoid []

-- | @- setConfigurationFromDictionary:@
setConfigurationFromDictionary :: (IsNSToolbar nsToolbar, IsNSDictionary configDict) => nsToolbar -> configDict -> IO ()
setConfigurationFromDictionary nsToolbar  configDict =
  withObjCPtr configDict $ \raw_configDict ->
      sendMsg nsToolbar (mkSelector "setConfigurationFromDictionary:") retVoid [argPtr (castPtr raw_configDict :: Ptr ())]

-- | Customizable toolbars must have a delegate, and must implement the required @NSToolbarDelegate@ methods.
--
-- ObjC selector: @- delegate@
delegate :: IsNSToolbar nsToolbar => nsToolbar -> IO RawId
delegate nsToolbar  =
    fmap (RawId . castPtr) $ sendMsg nsToolbar (mkSelector "delegate") (retPtr retVoid) []

-- | Customizable toolbars must have a delegate, and must implement the required @NSToolbarDelegate@ methods.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNSToolbar nsToolbar => nsToolbar -> RawId -> IO ()
setDelegate nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Toggles the visibility of the toolbar. This property may be modified by the user in toolbars with @allowsUserCustomization@ enabled. This property is key value observable on macOS 14.0 and higher.
--
-- ObjC selector: @- visible@
visible :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
visible nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "visible") retCULong []

-- | Toggles the visibility of the toolbar. This property may be modified by the user in toolbars with @allowsUserCustomization@ enabled. This property is key value observable on macOS 14.0 and higher.
--
-- ObjC selector: @- setVisible:@
setVisible :: IsNSToolbar nsToolbar => nsToolbar -> Bool -> IO ()
setVisible nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setVisible:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether or not the customization palette is currently running. On macOS 15.0 and above this property is key value observable.
--
-- ObjC selector: @- customizationPaletteIsRunning@
customizationPaletteIsRunning :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
customizationPaletteIsRunning nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "customizationPaletteIsRunning") retCULong []

-- | The current display mode of items in the toolbar. In toolbars with @allowsDisplayModeCustomization@ enabled this is a user modifiable property. This property is key value observable.
--
-- ObjC selector: @- displayMode@
displayMode :: IsNSToolbar nsToolbar => nsToolbar -> IO NSToolbarDisplayMode
displayMode nsToolbar  =
    fmap (coerce :: CULong -> NSToolbarDisplayMode) $ sendMsg nsToolbar (mkSelector "displayMode") retCULong []

-- | The current display mode of items in the toolbar. In toolbars with @allowsDisplayModeCustomization@ enabled this is a user modifiable property. This property is key value observable.
--
-- ObjC selector: @- setDisplayMode:@
setDisplayMode :: IsNSToolbar nsToolbar => nsToolbar -> NSToolbarDisplayMode -> IO ()
setDisplayMode nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setDisplayMode:") retVoid [argCULong (coerce value)]

-- | Sets the toolbar's selected item by identifier. Use this to force an item identifier to be selected. Toolbar manages selection of image items automatically. This method can be used to select identifiers of custom view items, or to force a selection change. See @-toolbarSelectableItemIdentifiers:@ delegate method for more details. This property is key value observable.
--
-- ObjC selector: @- selectedItemIdentifier@
selectedItemIdentifier :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSString)
selectedItemIdentifier nsToolbar  =
    sendMsg nsToolbar (mkSelector "selectedItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Sets the toolbar's selected item by identifier. Use this to force an item identifier to be selected. Toolbar manages selection of image items automatically. This method can be used to select identifiers of custom view items, or to force a selection change. See @-toolbarSelectableItemIdentifiers:@ delegate method for more details. This property is key value observable.
--
-- ObjC selector: @- setSelectedItemIdentifier:@
setSelectedItemIdentifier :: (IsNSToolbar nsToolbar, IsNSString value) => nsToolbar -> value -> IO ()
setSelectedItemIdentifier nsToolbar  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsToolbar (mkSelector "setSelectedItemIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This flag controls whether or not users can configure the toolbar by dragging items around, and whether or not the customization palette can be used. The default value is NO, but can be changed at any time. For instance, a developer may not want users to be able to edit the toolbar while some event is being processed.
--
-- ObjC selector: @- allowsUserCustomization@
allowsUserCustomization :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
allowsUserCustomization nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "allowsUserCustomization") retCULong []

-- | This flag controls whether or not users can configure the toolbar by dragging items around, and whether or not the customization palette can be used. The default value is NO, but can be changed at any time. For instance, a developer may not want users to be able to edit the toolbar while some event is being processed.
--
-- ObjC selector: @- setAllowsUserCustomization:@
setAllowsUserCustomization :: IsNSToolbar nsToolbar => nsToolbar -> Bool -> IO ()
setAllowsUserCustomization nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setAllowsUserCustomization:") retVoid [argCULong (if value then 1 else 0)]

-- | Whether or not the user is allowed to change display modes at run time. This functionality is independent of customizing the order of the items themselves. Only disable when the functionality or legibility of your toolbar could not be improved by another display mode. The user's selection will be persisted using the toolbar's @identifier@ when @autosavesConfiguration@ is enabled. The default is YES for apps linked on macOS 15.0 and above.
--
-- ObjC selector: @- allowsDisplayModeCustomization@
allowsDisplayModeCustomization :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
allowsDisplayModeCustomization nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "allowsDisplayModeCustomization") retCULong []

-- | Whether or not the user is allowed to change display modes at run time. This functionality is independent of customizing the order of the items themselves. Only disable when the functionality or legibility of your toolbar could not be improved by another display mode. The user's selection will be persisted using the toolbar's @identifier@ when @autosavesConfiguration@ is enabled. The default is YES for apps linked on macOS 15.0 and above.
--
-- ObjC selector: @- setAllowsDisplayModeCustomization:@
setAllowsDisplayModeCustomization :: IsNSToolbar nsToolbar => nsToolbar -> Bool -> IO ()
setAllowsDisplayModeCustomization nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setAllowsDisplayModeCustomization:") retVoid [argCULong (if value then 1 else 0)]

-- | All toolbars with the same name will share the same display attributes, and item order. If a toolbar autosaves its configuration, the item identifier will be used as the autosave name.
--
-- ObjC selector: @- identifier@
identifier :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSString)
identifier nsToolbar  =
    sendMsg nsToolbar (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Allows you to access all current items in the toolbar.
--
-- ObjC selector: @- items@
items :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSArray)
items nsToolbar  =
    sendMsg nsToolbar (mkSelector "items") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Allows you to access the current visible items (non clipped).
--
-- ObjC selector: @- visibleItems@
visibleItems :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSArray)
visibleItems nsToolbar  =
    sendMsg nsToolbar (mkSelector "visibleItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of itemIdentifiers that represent the current items in the toolbar. Setting this property will set the current items in the toolbar by diffing against items that already exist. Use this with great caution if @allowsUserCustomization@ is enabled as it will override any customizations the user has made. This property is key value observable.
--
-- ObjC selector: @- itemIdentifiers@
itemIdentifiers :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSArray)
itemIdentifiers nsToolbar  =
    sendMsg nsToolbar (mkSelector "itemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of itemIdentifiers that represent the current items in the toolbar. Setting this property will set the current items in the toolbar by diffing against items that already exist. Use this with great caution if @allowsUserCustomization@ is enabled as it will override any customizations the user has made. This property is key value observable.
--
-- ObjC selector: @- setItemIdentifiers:@
setItemIdentifiers :: (IsNSToolbar nsToolbar, IsNSArray value) => nsToolbar -> value -> IO ()
setItemIdentifiers nsToolbar  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsToolbar (mkSelector "setItemIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Items with centered identifiers will be centered together in the Toolbar relative to the window assuming space allows. The order of items is initially defined by the default set of identifiers, but may be customized by the user. Centered items may not be moved outside of the center set of items by the user. This property is archived.
--
-- ObjC selector: @- centeredItemIdentifiers@
centeredItemIdentifiers :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSSet)
centeredItemIdentifiers nsToolbar  =
    sendMsg nsToolbar (mkSelector "centeredItemIdentifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Items with centered identifiers will be centered together in the Toolbar relative to the window assuming space allows. The order of items is initially defined by the default set of identifiers, but may be customized by the user. Centered items may not be moved outside of the center set of items by the user. This property is archived.
--
-- ObjC selector: @- setCenteredItemIdentifiers:@
setCenteredItemIdentifiers :: (IsNSToolbar nsToolbar, IsNSSet value) => nsToolbar -> value -> IO ()
setCenteredItemIdentifiers nsToolbar  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsToolbar (mkSelector "setCenteredItemIdentifiers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | If @autosavesConfiguration@ is YES, the toolbar will automatically write changes the user makes to user defaults. Customizable toolbars will want to set this flag to YES. Setting this to NO means changes in configuration are not written automatically, however you can use the @configurationDictionary@ method to do it yourself. Default is NO.
--
-- ObjC selector: @- autosavesConfiguration@
autosavesConfiguration :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
autosavesConfiguration nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "autosavesConfiguration") retCULong []

-- | If @autosavesConfiguration@ is YES, the toolbar will automatically write changes the user makes to user defaults. Customizable toolbars will want to set this flag to YES. Setting this to NO means changes in configuration are not written automatically, however you can use the @configurationDictionary@ method to do it yourself. Default is NO.
--
-- ObjC selector: @- setAutosavesConfiguration:@
setAutosavesConfiguration :: IsNSToolbar nsToolbar => nsToolbar -> Bool -> IO ()
setAutosavesConfiguration nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setAutosavesConfiguration:") retVoid [argCULong (if value then 1 else 0)]

-- | When YES, the receiver can dynamically create toolbar items for Action extensions in the toolbar configuration panel. To be included, an extension needs to declare NSExtensionServiceAllowsToolbarItem=YES in its Info.plist. The default value is NO.
--
-- ObjC selector: @- allowsExtensionItems@
allowsExtensionItems :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
allowsExtensionItems nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "allowsExtensionItems") retCULong []

-- | When YES, the receiver can dynamically create toolbar items for Action extensions in the toolbar configuration panel. To be included, an extension needs to declare NSExtensionServiceAllowsToolbarItem=YES in its Info.plist. The default value is NO.
--
-- ObjC selector: @- setAllowsExtensionItems:@
setAllowsExtensionItems :: IsNSToolbar nsToolbar => nsToolbar -> Bool -> IO ()
setAllowsExtensionItems nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setAllowsExtensionItems:") retVoid [argCULong (if value then 1 else 0)]

-- | @- sizeMode@
sizeMode :: IsNSToolbar nsToolbar => nsToolbar -> IO NSToolbarSizeMode
sizeMode nsToolbar  =
    fmap (coerce :: CULong -> NSToolbarSizeMode) $ sendMsg nsToolbar (mkSelector "sizeMode") retCULong []

-- | @- setSizeMode:@
setSizeMode :: IsNSToolbar nsToolbar => nsToolbar -> NSToolbarSizeMode -> IO ()
setSizeMode nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setSizeMode:") retVoid [argCULong (coerce value)]

-- | @- centeredItemIdentifier@
centeredItemIdentifier :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSString)
centeredItemIdentifier nsToolbar  =
    sendMsg nsToolbar (mkSelector "centeredItemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCenteredItemIdentifier:@
setCenteredItemIdentifier :: (IsNSToolbar nsToolbar, IsNSString value) => nsToolbar -> value -> IO ()
setCenteredItemIdentifier nsToolbar  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsToolbar (mkSelector "setCenteredItemIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullScreenAccessoryView@
fullScreenAccessoryView :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSView)
fullScreenAccessoryView nsToolbar  =
    sendMsg nsToolbar (mkSelector "fullScreenAccessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFullScreenAccessoryView:@
setFullScreenAccessoryView :: (IsNSToolbar nsToolbar, IsNSView value) => nsToolbar -> value -> IO ()
setFullScreenAccessoryView nsToolbar  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsToolbar (mkSelector "setFullScreenAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fullScreenAccessoryViewMinHeight@
fullScreenAccessoryViewMinHeight :: IsNSToolbar nsToolbar => nsToolbar -> IO CDouble
fullScreenAccessoryViewMinHeight nsToolbar  =
    sendMsg nsToolbar (mkSelector "fullScreenAccessoryViewMinHeight") retCDouble []

-- | @- setFullScreenAccessoryViewMinHeight:@
setFullScreenAccessoryViewMinHeight :: IsNSToolbar nsToolbar => nsToolbar -> CDouble -> IO ()
setFullScreenAccessoryViewMinHeight nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setFullScreenAccessoryViewMinHeight:") retVoid [argCDouble value]

-- | @- fullScreenAccessoryViewMaxHeight@
fullScreenAccessoryViewMaxHeight :: IsNSToolbar nsToolbar => nsToolbar -> IO CDouble
fullScreenAccessoryViewMaxHeight nsToolbar  =
    sendMsg nsToolbar (mkSelector "fullScreenAccessoryViewMaxHeight") retCDouble []

-- | @- setFullScreenAccessoryViewMaxHeight:@
setFullScreenAccessoryViewMaxHeight :: IsNSToolbar nsToolbar => nsToolbar -> CDouble -> IO ()
setFullScreenAccessoryViewMaxHeight nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setFullScreenAccessoryViewMaxHeight:") retVoid [argCDouble value]

-- | @- showsBaselineSeparator@
showsBaselineSeparator :: IsNSToolbar nsToolbar => nsToolbar -> IO Bool
showsBaselineSeparator nsToolbar  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbar (mkSelector "showsBaselineSeparator") retCULong []

-- | @- setShowsBaselineSeparator:@
setShowsBaselineSeparator :: IsNSToolbar nsToolbar => nsToolbar -> Bool -> IO ()
setShowsBaselineSeparator nsToolbar  value =
    sendMsg nsToolbar (mkSelector "setShowsBaselineSeparator:") retVoid [argCULong (if value then 1 else 0)]

-- | @- configurationDictionary@
configurationDictionary :: IsNSToolbar nsToolbar => nsToolbar -> IO (Id NSDictionary)
configurationDictionary nsToolbar  =
    sendMsg nsToolbar (mkSelector "configurationDictionary") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @insertItemWithItemIdentifier:atIndex:@
insertItemWithItemIdentifier_atIndexSelector :: Selector
insertItemWithItemIdentifier_atIndexSelector = mkSelector "insertItemWithItemIdentifier:atIndex:"

-- | @Selector@ for @removeItemAtIndex:@
removeItemAtIndexSelector :: Selector
removeItemAtIndexSelector = mkSelector "removeItemAtIndex:"

-- | @Selector@ for @removeItemWithItemIdentifier:@
removeItemWithItemIdentifierSelector :: Selector
removeItemWithItemIdentifierSelector = mkSelector "removeItemWithItemIdentifier:"

-- | @Selector@ for @runCustomizationPalette:@
runCustomizationPaletteSelector :: Selector
runCustomizationPaletteSelector = mkSelector "runCustomizationPalette:"

-- | @Selector@ for @validateVisibleItems@
validateVisibleItemsSelector :: Selector
validateVisibleItemsSelector = mkSelector "validateVisibleItems"

-- | @Selector@ for @setConfigurationFromDictionary:@
setConfigurationFromDictionarySelector :: Selector
setConfigurationFromDictionarySelector = mkSelector "setConfigurationFromDictionary:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @setVisible:@
setVisibleSelector :: Selector
setVisibleSelector = mkSelector "setVisible:"

-- | @Selector@ for @customizationPaletteIsRunning@
customizationPaletteIsRunningSelector :: Selector
customizationPaletteIsRunningSelector = mkSelector "customizationPaletteIsRunning"

-- | @Selector@ for @displayMode@
displayModeSelector :: Selector
displayModeSelector = mkSelector "displayMode"

-- | @Selector@ for @setDisplayMode:@
setDisplayModeSelector :: Selector
setDisplayModeSelector = mkSelector "setDisplayMode:"

-- | @Selector@ for @selectedItemIdentifier@
selectedItemIdentifierSelector :: Selector
selectedItemIdentifierSelector = mkSelector "selectedItemIdentifier"

-- | @Selector@ for @setSelectedItemIdentifier:@
setSelectedItemIdentifierSelector :: Selector
setSelectedItemIdentifierSelector = mkSelector "setSelectedItemIdentifier:"

-- | @Selector@ for @allowsUserCustomization@
allowsUserCustomizationSelector :: Selector
allowsUserCustomizationSelector = mkSelector "allowsUserCustomization"

-- | @Selector@ for @setAllowsUserCustomization:@
setAllowsUserCustomizationSelector :: Selector
setAllowsUserCustomizationSelector = mkSelector "setAllowsUserCustomization:"

-- | @Selector@ for @allowsDisplayModeCustomization@
allowsDisplayModeCustomizationSelector :: Selector
allowsDisplayModeCustomizationSelector = mkSelector "allowsDisplayModeCustomization"

-- | @Selector@ for @setAllowsDisplayModeCustomization:@
setAllowsDisplayModeCustomizationSelector :: Selector
setAllowsDisplayModeCustomizationSelector = mkSelector "setAllowsDisplayModeCustomization:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @items@
itemsSelector :: Selector
itemsSelector = mkSelector "items"

-- | @Selector@ for @visibleItems@
visibleItemsSelector :: Selector
visibleItemsSelector = mkSelector "visibleItems"

-- | @Selector@ for @itemIdentifiers@
itemIdentifiersSelector :: Selector
itemIdentifiersSelector = mkSelector "itemIdentifiers"

-- | @Selector@ for @setItemIdentifiers:@
setItemIdentifiersSelector :: Selector
setItemIdentifiersSelector = mkSelector "setItemIdentifiers:"

-- | @Selector@ for @centeredItemIdentifiers@
centeredItemIdentifiersSelector :: Selector
centeredItemIdentifiersSelector = mkSelector "centeredItemIdentifiers"

-- | @Selector@ for @setCenteredItemIdentifiers:@
setCenteredItemIdentifiersSelector :: Selector
setCenteredItemIdentifiersSelector = mkSelector "setCenteredItemIdentifiers:"

-- | @Selector@ for @autosavesConfiguration@
autosavesConfigurationSelector :: Selector
autosavesConfigurationSelector = mkSelector "autosavesConfiguration"

-- | @Selector@ for @setAutosavesConfiguration:@
setAutosavesConfigurationSelector :: Selector
setAutosavesConfigurationSelector = mkSelector "setAutosavesConfiguration:"

-- | @Selector@ for @allowsExtensionItems@
allowsExtensionItemsSelector :: Selector
allowsExtensionItemsSelector = mkSelector "allowsExtensionItems"

-- | @Selector@ for @setAllowsExtensionItems:@
setAllowsExtensionItemsSelector :: Selector
setAllowsExtensionItemsSelector = mkSelector "setAllowsExtensionItems:"

-- | @Selector@ for @sizeMode@
sizeModeSelector :: Selector
sizeModeSelector = mkSelector "sizeMode"

-- | @Selector@ for @setSizeMode:@
setSizeModeSelector :: Selector
setSizeModeSelector = mkSelector "setSizeMode:"

-- | @Selector@ for @centeredItemIdentifier@
centeredItemIdentifierSelector :: Selector
centeredItemIdentifierSelector = mkSelector "centeredItemIdentifier"

-- | @Selector@ for @setCenteredItemIdentifier:@
setCenteredItemIdentifierSelector :: Selector
setCenteredItemIdentifierSelector = mkSelector "setCenteredItemIdentifier:"

-- | @Selector@ for @fullScreenAccessoryView@
fullScreenAccessoryViewSelector :: Selector
fullScreenAccessoryViewSelector = mkSelector "fullScreenAccessoryView"

-- | @Selector@ for @setFullScreenAccessoryView:@
setFullScreenAccessoryViewSelector :: Selector
setFullScreenAccessoryViewSelector = mkSelector "setFullScreenAccessoryView:"

-- | @Selector@ for @fullScreenAccessoryViewMinHeight@
fullScreenAccessoryViewMinHeightSelector :: Selector
fullScreenAccessoryViewMinHeightSelector = mkSelector "fullScreenAccessoryViewMinHeight"

-- | @Selector@ for @setFullScreenAccessoryViewMinHeight:@
setFullScreenAccessoryViewMinHeightSelector :: Selector
setFullScreenAccessoryViewMinHeightSelector = mkSelector "setFullScreenAccessoryViewMinHeight:"

-- | @Selector@ for @fullScreenAccessoryViewMaxHeight@
fullScreenAccessoryViewMaxHeightSelector :: Selector
fullScreenAccessoryViewMaxHeightSelector = mkSelector "fullScreenAccessoryViewMaxHeight"

-- | @Selector@ for @setFullScreenAccessoryViewMaxHeight:@
setFullScreenAccessoryViewMaxHeightSelector :: Selector
setFullScreenAccessoryViewMaxHeightSelector = mkSelector "setFullScreenAccessoryViewMaxHeight:"

-- | @Selector@ for @showsBaselineSeparator@
showsBaselineSeparatorSelector :: Selector
showsBaselineSeparatorSelector = mkSelector "showsBaselineSeparator"

-- | @Selector@ for @setShowsBaselineSeparator:@
setShowsBaselineSeparatorSelector :: Selector
setShowsBaselineSeparatorSelector = mkSelector "setShowsBaselineSeparator:"

-- | @Selector@ for @configurationDictionary@
configurationDictionarySelector :: Selector
configurationDictionarySelector = mkSelector "configurationDictionary"

