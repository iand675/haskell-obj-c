{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSToolbarItem@.
module ObjC.AppKit.NSToolbarItem
  ( NSToolbarItem
  , IsNSToolbarItem(..)
  , initWithItemIdentifier
  , validate
  , itemIdentifier
  , toolbar
  , label
  , setLabel
  , paletteLabel
  , setPaletteLabel
  , possibleLabels
  , setPossibleLabels
  , toolTip
  , setToolTip
  , menuFormRepresentation
  , setMenuFormRepresentation
  , tag
  , setTag
  , target
  , setTarget
  , action
  , setAction
  , enabled
  , setEnabled
  , image
  , setImage
  , title
  , setTitle
  , bordered
  , setBordered
  , backgroundTintColor
  , setBackgroundTintColor
  , style
  , setStyle
  , navigational
  , setNavigational
  , view
  , setView
  , visible
  , hidden
  , setHidden
  , minSize
  , setMinSize
  , maxSize
  , setMaxSize
  , visibilityPriority
  , setVisibilityPriority
  , badge
  , setBadge
  , autovalidates
  , setAutovalidates
  , allowsDuplicatesInToolbar
  , actionSelector
  , allowsDuplicatesInToolbarSelector
  , autovalidatesSelector
  , backgroundTintColorSelector
  , badgeSelector
  , borderedSelector
  , enabledSelector
  , hiddenSelector
  , imageSelector
  , initWithItemIdentifierSelector
  , itemIdentifierSelector
  , labelSelector
  , maxSizeSelector
  , menuFormRepresentationSelector
  , minSizeSelector
  , navigationalSelector
  , paletteLabelSelector
  , possibleLabelsSelector
  , setActionSelector
  , setAutovalidatesSelector
  , setBackgroundTintColorSelector
  , setBadgeSelector
  , setBorderedSelector
  , setEnabledSelector
  , setHiddenSelector
  , setImageSelector
  , setLabelSelector
  , setMaxSizeSelector
  , setMenuFormRepresentationSelector
  , setMinSizeSelector
  , setNavigationalSelector
  , setPaletteLabelSelector
  , setPossibleLabelsSelector
  , setStyleSelector
  , setTagSelector
  , setTargetSelector
  , setTitleSelector
  , setToolTipSelector
  , setViewSelector
  , setVisibilityPrioritySelector
  , styleSelector
  , tagSelector
  , targetSelector
  , titleSelector
  , toolTipSelector
  , toolbarSelector
  , validateSelector
  , viewSelector
  , visibilityPrioritySelector
  , visibleSelector

  -- * Enum types
  , NSToolbarItemStyle(NSToolbarItemStyle)
  , pattern NSToolbarItemStylePlain
  , pattern NSToolbarItemStyleProminent

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

-- | Initialize the toolbar item with an identifier which is a development language string used by the toolbar and its delegate for identification purposes.
--
-- ObjC selector: @- initWithItemIdentifier:@
initWithItemIdentifier :: (IsNSToolbarItem nsToolbarItem, IsNSString itemIdentifier) => nsToolbarItem -> itemIdentifier -> IO (Id NSToolbarItem)
initWithItemIdentifier nsToolbarItem itemIdentifier =
  sendOwnedMessage nsToolbarItem initWithItemIdentifierSelector (toNSString itemIdentifier)

-- | Typically you should not invoke this method. This method is called by its toolbar during validation. Standard items validate themselves by sending the @-validateToolbarItem:@ validate message to the current validator. Since items with custom views don't always have meaningful target/actions, they do nothing. So for your custom items it may be useful to override this method and invent your own validation.
--
-- ObjC selector: @- validate@
validate :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO ()
validate nsToolbarItem =
  sendMessage nsToolbarItem validateSelector

-- | @- itemIdentifier@
itemIdentifier :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
itemIdentifier nsToolbarItem =
  sendMessage nsToolbarItem itemIdentifierSelector

-- | Use this to determine the toolbar in which an item is currently displayed.
--
-- ObjC selector: @- toolbar@
toolbar :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSToolbar)
toolbar nsToolbarItem =
  sendMessage nsToolbarItem toolbarSelector

-- | Use this to set the item's label that appears in the toolbar. The label may also be used for the default @menuFormRepresentation@ of the item. Also, developers should make sure the length of the label is appropriate and not too long.
--
-- ObjC selector: @- label@
label :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
label nsToolbarItem =
  sendMessage nsToolbarItem labelSelector

-- | Use this to set the item's label that appears in the toolbar. The label may also be used for the default @menuFormRepresentation@ of the item. Also, developers should make sure the length of the label is appropriate and not too long.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setLabel nsToolbarItem value =
  sendMessage nsToolbarItem setLabelSelector (toNSString value)

-- | Use this to set the item's label that appears when the item is in the customization palette. All Items must have a palette label, and for most things it is reasonable to set them to the same string as the label used in the toolbar.
--
-- ObjC selector: @- paletteLabel@
paletteLabel :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
paletteLabel nsToolbarItem =
  sendMessage nsToolbarItem paletteLabelSelector

-- | Use this to set the item's label that appears when the item is in the customization palette. All Items must have a palette label, and for most things it is reasonable to set them to the same string as the label used in the toolbar.
--
-- ObjC selector: @- setPaletteLabel:@
setPaletteLabel :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setPaletteLabel nsToolbarItem value =
  sendMessage nsToolbarItem setPaletteLabelSelector (toNSString value)

-- | An array of all alternate labels this item may display. The item will use the size of the longest label to prevent resizing when the label is changed.
--
-- ObjC selector: @- possibleLabels@
possibleLabels :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSSet)
possibleLabels nsToolbarItem =
  sendMessage nsToolbarItem possibleLabelsSelector

-- | An array of all alternate labels this item may display. The item will use the size of the longest label to prevent resizing when the label is changed.
--
-- ObjC selector: @- setPossibleLabels:@
setPossibleLabels :: (IsNSToolbarItem nsToolbarItem, IsNSSet value) => nsToolbarItem -> value -> IO ()
setPossibleLabels nsToolbarItem value =
  sendMessage nsToolbarItem setPossibleLabelsSelector (toNSSet value)

-- | Use this to set a tooltip to be used when the item is displayed in the toolbar. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- toolTip@
toolTip :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
toolTip nsToolbarItem =
  sendMessage nsToolbarItem toolTipSelector

-- | Use this to set a tooltip to be used when the item is displayed in the toolbar. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setToolTip:@
setToolTip :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setToolTip nsToolbarItem value =
  sendMessage nsToolbarItem setToolTipSelector (toNSString value)

-- | The menu form of a toolbar item's purpose is twofold. First, when the window is too small to display an item, it will be clipped but remain accessible from a "clipped items" menu containing the menu item returned here. Second, in text only mode, the menu returned will be used to create the displayed items. Singleton menu items will be clickable, while submenu items will be represented as a pull down. For instance, say you want a button that allows you to switch between modes A, B, and C. You could represent this as a menu by: a menu item "mode" with three submenu items "A", "B", and "C". By default, this method returns a singleton menu item with item label as the title. For standard items, the target, action is set.
--
-- ObjC selector: @- menuFormRepresentation@
menuFormRepresentation :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSMenuItem)
menuFormRepresentation nsToolbarItem =
  sendMessage nsToolbarItem menuFormRepresentationSelector

-- | The menu form of a toolbar item's purpose is twofold. First, when the window is too small to display an item, it will be clipped but remain accessible from a "clipped items" menu containing the menu item returned here. Second, in text only mode, the menu returned will be used to create the displayed items. Singleton menu items will be clickable, while submenu items will be represented as a pull down. For instance, say you want a button that allows you to switch between modes A, B, and C. You could represent this as a menu by: a menu item "mode" with three submenu items "A", "B", and "C". By default, this method returns a singleton menu item with item label as the title. For standard items, the target, action is set.
--
-- ObjC selector: @- setMenuFormRepresentation:@
setMenuFormRepresentation :: (IsNSToolbarItem nsToolbarItem, IsNSMenuItem value) => nsToolbarItem -> value -> IO ()
setMenuFormRepresentation nsToolbarItem value =
  sendMessage nsToolbarItem setMenuFormRepresentationSelector (toNSMenuItem value)

-- | Tag for your own custom purpose. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- tag@
tag :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO CLong
tag nsToolbarItem =
  sendMessage nsToolbarItem tagSelector

-- | Tag for your own custom purpose. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setTag:@
setTag :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> CLong -> IO ()
setTag nsToolbarItem value =
  sendMessage nsToolbarItem setTagSelector value

-- | Set and get the action of an item. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- target@
target :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO RawId
target nsToolbarItem =
  sendMessage nsToolbarItem targetSelector

-- | Set and get the action of an item. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setTarget:@
setTarget :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> RawId -> IO ()
setTarget nsToolbarItem value =
  sendMessage nsToolbarItem setTargetSelector value

-- | Set and get the action of an item. For custom views, this method will call @-setAction:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- action@
action :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Sel
action nsToolbarItem =
  sendMessage nsToolbarItem actionSelector

-- | Set and get the action of an item. For custom views, this method will call @-setAction:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setAction:@
setAction :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Sel -> IO ()
setAction nsToolbarItem value =
  sendMessage nsToolbarItem setActionSelector value

-- | Set and get the enabled flag of an item. For custom views, this method will call @-setEnabled:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- enabled@
enabled :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
enabled nsToolbarItem =
  sendMessage nsToolbarItem enabledSelector

-- | Set and get the enabled flag of an item. For custom views, this method will call @-setEnabled:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setEnabled nsToolbarItem value =
  sendMessage nsToolbarItem setEnabledSelector value

-- | @- image@
image :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSImage)
image nsToolbarItem =
  sendMessage nsToolbarItem imageSelector

-- | @- setImage:@
setImage :: (IsNSToolbarItem nsToolbarItem, IsNSImage value) => nsToolbarItem -> value -> IO ()
setImage nsToolbarItem value =
  sendMessage nsToolbarItem setImageSelector (toNSImage value)

-- | Set and get the title of an item. For custom views, this method will call @-setTitle:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- title@
title :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
title nsToolbarItem =
  sendMessage nsToolbarItem titleSelector

-- | Set and get the title of an item. For custom views, this method will call @-setTitle:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setTitle nsToolbarItem value =
  sendMessage nsToolbarItem setTitleSelector (toNSString value)

-- | When set on an item without a custom view, the button produced will have a bordered style. Defaults to NO.
--
-- ObjC selector: @- bordered@
bordered :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
bordered nsToolbarItem =
  sendMessage nsToolbarItem borderedSelector

-- | When set on an item without a custom view, the button produced will have a bordered style. Defaults to NO.
--
-- ObjC selector: @- setBordered:@
setBordered :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setBordered nsToolbarItem value =
  sendMessage nsToolbarItem setBorderedSelector value

-- | @- backgroundTintColor@
backgroundTintColor :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSColor)
backgroundTintColor nsToolbarItem =
  sendMessage nsToolbarItem backgroundTintColorSelector

-- | @- setBackgroundTintColor:@
setBackgroundTintColor :: (IsNSToolbarItem nsToolbarItem, IsNSColor value) => nsToolbarItem -> value -> IO ()
setBackgroundTintColor nsToolbarItem value =
  sendMessage nsToolbarItem setBackgroundTintColorSelector (toNSColor value)

-- | Defines the toolbar item’s appearance. The default style is plain. Prominent style tints the background. If a background tint color is set, it uses it; otherwise, it uses the app’s or system’s accent color. If grouped with other items, it moves to its own to avoid tinting other items' background.
--
-- ObjC selector: @- style@
style :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO NSToolbarItemStyle
style nsToolbarItem =
  sendMessage nsToolbarItem styleSelector

-- | Defines the toolbar item’s appearance. The default style is plain. Prominent style tints the background. If a background tint color is set, it uses it; otherwise, it uses the app’s or system’s accent color. If grouped with other items, it moves to its own to avoid tinting other items' background.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> NSToolbarItemStyle -> IO ()
setStyle nsToolbarItem value =
  sendMessage nsToolbarItem setStyleSelector value

-- | Whether or not the item behaves as a navigation item (i.e. back/forward) in the toolbar. Navigation items may be specially positioned by the system outside the normal list of items of the toolbar in the order specified by @-toolbarDefaultItemIdentifiers:@. Defaults to NO.
--
-- ObjC selector: @- navigational@
navigational :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
navigational nsToolbarItem =
  sendMessage nsToolbarItem navigationalSelector

-- | Whether or not the item behaves as a navigation item (i.e. back/forward) in the toolbar. Navigation items may be specially positioned by the system outside the normal list of items of the toolbar in the order specified by @-toolbarDefaultItemIdentifiers:@. Defaults to NO.
--
-- ObjC selector: @- setNavigational:@
setNavigational :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setNavigational nsToolbarItem value =
  sendMessage nsToolbarItem setNavigationalSelector value

-- | Items with automatically generated views will return nil from this getter. Custom views may be provided but not all @NSToolbarItem@ subclasses support custom views. Note that, by default, many of the set/get methods will be implemented by calls forwarded to the view you set, if it responds to it.
--
-- ObjC selector: @- view@
view :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSView)
view nsToolbarItem =
  sendMessage nsToolbarItem viewSelector

-- | Items with automatically generated views will return nil from this getter. Custom views may be provided but not all @NSToolbarItem@ subclasses support custom views. Note that, by default, many of the set/get methods will be implemented by calls forwarded to the view you set, if it responds to it.
--
-- ObjC selector: @- setView:@
setView :: (IsNSToolbarItem nsToolbarItem, IsNSView value) => nsToolbarItem -> value -> IO ()
setView nsToolbarItem value =
  sendMessage nsToolbarItem setViewSelector (toNSView value)

-- | An item is visible if it is present in the NSToolbar and not in the overflow menu. This property is key value observable.
--
-- ObjC selector: @- visible@
visible :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
visible nsToolbarItem =
  sendMessage nsToolbarItem visibleSelector

-- | When an item is hidden it will not be visible in the toolbar. The item will still be visible in the customization panel. Because hidden items may be visible during user customization, use the @visible@ property to determine if an item is currently displayed. Note that even hidden toolbar items are sync'd to other toolbars with a shared identifier, but its @hidden@ state can be unique to each instance. Use this property to show a toolbar item in one toolbar instance but not another.
--
-- ObjC selector: @- hidden@
hidden :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
hidden nsToolbarItem =
  sendMessage nsToolbarItem hiddenSelector

-- | When an item is hidden it will not be visible in the toolbar. The item will still be visible in the customization panel. Because hidden items may be visible during user customization, use the @visible@ property to determine if an item is currently displayed. Note that even hidden toolbar items are sync'd to other toolbars with a shared identifier, but its @hidden@ state can be unique to each instance. Use this property to show a toolbar item in one toolbar instance but not another.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setHidden nsToolbarItem value =
  sendMessage nsToolbarItem setHiddenSelector value

-- | Unless you have already set your own custom view, you should not call these methods. The min size should be small enough to look nice in all display modes. If you do not set a min/max size, the view's size properties will be calculated using constraints. Apps linked before 10.14 will use the view's current size. In general, apps should rely on the automatic measurements and constraints to define min/max sizes rather than setting these properties since this will account for localizations.
--
-- ObjC selector: @- minSize@
minSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO NSSize
minSize nsToolbarItem =
  sendMessage nsToolbarItem minSizeSelector

-- | Unless you have already set your own custom view, you should not call these methods. The min size should be small enough to look nice in all display modes. If you do not set a min/max size, the view's size properties will be calculated using constraints. Apps linked before 10.14 will use the view's current size. In general, apps should rely on the automatic measurements and constraints to define min/max sizes rather than setting these properties since this will account for localizations.
--
-- ObjC selector: @- setMinSize:@
setMinSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> NSSize -> IO ()
setMinSize nsToolbarItem value =
  sendMessage nsToolbarItem setMinSizeSelector value

-- | @- maxSize@
maxSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO NSSize
maxSize nsToolbarItem =
  sendMessage nsToolbarItem maxSizeSelector

-- | @- setMaxSize:@
setMaxSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> NSSize -> IO ()
setMaxSize nsToolbarItem value =
  sendMessage nsToolbarItem setMaxSizeSelector value

-- | When a toolbar does not have enough space to fit all its items, it must push some into the overflow menu. Items with the highest @visibilityPriority@ level are chosen last for the overflow menu. The default @visibilityPriority@ value is @NSToolbarItemVisibilityPriorityStandard@. To suggest that an item always remain visible, give it a value greater than @NSToolbarItemVisibilityPriorityStandard@, but less than @NSToolbarItemVisibilityPriorityUser@. In 10.7, users can no longer modify the toolbar item visibility priority.
--
-- ObjC selector: @- visibilityPriority@
visibilityPriority :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO CLong
visibilityPriority nsToolbarItem =
  sendMessage nsToolbarItem visibilityPrioritySelector

-- | When a toolbar does not have enough space to fit all its items, it must push some into the overflow menu. Items with the highest @visibilityPriority@ level are chosen last for the overflow menu. The default @visibilityPriority@ value is @NSToolbarItemVisibilityPriorityStandard@. To suggest that an item always remain visible, give it a value greater than @NSToolbarItemVisibilityPriorityStandard@, but less than @NSToolbarItemVisibilityPriorityUser@. In 10.7, users can no longer modify the toolbar item visibility priority.
--
-- ObjC selector: @- setVisibilityPriority:@
setVisibilityPriority :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> CLong -> IO ()
setVisibilityPriority nsToolbarItem value =
  sendMessage nsToolbarItem setVisibilityPrioritySelector value

-- | A badge that can be attached to an NSToolbarItem. This provides a way to display small visual indicators that can be used to highlight important information, such as unread notifications or status indicators.
--
-- ObjC selector: @- badge@
badge :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSItemBadge)
badge nsToolbarItem =
  sendMessage nsToolbarItem badgeSelector

-- | A badge that can be attached to an NSToolbarItem. This provides a way to display small visual indicators that can be used to highlight important information, such as unread notifications or status indicators.
--
-- ObjC selector: @- setBadge:@
setBadge :: (IsNSToolbarItem nsToolbarItem, IsNSItemBadge value) => nsToolbarItem -> value -> IO ()
setBadge nsToolbarItem value =
  sendMessage nsToolbarItem setBadgeSelector (toNSItemBadge value)

-- | This property only affects automatic validation performed by NSToolbar. Explicit validation requests, such as the @-[NSToolbar validateVisibleItems]@ method, will invoke the @-validate@ method even if @autovalidates@ is @NO@. Defaults to YES.
--
-- ObjC selector: @- autovalidates@
autovalidates :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
autovalidates nsToolbarItem =
  sendMessage nsToolbarItem autovalidatesSelector

-- | This property only affects automatic validation performed by NSToolbar. Explicit validation requests, such as the @-[NSToolbar validateVisibleItems]@ method, will invoke the @-validate@ method even if @autovalidates@ is @NO@. Defaults to YES.
--
-- ObjC selector: @- setAutovalidates:@
setAutovalidates :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setAutovalidates nsToolbarItem value =
  sendMessage nsToolbarItem setAutovalidatesSelector value

-- | Duplicate items outside of spaces are not allowed.
--
-- ObjC selector: @- allowsDuplicatesInToolbar@
allowsDuplicatesInToolbar :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
allowsDuplicatesInToolbar nsToolbarItem =
  sendMessage nsToolbarItem allowsDuplicatesInToolbarSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemIdentifier:@
initWithItemIdentifierSelector :: Selector '[Id NSString] (Id NSToolbarItem)
initWithItemIdentifierSelector = mkSelector "initWithItemIdentifier:"

-- | @Selector@ for @validate@
validateSelector :: Selector '[] ()
validateSelector = mkSelector "validate"

-- | @Selector@ for @itemIdentifier@
itemIdentifierSelector :: Selector '[] (Id NSString)
itemIdentifierSelector = mkSelector "itemIdentifier"

-- | @Selector@ for @toolbar@
toolbarSelector :: Selector '[] (Id NSToolbar)
toolbarSelector = mkSelector "toolbar"

-- | @Selector@ for @label@
labelSelector :: Selector '[] (Id NSString)
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector '[Id NSString] ()
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @paletteLabel@
paletteLabelSelector :: Selector '[] (Id NSString)
paletteLabelSelector = mkSelector "paletteLabel"

-- | @Selector@ for @setPaletteLabel:@
setPaletteLabelSelector :: Selector '[Id NSString] ()
setPaletteLabelSelector = mkSelector "setPaletteLabel:"

-- | @Selector@ for @possibleLabels@
possibleLabelsSelector :: Selector '[] (Id NSSet)
possibleLabelsSelector = mkSelector "possibleLabels"

-- | @Selector@ for @setPossibleLabels:@
setPossibleLabelsSelector :: Selector '[Id NSSet] ()
setPossibleLabelsSelector = mkSelector "setPossibleLabels:"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] (Id NSString)
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector '[Id NSString] ()
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @menuFormRepresentation@
menuFormRepresentationSelector :: Selector '[] (Id NSMenuItem)
menuFormRepresentationSelector = mkSelector "menuFormRepresentation"

-- | @Selector@ for @setMenuFormRepresentation:@
setMenuFormRepresentationSelector :: Selector '[Id NSMenuItem] ()
setMenuFormRepresentationSelector = mkSelector "setMenuFormRepresentation:"

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CLong
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[CLong] ()
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector '[] Bool
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector '[Bool] ()
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @backgroundTintColor@
backgroundTintColorSelector :: Selector '[] (Id NSColor)
backgroundTintColorSelector = mkSelector "backgroundTintColor"

-- | @Selector@ for @setBackgroundTintColor:@
setBackgroundTintColorSelector :: Selector '[Id NSColor] ()
setBackgroundTintColorSelector = mkSelector "setBackgroundTintColor:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSToolbarItemStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[NSToolbarItemStyle] ()
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @navigational@
navigationalSelector :: Selector '[] Bool
navigationalSelector = mkSelector "navigational"

-- | @Selector@ for @setNavigational:@
setNavigationalSelector :: Selector '[Bool] ()
setNavigationalSelector = mkSelector "setNavigational:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @visible@
visibleSelector :: Selector '[] Bool
visibleSelector = mkSelector "visible"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @minSize@
minSizeSelector :: Selector '[] NSSize
minSizeSelector = mkSelector "minSize"

-- | @Selector@ for @setMinSize:@
setMinSizeSelector :: Selector '[NSSize] ()
setMinSizeSelector = mkSelector "setMinSize:"

-- | @Selector@ for @maxSize@
maxSizeSelector :: Selector '[] NSSize
maxSizeSelector = mkSelector "maxSize"

-- | @Selector@ for @setMaxSize:@
setMaxSizeSelector :: Selector '[NSSize] ()
setMaxSizeSelector = mkSelector "setMaxSize:"

-- | @Selector@ for @visibilityPriority@
visibilityPrioritySelector :: Selector '[] CLong
visibilityPrioritySelector = mkSelector "visibilityPriority"

-- | @Selector@ for @setVisibilityPriority:@
setVisibilityPrioritySelector :: Selector '[CLong] ()
setVisibilityPrioritySelector = mkSelector "setVisibilityPriority:"

-- | @Selector@ for @badge@
badgeSelector :: Selector '[] (Id NSItemBadge)
badgeSelector = mkSelector "badge"

-- | @Selector@ for @setBadge:@
setBadgeSelector :: Selector '[Id NSItemBadge] ()
setBadgeSelector = mkSelector "setBadge:"

-- | @Selector@ for @autovalidates@
autovalidatesSelector :: Selector '[] Bool
autovalidatesSelector = mkSelector "autovalidates"

-- | @Selector@ for @setAutovalidates:@
setAutovalidatesSelector :: Selector '[Bool] ()
setAutovalidatesSelector = mkSelector "setAutovalidates:"

-- | @Selector@ for @allowsDuplicatesInToolbar@
allowsDuplicatesInToolbarSelector :: Selector '[] Bool
allowsDuplicatesInToolbarSelector = mkSelector "allowsDuplicatesInToolbar"

