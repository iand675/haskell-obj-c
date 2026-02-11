{-# LANGUAGE PatternSynonyms #-}
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
  , toolTip
  , setToolTip
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
  , style
  , setStyle
  , navigational
  , setNavigational
  , visible
  , hidden
  , setHidden
  , minSize
  , setMinSize
  , maxSize
  , setMaxSize
  , visibilityPriority
  , setVisibilityPriority
  , autovalidates
  , setAutovalidates
  , allowsDuplicatesInToolbar
  , initWithItemIdentifierSelector
  , validateSelector
  , itemIdentifierSelector
  , toolbarSelector
  , labelSelector
  , setLabelSelector
  , paletteLabelSelector
  , setPaletteLabelSelector
  , toolTipSelector
  , setToolTipSelector
  , tagSelector
  , setTagSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , enabledSelector
  , setEnabledSelector
  , imageSelector
  , setImageSelector
  , titleSelector
  , setTitleSelector
  , borderedSelector
  , setBorderedSelector
  , styleSelector
  , setStyleSelector
  , navigationalSelector
  , setNavigationalSelector
  , visibleSelector
  , hiddenSelector
  , setHiddenSelector
  , minSizeSelector
  , setMinSizeSelector
  , maxSizeSelector
  , setMaxSizeSelector
  , visibilityPrioritySelector
  , setVisibilityPrioritySelector
  , autovalidatesSelector
  , setAutovalidatesSelector
  , allowsDuplicatesInToolbarSelector

  -- * Enum types
  , NSToolbarItemStyle(NSToolbarItemStyle)
  , pattern NSToolbarItemStylePlain
  , pattern NSToolbarItemStyleProminent

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

-- | Initialize the toolbar item with an identifier which is a development language string used by the toolbar and its delegate for identification purposes.
--
-- ObjC selector: @- initWithItemIdentifier:@
initWithItemIdentifier :: (IsNSToolbarItem nsToolbarItem, IsNSString itemIdentifier) => nsToolbarItem -> itemIdentifier -> IO (Id NSToolbarItem)
initWithItemIdentifier nsToolbarItem  itemIdentifier =
withObjCPtr itemIdentifier $ \raw_itemIdentifier ->
    sendMsg nsToolbarItem (mkSelector "initWithItemIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_itemIdentifier :: Ptr ())] >>= ownedObject . castPtr

-- | Typically you should not invoke this method. This method is called by its toolbar during validation. Standard items validate themselves by sending the @-validateToolbarItem:@ validate message to the current validator. Since items with custom views don't always have meaningful target/actions, they do nothing. So for your custom items it may be useful to override this method and invent your own validation.
--
-- ObjC selector: @- validate@
validate :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO ()
validate nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "validate") retVoid []

-- | @- itemIdentifier@
itemIdentifier :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
itemIdentifier nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "itemIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this to determine the toolbar in which an item is currently displayed.
--
-- ObjC selector: @- toolbar@
toolbar :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSToolbar)
toolbar nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "toolbar") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this to set the item's label that appears in the toolbar. The label may also be used for the default @menuFormRepresentation@ of the item. Also, developers should make sure the length of the label is appropriate and not too long.
--
-- ObjC selector: @- label@
label :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
label nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this to set the item's label that appears in the toolbar. The label may also be used for the default @menuFormRepresentation@ of the item. Also, developers should make sure the length of the label is appropriate and not too long.
--
-- ObjC selector: @- setLabel:@
setLabel :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setLabel nsToolbarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsToolbarItem (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Use this to set the item's label that appears when the item is in the customization palette. All Items must have a palette label, and for most things it is reasonable to set them to the same string as the label used in the toolbar.
--
-- ObjC selector: @- paletteLabel@
paletteLabel :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
paletteLabel nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "paletteLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this to set the item's label that appears when the item is in the customization palette. All Items must have a palette label, and for most things it is reasonable to set them to the same string as the label used in the toolbar.
--
-- ObjC selector: @- setPaletteLabel:@
setPaletteLabel :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setPaletteLabel nsToolbarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsToolbarItem (mkSelector "setPaletteLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Use this to set a tooltip to be used when the item is displayed in the toolbar. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- toolTip@
toolTip :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
toolTip nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "toolTip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Use this to set a tooltip to be used when the item is displayed in the toolbar. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setToolTip:@
setToolTip :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setToolTip nsToolbarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsToolbarItem (mkSelector "setToolTip:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Tag for your own custom purpose. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- tag@
tag :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO CLong
tag nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "tag") retCLong []

-- | Tag for your own custom purpose. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setTag:@
setTag :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> CLong -> IO ()
setTag nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setTag:") retVoid [argCLong (fromIntegral value)]

-- | Set and get the action of an item. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- target@
target :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO RawId
target nsToolbarItem  =
  fmap (RawId . castPtr) $ sendMsg nsToolbarItem (mkSelector "target") (retPtr retVoid) []

-- | Set and get the action of an item. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setTarget:@
setTarget :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> RawId -> IO ()
setTarget nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Set and get the action of an item. For custom views, this method will call @-setAction:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- action@
action :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Selector
action nsToolbarItem  =
  fmap (Selector . castPtr) $ sendMsg nsToolbarItem (mkSelector "action") (retPtr retVoid) []

-- | Set and get the action of an item. For custom views, this method will call @-setAction:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setAction:@
setAction :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Selector -> IO ()
setAction nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | Set and get the enabled flag of an item. For custom views, this method will call @-setEnabled:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- enabled@
enabled :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
enabled nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "enabled") retCULong []

-- | Set and get the enabled flag of an item. For custom views, this method will call @-setEnabled:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setEnabled nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- image@
image :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSImage)
image nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSToolbarItem nsToolbarItem, IsNSImage value) => nsToolbarItem -> value -> IO ()
setImage nsToolbarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsToolbarItem (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set and get the title of an item. For custom views, this method will call @-setTitle:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- title@
title :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO (Id NSString)
title nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Set and get the title of an item. For custom views, this method will call @-setTitle:@ on the view if it responds. (forwards to @-view@ if it responds)
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSToolbarItem nsToolbarItem, IsNSString value) => nsToolbarItem -> value -> IO ()
setTitle nsToolbarItem  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsToolbarItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | When set on an item without a custom view, the button produced will have a bordered style. Defaults to NO.
--
-- ObjC selector: @- bordered@
bordered :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
bordered nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "bordered") retCULong []

-- | When set on an item without a custom view, the button produced will have a bordered style. Defaults to NO.
--
-- ObjC selector: @- setBordered:@
setBordered :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setBordered nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | Defines the toolbar item’s appearance. The default style is plain. Prominent style tints the background. If a background tint color is set, it uses it; otherwise, it uses the app’s or system’s accent color. If grouped with other items, it moves to its own to avoid tinting other items' background.
--
-- ObjC selector: @- style@
style :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO NSToolbarItemStyle
style nsToolbarItem  =
  fmap (coerce :: CLong -> NSToolbarItemStyle) $ sendMsg nsToolbarItem (mkSelector "style") retCLong []

-- | Defines the toolbar item’s appearance. The default style is plain. Prominent style tints the background. If a background tint color is set, it uses it; otherwise, it uses the app’s or system’s accent color. If grouped with other items, it moves to its own to avoid tinting other items' background.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> NSToolbarItemStyle -> IO ()
setStyle nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- | Whether or not the item behaves as a navigation item (i.e. back/forward) in the toolbar. Navigation items may be specially positioned by the system outside the normal list of items of the toolbar in the order specified by @-toolbarDefaultItemIdentifiers:@. Defaults to NO.
--
-- ObjC selector: @- navigational@
navigational :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
navigational nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "navigational") retCULong []

-- | Whether or not the item behaves as a navigation item (i.e. back/forward) in the toolbar. Navigation items may be specially positioned by the system outside the normal list of items of the toolbar in the order specified by @-toolbarDefaultItemIdentifiers:@. Defaults to NO.
--
-- ObjC selector: @- setNavigational:@
setNavigational :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setNavigational nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setNavigational:") retVoid [argCULong (if value then 1 else 0)]

-- | An item is visible if it is present in the NSToolbar and not in the overflow menu. This property is key value observable.
--
-- ObjC selector: @- visible@
visible :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
visible nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "visible") retCULong []

-- | When an item is hidden it will not be visible in the toolbar. The item will still be visible in the customization panel. Because hidden items may be visible during user customization, use the @visible@ property to determine if an item is currently displayed. Note that even hidden toolbar items are sync'd to other toolbars with a shared identifier, but its @hidden@ state can be unique to each instance. Use this property to show a toolbar item in one toolbar instance but not another.
--
-- ObjC selector: @- hidden@
hidden :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
hidden nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "hidden") retCULong []

-- | When an item is hidden it will not be visible in the toolbar. The item will still be visible in the customization panel. Because hidden items may be visible during user customization, use the @visible@ property to determine if an item is currently displayed. Note that even hidden toolbar items are sync'd to other toolbars with a shared identifier, but its @hidden@ state can be unique to each instance. Use this property to show a toolbar item in one toolbar instance but not another.
--
-- ObjC selector: @- setHidden:@
setHidden :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setHidden nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | Unless you have already set your own custom view, you should not call these methods. The min size should be small enough to look nice in all display modes. If you do not set a min/max size, the view's size properties will be calculated using constraints. Apps linked before 10.14 will use the view's current size. In general, apps should rely on the automatic measurements and constraints to define min/max sizes rather than setting these properties since this will account for localizations.
--
-- ObjC selector: @- minSize@
minSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO NSSize
minSize nsToolbarItem  =
  sendMsgStret nsToolbarItem (mkSelector "minSize") retNSSize []

-- | Unless you have already set your own custom view, you should not call these methods. The min size should be small enough to look nice in all display modes. If you do not set a min/max size, the view's size properties will be calculated using constraints. Apps linked before 10.14 will use the view's current size. In general, apps should rely on the automatic measurements and constraints to define min/max sizes rather than setting these properties since this will account for localizations.
--
-- ObjC selector: @- setMinSize:@
setMinSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> NSSize -> IO ()
setMinSize nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setMinSize:") retVoid [argNSSize value]

-- | @- maxSize@
maxSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO NSSize
maxSize nsToolbarItem  =
  sendMsgStret nsToolbarItem (mkSelector "maxSize") retNSSize []

-- | @- setMaxSize:@
setMaxSize :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> NSSize -> IO ()
setMaxSize nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setMaxSize:") retVoid [argNSSize value]

-- | When a toolbar does not have enough space to fit all its items, it must push some into the overflow menu. Items with the highest @visibilityPriority@ level are chosen last for the overflow menu. The default @visibilityPriority@ value is @NSToolbarItemVisibilityPriorityStandard@. To suggest that an item always remain visible, give it a value greater than @NSToolbarItemVisibilityPriorityStandard@, but less than @NSToolbarItemVisibilityPriorityUser@. In 10.7, users can no longer modify the toolbar item visibility priority.
--
-- ObjC selector: @- visibilityPriority@
visibilityPriority :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO CLong
visibilityPriority nsToolbarItem  =
  sendMsg nsToolbarItem (mkSelector "visibilityPriority") retCLong []

-- | When a toolbar does not have enough space to fit all its items, it must push some into the overflow menu. Items with the highest @visibilityPriority@ level are chosen last for the overflow menu. The default @visibilityPriority@ value is @NSToolbarItemVisibilityPriorityStandard@. To suggest that an item always remain visible, give it a value greater than @NSToolbarItemVisibilityPriorityStandard@, but less than @NSToolbarItemVisibilityPriorityUser@. In 10.7, users can no longer modify the toolbar item visibility priority.
--
-- ObjC selector: @- setVisibilityPriority:@
setVisibilityPriority :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> CLong -> IO ()
setVisibilityPriority nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setVisibilityPriority:") retVoid [argCLong (fromIntegral value)]

-- | This property only affects automatic validation performed by NSToolbar. Explicit validation requests, such as the @-[NSToolbar validateVisibleItems]@ method, will invoke the @-validate@ method even if @autovalidates@ is @NO@. Defaults to YES.
--
-- ObjC selector: @- autovalidates@
autovalidates :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
autovalidates nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "autovalidates") retCULong []

-- | This property only affects automatic validation performed by NSToolbar. Explicit validation requests, such as the @-[NSToolbar validateVisibleItems]@ method, will invoke the @-validate@ method even if @autovalidates@ is @NO@. Defaults to YES.
--
-- ObjC selector: @- setAutovalidates:@
setAutovalidates :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> Bool -> IO ()
setAutovalidates nsToolbarItem  value =
  sendMsg nsToolbarItem (mkSelector "setAutovalidates:") retVoid [argCULong (if value then 1 else 0)]

-- | Duplicate items outside of spaces are not allowed.
--
-- ObjC selector: @- allowsDuplicatesInToolbar@
allowsDuplicatesInToolbar :: IsNSToolbarItem nsToolbarItem => nsToolbarItem -> IO Bool
allowsDuplicatesInToolbar nsToolbarItem  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsToolbarItem (mkSelector "allowsDuplicatesInToolbar") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithItemIdentifier:@
initWithItemIdentifierSelector :: Selector
initWithItemIdentifierSelector = mkSelector "initWithItemIdentifier:"

-- | @Selector@ for @validate@
validateSelector :: Selector
validateSelector = mkSelector "validate"

-- | @Selector@ for @itemIdentifier@
itemIdentifierSelector :: Selector
itemIdentifierSelector = mkSelector "itemIdentifier"

-- | @Selector@ for @toolbar@
toolbarSelector :: Selector
toolbarSelector = mkSelector "toolbar"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

-- | @Selector@ for @paletteLabel@
paletteLabelSelector :: Selector
paletteLabelSelector = mkSelector "paletteLabel"

-- | @Selector@ for @setPaletteLabel:@
setPaletteLabelSelector :: Selector
setPaletteLabelSelector = mkSelector "setPaletteLabel:"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

-- | @Selector@ for @navigational@
navigationalSelector :: Selector
navigationalSelector = mkSelector "navigational"

-- | @Selector@ for @setNavigational:@
setNavigationalSelector :: Selector
setNavigationalSelector = mkSelector "setNavigational:"

-- | @Selector@ for @visible@
visibleSelector :: Selector
visibleSelector = mkSelector "visible"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @minSize@
minSizeSelector :: Selector
minSizeSelector = mkSelector "minSize"

-- | @Selector@ for @setMinSize:@
setMinSizeSelector :: Selector
setMinSizeSelector = mkSelector "setMinSize:"

-- | @Selector@ for @maxSize@
maxSizeSelector :: Selector
maxSizeSelector = mkSelector "maxSize"

-- | @Selector@ for @setMaxSize:@
setMaxSizeSelector :: Selector
setMaxSizeSelector = mkSelector "setMaxSize:"

-- | @Selector@ for @visibilityPriority@
visibilityPrioritySelector :: Selector
visibilityPrioritySelector = mkSelector "visibilityPriority"

-- | @Selector@ for @setVisibilityPriority:@
setVisibilityPrioritySelector :: Selector
setVisibilityPrioritySelector = mkSelector "setVisibilityPriority:"

-- | @Selector@ for @autovalidates@
autovalidatesSelector :: Selector
autovalidatesSelector = mkSelector "autovalidates"

-- | @Selector@ for @setAutovalidates:@
setAutovalidatesSelector :: Selector
setAutovalidatesSelector = mkSelector "setAutovalidates:"

-- | @Selector@ for @allowsDuplicatesInToolbar@
allowsDuplicatesInToolbarSelector :: Selector
allowsDuplicatesInToolbarSelector = mkSelector "allowsDuplicatesInToolbar"

