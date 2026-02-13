{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMenuItem@.
module ObjC.AppKit.NSMenuItem
  ( NSMenuItem
  , IsNSMenuItem(..)
  , nsMenuItemSeparatorItem
  , sectionHeaderWithTitle
  , initWithTitle_action_keyEquivalent
  , initWithCoder
  , setMnemonicLocation
  , mnemonicLocation
  , mnemonic
  , setTitleWithMnemonic
  , usesUserKeyEquivalents
  , setUsesUserKeyEquivalents
  , writingToolsItems
  , menu
  , setMenu
  , hasSubmenu
  , submenu
  , setSubmenu
  , parentItem
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , subtitle
  , setSubtitle
  , separatorItem
  , sectionHeader
  , keyEquivalent
  , setKeyEquivalent
  , keyEquivalentModifierMask
  , setKeyEquivalentModifierMask
  , userKeyEquivalent
  , allowsKeyEquivalentWhenHidden
  , setAllowsKeyEquivalentWhenHidden
  , allowsAutomaticKeyEquivalentLocalization
  , setAllowsAutomaticKeyEquivalentLocalization
  , allowsAutomaticKeyEquivalentMirroring
  , setAllowsAutomaticKeyEquivalentMirroring
  , image
  , setImage
  , state
  , setState
  , onStateImage
  , setOnStateImage
  , offStateImage
  , setOffStateImage
  , mixedStateImage
  , setMixedStateImage
  , enabled
  , setEnabled
  , alternate
  , setAlternate
  , indentationLevel
  , setIndentationLevel
  , target
  , setTarget
  , action
  , setAction
  , tag
  , setTag
  , representedObject
  , setRepresentedObject
  , view
  , setView
  , highlighted
  , hidden
  , setHidden
  , hiddenOrHasHiddenAncestor
  , toolTip
  , setToolTip
  , badge
  , setBadge
  , actionSelector
  , allowsAutomaticKeyEquivalentLocalizationSelector
  , allowsAutomaticKeyEquivalentMirroringSelector
  , allowsKeyEquivalentWhenHiddenSelector
  , alternateSelector
  , attributedTitleSelector
  , badgeSelector
  , enabledSelector
  , hasSubmenuSelector
  , hiddenOrHasHiddenAncestorSelector
  , hiddenSelector
  , highlightedSelector
  , imageSelector
  , indentationLevelSelector
  , initWithCoderSelector
  , initWithTitle_action_keyEquivalentSelector
  , keyEquivalentModifierMaskSelector
  , keyEquivalentSelector
  , menuSelector
  , mixedStateImageSelector
  , mnemonicLocationSelector
  , mnemonicSelector
  , nsMenuItemSeparatorItemSelector
  , offStateImageSelector
  , onStateImageSelector
  , parentItemSelector
  , representedObjectSelector
  , sectionHeaderSelector
  , sectionHeaderWithTitleSelector
  , separatorItemSelector
  , setActionSelector
  , setAllowsAutomaticKeyEquivalentLocalizationSelector
  , setAllowsAutomaticKeyEquivalentMirroringSelector
  , setAllowsKeyEquivalentWhenHiddenSelector
  , setAlternateSelector
  , setAttributedTitleSelector
  , setBadgeSelector
  , setEnabledSelector
  , setHiddenSelector
  , setImageSelector
  , setIndentationLevelSelector
  , setKeyEquivalentModifierMaskSelector
  , setKeyEquivalentSelector
  , setMenuSelector
  , setMixedStateImageSelector
  , setMnemonicLocationSelector
  , setOffStateImageSelector
  , setOnStateImageSelector
  , setRepresentedObjectSelector
  , setStateSelector
  , setSubmenuSelector
  , setSubtitleSelector
  , setTagSelector
  , setTargetSelector
  , setTitleSelector
  , setTitleWithMnemonicSelector
  , setToolTipSelector
  , setUsesUserKeyEquivalentsSelector
  , setViewSelector
  , stateSelector
  , submenuSelector
  , subtitleSelector
  , tagSelector
  , targetSelector
  , titleSelector
  , toolTipSelector
  , userKeyEquivalentSelector
  , usesUserKeyEquivalentsSelector
  , viewSelector
  , writingToolsItemsSelector

  -- * Enum types
  , NSEventModifierFlags(NSEventModifierFlags)
  , pattern NSEventModifierFlagCapsLock
  , pattern NSEventModifierFlagShift
  , pattern NSEventModifierFlagControl
  , pattern NSEventModifierFlagOption
  , pattern NSEventModifierFlagCommand
  , pattern NSEventModifierFlagNumericPad
  , pattern NSEventModifierFlagHelp
  , pattern NSEventModifierFlagFunction
  , pattern NSEventModifierFlagDeviceIndependentFlagsMask

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ separatorItem@
nsMenuItemSeparatorItem :: IO (Id NSMenuItem)
nsMenuItemSeparatorItem  =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMessage cls' nsMenuItemSeparatorItemSelector

-- | Creates a menu item representing a section header with the provided title. Section header items are used to provide context to a grouping of menu items. Items created using this method are non-interactive and do not perform an action.
--
-- ObjC selector: @+ sectionHeaderWithTitle:@
sectionHeaderWithTitle :: IsNSString title => title -> IO (Id NSMenuItem)
sectionHeaderWithTitle title =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMessage cls' sectionHeaderWithTitleSelector (toNSString title)

-- | @- initWithTitle:action:keyEquivalent:@
initWithTitle_action_keyEquivalent :: (IsNSMenuItem nsMenuItem, IsNSString string, IsNSString charCode) => nsMenuItem -> string -> Sel -> charCode -> IO (Id NSMenuItem)
initWithTitle_action_keyEquivalent nsMenuItem string selector charCode =
  sendOwnedMessage nsMenuItem initWithTitle_action_keyEquivalentSelector (toNSString string) selector (toNSString charCode)

-- | @- initWithCoder:@
initWithCoder :: (IsNSMenuItem nsMenuItem, IsNSCoder coder) => nsMenuItem -> coder -> IO (Id NSMenuItem)
initWithCoder nsMenuItem coder =
  sendOwnedMessage nsMenuItem initWithCoderSelector (toNSCoder coder)

-- | @- setMnemonicLocation:@
setMnemonicLocation :: IsNSMenuItem nsMenuItem => nsMenuItem -> CULong -> IO ()
setMnemonicLocation nsMenuItem location =
  sendMessage nsMenuItem setMnemonicLocationSelector location

-- | @- mnemonicLocation@
mnemonicLocation :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CULong
mnemonicLocation nsMenuItem =
  sendMessage nsMenuItem mnemonicLocationSelector

-- | @- mnemonic@
mnemonic :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
mnemonic nsMenuItem =
  sendMessage nsMenuItem mnemonicSelector

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSMenuItem nsMenuItem, IsNSString stringWithAmpersand) => nsMenuItem -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsMenuItem stringWithAmpersand =
  sendMessage nsMenuItem setTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | @+ usesUserKeyEquivalents@
usesUserKeyEquivalents :: IO Bool
usesUserKeyEquivalents  =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMessage cls' usesUserKeyEquivalentsSelector

-- | @+ setUsesUserKeyEquivalents:@
setUsesUserKeyEquivalents :: Bool -> IO ()
setUsesUserKeyEquivalents value =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMessage cls' setUsesUserKeyEquivalentsSelector value

-- | An array of standard menu items related to Writing Tools. Each call to this method returns an array of newly allocated instances of NSMenuItem.
--
-- ObjC selector: @+ writingToolsItems@
writingToolsItems :: IO (Id NSArray)
writingToolsItems  =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMessage cls' writingToolsItemsSelector

-- | Note: Never call the setter method directly: it is there only for subclassers.
--
-- ObjC selector: @- menu@
menu :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenu)
menu nsMenuItem =
  sendMessage nsMenuItem menuSelector

-- | Note: Never call the setter method directly: it is there only for subclassers.
--
-- ObjC selector: @- setMenu:@
setMenu :: (IsNSMenuItem nsMenuItem, IsNSMenu value) => nsMenuItem -> value -> IO ()
setMenu nsMenuItem value =
  sendMessage nsMenuItem setMenuSelector (toNSMenu value)

-- | @- hasSubmenu@
hasSubmenu :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
hasSubmenu nsMenuItem =
  sendMessage nsMenuItem hasSubmenuSelector

-- | @- submenu@
submenu :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenu)
submenu nsMenuItem =
  sendMessage nsMenuItem submenuSelector

-- | @- setSubmenu:@
setSubmenu :: (IsNSMenuItem nsMenuItem, IsNSMenu value) => nsMenuItem -> value -> IO ()
setSubmenu nsMenuItem value =
  sendMessage nsMenuItem setSubmenuSelector (toNSMenu value)

-- | Returns: The @NSMenuItem@ whose submenu contains the receiver, or nil if the receiver does not have a parent item.
--
-- ObjC selector: @- parentItem@
parentItem :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenuItem)
parentItem nsMenuItem =
  sendMessage nsMenuItem parentItemSelector

-- | @- title@
title :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
title nsMenuItem =
  sendMessage nsMenuItem titleSelector

-- | @- setTitle:@
setTitle :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setTitle nsMenuItem value =
  sendMessage nsMenuItem setTitleSelector (toNSString value)

-- | @- attributedTitle@
attributedTitle :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSAttributedString)
attributedTitle nsMenuItem =
  sendMessage nsMenuItem attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSMenuItem nsMenuItem, IsNSAttributedString value) => nsMenuItem -> value -> IO ()
setAttributedTitle nsMenuItem value =
  sendMessage nsMenuItem setAttributedTitleSelector (toNSAttributedString value)

-- | Used to specify a standard subtitle for the menu item.
--
-- The subtitle is displayed below the standard title.
--
-- Note: On macOS 14, a menu item with an attributed title does not show the subtitle. The subtitle is shown on macOS 15 and later.
--
-- ObjC selector: @- subtitle@
subtitle :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
subtitle nsMenuItem =
  sendMessage nsMenuItem subtitleSelector

-- | Used to specify a standard subtitle for the menu item.
--
-- The subtitle is displayed below the standard title.
--
-- Note: On macOS 14, a menu item with an attributed title does not show the subtitle. The subtitle is shown on macOS 15 and later.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setSubtitle nsMenuItem value =
  sendMessage nsMenuItem setSubtitleSelector (toNSString value)

-- | @- separatorItem@
separatorItem :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
separatorItem nsMenuItem =
  sendMessage nsMenuItem separatorItemSelector

-- | Indicates whether the item is a section header. Section header items are created using the @sectionHeader(title:)@ class method.
--
-- ObjC selector: @- sectionHeader@
sectionHeader :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
sectionHeader nsMenuItem =
  sendMessage nsMenuItem sectionHeaderSelector

-- | @- keyEquivalent@
keyEquivalent :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
keyEquivalent nsMenuItem =
  sendMessage nsMenuItem keyEquivalentSelector

-- | @- setKeyEquivalent:@
setKeyEquivalent :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setKeyEquivalent nsMenuItem value =
  sendMessage nsMenuItem setKeyEquivalentSelector (toNSString value)

-- | @- keyEquivalentModifierMask@
keyEquivalentModifierMask :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO NSEventModifierFlags
keyEquivalentModifierMask nsMenuItem =
  sendMessage nsMenuItem keyEquivalentModifierMaskSelector

-- | @- setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMask :: IsNSMenuItem nsMenuItem => nsMenuItem -> NSEventModifierFlags -> IO ()
setKeyEquivalentModifierMask nsMenuItem value =
  sendMessage nsMenuItem setKeyEquivalentModifierMaskSelector value

-- | @- userKeyEquivalent@
userKeyEquivalent :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
userKeyEquivalent nsMenuItem =
  sendMessage nsMenuItem userKeyEquivalentSelector

-- | @- allowsKeyEquivalentWhenHidden@
allowsKeyEquivalentWhenHidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
allowsKeyEquivalentWhenHidden nsMenuItem =
  sendMessage nsMenuItem allowsKeyEquivalentWhenHiddenSelector

-- | @- setAllowsKeyEquivalentWhenHidden:@
setAllowsKeyEquivalentWhenHidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAllowsKeyEquivalentWhenHidden nsMenuItem value =
  sendMessage nsMenuItem setAllowsKeyEquivalentWhenHiddenSelector value

-- | @- allowsAutomaticKeyEquivalentLocalization@
allowsAutomaticKeyEquivalentLocalization :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
allowsAutomaticKeyEquivalentLocalization nsMenuItem =
  sendMessage nsMenuItem allowsAutomaticKeyEquivalentLocalizationSelector

-- | @- setAllowsAutomaticKeyEquivalentLocalization:@
setAllowsAutomaticKeyEquivalentLocalization :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAllowsAutomaticKeyEquivalentLocalization nsMenuItem value =
  sendMessage nsMenuItem setAllowsAutomaticKeyEquivalentLocalizationSelector value

-- | @- allowsAutomaticKeyEquivalentMirroring@
allowsAutomaticKeyEquivalentMirroring :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
allowsAutomaticKeyEquivalentMirroring nsMenuItem =
  sendMessage nsMenuItem allowsAutomaticKeyEquivalentMirroringSelector

-- | @- setAllowsAutomaticKeyEquivalentMirroring:@
setAllowsAutomaticKeyEquivalentMirroring :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAllowsAutomaticKeyEquivalentMirroring nsMenuItem value =
  sendMessage nsMenuItem setAllowsAutomaticKeyEquivalentMirroringSelector value

-- | @- image@
image :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
image nsMenuItem =
  sendMessage nsMenuItem imageSelector

-- | @- setImage:@
setImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setImage nsMenuItem value =
  sendMessage nsMenuItem setImageSelector (toNSImage value)

-- | @- state@
state :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CLong
state nsMenuItem =
  sendMessage nsMenuItem stateSelector

-- | @- setState:@
setState :: IsNSMenuItem nsMenuItem => nsMenuItem -> CLong -> IO ()
setState nsMenuItem value =
  sendMessage nsMenuItem setStateSelector value

-- | @- onStateImage@
onStateImage :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
onStateImage nsMenuItem =
  sendMessage nsMenuItem onStateImageSelector

-- | @- setOnStateImage:@
setOnStateImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setOnStateImage nsMenuItem value =
  sendMessage nsMenuItem setOnStateImageSelector (toNSImage value)

-- | @- offStateImage@
offStateImage :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
offStateImage nsMenuItem =
  sendMessage nsMenuItem offStateImageSelector

-- | @- setOffStateImage:@
setOffStateImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setOffStateImage nsMenuItem value =
  sendMessage nsMenuItem setOffStateImageSelector (toNSImage value)

-- | @- mixedStateImage@
mixedStateImage :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
mixedStateImage nsMenuItem =
  sendMessage nsMenuItem mixedStateImageSelector

-- | @- setMixedStateImage:@
setMixedStateImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setMixedStateImage nsMenuItem value =
  sendMessage nsMenuItem setMixedStateImageSelector (toNSImage value)

-- | @- enabled@
enabled :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
enabled nsMenuItem =
  sendMessage nsMenuItem enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setEnabled nsMenuItem value =
  sendMessage nsMenuItem setEnabledSelector value

-- | @- alternate@
alternate :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
alternate nsMenuItem =
  sendMessage nsMenuItem alternateSelector

-- | @- setAlternate:@
setAlternate :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAlternate nsMenuItem value =
  sendMessage nsMenuItem setAlternateSelector value

-- | @- indentationLevel@
indentationLevel :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CLong
indentationLevel nsMenuItem =
  sendMessage nsMenuItem indentationLevelSelector

-- | @- setIndentationLevel:@
setIndentationLevel :: IsNSMenuItem nsMenuItem => nsMenuItem -> CLong -> IO ()
setIndentationLevel nsMenuItem value =
  sendMessage nsMenuItem setIndentationLevelSelector value

-- | @- target@
target :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO RawId
target nsMenuItem =
  sendMessage nsMenuItem targetSelector

-- | @- setTarget:@
setTarget :: IsNSMenuItem nsMenuItem => nsMenuItem -> RawId -> IO ()
setTarget nsMenuItem value =
  sendMessage nsMenuItem setTargetSelector value

-- | @- action@
action :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Sel
action nsMenuItem =
  sendMessage nsMenuItem actionSelector

-- | @- setAction:@
setAction :: IsNSMenuItem nsMenuItem => nsMenuItem -> Sel -> IO ()
setAction nsMenuItem value =
  sendMessage nsMenuItem setActionSelector value

-- | @- tag@
tag :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CLong
tag nsMenuItem =
  sendMessage nsMenuItem tagSelector

-- | @- setTag:@
setTag :: IsNSMenuItem nsMenuItem => nsMenuItem -> CLong -> IO ()
setTag nsMenuItem value =
  sendMessage nsMenuItem setTagSelector value

-- | @- representedObject@
representedObject :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO RawId
representedObject nsMenuItem =
  sendMessage nsMenuItem representedObjectSelector

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSMenuItem nsMenuItem => nsMenuItem -> RawId -> IO ()
setRepresentedObject nsMenuItem value =
  sendMessage nsMenuItem setRepresentedObjectSelector value

-- | @- view@
view :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSView)
view nsMenuItem =
  sendMessage nsMenuItem viewSelector

-- | @- setView:@
setView :: (IsNSMenuItem nsMenuItem, IsNSView value) => nsMenuItem -> value -> IO ()
setView nsMenuItem value =
  sendMessage nsMenuItem setViewSelector (toNSView value)

-- | @- highlighted@
highlighted :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
highlighted nsMenuItem =
  sendMessage nsMenuItem highlightedSelector

-- | @- hidden@
hidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
hidden nsMenuItem =
  sendMessage nsMenuItem hiddenSelector

-- | @- setHidden:@
setHidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setHidden nsMenuItem value =
  sendMessage nsMenuItem setHiddenSelector value

-- | @- hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestor :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
hiddenOrHasHiddenAncestor nsMenuItem =
  sendMessage nsMenuItem hiddenOrHasHiddenAncestorSelector

-- | @- toolTip@
toolTip :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
toolTip nsMenuItem =
  sendMessage nsMenuItem toolTipSelector

-- | @- setToolTip:@
setToolTip :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setToolTip nsMenuItem value =
  sendMessage nsMenuItem setToolTipSelector (toNSString value)

-- | A badge used to provide additional quantitative information specific to the menu item, such as the number of available updates.
--
-- Note: The default value of this property is @nil.@
--
-- ObjC selector: @- badge@
badge :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenuItemBadge)
badge nsMenuItem =
  sendMessage nsMenuItem badgeSelector

-- | A badge used to provide additional quantitative information specific to the menu item, such as the number of available updates.
--
-- Note: The default value of this property is @nil.@
--
-- ObjC selector: @- setBadge:@
setBadge :: (IsNSMenuItem nsMenuItem, IsNSMenuItemBadge value) => nsMenuItem -> value -> IO ()
setBadge nsMenuItem value =
  sendMessage nsMenuItem setBadgeSelector (toNSMenuItemBadge value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @separatorItem@
nsMenuItemSeparatorItemSelector :: Selector '[] (Id NSMenuItem)
nsMenuItemSeparatorItemSelector = mkSelector "separatorItem"

-- | @Selector@ for @sectionHeaderWithTitle:@
sectionHeaderWithTitleSelector :: Selector '[Id NSString] (Id NSMenuItem)
sectionHeaderWithTitleSelector = mkSelector "sectionHeaderWithTitle:"

-- | @Selector@ for @initWithTitle:action:keyEquivalent:@
initWithTitle_action_keyEquivalentSelector :: Selector '[Id NSString, Sel, Id NSString] (Id NSMenuItem)
initWithTitle_action_keyEquivalentSelector = mkSelector "initWithTitle:action:keyEquivalent:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSMenuItem)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setMnemonicLocation:@
setMnemonicLocationSelector :: Selector '[CULong] ()
setMnemonicLocationSelector = mkSelector "setMnemonicLocation:"

-- | @Selector@ for @mnemonicLocation@
mnemonicLocationSelector :: Selector '[] CULong
mnemonicLocationSelector = mkSelector "mnemonicLocation"

-- | @Selector@ for @mnemonic@
mnemonicSelector :: Selector '[] (Id NSString)
mnemonicSelector = mkSelector "mnemonic"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @usesUserKeyEquivalents@
usesUserKeyEquivalentsSelector :: Selector '[] Bool
usesUserKeyEquivalentsSelector = mkSelector "usesUserKeyEquivalents"

-- | @Selector@ for @setUsesUserKeyEquivalents:@
setUsesUserKeyEquivalentsSelector :: Selector '[Bool] ()
setUsesUserKeyEquivalentsSelector = mkSelector "setUsesUserKeyEquivalents:"

-- | @Selector@ for @writingToolsItems@
writingToolsItemsSelector :: Selector '[] (Id NSArray)
writingToolsItemsSelector = mkSelector "writingToolsItems"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @hasSubmenu@
hasSubmenuSelector :: Selector '[] Bool
hasSubmenuSelector = mkSelector "hasSubmenu"

-- | @Selector@ for @submenu@
submenuSelector :: Selector '[] (Id NSMenu)
submenuSelector = mkSelector "submenu"

-- | @Selector@ for @setSubmenu:@
setSubmenuSelector :: Selector '[Id NSMenu] ()
setSubmenuSelector = mkSelector "setSubmenu:"

-- | @Selector@ for @parentItem@
parentItemSelector :: Selector '[] (Id NSMenuItem)
parentItemSelector = mkSelector "parentItem"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector '[] (Id NSAttributedString)
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @separatorItem@
separatorItemSelector :: Selector '[] Bool
separatorItemSelector = mkSelector "separatorItem"

-- | @Selector@ for @sectionHeader@
sectionHeaderSelector :: Selector '[] Bool
sectionHeaderSelector = mkSelector "sectionHeader"

-- | @Selector@ for @keyEquivalent@
keyEquivalentSelector :: Selector '[] (Id NSString)
keyEquivalentSelector = mkSelector "keyEquivalent"

-- | @Selector@ for @setKeyEquivalent:@
setKeyEquivalentSelector :: Selector '[Id NSString] ()
setKeyEquivalentSelector = mkSelector "setKeyEquivalent:"

-- | @Selector@ for @keyEquivalentModifierMask@
keyEquivalentModifierMaskSelector :: Selector '[] NSEventModifierFlags
keyEquivalentModifierMaskSelector = mkSelector "keyEquivalentModifierMask"

-- | @Selector@ for @setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMaskSelector :: Selector '[NSEventModifierFlags] ()
setKeyEquivalentModifierMaskSelector = mkSelector "setKeyEquivalentModifierMask:"

-- | @Selector@ for @userKeyEquivalent@
userKeyEquivalentSelector :: Selector '[] (Id NSString)
userKeyEquivalentSelector = mkSelector "userKeyEquivalent"

-- | @Selector@ for @allowsKeyEquivalentWhenHidden@
allowsKeyEquivalentWhenHiddenSelector :: Selector '[] Bool
allowsKeyEquivalentWhenHiddenSelector = mkSelector "allowsKeyEquivalentWhenHidden"

-- | @Selector@ for @setAllowsKeyEquivalentWhenHidden:@
setAllowsKeyEquivalentWhenHiddenSelector :: Selector '[Bool] ()
setAllowsKeyEquivalentWhenHiddenSelector = mkSelector "setAllowsKeyEquivalentWhenHidden:"

-- | @Selector@ for @allowsAutomaticKeyEquivalentLocalization@
allowsAutomaticKeyEquivalentLocalizationSelector :: Selector '[] Bool
allowsAutomaticKeyEquivalentLocalizationSelector = mkSelector "allowsAutomaticKeyEquivalentLocalization"

-- | @Selector@ for @setAllowsAutomaticKeyEquivalentLocalization:@
setAllowsAutomaticKeyEquivalentLocalizationSelector :: Selector '[Bool] ()
setAllowsAutomaticKeyEquivalentLocalizationSelector = mkSelector "setAllowsAutomaticKeyEquivalentLocalization:"

-- | @Selector@ for @allowsAutomaticKeyEquivalentMirroring@
allowsAutomaticKeyEquivalentMirroringSelector :: Selector '[] Bool
allowsAutomaticKeyEquivalentMirroringSelector = mkSelector "allowsAutomaticKeyEquivalentMirroring"

-- | @Selector@ for @setAllowsAutomaticKeyEquivalentMirroring:@
setAllowsAutomaticKeyEquivalentMirroringSelector :: Selector '[Bool] ()
setAllowsAutomaticKeyEquivalentMirroringSelector = mkSelector "setAllowsAutomaticKeyEquivalentMirroring:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CLong
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[CLong] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @onStateImage@
onStateImageSelector :: Selector '[] (Id NSImage)
onStateImageSelector = mkSelector "onStateImage"

-- | @Selector@ for @setOnStateImage:@
setOnStateImageSelector :: Selector '[Id NSImage] ()
setOnStateImageSelector = mkSelector "setOnStateImage:"

-- | @Selector@ for @offStateImage@
offStateImageSelector :: Selector '[] (Id NSImage)
offStateImageSelector = mkSelector "offStateImage"

-- | @Selector@ for @setOffStateImage:@
setOffStateImageSelector :: Selector '[Id NSImage] ()
setOffStateImageSelector = mkSelector "setOffStateImage:"

-- | @Selector@ for @mixedStateImage@
mixedStateImageSelector :: Selector '[] (Id NSImage)
mixedStateImageSelector = mkSelector "mixedStateImage"

-- | @Selector@ for @setMixedStateImage:@
setMixedStateImageSelector :: Selector '[Id NSImage] ()
setMixedStateImageSelector = mkSelector "setMixedStateImage:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @alternate@
alternateSelector :: Selector '[] Bool
alternateSelector = mkSelector "alternate"

-- | @Selector@ for @setAlternate:@
setAlternateSelector :: Selector '[Bool] ()
setAlternateSelector = mkSelector "setAlternate:"

-- | @Selector@ for @indentationLevel@
indentationLevelSelector :: Selector '[] CLong
indentationLevelSelector = mkSelector "indentationLevel"

-- | @Selector@ for @setIndentationLevel:@
setIndentationLevelSelector :: Selector '[CLong] ()
setIndentationLevelSelector = mkSelector "setIndentationLevel:"

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

-- | @Selector@ for @tag@
tagSelector :: Selector '[] CLong
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector '[CLong] ()
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector '[] RawId
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector '[RawId] ()
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector '[Id NSView] ()
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector '[] Bool
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector '[] Bool
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector '[Bool] ()
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestorSelector :: Selector '[] Bool
hiddenOrHasHiddenAncestorSelector = mkSelector "hiddenOrHasHiddenAncestor"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector '[] (Id NSString)
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector '[Id NSString] ()
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @badge@
badgeSelector :: Selector '[] (Id NSMenuItemBadge)
badgeSelector = mkSelector "badge"

-- | @Selector@ for @setBadge:@
setBadgeSelector :: Selector '[Id NSMenuItemBadge] ()
setBadgeSelector = mkSelector "setBadge:"

