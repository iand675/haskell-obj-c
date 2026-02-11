{-# LANGUAGE PatternSynonyms #-}
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
  , separatorItemSelector
  , sectionHeaderWithTitleSelector
  , initWithTitle_action_keyEquivalentSelector
  , initWithCoderSelector
  , setMnemonicLocationSelector
  , mnemonicLocationSelector
  , mnemonicSelector
  , setTitleWithMnemonicSelector
  , usesUserKeyEquivalentsSelector
  , setUsesUserKeyEquivalentsSelector
  , writingToolsItemsSelector
  , menuSelector
  , setMenuSelector
  , hasSubmenuSelector
  , submenuSelector
  , setSubmenuSelector
  , parentItemSelector
  , titleSelector
  , setTitleSelector
  , attributedTitleSelector
  , setAttributedTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , sectionHeaderSelector
  , keyEquivalentSelector
  , setKeyEquivalentSelector
  , keyEquivalentModifierMaskSelector
  , setKeyEquivalentModifierMaskSelector
  , userKeyEquivalentSelector
  , allowsKeyEquivalentWhenHiddenSelector
  , setAllowsKeyEquivalentWhenHiddenSelector
  , allowsAutomaticKeyEquivalentLocalizationSelector
  , setAllowsAutomaticKeyEquivalentLocalizationSelector
  , allowsAutomaticKeyEquivalentMirroringSelector
  , setAllowsAutomaticKeyEquivalentMirroringSelector
  , imageSelector
  , setImageSelector
  , stateSelector
  , setStateSelector
  , onStateImageSelector
  , setOnStateImageSelector
  , offStateImageSelector
  , setOffStateImageSelector
  , mixedStateImageSelector
  , setMixedStateImageSelector
  , enabledSelector
  , setEnabledSelector
  , alternateSelector
  , setAlternateSelector
  , indentationLevelSelector
  , setIndentationLevelSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , tagSelector
  , setTagSelector
  , representedObjectSelector
  , setRepresentedObjectSelector
  , viewSelector
  , setViewSelector
  , highlightedSelector
  , hiddenSelector
  , setHiddenSelector
  , hiddenOrHasHiddenAncestorSelector
  , toolTipSelector
  , setToolTipSelector
  , badgeSelector
  , setBadgeSelector

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

-- | @+ separatorItem@
nsMenuItemSeparatorItem :: IO (Id NSMenuItem)
nsMenuItemSeparatorItem  =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMsg cls' (mkSelector "separatorItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates a menu item representing a section header with the provided title. Section header items are used to provide context to a grouping of menu items. Items created using this method are non-interactive and do not perform an action.
--
-- ObjC selector: @+ sectionHeaderWithTitle:@
sectionHeaderWithTitle :: IsNSString title => title -> IO (Id NSMenuItem)
sectionHeaderWithTitle title =
  do
    cls' <- getRequiredClass "NSMenuItem"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "sectionHeaderWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithTitle:action:keyEquivalent:@
initWithTitle_action_keyEquivalent :: (IsNSMenuItem nsMenuItem, IsNSString string, IsNSString charCode) => nsMenuItem -> string -> Selector -> charCode -> IO (Id NSMenuItem)
initWithTitle_action_keyEquivalent nsMenuItem  string selector charCode =
  withObjCPtr string $ \raw_string ->
    withObjCPtr charCode $ \raw_charCode ->
        sendMsg nsMenuItem (mkSelector "initWithTitle:action:keyEquivalent:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ()), argPtr (unSelector selector), argPtr (castPtr raw_charCode :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSMenuItem nsMenuItem, IsNSCoder coder) => nsMenuItem -> coder -> IO (Id NSMenuItem)
initWithCoder nsMenuItem  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsMenuItem (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- setMnemonicLocation:@
setMnemonicLocation :: IsNSMenuItem nsMenuItem => nsMenuItem -> CULong -> IO ()
setMnemonicLocation nsMenuItem  location =
    sendMsg nsMenuItem (mkSelector "setMnemonicLocation:") retVoid [argCULong location]

-- | @- mnemonicLocation@
mnemonicLocation :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CULong
mnemonicLocation nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "mnemonicLocation") retCULong []

-- | @- mnemonic@
mnemonic :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
mnemonic nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "mnemonic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSMenuItem nsMenuItem, IsNSString stringWithAmpersand) => nsMenuItem -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsMenuItem  stringWithAmpersand =
  withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
      sendMsg nsMenuItem (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | @+ usesUserKeyEquivalents@
usesUserKeyEquivalents :: IO Bool
usesUserKeyEquivalents  =
  do
    cls' <- getRequiredClass "NSMenuItem"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "usesUserKeyEquivalents") retCULong []

-- | @+ setUsesUserKeyEquivalents:@
setUsesUserKeyEquivalents :: Bool -> IO ()
setUsesUserKeyEquivalents value =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMsg cls' (mkSelector "setUsesUserKeyEquivalents:") retVoid [argCULong (if value then 1 else 0)]

-- | An array of standard menu items related to Writing Tools. Each call to this method returns an array of newly allocated instances of NSMenuItem.
--
-- ObjC selector: @+ writingToolsItems@
writingToolsItems :: IO (Id NSArray)
writingToolsItems  =
  do
    cls' <- getRequiredClass "NSMenuItem"
    sendClassMsg cls' (mkSelector "writingToolsItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Note: Never call the setter method directly: it is there only for subclassers.
--
-- ObjC selector: @- menu@
menu :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenu)
menu nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Note: Never call the setter method directly: it is there only for subclassers.
--
-- ObjC selector: @- setMenu:@
setMenu :: (IsNSMenuItem nsMenuItem, IsNSMenu value) => nsMenuItem -> value -> IO ()
setMenu nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasSubmenu@
hasSubmenu :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
hasSubmenu nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "hasSubmenu") retCULong []

-- | @- submenu@
submenu :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenu)
submenu nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "submenu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubmenu:@
setSubmenu :: (IsNSMenuItem nsMenuItem, IsNSMenu value) => nsMenuItem -> value -> IO ()
setSubmenu nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setSubmenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Returns: The @NSMenuItem@ whose submenu contains the receiver, or nil if the receiver does not have a parent item.
--
-- ObjC selector: @- parentItem@
parentItem :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenuItem)
parentItem nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "parentItem") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
title nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setTitle nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedTitle@
attributedTitle :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSAttributedString)
attributedTitle nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSMenuItem nsMenuItem, IsNSAttributedString value) => nsMenuItem -> value -> IO ()
setAttributedTitle nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Used to specify a standard subtitle for the menu item.
--
-- The subtitle is displayed below the standard title.
--
-- Note: On macOS 14, a menu item with an attributed title does not show the subtitle. The subtitle is shown on macOS 15 and later.
--
-- ObjC selector: @- subtitle@
subtitle :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
subtitle nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Used to specify a standard subtitle for the menu item.
--
-- The subtitle is displayed below the standard title.
--
-- Note: On macOS 14, a menu item with an attributed title does not show the subtitle. The subtitle is shown on macOS 15 and later.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setSubtitle nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- separatorItem@
separatorItem :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
separatorItem nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "separatorItem") retCULong []

-- | Indicates whether the item is a section header. Section header items are created using the @sectionHeader(title:)@ class method.
--
-- ObjC selector: @- sectionHeader@
sectionHeader :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
sectionHeader nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "sectionHeader") retCULong []

-- | @- keyEquivalent@
keyEquivalent :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
keyEquivalent nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "keyEquivalent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyEquivalent:@
setKeyEquivalent :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setKeyEquivalent nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setKeyEquivalent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyEquivalentModifierMask@
keyEquivalentModifierMask :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO NSEventModifierFlags
keyEquivalentModifierMask nsMenuItem  =
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg nsMenuItem (mkSelector "keyEquivalentModifierMask") retCULong []

-- | @- setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMask :: IsNSMenuItem nsMenuItem => nsMenuItem -> NSEventModifierFlags -> IO ()
setKeyEquivalentModifierMask nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setKeyEquivalentModifierMask:") retVoid [argCULong (coerce value)]

-- | @- userKeyEquivalent@
userKeyEquivalent :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
userKeyEquivalent nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "userKeyEquivalent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- allowsKeyEquivalentWhenHidden@
allowsKeyEquivalentWhenHidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
allowsKeyEquivalentWhenHidden nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "allowsKeyEquivalentWhenHidden") retCULong []

-- | @- setAllowsKeyEquivalentWhenHidden:@
setAllowsKeyEquivalentWhenHidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAllowsKeyEquivalentWhenHidden nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setAllowsKeyEquivalentWhenHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsAutomaticKeyEquivalentLocalization@
allowsAutomaticKeyEquivalentLocalization :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
allowsAutomaticKeyEquivalentLocalization nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "allowsAutomaticKeyEquivalentLocalization") retCULong []

-- | @- setAllowsAutomaticKeyEquivalentLocalization:@
setAllowsAutomaticKeyEquivalentLocalization :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAllowsAutomaticKeyEquivalentLocalization nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setAllowsAutomaticKeyEquivalentLocalization:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsAutomaticKeyEquivalentMirroring@
allowsAutomaticKeyEquivalentMirroring :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
allowsAutomaticKeyEquivalentMirroring nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "allowsAutomaticKeyEquivalentMirroring") retCULong []

-- | @- setAllowsAutomaticKeyEquivalentMirroring:@
setAllowsAutomaticKeyEquivalentMirroring :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAllowsAutomaticKeyEquivalentMirroring nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setAllowsAutomaticKeyEquivalentMirroring:") retVoid [argCULong (if value then 1 else 0)]

-- | @- image@
image :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
image nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setImage nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- state@
state :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CLong
state nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "state") retCLong []

-- | @- setState:@
setState :: IsNSMenuItem nsMenuItem => nsMenuItem -> CLong -> IO ()
setState nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setState:") retVoid [argCLong value]

-- | @- onStateImage@
onStateImage :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
onStateImage nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "onStateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOnStateImage:@
setOnStateImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setOnStateImage nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setOnStateImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- offStateImage@
offStateImage :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
offStateImage nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "offStateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffStateImage:@
setOffStateImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setOffStateImage nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setOffStateImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- mixedStateImage@
mixedStateImage :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSImage)
mixedStateImage nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "mixedStateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMixedStateImage:@
setMixedStateImage :: (IsNSMenuItem nsMenuItem, IsNSImage value) => nsMenuItem -> value -> IO ()
setMixedStateImage nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setMixedStateImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- enabled@
enabled :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
enabled nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setEnabled nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- alternate@
alternate :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
alternate nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "alternate") retCULong []

-- | @- setAlternate:@
setAlternate :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setAlternate nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setAlternate:") retVoid [argCULong (if value then 1 else 0)]

-- | @- indentationLevel@
indentationLevel :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CLong
indentationLevel nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "indentationLevel") retCLong []

-- | @- setIndentationLevel:@
setIndentationLevel :: IsNSMenuItem nsMenuItem => nsMenuItem -> CLong -> IO ()
setIndentationLevel nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setIndentationLevel:") retVoid [argCLong value]

-- | @- target@
target :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO RawId
target nsMenuItem  =
    fmap (RawId . castPtr) $ sendMsg nsMenuItem (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSMenuItem nsMenuItem => nsMenuItem -> RawId -> IO ()
setTarget nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Selector
action nsMenuItem  =
    fmap (Selector . castPtr) $ sendMsg nsMenuItem (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSMenuItem nsMenuItem => nsMenuItem -> Selector -> IO ()
setAction nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- tag@
tag :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO CLong
tag nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "tag") retCLong []

-- | @- setTag:@
setTag :: IsNSMenuItem nsMenuItem => nsMenuItem -> CLong -> IO ()
setTag nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setTag:") retVoid [argCLong value]

-- | @- representedObject@
representedObject :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO RawId
representedObject nsMenuItem  =
    fmap (RawId . castPtr) $ sendMsg nsMenuItem (mkSelector "representedObject") (retPtr retVoid) []

-- | @- setRepresentedObject:@
setRepresentedObject :: IsNSMenuItem nsMenuItem => nsMenuItem -> RawId -> IO ()
setRepresentedObject nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setRepresentedObject:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- view@
view :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSView)
view nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setView:@
setView :: (IsNSMenuItem nsMenuItem, IsNSView value) => nsMenuItem -> value -> IO ()
setView nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- highlighted@
highlighted :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
highlighted nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "highlighted") retCULong []

-- | @- hidden@
hidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
hidden nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "hidden") retCULong []

-- | @- setHidden:@
setHidden :: IsNSMenuItem nsMenuItem => nsMenuItem -> Bool -> IO ()
setHidden nsMenuItem  value =
    sendMsg nsMenuItem (mkSelector "setHidden:") retVoid [argCULong (if value then 1 else 0)]

-- | @- hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestor :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO Bool
hiddenOrHasHiddenAncestor nsMenuItem  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMenuItem (mkSelector "hiddenOrHasHiddenAncestor") retCULong []

-- | @- toolTip@
toolTip :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSString)
toolTip nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "toolTip") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setToolTip:@
setToolTip :: (IsNSMenuItem nsMenuItem, IsNSString value) => nsMenuItem -> value -> IO ()
setToolTip nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setToolTip:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A badge used to provide additional quantitative information specific to the menu item, such as the number of available updates.
--
-- Note: The default value of this property is @nil.@
--
-- ObjC selector: @- badge@
badge :: IsNSMenuItem nsMenuItem => nsMenuItem -> IO (Id NSMenuItemBadge)
badge nsMenuItem  =
    sendMsg nsMenuItem (mkSelector "badge") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A badge used to provide additional quantitative information specific to the menu item, such as the number of available updates.
--
-- Note: The default value of this property is @nil.@
--
-- ObjC selector: @- setBadge:@
setBadge :: (IsNSMenuItem nsMenuItem, IsNSMenuItemBadge value) => nsMenuItem -> value -> IO ()
setBadge nsMenuItem  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsMenuItem (mkSelector "setBadge:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @separatorItem@
separatorItemSelector :: Selector
separatorItemSelector = mkSelector "separatorItem"

-- | @Selector@ for @sectionHeaderWithTitle:@
sectionHeaderWithTitleSelector :: Selector
sectionHeaderWithTitleSelector = mkSelector "sectionHeaderWithTitle:"

-- | @Selector@ for @initWithTitle:action:keyEquivalent:@
initWithTitle_action_keyEquivalentSelector :: Selector
initWithTitle_action_keyEquivalentSelector = mkSelector "initWithTitle:action:keyEquivalent:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setMnemonicLocation:@
setMnemonicLocationSelector :: Selector
setMnemonicLocationSelector = mkSelector "setMnemonicLocation:"

-- | @Selector@ for @mnemonicLocation@
mnemonicLocationSelector :: Selector
mnemonicLocationSelector = mkSelector "mnemonicLocation"

-- | @Selector@ for @mnemonic@
mnemonicSelector :: Selector
mnemonicSelector = mkSelector "mnemonic"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @usesUserKeyEquivalents@
usesUserKeyEquivalentsSelector :: Selector
usesUserKeyEquivalentsSelector = mkSelector "usesUserKeyEquivalents"

-- | @Selector@ for @setUsesUserKeyEquivalents:@
setUsesUserKeyEquivalentsSelector :: Selector
setUsesUserKeyEquivalentsSelector = mkSelector "setUsesUserKeyEquivalents:"

-- | @Selector@ for @writingToolsItems@
writingToolsItemsSelector :: Selector
writingToolsItemsSelector = mkSelector "writingToolsItems"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @hasSubmenu@
hasSubmenuSelector :: Selector
hasSubmenuSelector = mkSelector "hasSubmenu"

-- | @Selector@ for @submenu@
submenuSelector :: Selector
submenuSelector = mkSelector "submenu"

-- | @Selector@ for @setSubmenu:@
setSubmenuSelector :: Selector
setSubmenuSelector = mkSelector "setSubmenu:"

-- | @Selector@ for @parentItem@
parentItemSelector :: Selector
parentItemSelector = mkSelector "parentItem"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @attributedTitle@
attributedTitleSelector :: Selector
attributedTitleSelector = mkSelector "attributedTitle"

-- | @Selector@ for @setAttributedTitle:@
setAttributedTitleSelector :: Selector
setAttributedTitleSelector = mkSelector "setAttributedTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @sectionHeader@
sectionHeaderSelector :: Selector
sectionHeaderSelector = mkSelector "sectionHeader"

-- | @Selector@ for @keyEquivalent@
keyEquivalentSelector :: Selector
keyEquivalentSelector = mkSelector "keyEquivalent"

-- | @Selector@ for @setKeyEquivalent:@
setKeyEquivalentSelector :: Selector
setKeyEquivalentSelector = mkSelector "setKeyEquivalent:"

-- | @Selector@ for @keyEquivalentModifierMask@
keyEquivalentModifierMaskSelector :: Selector
keyEquivalentModifierMaskSelector = mkSelector "keyEquivalentModifierMask"

-- | @Selector@ for @setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMaskSelector :: Selector
setKeyEquivalentModifierMaskSelector = mkSelector "setKeyEquivalentModifierMask:"

-- | @Selector@ for @userKeyEquivalent@
userKeyEquivalentSelector :: Selector
userKeyEquivalentSelector = mkSelector "userKeyEquivalent"

-- | @Selector@ for @allowsKeyEquivalentWhenHidden@
allowsKeyEquivalentWhenHiddenSelector :: Selector
allowsKeyEquivalentWhenHiddenSelector = mkSelector "allowsKeyEquivalentWhenHidden"

-- | @Selector@ for @setAllowsKeyEquivalentWhenHidden:@
setAllowsKeyEquivalentWhenHiddenSelector :: Selector
setAllowsKeyEquivalentWhenHiddenSelector = mkSelector "setAllowsKeyEquivalentWhenHidden:"

-- | @Selector@ for @allowsAutomaticKeyEquivalentLocalization@
allowsAutomaticKeyEquivalentLocalizationSelector :: Selector
allowsAutomaticKeyEquivalentLocalizationSelector = mkSelector "allowsAutomaticKeyEquivalentLocalization"

-- | @Selector@ for @setAllowsAutomaticKeyEquivalentLocalization:@
setAllowsAutomaticKeyEquivalentLocalizationSelector :: Selector
setAllowsAutomaticKeyEquivalentLocalizationSelector = mkSelector "setAllowsAutomaticKeyEquivalentLocalization:"

-- | @Selector@ for @allowsAutomaticKeyEquivalentMirroring@
allowsAutomaticKeyEquivalentMirroringSelector :: Selector
allowsAutomaticKeyEquivalentMirroringSelector = mkSelector "allowsAutomaticKeyEquivalentMirroring"

-- | @Selector@ for @setAllowsAutomaticKeyEquivalentMirroring:@
setAllowsAutomaticKeyEquivalentMirroringSelector :: Selector
setAllowsAutomaticKeyEquivalentMirroringSelector = mkSelector "setAllowsAutomaticKeyEquivalentMirroring:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @onStateImage@
onStateImageSelector :: Selector
onStateImageSelector = mkSelector "onStateImage"

-- | @Selector@ for @setOnStateImage:@
setOnStateImageSelector :: Selector
setOnStateImageSelector = mkSelector "setOnStateImage:"

-- | @Selector@ for @offStateImage@
offStateImageSelector :: Selector
offStateImageSelector = mkSelector "offStateImage"

-- | @Selector@ for @setOffStateImage:@
setOffStateImageSelector :: Selector
setOffStateImageSelector = mkSelector "setOffStateImage:"

-- | @Selector@ for @mixedStateImage@
mixedStateImageSelector :: Selector
mixedStateImageSelector = mkSelector "mixedStateImage"

-- | @Selector@ for @setMixedStateImage:@
setMixedStateImageSelector :: Selector
setMixedStateImageSelector = mkSelector "setMixedStateImage:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @alternate@
alternateSelector :: Selector
alternateSelector = mkSelector "alternate"

-- | @Selector@ for @setAlternate:@
setAlternateSelector :: Selector
setAlternateSelector = mkSelector "setAlternate:"

-- | @Selector@ for @indentationLevel@
indentationLevelSelector :: Selector
indentationLevelSelector = mkSelector "indentationLevel"

-- | @Selector@ for @setIndentationLevel:@
setIndentationLevelSelector :: Selector
setIndentationLevelSelector = mkSelector "setIndentationLevel:"

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

-- | @Selector@ for @tag@
tagSelector :: Selector
tagSelector = mkSelector "tag"

-- | @Selector@ for @setTag:@
setTagSelector :: Selector
setTagSelector = mkSelector "setTag:"

-- | @Selector@ for @representedObject@
representedObjectSelector :: Selector
representedObjectSelector = mkSelector "representedObject"

-- | @Selector@ for @setRepresentedObject:@
setRepresentedObjectSelector :: Selector
setRepresentedObjectSelector = mkSelector "setRepresentedObject:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @setView:@
setViewSelector :: Selector
setViewSelector = mkSelector "setView:"

-- | @Selector@ for @highlighted@
highlightedSelector :: Selector
highlightedSelector = mkSelector "highlighted"

-- | @Selector@ for @hidden@
hiddenSelector :: Selector
hiddenSelector = mkSelector "hidden"

-- | @Selector@ for @setHidden:@
setHiddenSelector :: Selector
setHiddenSelector = mkSelector "setHidden:"

-- | @Selector@ for @hiddenOrHasHiddenAncestor@
hiddenOrHasHiddenAncestorSelector :: Selector
hiddenOrHasHiddenAncestorSelector = mkSelector "hiddenOrHasHiddenAncestor"

-- | @Selector@ for @toolTip@
toolTipSelector :: Selector
toolTipSelector = mkSelector "toolTip"

-- | @Selector@ for @setToolTip:@
setToolTipSelector :: Selector
setToolTipSelector = mkSelector "setToolTip:"

-- | @Selector@ for @badge@
badgeSelector :: Selector
badgeSelector = mkSelector "badge"

-- | @Selector@ for @setBadge:@
setBadgeSelector :: Selector
setBadgeSelector = mkSelector "setBadge:"

