{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSComboButton@.
module ObjC.AppKit.NSComboButton
  ( NSComboButton
  , IsNSComboButton(..)
  , comboButtonWithTitle_menu_target_action
  , comboButtonWithImage_menu_target_action
  , comboButtonWithTitle_image_menu_target_action
  , title
  , setTitle
  , image
  , setImage
  , imageScaling
  , setImageScaling
  , menu
  , setMenu
  , style
  , setStyle
  , comboButtonWithImage_menu_target_actionSelector
  , comboButtonWithTitle_image_menu_target_actionSelector
  , comboButtonWithTitle_menu_target_actionSelector
  , imageScalingSelector
  , imageSelector
  , menuSelector
  , setImageScalingSelector
  , setImageSelector
  , setMenuSelector
  , setStyleSelector
  , setTitleSelector
  , styleSelector
  , titleSelector

  -- * Enum types
  , NSComboButtonStyle(NSComboButtonStyle)
  , pattern NSComboButtonStyleSplit
  , pattern NSComboButtonStyleUnified
  , NSImageScaling(NSImageScaling)
  , pattern NSImageScaleProportionallyDown
  , pattern NSImageScaleAxesIndependently
  , pattern NSImageScaleNone
  , pattern NSImageScaleProportionallyUpOrDown
  , pattern NSScaleProportionally
  , pattern NSScaleToFit
  , pattern NSScaleNone

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

-- | Creates a standard combo button with a title, menu, and primary action.
--
-- @title@ — The localized title string that is displayed on the button.
--
-- @menu@ — The additional menu to display on the button.
--
-- @target@ — The target object that receives primary action messages from the control.
--
-- @action@ — The action message sent by the primary action portion of the control.
--
-- ObjC selector: @+ comboButtonWithTitle:menu:target:action:@
comboButtonWithTitle_menu_target_action :: (IsNSString title, IsNSMenu menu) => title -> menu -> RawId -> Sel -> IO (Id NSComboButton)
comboButtonWithTitle_menu_target_action title menu target action =
  do
    cls' <- getRequiredClass "NSComboButton"
    sendClassMessage cls' comboButtonWithTitle_menu_target_actionSelector (toNSString title) (toNSMenu menu) target action

-- | Creates a standard combo button with a image, menu, and primary action.
--
-- @image@ — The image to display in the body of the button.
--
-- @menu@ — The additional menu to display on the button.
--
-- @target@ — The target object that receives primary action messages from the control.
--
-- @action@ — The action message sent by the primary action portion of the control.
--
-- ObjC selector: @+ comboButtonWithImage:menu:target:action:@
comboButtonWithImage_menu_target_action :: (IsNSImage image, IsNSMenu menu) => image -> menu -> RawId -> Sel -> IO (Id NSComboButton)
comboButtonWithImage_menu_target_action image menu target action =
  do
    cls' <- getRequiredClass "NSComboButton"
    sendClassMessage cls' comboButtonWithImage_menu_target_actionSelector (toNSImage image) (toNSMenu menu) target action

-- | Creates a standard combo button with a title, image, menu, and primary action.
--
-- @title@ — The localized title string that is displayed on the button.
--
-- @image@ — The image to display in the body of the button.
--
-- @menu@ — The additional menu to display on the button.
--
-- @target@ — The target object that receives primary action messages from the control.
--
-- @action@ — The action message sent by the primary action portion of the control.
--
-- ObjC selector: @+ comboButtonWithTitle:image:menu:target:action:@
comboButtonWithTitle_image_menu_target_action :: (IsNSString title, IsNSImage image, IsNSMenu menu) => title -> image -> menu -> RawId -> Sel -> IO (Id NSComboButton)
comboButtonWithTitle_image_menu_target_action title image menu target action =
  do
    cls' <- getRequiredClass "NSComboButton"
    sendClassMessage cls' comboButtonWithTitle_image_menu_target_actionSelector (toNSString title) (toNSImage image) (toNSMenu menu) target action

-- | The title displayed on the control. The default value is an empty string.
--
-- ObjC selector: @- title@
title :: IsNSComboButton nsComboButton => nsComboButton -> IO (Id NSString)
title nsComboButton =
  sendMessage nsComboButton titleSelector

-- | The title displayed on the control. The default value is an empty string.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSComboButton nsComboButton, IsNSString value) => nsComboButton -> value -> IO ()
setTitle nsComboButton value =
  sendMessage nsComboButton setTitleSelector (toNSString value)

-- | The image displayed on the control. The default value is @nil@.
--
-- ObjC selector: @- image@
image :: IsNSComboButton nsComboButton => nsComboButton -> IO (Id NSImage)
image nsComboButton =
  sendMessage nsComboButton imageSelector

-- | The image displayed on the control. The default value is @nil@.
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSComboButton nsComboButton, IsNSImage value) => nsComboButton -> value -> IO ()
setImage nsComboButton value =
  sendMessage nsComboButton setImageSelector (toNSImage value)

-- | The scaling mode applied to fit the button's image within the content area. The default value is @NSImageScaleProportionallyDown@.
--
-- ObjC selector: @- imageScaling@
imageScaling :: IsNSComboButton nsComboButton => nsComboButton -> IO NSImageScaling
imageScaling nsComboButton =
  sendMessage nsComboButton imageScalingSelector

-- | The scaling mode applied to fit the button's image within the content area. The default value is @NSImageScaleProportionallyDown@.
--
-- ObjC selector: @- setImageScaling:@
setImageScaling :: IsNSComboButton nsComboButton => nsComboButton -> NSImageScaling -> IO ()
setImageScaling nsComboButton value =
  sendMessage nsComboButton setImageScalingSelector value

-- | Overrides behavior of NSResponder menu. This menu is shown when interacting with the button (see NSComboButtonStyle). NSComboButton does not have a context menu. Items in this menu specify their own target and action independent of the primary action that is configured on the control.
--
-- ObjC selector: @- menu@
menu :: IsNSComboButton nsComboButton => nsComboButton -> IO (Id NSMenu)
menu nsComboButton =
  sendMessage nsComboButton menuSelector

-- | Overrides behavior of NSResponder menu. This menu is shown when interacting with the button (see NSComboButtonStyle). NSComboButton does not have a context menu. Items in this menu specify their own target and action independent of the primary action that is configured on the control.
--
-- ObjC selector: @- setMenu:@
setMenu :: (IsNSComboButton nsComboButton, IsNSMenu value) => nsComboButton -> value -> IO ()
setMenu nsComboButton value =
  sendMessage nsComboButton setMenuSelector (toNSMenu value)

-- | Specifies the visual presentation and behavior for NSComboButton's primary action and its menu. The default value is @NSComboButtonStyleSplit@.
--
-- ObjC selector: @- style@
style :: IsNSComboButton nsComboButton => nsComboButton -> IO NSComboButtonStyle
style nsComboButton =
  sendMessage nsComboButton styleSelector

-- | Specifies the visual presentation and behavior for NSComboButton's primary action and its menu. The default value is @NSComboButtonStyleSplit@.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsNSComboButton nsComboButton => nsComboButton -> NSComboButtonStyle -> IO ()
setStyle nsComboButton value =
  sendMessage nsComboButton setStyleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @comboButtonWithTitle:menu:target:action:@
comboButtonWithTitle_menu_target_actionSelector :: Selector '[Id NSString, Id NSMenu, RawId, Sel] (Id NSComboButton)
comboButtonWithTitle_menu_target_actionSelector = mkSelector "comboButtonWithTitle:menu:target:action:"

-- | @Selector@ for @comboButtonWithImage:menu:target:action:@
comboButtonWithImage_menu_target_actionSelector :: Selector '[Id NSImage, Id NSMenu, RawId, Sel] (Id NSComboButton)
comboButtonWithImage_menu_target_actionSelector = mkSelector "comboButtonWithImage:menu:target:action:"

-- | @Selector@ for @comboButtonWithTitle:image:menu:target:action:@
comboButtonWithTitle_image_menu_target_actionSelector :: Selector '[Id NSString, Id NSImage, Id NSMenu, RawId, Sel] (Id NSComboButton)
comboButtonWithTitle_image_menu_target_actionSelector = mkSelector "comboButtonWithTitle:image:menu:target:action:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector '[] NSImageScaling
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector '[NSImageScaling] ()
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @menu@
menuSelector :: Selector '[] (Id NSMenu)
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector '[Id NSMenu] ()
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @style@
styleSelector :: Selector '[] NSComboButtonStyle
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector '[NSComboButtonStyle] ()
setStyleSelector = mkSelector "setStyle:"

