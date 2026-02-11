{-# LANGUAGE PatternSynonyms #-}
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
  , comboButtonWithTitle_menu_target_actionSelector
  , comboButtonWithImage_menu_target_actionSelector
  , comboButtonWithTitle_image_menu_target_actionSelector
  , titleSelector
  , setTitleSelector
  , imageSelector
  , setImageSelector
  , imageScalingSelector
  , setImageScalingSelector
  , menuSelector
  , setMenuSelector
  , styleSelector
  , setStyleSelector

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
comboButtonWithTitle_menu_target_action :: (IsNSString title, IsNSMenu menu) => title -> menu -> RawId -> Selector -> IO (Id NSComboButton)
comboButtonWithTitle_menu_target_action title menu target action =
  do
    cls' <- getRequiredClass "NSComboButton"
    withObjCPtr title $ \raw_title ->
      withObjCPtr menu $ \raw_menu ->
        sendClassMsg cls' (mkSelector "comboButtonWithTitle:menu:target:action:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

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
comboButtonWithImage_menu_target_action :: (IsNSImage image, IsNSMenu menu) => image -> menu -> RawId -> Selector -> IO (Id NSComboButton)
comboButtonWithImage_menu_target_action image menu target action =
  do
    cls' <- getRequiredClass "NSComboButton"
    withObjCPtr image $ \raw_image ->
      withObjCPtr menu $ \raw_menu ->
        sendClassMsg cls' (mkSelector "comboButtonWithImage:menu:target:action:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

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
comboButtonWithTitle_image_menu_target_action :: (IsNSString title, IsNSImage image, IsNSMenu menu) => title -> image -> menu -> RawId -> Selector -> IO (Id NSComboButton)
comboButtonWithTitle_image_menu_target_action title image menu target action =
  do
    cls' <- getRequiredClass "NSComboButton"
    withObjCPtr title $ \raw_title ->
      withObjCPtr image $ \raw_image ->
        withObjCPtr menu $ \raw_menu ->
          sendClassMsg cls' (mkSelector "comboButtonWithTitle:image:menu:target:action:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr raw_menu :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | The title displayed on the control. The default value is an empty string.
--
-- ObjC selector: @- title@
title :: IsNSComboButton nsComboButton => nsComboButton -> IO (Id NSString)
title nsComboButton  =
  sendMsg nsComboButton (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title displayed on the control. The default value is an empty string.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSComboButton nsComboButton, IsNSString value) => nsComboButton -> value -> IO ()
setTitle nsComboButton  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsComboButton (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The image displayed on the control. The default value is @nil@.
--
-- ObjC selector: @- image@
image :: IsNSComboButton nsComboButton => nsComboButton -> IO (Id NSImage)
image nsComboButton  =
  sendMsg nsComboButton (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The image displayed on the control. The default value is @nil@.
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSComboButton nsComboButton, IsNSImage value) => nsComboButton -> value -> IO ()
setImage nsComboButton  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsComboButton (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The scaling mode applied to fit the button's image within the content area. The default value is @NSImageScaleProportionallyDown@.
--
-- ObjC selector: @- imageScaling@
imageScaling :: IsNSComboButton nsComboButton => nsComboButton -> IO NSImageScaling
imageScaling nsComboButton  =
  fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsComboButton (mkSelector "imageScaling") retCULong []

-- | The scaling mode applied to fit the button's image within the content area. The default value is @NSImageScaleProportionallyDown@.
--
-- ObjC selector: @- setImageScaling:@
setImageScaling :: IsNSComboButton nsComboButton => nsComboButton -> NSImageScaling -> IO ()
setImageScaling nsComboButton  value =
  sendMsg nsComboButton (mkSelector "setImageScaling:") retVoid [argCULong (coerce value)]

-- | Overrides behavior of NSResponder menu. This menu is shown when interacting with the button (see NSComboButtonStyle). NSComboButton does not have a context menu. Items in this menu specify their own target and action independent of the primary action that is configured on the control.
--
-- ObjC selector: @- menu@
menu :: IsNSComboButton nsComboButton => nsComboButton -> IO (Id NSMenu)
menu nsComboButton  =
  sendMsg nsComboButton (mkSelector "menu") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Overrides behavior of NSResponder menu. This menu is shown when interacting with the button (see NSComboButtonStyle). NSComboButton does not have a context menu. Items in this menu specify their own target and action independent of the primary action that is configured on the control.
--
-- ObjC selector: @- setMenu:@
setMenu :: (IsNSComboButton nsComboButton, IsNSMenu value) => nsComboButton -> value -> IO ()
setMenu nsComboButton  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsComboButton (mkSelector "setMenu:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Specifies the visual presentation and behavior for NSComboButton's primary action and its menu. The default value is @NSComboButtonStyleSplit@.
--
-- ObjC selector: @- style@
style :: IsNSComboButton nsComboButton => nsComboButton -> IO NSComboButtonStyle
style nsComboButton  =
  fmap (coerce :: CLong -> NSComboButtonStyle) $ sendMsg nsComboButton (mkSelector "style") retCLong []

-- | Specifies the visual presentation and behavior for NSComboButton's primary action and its menu. The default value is @NSComboButtonStyleSplit@.
--
-- ObjC selector: @- setStyle:@
setStyle :: IsNSComboButton nsComboButton => nsComboButton -> NSComboButtonStyle -> IO ()
setStyle nsComboButton  value =
  sendMsg nsComboButton (mkSelector "setStyle:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @comboButtonWithTitle:menu:target:action:@
comboButtonWithTitle_menu_target_actionSelector :: Selector
comboButtonWithTitle_menu_target_actionSelector = mkSelector "comboButtonWithTitle:menu:target:action:"

-- | @Selector@ for @comboButtonWithImage:menu:target:action:@
comboButtonWithImage_menu_target_actionSelector :: Selector
comboButtonWithImage_menu_target_actionSelector = mkSelector "comboButtonWithImage:menu:target:action:"

-- | @Selector@ for @comboButtonWithTitle:image:menu:target:action:@
comboButtonWithTitle_image_menu_target_actionSelector :: Selector
comboButtonWithTitle_image_menu_target_actionSelector = mkSelector "comboButtonWithTitle:image:menu:target:action:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @menu@
menuSelector :: Selector
menuSelector = mkSelector "menu"

-- | @Selector@ for @setMenu:@
setMenuSelector :: Selector
setMenuSelector = mkSelector "setMenu:"

-- | @Selector@ for @style@
styleSelector :: Selector
styleSelector = mkSelector "style"

-- | @Selector@ for @setStyle:@
setStyleSelector :: Selector
setStyleSelector = mkSelector "setStyle:"

