{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSButton@.
module ObjC.AppKit.NSButton
  ( NSButton
  , IsNSButton(..)
  , buttonWithTitle_image_target_action
  , buttonWithTitle_target_action
  , buttonWithImage_target_action
  , checkboxWithTitle_target_action
  , radioButtonWithTitle_target_action
  , setButtonType
  , setPeriodicDelay_interval
  , getPeriodicDelay_interval
  , setNextState
  , highlight
  , performKeyEquivalent
  , compressWithPrioritizedCompressionOptions
  , minimumSizeWithPrioritizedCompressionOptions
  , setTitleWithMnemonic
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , alternateTitle
  , setAlternateTitle
  , attributedAlternateTitle
  , setAttributedAlternateTitle
  , hasDestructiveAction
  , setHasDestructiveAction
  , sound
  , setSound
  , springLoaded
  , setSpringLoaded
  , maxAcceleratorLevel
  , setMaxAcceleratorLevel
  , bezelStyle
  , setBezelStyle
  , bordered
  , setBordered
  , transparent
  , setTransparent
  , showsBorderOnlyWhileMouseInside
  , setShowsBorderOnlyWhileMouseInside
  , bezelColor
  , setBezelColor
  , contentTintColor
  , setContentTintColor
  , tintProminence
  , setTintProminence
  , image
  , setImage
  , alternateImage
  , setAlternateImage
  , imagePosition
  , setImagePosition
  , imageScaling
  , setImageScaling
  , imageHugsTitle
  , setImageHugsTitle
  , symbolConfiguration
  , setSymbolConfiguration
  , state
  , setState
  , allowsMixedState
  , setAllowsMixedState
  , keyEquivalent
  , setKeyEquivalent
  , keyEquivalentModifierMask
  , setKeyEquivalentModifierMask
  , activeCompressionOptions
  , borderShape
  , setBorderShape
  , buttonWithTitle_image_target_actionSelector
  , buttonWithTitle_target_actionSelector
  , buttonWithImage_target_actionSelector
  , checkboxWithTitle_target_actionSelector
  , radioButtonWithTitle_target_actionSelector
  , setButtonTypeSelector
  , setPeriodicDelay_intervalSelector
  , getPeriodicDelay_intervalSelector
  , setNextStateSelector
  , highlightSelector
  , performKeyEquivalentSelector
  , compressWithPrioritizedCompressionOptionsSelector
  , minimumSizeWithPrioritizedCompressionOptionsSelector
  , setTitleWithMnemonicSelector
  , titleSelector
  , setTitleSelector
  , attributedTitleSelector
  , setAttributedTitleSelector
  , alternateTitleSelector
  , setAlternateTitleSelector
  , attributedAlternateTitleSelector
  , setAttributedAlternateTitleSelector
  , hasDestructiveActionSelector
  , setHasDestructiveActionSelector
  , soundSelector
  , setSoundSelector
  , springLoadedSelector
  , setSpringLoadedSelector
  , maxAcceleratorLevelSelector
  , setMaxAcceleratorLevelSelector
  , bezelStyleSelector
  , setBezelStyleSelector
  , borderedSelector
  , setBorderedSelector
  , transparentSelector
  , setTransparentSelector
  , showsBorderOnlyWhileMouseInsideSelector
  , setShowsBorderOnlyWhileMouseInsideSelector
  , bezelColorSelector
  , setBezelColorSelector
  , contentTintColorSelector
  , setContentTintColorSelector
  , tintProminenceSelector
  , setTintProminenceSelector
  , imageSelector
  , setImageSelector
  , alternateImageSelector
  , setAlternateImageSelector
  , imagePositionSelector
  , setImagePositionSelector
  , imageScalingSelector
  , setImageScalingSelector
  , imageHugsTitleSelector
  , setImageHugsTitleSelector
  , symbolConfigurationSelector
  , setSymbolConfigurationSelector
  , stateSelector
  , setStateSelector
  , allowsMixedStateSelector
  , setAllowsMixedStateSelector
  , keyEquivalentSelector
  , setKeyEquivalentSelector
  , keyEquivalentModifierMaskSelector
  , setKeyEquivalentModifierMaskSelector
  , activeCompressionOptionsSelector
  , borderShapeSelector
  , setBorderShapeSelector

  -- * Enum types
  , NSBezelStyle(NSBezelStyle)
  , pattern NSBezelStyleAutomatic
  , pattern NSBezelStylePush
  , pattern NSBezelStyleFlexiblePush
  , pattern NSBezelStyleDisclosure
  , pattern NSBezelStyleCircular
  , pattern NSBezelStyleHelpButton
  , pattern NSBezelStyleSmallSquare
  , pattern NSBezelStyleToolbar
  , pattern NSBezelStyleAccessoryBarAction
  , pattern NSBezelStyleAccessoryBar
  , pattern NSBezelStylePushDisclosure
  , pattern NSBezelStyleBadge
  , pattern NSBezelStyleGlass
  , pattern NSBezelStyleShadowlessSquare
  , pattern NSBezelStyleTexturedSquare
  , pattern NSBezelStyleRounded
  , pattern NSBezelStyleRegularSquare
  , pattern NSBezelStyleTexturedRounded
  , pattern NSBezelStyleRoundRect
  , pattern NSBezelStyleRecessed
  , pattern NSBezelStyleRoundedDisclosure
  , pattern NSBezelStyleInline
  , NSButtonType(NSButtonType)
  , pattern NSButtonTypeMomentaryLight
  , pattern NSButtonTypePushOnPushOff
  , pattern NSButtonTypeToggle
  , pattern NSButtonTypeSwitch
  , pattern NSButtonTypeRadio
  , pattern NSButtonTypeMomentaryChange
  , pattern NSButtonTypeOnOff
  , pattern NSButtonTypeMomentaryPushIn
  , pattern NSButtonTypeAccelerator
  , pattern NSButtonTypeMultiLevelAccelerator
  , NSCellImagePosition(NSCellImagePosition)
  , pattern NSNoImage
  , pattern NSImageOnly
  , pattern NSImageLeft
  , pattern NSImageRight
  , pattern NSImageBelow
  , pattern NSImageAbove
  , pattern NSImageOverlaps
  , pattern NSImageLeading
  , pattern NSImageTrailing
  , NSControlBorderShape(NSControlBorderShape)
  , pattern NSControlBorderShapeAutomatic
  , pattern NSControlBorderShapeCapsule
  , pattern NSControlBorderShapeRoundedRectangle
  , pattern NSControlBorderShapeCircle
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
  , NSImageScaling(NSImageScaling)
  , pattern NSImageScaleProportionallyDown
  , pattern NSImageScaleAxesIndependently
  , pattern NSImageScaleNone
  , pattern NSImageScaleProportionallyUpOrDown
  , pattern NSScaleProportionally
  , pattern NSScaleToFit
  , pattern NSScaleNone
  , NSTintProminence(NSTintProminence)
  , pattern NSTintProminenceAutomatic
  , pattern NSTintProminenceNone
  , pattern NSTintProminencePrimary
  , pattern NSTintProminenceSecondary

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

-- | Creates a standard push button with a title and image.
--
-- @title@ — The localized title string that is displayed on the button.
--
-- @image@ — The image that is displayed alongside the title. In left-to-right localizations, the image is displayed to the left of the title. In right-to-left localizations, it is displayed to the right.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized button object.
--
-- ObjC selector: @+ buttonWithTitle:image:target:action:@
buttonWithTitle_image_target_action :: (IsNSString title, IsNSImage image) => title -> image -> RawId -> Selector -> IO (Id NSButton)
buttonWithTitle_image_target_action title image target action =
  do
    cls' <- getRequiredClass "NSButton"
    withObjCPtr title $ \raw_title ->
      withObjCPtr image $ \raw_image ->
        sendClassMsg cls' (mkSelector "buttonWithTitle:image:target:action:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Creates a standard push button with the provided title.
--
-- @title@ — The localized title string that is displayed on the button.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized button object.
--
-- ObjC selector: @+ buttonWithTitle:target:action:@
buttonWithTitle_target_action :: IsNSString title => title -> RawId -> Selector -> IO (Id NSButton)
buttonWithTitle_target_action title target action =
  do
    cls' <- getRequiredClass "NSButton"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "buttonWithTitle:target:action:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Creates a standard push button with the provided image. Set the image's accessibilityDescription property to ensure accessibility for this control.
--
-- @image@ — The image to display in the body of the button.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized button object.
--
-- ObjC selector: @+ buttonWithImage:target:action:@
buttonWithImage_target_action :: IsNSImage image => image -> RawId -> Selector -> IO (Id NSButton)
buttonWithImage_target_action image target action =
  do
    cls' <- getRequiredClass "NSButton"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "buttonWithImage:target:action:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Creates a standard checkbox with the provided title.
--
-- @title@ — The localized title string that is displayed alongside the checkbox.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized button object.
--
-- ObjC selector: @+ checkboxWithTitle:target:action:@
checkboxWithTitle_target_action :: IsNSString title => title -> RawId -> Selector -> IO (Id NSButton)
checkboxWithTitle_target_action title target action =
  do
    cls' <- getRequiredClass "NSButton"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "checkboxWithTitle:target:action:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Creates a standard radio button with the provided title.
--
-- @title@ — The localized title string that is displayed alongside the radio button.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized button object.
--
-- ObjC selector: @+ radioButtonWithTitle:target:action:@
radioButtonWithTitle_target_action :: IsNSString title => title -> RawId -> Selector -> IO (Id NSButton)
radioButtonWithTitle_target_action title target action =
  do
    cls' <- getRequiredClass "NSButton"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "radioButtonWithTitle:target:action:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Sets the button’s type, which affects its user interface and behavior when clicked. See the NSButtonType enumeration for possible options and their behaviors.
--
-- ObjC selector: @- setButtonType:@
setButtonType :: IsNSButton nsButton => nsButton -> NSButtonType -> IO ()
setButtonType nsButton  type_ =
    sendMsg nsButton (mkSelector "setButtonType:") retVoid [argCULong (coerce type_)]

-- | Sets the initial delay and repeat interval, in seconds, for repeated action messages sent when @continuous@ is YES.
--
-- ObjC selector: @- setPeriodicDelay:interval:@
setPeriodicDelay_interval :: IsNSButton nsButton => nsButton -> CFloat -> CFloat -> IO ()
setPeriodicDelay_interval nsButton  delay interval =
    sendMsg nsButton (mkSelector "setPeriodicDelay:interval:") retVoid [argCFloat delay, argCFloat interval]

-- | Gets the initial delay and repeat interval, in seconds, for repeated action messages sent when @continuous@ is YES. Both parameters to this method must not be NULL.
--
-- ObjC selector: @- getPeriodicDelay:interval:@
getPeriodicDelay_interval :: IsNSButton nsButton => nsButton -> Ptr CFloat -> Ptr CFloat -> IO ()
getPeriodicDelay_interval nsButton  delay interval =
    sendMsg nsButton (mkSelector "getPeriodicDelay:interval:") retVoid [argPtr delay, argPtr interval]

-- | Sets the button to its next eligible state. If the button allows mixed state, this cycles through the states in the order: on, off, mixed, on, etc. If the button does not allow mixed state, it toggles between off and on.
--
-- ObjC selector: @- setNextState@
setNextState :: IsNSButton nsButton => nsButton -> IO ()
setNextState nsButton  =
    sendMsg nsButton (mkSelector "setNextState") retVoid []

-- | Highlights, or un-highlights, the button. Highlighting makes the button appear "pressed", which may include showing an illuminated bezel, or showing the alternate image or title, depending on the type of button.
--
-- ObjC selector: @- highlight:@
highlight :: IsNSButton nsButton => nsButton -> Bool -> IO ()
highlight nsButton  flag =
    sendMsg nsButton (mkSelector "highlight:") retVoid [argCULong (if flag then 1 else 0)]

-- | If the event parameter matches the button's key equivalent, the button briefly highlights and performs its action, and then returns YES. Otherwise, returns NO.
--
-- ObjC selector: @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSButton nsButton, IsNSEvent key) => nsButton -> key -> IO Bool
performKeyEquivalent nsButton  key =
  withObjCPtr key $ \raw_key ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "performKeyEquivalent:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptions :: (IsNSButton nsButton, IsNSArray prioritizedOptions) => nsButton -> prioritizedOptions -> IO ()
compressWithPrioritizedCompressionOptions nsButton  prioritizedOptions =
  withObjCPtr prioritizedOptions $ \raw_prioritizedOptions ->
      sendMsg nsButton (mkSelector "compressWithPrioritizedCompressionOptions:") retVoid [argPtr (castPtr raw_prioritizedOptions :: Ptr ())]

-- | @- minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptions :: (IsNSButton nsButton, IsNSArray prioritizedOptions) => nsButton -> prioritizedOptions -> IO NSSize
minimumSizeWithPrioritizedCompressionOptions nsButton  prioritizedOptions =
  withObjCPtr prioritizedOptions $ \raw_prioritizedOptions ->
      sendMsgStret nsButton (mkSelector "minimumSizeWithPrioritizedCompressionOptions:") retNSSize [argPtr (castPtr raw_prioritizedOptions :: Ptr ())]

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSButton nsButton, IsNSString stringWithAmpersand) => nsButton -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsButton  stringWithAmpersand =
  withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
      sendMsg nsButton (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | The title displayed on the button when it’s in an off state, or an empty string if the button does not display a title. By default, a button's title is "Button".
--
-- ObjC selector: @- title@
title :: IsNSButton nsButton => nsButton -> IO (Id NSString)
title nsButton  =
    sendMsg nsButton (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title displayed on the button when it’s in an off state, or an empty string if the button does not display a title. By default, a button's title is "Button".
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSButton nsButton, IsNSString value) => nsButton -> value -> IO ()
setTitle nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The button's title, expressed as an attributed string.
--
-- ObjC selector: @- attributedTitle@
attributedTitle :: IsNSButton nsButton => nsButton -> IO (Id NSAttributedString)
attributedTitle nsButton  =
    sendMsg nsButton (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The button's title, expressed as an attributed string.
--
-- ObjC selector: @- setAttributedTitle:@
setAttributedTitle :: (IsNSButton nsButton, IsNSAttributedString value) => nsButton -> value -> IO ()
setAttributedTitle nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The title that the button displays when the button is in an on state, or an empty string if there is no such title. Note that some button types do not display an alternate title.
--
-- ObjC selector: @- alternateTitle@
alternateTitle :: IsNSButton nsButton => nsButton -> IO (Id NSString)
alternateTitle nsButton  =
    sendMsg nsButton (mkSelector "alternateTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The title that the button displays when the button is in an on state, or an empty string if there is no such title. Note that some button types do not display an alternate title.
--
-- ObjC selector: @- setAlternateTitle:@
setAlternateTitle :: (IsNSButton nsButton, IsNSString value) => nsButton -> value -> IO ()
setAlternateTitle nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setAlternateTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The alternate title, expressed as an attributed string.
--
-- ObjC selector: @- attributedAlternateTitle@
attributedAlternateTitle :: IsNSButton nsButton => nsButton -> IO (Id NSAttributedString)
attributedAlternateTitle nsButton  =
    sendMsg nsButton (mkSelector "attributedAlternateTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The alternate title, expressed as an attributed string.
--
-- ObjC selector: @- setAttributedAlternateTitle:@
setAttributedAlternateTitle :: (IsNSButton nsButton, IsNSAttributedString value) => nsButton -> value -> IO ()
setAttributedAlternateTitle nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setAttributedAlternateTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates whether the button's action has a destructive effect on user data.  AppKit may guard a destructive-actioned button against accidental presses, and may give the button a special appearance in certain contexts to caution against unintentional use.  Defaults to NO.
--
-- ObjC selector: @- hasDestructiveAction@
hasDestructiveAction :: IsNSButton nsButton => nsButton -> IO Bool
hasDestructiveAction nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "hasDestructiveAction") retCULong []

-- | Indicates whether the button's action has a destructive effect on user data.  AppKit may guard a destructive-actioned button against accidental presses, and may give the button a special appearance in certain contexts to caution against unintentional use.  Defaults to NO.
--
-- ObjC selector: @- setHasDestructiveAction:@
setHasDestructiveAction :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setHasDestructiveAction nsButton  value =
    sendMsg nsButton (mkSelector "setHasDestructiveAction:") retVoid [argCULong (if value then 1 else 0)]

-- | The sound that plays when the user clicks the button, or nil if the button should not play a sound. The default value is nil.
--
-- ObjC selector: @- sound@
sound :: IsNSButton nsButton => nsButton -> IO (Id NSSound)
sound nsButton  =
    sendMsg nsButton (mkSelector "sound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The sound that plays when the user clicks the button, or nil if the button should not play a sound. The default value is nil.
--
-- ObjC selector: @- setSound:@
setSound :: (IsNSButton nsButton, IsNSSound value) => nsButton -> value -> IO ()
setSound nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setSound:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Sends action on deep-press or extended hover while dragging. Defaults to NO.
--
-- ObjC selector: @- springLoaded@
springLoaded :: IsNSButton nsButton => nsButton -> IO Bool
springLoaded nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "springLoaded") retCULong []

-- | Sends action on deep-press or extended hover while dragging. Defaults to NO.
--
-- ObjC selector: @- setSpringLoaded:@
setSpringLoaded :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setSpringLoaded nsButton  value =
    sendMsg nsButton (mkSelector "setSpringLoaded:") retVoid [argCULong (if value then 1 else 0)]

-- | Configures the maximum allowed level for an NSMultiLevelAcceleratorButton, allowed values range from [1,5]. Defaults to 2.
--
-- ObjC selector: @- maxAcceleratorLevel@
maxAcceleratorLevel :: IsNSButton nsButton => nsButton -> IO CLong
maxAcceleratorLevel nsButton  =
    sendMsg nsButton (mkSelector "maxAcceleratorLevel") retCLong []

-- | Configures the maximum allowed level for an NSMultiLevelAcceleratorButton, allowed values range from [1,5]. Defaults to 2.
--
-- ObjC selector: @- setMaxAcceleratorLevel:@
setMaxAcceleratorLevel :: IsNSButton nsButton => nsButton -> CLong -> IO ()
setMaxAcceleratorLevel nsButton  value =
    sendMsg nsButton (mkSelector "setMaxAcceleratorLevel:") retVoid [argCLong value]

-- | The bezel style of the button, which provides a set of bezel artwork, layout metrics, and content styling from a set of system-provided styles. See the NSBezelStyle enumeration for a list of available styles. The bezel style is not used if the @bordered@ property is set to @NO@.
--
-- ObjC selector: @- bezelStyle@
bezelStyle :: IsNSButton nsButton => nsButton -> IO NSBezelStyle
bezelStyle nsButton  =
    fmap (coerce :: CULong -> NSBezelStyle) $ sendMsg nsButton (mkSelector "bezelStyle") retCULong []

-- | The bezel style of the button, which provides a set of bezel artwork, layout metrics, and content styling from a set of system-provided styles. See the NSBezelStyle enumeration for a list of available styles. The bezel style is not used if the @bordered@ property is set to @NO@.
--
-- ObjC selector: @- setBezelStyle:@
setBezelStyle :: IsNSButton nsButton => nsButton -> NSBezelStyle -> IO ()
setBezelStyle nsButton  value =
    sendMsg nsButton (mkSelector "setBezelStyle:") retVoid [argCULong (coerce value)]

-- | A Boolean value that determines whether the button draws a border.
--
-- ObjC selector: @- bordered@
bordered :: IsNSButton nsButton => nsButton -> IO Bool
bordered nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "bordered") retCULong []

-- | A Boolean value that determines whether the button draws a border.
--
-- ObjC selector: @- setBordered:@
setBordered :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setBordered nsButton  value =
    sendMsg nsButton (mkSelector "setBordered:") retVoid [argCULong (if value then 1 else 0)]

-- | A Boolean value that indicates whether the button is transparent. A transparent button never draws itself, but it receives mouse events, sends its action, and tracks the mouse properly.
--
-- ObjC selector: @- transparent@
transparent :: IsNSButton nsButton => nsButton -> IO Bool
transparent nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "transparent") retCULong []

-- | A Boolean value that indicates whether the button is transparent. A transparent button never draws itself, but it receives mouse events, sends its action, and tracks the mouse properly.
--
-- ObjC selector: @- setTransparent:@
setTransparent :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setTransparent nsButton  value =
    sendMsg nsButton (mkSelector "setTransparent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInside :: IsNSButton nsButton => nsButton -> IO Bool
showsBorderOnlyWhileMouseInside nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "showsBorderOnlyWhileMouseInside") retCULong []

-- | @- setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInside :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setShowsBorderOnlyWhileMouseInside nsButton  value =
    sendMsg nsButton (mkSelector "setShowsBorderOnlyWhileMouseInside:") retVoid [argCULong (if value then 1 else 0)]

-- | Applies a custom color to the button's bezel, in appearances that support it. A nil value indicates an unmodified button appearance. The default value is nil.
--
-- ObjC selector: @- bezelColor@
bezelColor :: IsNSButton nsButton => nsButton -> IO (Id NSColor)
bezelColor nsButton  =
    sendMsg nsButton (mkSelector "bezelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a custom color to the button's bezel, in appearances that support it. A nil value indicates an unmodified button appearance. The default value is nil.
--
-- ObjC selector: @- setBezelColor:@
setBezelColor :: (IsNSButton nsButton, IsNSColor value) => nsButton -> value -> IO ()
setBezelColor nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setBezelColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Applies a tint color to template image and text content, in combination with other theme-appropriate effects. Only applicable to borderless buttons. A nil value indicates the standard set of effects without color modification. The default value is nil. Non-template images and attributed string values are not affected by the contentTintColor.
--
-- ObjC selector: @- contentTintColor@
contentTintColor :: IsNSButton nsButton => nsButton -> IO (Id NSColor)
contentTintColor nsButton  =
    sendMsg nsButton (mkSelector "contentTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Applies a tint color to template image and text content, in combination with other theme-appropriate effects. Only applicable to borderless buttons. A nil value indicates the standard set of effects without color modification. The default value is nil. Non-template images and attributed string values are not affected by the contentTintColor.
--
-- ObjC selector: @- setContentTintColor:@
setContentTintColor :: (IsNSButton nsButton, IsNSColor value) => nsButton -> value -> IO ()
setContentTintColor nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setContentTintColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The tint prominence of the button. Use tint prominence to gently suggest a hierarchy when multiple buttons perform similar actions. A button with primary tint prominence suggests the most preferred option, while secondary prominence indicates a reasonable alternative. See ``NSTintProminence`` for a list of possible values.
--
-- ObjC selector: @- tintProminence@
tintProminence :: IsNSButton nsButton => nsButton -> IO NSTintProminence
tintProminence nsButton  =
    fmap (coerce :: CLong -> NSTintProminence) $ sendMsg nsButton (mkSelector "tintProminence") retCLong []

-- | The tint prominence of the button. Use tint prominence to gently suggest a hierarchy when multiple buttons perform similar actions. A button with primary tint prominence suggests the most preferred option, while secondary prominence indicates a reasonable alternative. See ``NSTintProminence`` for a list of possible values.
--
-- ObjC selector: @- setTintProminence:@
setTintProminence :: IsNSButton nsButton => nsButton -> NSTintProminence -> IO ()
setTintProminence nsButton  value =
    sendMsg nsButton (mkSelector "setTintProminence:") retVoid [argCLong (coerce value)]

-- | The image that appears on the button when it’s in an off state, or nil if there is no such image.
--
-- ObjC selector: @- image@
image :: IsNSButton nsButton => nsButton -> IO (Id NSImage)
image nsButton  =
    sendMsg nsButton (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The image that appears on the button when it’s in an off state, or nil if there is no such image.
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSButton nsButton, IsNSImage value) => nsButton -> value -> IO ()
setImage nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | An alternate image that appears on the button when the button is in an on state, or nil if there is no such image. Note that some button types do not display an alternate image.
--
-- ObjC selector: @- alternateImage@
alternateImage :: IsNSButton nsButton => nsButton -> IO (Id NSImage)
alternateImage nsButton  =
    sendMsg nsButton (mkSelector "alternateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An alternate image that appears on the button when the button is in an on state, or nil if there is no such image. Note that some button types do not display an alternate image.
--
-- ObjC selector: @- setAlternateImage:@
setAlternateImage :: (IsNSButton nsButton, IsNSImage value) => nsButton -> value -> IO ()
setAlternateImage nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setAlternateImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The position of the button's image relative to its title. See the NSCellImagePosition enumeration for possible values.
--
-- ObjC selector: @- imagePosition@
imagePosition :: IsNSButton nsButton => nsButton -> IO NSCellImagePosition
imagePosition nsButton  =
    fmap (coerce :: CULong -> NSCellImagePosition) $ sendMsg nsButton (mkSelector "imagePosition") retCULong []

-- | The position of the button's image relative to its title. See the NSCellImagePosition enumeration for possible values.
--
-- ObjC selector: @- setImagePosition:@
setImagePosition :: IsNSButton nsButton => nsButton -> NSCellImagePosition -> IO ()
setImagePosition nsButton  value =
    sendMsg nsButton (mkSelector "setImagePosition:") retVoid [argCULong (coerce value)]

-- | The scaling mode applied to make the button's image fit within its bounds.
--
-- ObjC selector: @- imageScaling@
imageScaling :: IsNSButton nsButton => nsButton -> IO NSImageScaling
imageScaling nsButton  =
    fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsButton (mkSelector "imageScaling") retCULong []

-- | The scaling mode applied to make the button's image fit within its bounds.
--
-- ObjC selector: @- setImageScaling:@
setImageScaling :: IsNSButton nsButton => nsButton -> NSImageScaling -> IO ()
setImageScaling nsButton  value =
    sendMsg nsButton (mkSelector "setImageScaling:") retVoid [argCULong (coerce value)]

-- | A Boolean value that determines how the button's image and title are positioned together within the button bezel. If false, the image is positioned according to the imagePosition property at the edge of the button bezel, and the title is positioned within the remaining space. If true, the button’s image is positioned directly adjacent to the title based on the imagePosition property, and the image and title are positioned within the button bezel as a single unit.
--
-- ObjC selector: @- imageHugsTitle@
imageHugsTitle :: IsNSButton nsButton => nsButton -> IO Bool
imageHugsTitle nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "imageHugsTitle") retCULong []

-- | A Boolean value that determines how the button's image and title are positioned together within the button bezel. If false, the image is positioned according to the imagePosition property at the edge of the button bezel, and the title is positioned within the remaining space. If true, the button’s image is positioned directly adjacent to the title based on the imagePosition property, and the image and title are positioned within the button bezel as a single unit.
--
-- ObjC selector: @- setImageHugsTitle:@
setImageHugsTitle :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setImageHugsTitle nsButton  value =
    sendMsg nsButton (mkSelector "setImageHugsTitle:") retVoid [argCULong (if value then 1 else 0)]

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the symbol is matched to the button's @font@ property. The default value is nil.
--
-- ObjC selector: @- symbolConfiguration@
symbolConfiguration :: IsNSButton nsButton => nsButton -> IO (Id NSImageSymbolConfiguration)
symbolConfiguration nsButton  =
    sendMsg nsButton (mkSelector "symbolConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the symbol is matched to the button's @font@ property. The default value is nil.
--
-- ObjC selector: @- setSymbolConfiguration:@
setSymbolConfiguration :: (IsNSButton nsButton, IsNSImageSymbolConfiguration value) => nsButton -> value -> IO ()
setSymbolConfiguration nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setSymbolConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The button's state. Buttons support the off and on states, and an additional mixed state depending on the value of the @allowsMixedState@ property.
--
-- ObjC selector: @- state@
state :: IsNSButton nsButton => nsButton -> IO CLong
state nsButton  =
    sendMsg nsButton (mkSelector "state") retCLong []

-- | The button's state. Buttons support the off and on states, and an additional mixed state depending on the value of the @allowsMixedState@ property.
--
-- ObjC selector: @- setState:@
setState :: IsNSButton nsButton => nsButton -> CLong -> IO ()
setState nsButton  value =
    sendMsg nsButton (mkSelector "setState:") retVoid [argCLong value]

-- | A Boolean value that indicates whether the button allows a mixed state. If NO, the button has two states (on and off), and if YES, the button has three states (on, off, and mixed). The mixed state is commonly used with checkboxes and radio buttons to indicate a value which is partially on.
--
-- ObjC selector: @- allowsMixedState@
allowsMixedState :: IsNSButton nsButton => nsButton -> IO Bool
allowsMixedState nsButton  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButton (mkSelector "allowsMixedState") retCULong []

-- | A Boolean value that indicates whether the button allows a mixed state. If NO, the button has two states (on and off), and if YES, the button has three states (on, off, and mixed). The mixed state is commonly used with checkboxes and radio buttons to indicate a value which is partially on.
--
-- ObjC selector: @- setAllowsMixedState:@
setAllowsMixedState :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setAllowsMixedState nsButton  value =
    sendMsg nsButton (mkSelector "setAllowsMixedState:") retVoid [argCULong (if value then 1 else 0)]

-- | This property contains the button's key equivalent, or the empty string if no equivalent has been defined. Buttons don’t have a default key equivalent. Setting the key equivalent to the Return character causes it to act as the default button for its window.
--
-- ObjC selector: @- keyEquivalent@
keyEquivalent :: IsNSButton nsButton => nsButton -> IO (Id NSString)
keyEquivalent nsButton  =
    sendMsg nsButton (mkSelector "keyEquivalent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | This property contains the button's key equivalent, or the empty string if no equivalent has been defined. Buttons don’t have a default key equivalent. Setting the key equivalent to the Return character causes it to act as the default button for its window.
--
-- ObjC selector: @- setKeyEquivalent:@
setKeyEquivalent :: (IsNSButton nsButton, IsNSString value) => nsButton -> value -> IO ()
setKeyEquivalent nsButton  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButton (mkSelector "setKeyEquivalent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A bitmask specifying the modifier keys that are applied to the button's key equivalent. Mask bits are defined by the NSEventModifierFlags option set. The only mask bits relevant in button key-equivalent modifier masks are NSEventModifierFlagControl, NSEventModifierFlagOption, and NSEventModifierFlagCommand.
--
-- ObjC selector: @- keyEquivalentModifierMask@
keyEquivalentModifierMask :: IsNSButton nsButton => nsButton -> IO NSEventModifierFlags
keyEquivalentModifierMask nsButton  =
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg nsButton (mkSelector "keyEquivalentModifierMask") retCULong []

-- | A bitmask specifying the modifier keys that are applied to the button's key equivalent. Mask bits are defined by the NSEventModifierFlags option set. The only mask bits relevant in button key-equivalent modifier masks are NSEventModifierFlagControl, NSEventModifierFlagOption, and NSEventModifierFlagCommand.
--
-- ObjC selector: @- setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMask :: IsNSButton nsButton => nsButton -> NSEventModifierFlags -> IO ()
setKeyEquivalentModifierMask nsButton  value =
    sendMsg nsButton (mkSelector "setKeyEquivalentModifierMask:") retVoid [argCULong (coerce value)]

-- | @- activeCompressionOptions@
activeCompressionOptions :: IsNSButton nsButton => nsButton -> IO (Id NSUserInterfaceCompressionOptions)
activeCompressionOptions nsButton  =
    sendMsg nsButton (mkSelector "activeCompressionOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- borderShape@
borderShape :: IsNSButton nsButton => nsButton -> IO NSControlBorderShape
borderShape nsButton  =
    fmap (coerce :: CLong -> NSControlBorderShape) $ sendMsg nsButton (mkSelector "borderShape") retCLong []

-- | @- setBorderShape:@
setBorderShape :: IsNSButton nsButton => nsButton -> NSControlBorderShape -> IO ()
setBorderShape nsButton  value =
    sendMsg nsButton (mkSelector "setBorderShape:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonWithTitle:image:target:action:@
buttonWithTitle_image_target_actionSelector :: Selector
buttonWithTitle_image_target_actionSelector = mkSelector "buttonWithTitle:image:target:action:"

-- | @Selector@ for @buttonWithTitle:target:action:@
buttonWithTitle_target_actionSelector :: Selector
buttonWithTitle_target_actionSelector = mkSelector "buttonWithTitle:target:action:"

-- | @Selector@ for @buttonWithImage:target:action:@
buttonWithImage_target_actionSelector :: Selector
buttonWithImage_target_actionSelector = mkSelector "buttonWithImage:target:action:"

-- | @Selector@ for @checkboxWithTitle:target:action:@
checkboxWithTitle_target_actionSelector :: Selector
checkboxWithTitle_target_actionSelector = mkSelector "checkboxWithTitle:target:action:"

-- | @Selector@ for @radioButtonWithTitle:target:action:@
radioButtonWithTitle_target_actionSelector :: Selector
radioButtonWithTitle_target_actionSelector = mkSelector "radioButtonWithTitle:target:action:"

-- | @Selector@ for @setButtonType:@
setButtonTypeSelector :: Selector
setButtonTypeSelector = mkSelector "setButtonType:"

-- | @Selector@ for @setPeriodicDelay:interval:@
setPeriodicDelay_intervalSelector :: Selector
setPeriodicDelay_intervalSelector = mkSelector "setPeriodicDelay:interval:"

-- | @Selector@ for @getPeriodicDelay:interval:@
getPeriodicDelay_intervalSelector :: Selector
getPeriodicDelay_intervalSelector = mkSelector "getPeriodicDelay:interval:"

-- | @Selector@ for @setNextState@
setNextStateSelector :: Selector
setNextStateSelector = mkSelector "setNextState"

-- | @Selector@ for @highlight:@
highlightSelector :: Selector
highlightSelector = mkSelector "highlight:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptionsSelector :: Selector
compressWithPrioritizedCompressionOptionsSelector = mkSelector "compressWithPrioritizedCompressionOptions:"

-- | @Selector@ for @minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptionsSelector :: Selector
minimumSizeWithPrioritizedCompressionOptionsSelector = mkSelector "minimumSizeWithPrioritizedCompressionOptions:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

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

-- | @Selector@ for @alternateTitle@
alternateTitleSelector :: Selector
alternateTitleSelector = mkSelector "alternateTitle"

-- | @Selector@ for @setAlternateTitle:@
setAlternateTitleSelector :: Selector
setAlternateTitleSelector = mkSelector "setAlternateTitle:"

-- | @Selector@ for @attributedAlternateTitle@
attributedAlternateTitleSelector :: Selector
attributedAlternateTitleSelector = mkSelector "attributedAlternateTitle"

-- | @Selector@ for @setAttributedAlternateTitle:@
setAttributedAlternateTitleSelector :: Selector
setAttributedAlternateTitleSelector = mkSelector "setAttributedAlternateTitle:"

-- | @Selector@ for @hasDestructiveAction@
hasDestructiveActionSelector :: Selector
hasDestructiveActionSelector = mkSelector "hasDestructiveAction"

-- | @Selector@ for @setHasDestructiveAction:@
setHasDestructiveActionSelector :: Selector
setHasDestructiveActionSelector = mkSelector "setHasDestructiveAction:"

-- | @Selector@ for @sound@
soundSelector :: Selector
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @springLoaded@
springLoadedSelector :: Selector
springLoadedSelector = mkSelector "springLoaded"

-- | @Selector@ for @setSpringLoaded:@
setSpringLoadedSelector :: Selector
setSpringLoadedSelector = mkSelector "setSpringLoaded:"

-- | @Selector@ for @maxAcceleratorLevel@
maxAcceleratorLevelSelector :: Selector
maxAcceleratorLevelSelector = mkSelector "maxAcceleratorLevel"

-- | @Selector@ for @setMaxAcceleratorLevel:@
setMaxAcceleratorLevelSelector :: Selector
setMaxAcceleratorLevelSelector = mkSelector "setMaxAcceleratorLevel:"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector
setBezelStyleSelector = mkSelector "setBezelStyle:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @transparent@
transparentSelector :: Selector
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @setTransparent:@
setTransparentSelector :: Selector
setTransparentSelector = mkSelector "setTransparent:"

-- | @Selector@ for @showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInsideSelector :: Selector
showsBorderOnlyWhileMouseInsideSelector = mkSelector "showsBorderOnlyWhileMouseInside"

-- | @Selector@ for @setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInsideSelector :: Selector
setShowsBorderOnlyWhileMouseInsideSelector = mkSelector "setShowsBorderOnlyWhileMouseInside:"

-- | @Selector@ for @bezelColor@
bezelColorSelector :: Selector
bezelColorSelector = mkSelector "bezelColor"

-- | @Selector@ for @setBezelColor:@
setBezelColorSelector :: Selector
setBezelColorSelector = mkSelector "setBezelColor:"

-- | @Selector@ for @contentTintColor@
contentTintColorSelector :: Selector
contentTintColorSelector = mkSelector "contentTintColor"

-- | @Selector@ for @setContentTintColor:@
setContentTintColorSelector :: Selector
setContentTintColorSelector = mkSelector "setContentTintColor:"

-- | @Selector@ for @tintProminence@
tintProminenceSelector :: Selector
tintProminenceSelector = mkSelector "tintProminence"

-- | @Selector@ for @setTintProminence:@
setTintProminenceSelector :: Selector
setTintProminenceSelector = mkSelector "setTintProminence:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @setAlternateImage:@
setAlternateImageSelector :: Selector
setAlternateImageSelector = mkSelector "setAlternateImage:"

-- | @Selector@ for @imagePosition@
imagePositionSelector :: Selector
imagePositionSelector = mkSelector "imagePosition"

-- | @Selector@ for @setImagePosition:@
setImagePositionSelector :: Selector
setImagePositionSelector = mkSelector "setImagePosition:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @imageHugsTitle@
imageHugsTitleSelector :: Selector
imageHugsTitleSelector = mkSelector "imageHugsTitle"

-- | @Selector@ for @setImageHugsTitle:@
setImageHugsTitleSelector :: Selector
setImageHugsTitleSelector = mkSelector "setImageHugsTitle:"

-- | @Selector@ for @symbolConfiguration@
symbolConfigurationSelector :: Selector
symbolConfigurationSelector = mkSelector "symbolConfiguration"

-- | @Selector@ for @setSymbolConfiguration:@
setSymbolConfigurationSelector :: Selector
setSymbolConfigurationSelector = mkSelector "setSymbolConfiguration:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @allowsMixedState@
allowsMixedStateSelector :: Selector
allowsMixedStateSelector = mkSelector "allowsMixedState"

-- | @Selector@ for @setAllowsMixedState:@
setAllowsMixedStateSelector :: Selector
setAllowsMixedStateSelector = mkSelector "setAllowsMixedState:"

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

-- | @Selector@ for @activeCompressionOptions@
activeCompressionOptionsSelector :: Selector
activeCompressionOptionsSelector = mkSelector "activeCompressionOptions"

-- | @Selector@ for @borderShape@
borderShapeSelector :: Selector
borderShapeSelector = mkSelector "borderShape"

-- | @Selector@ for @setBorderShape:@
setBorderShapeSelector :: Selector
setBorderShapeSelector = mkSelector "setBorderShape:"

