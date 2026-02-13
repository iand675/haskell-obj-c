{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , activeCompressionOptionsSelector
  , allowsMixedStateSelector
  , alternateImageSelector
  , alternateTitleSelector
  , attributedAlternateTitleSelector
  , attributedTitleSelector
  , bezelColorSelector
  , bezelStyleSelector
  , borderShapeSelector
  , borderedSelector
  , buttonWithImage_target_actionSelector
  , buttonWithTitle_image_target_actionSelector
  , buttonWithTitle_target_actionSelector
  , checkboxWithTitle_target_actionSelector
  , compressWithPrioritizedCompressionOptionsSelector
  , contentTintColorSelector
  , getPeriodicDelay_intervalSelector
  , hasDestructiveActionSelector
  , highlightSelector
  , imageHugsTitleSelector
  , imagePositionSelector
  , imageScalingSelector
  , imageSelector
  , keyEquivalentModifierMaskSelector
  , keyEquivalentSelector
  , maxAcceleratorLevelSelector
  , minimumSizeWithPrioritizedCompressionOptionsSelector
  , performKeyEquivalentSelector
  , radioButtonWithTitle_target_actionSelector
  , setAllowsMixedStateSelector
  , setAlternateImageSelector
  , setAlternateTitleSelector
  , setAttributedAlternateTitleSelector
  , setAttributedTitleSelector
  , setBezelColorSelector
  , setBezelStyleSelector
  , setBorderShapeSelector
  , setBorderedSelector
  , setButtonTypeSelector
  , setContentTintColorSelector
  , setHasDestructiveActionSelector
  , setImageHugsTitleSelector
  , setImagePositionSelector
  , setImageScalingSelector
  , setImageSelector
  , setKeyEquivalentModifierMaskSelector
  , setKeyEquivalentSelector
  , setMaxAcceleratorLevelSelector
  , setNextStateSelector
  , setPeriodicDelay_intervalSelector
  , setShowsBorderOnlyWhileMouseInsideSelector
  , setSoundSelector
  , setSpringLoadedSelector
  , setStateSelector
  , setSymbolConfigurationSelector
  , setTintProminenceSelector
  , setTitleSelector
  , setTitleWithMnemonicSelector
  , setTransparentSelector
  , showsBorderOnlyWhileMouseInsideSelector
  , soundSelector
  , springLoadedSelector
  , stateSelector
  , symbolConfigurationSelector
  , tintProminenceSelector
  , titleSelector
  , transparentSelector

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
buttonWithTitle_image_target_action :: (IsNSString title, IsNSImage image) => title -> image -> RawId -> Sel -> IO (Id NSButton)
buttonWithTitle_image_target_action title image target action =
  do
    cls' <- getRequiredClass "NSButton"
    sendClassMessage cls' buttonWithTitle_image_target_actionSelector (toNSString title) (toNSImage image) target action

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
buttonWithTitle_target_action :: IsNSString title => title -> RawId -> Sel -> IO (Id NSButton)
buttonWithTitle_target_action title target action =
  do
    cls' <- getRequiredClass "NSButton"
    sendClassMessage cls' buttonWithTitle_target_actionSelector (toNSString title) target action

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
buttonWithImage_target_action :: IsNSImage image => image -> RawId -> Sel -> IO (Id NSButton)
buttonWithImage_target_action image target action =
  do
    cls' <- getRequiredClass "NSButton"
    sendClassMessage cls' buttonWithImage_target_actionSelector (toNSImage image) target action

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
checkboxWithTitle_target_action :: IsNSString title => title -> RawId -> Sel -> IO (Id NSButton)
checkboxWithTitle_target_action title target action =
  do
    cls' <- getRequiredClass "NSButton"
    sendClassMessage cls' checkboxWithTitle_target_actionSelector (toNSString title) target action

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
radioButtonWithTitle_target_action :: IsNSString title => title -> RawId -> Sel -> IO (Id NSButton)
radioButtonWithTitle_target_action title target action =
  do
    cls' <- getRequiredClass "NSButton"
    sendClassMessage cls' radioButtonWithTitle_target_actionSelector (toNSString title) target action

-- | Sets the button’s type, which affects its user interface and behavior when clicked. See the NSButtonType enumeration for possible options and their behaviors.
--
-- ObjC selector: @- setButtonType:@
setButtonType :: IsNSButton nsButton => nsButton -> NSButtonType -> IO ()
setButtonType nsButton type_ =
  sendMessage nsButton setButtonTypeSelector type_

-- | Sets the initial delay and repeat interval, in seconds, for repeated action messages sent when @continuous@ is YES.
--
-- ObjC selector: @- setPeriodicDelay:interval:@
setPeriodicDelay_interval :: IsNSButton nsButton => nsButton -> CFloat -> CFloat -> IO ()
setPeriodicDelay_interval nsButton delay interval =
  sendMessage nsButton setPeriodicDelay_intervalSelector delay interval

-- | Gets the initial delay and repeat interval, in seconds, for repeated action messages sent when @continuous@ is YES. Both parameters to this method must not be NULL.
--
-- ObjC selector: @- getPeriodicDelay:interval:@
getPeriodicDelay_interval :: IsNSButton nsButton => nsButton -> Ptr CFloat -> Ptr CFloat -> IO ()
getPeriodicDelay_interval nsButton delay interval =
  sendMessage nsButton getPeriodicDelay_intervalSelector delay interval

-- | Sets the button to its next eligible state. If the button allows mixed state, this cycles through the states in the order: on, off, mixed, on, etc. If the button does not allow mixed state, it toggles between off and on.
--
-- ObjC selector: @- setNextState@
setNextState :: IsNSButton nsButton => nsButton -> IO ()
setNextState nsButton =
  sendMessage nsButton setNextStateSelector

-- | Highlights, or un-highlights, the button. Highlighting makes the button appear "pressed", which may include showing an illuminated bezel, or showing the alternate image or title, depending on the type of button.
--
-- ObjC selector: @- highlight:@
highlight :: IsNSButton nsButton => nsButton -> Bool -> IO ()
highlight nsButton flag =
  sendMessage nsButton highlightSelector flag

-- | If the event parameter matches the button's key equivalent, the button briefly highlights and performs its action, and then returns YES. Otherwise, returns NO.
--
-- ObjC selector: @- performKeyEquivalent:@
performKeyEquivalent :: (IsNSButton nsButton, IsNSEvent key) => nsButton -> key -> IO Bool
performKeyEquivalent nsButton key =
  sendMessage nsButton performKeyEquivalentSelector (toNSEvent key)

-- | @- compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptions :: (IsNSButton nsButton, IsNSArray prioritizedOptions) => nsButton -> prioritizedOptions -> IO ()
compressWithPrioritizedCompressionOptions nsButton prioritizedOptions =
  sendMessage nsButton compressWithPrioritizedCompressionOptionsSelector (toNSArray prioritizedOptions)

-- | @- minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptions :: (IsNSButton nsButton, IsNSArray prioritizedOptions) => nsButton -> prioritizedOptions -> IO NSSize
minimumSizeWithPrioritizedCompressionOptions nsButton prioritizedOptions =
  sendMessage nsButton minimumSizeWithPrioritizedCompressionOptionsSelector (toNSArray prioritizedOptions)

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSButton nsButton, IsNSString stringWithAmpersand) => nsButton -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsButton stringWithAmpersand =
  sendMessage nsButton setTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | The title displayed on the button when it’s in an off state, or an empty string if the button does not display a title. By default, a button's title is "Button".
--
-- ObjC selector: @- title@
title :: IsNSButton nsButton => nsButton -> IO (Id NSString)
title nsButton =
  sendMessage nsButton titleSelector

-- | The title displayed on the button when it’s in an off state, or an empty string if the button does not display a title. By default, a button's title is "Button".
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsNSButton nsButton, IsNSString value) => nsButton -> value -> IO ()
setTitle nsButton value =
  sendMessage nsButton setTitleSelector (toNSString value)

-- | The button's title, expressed as an attributed string.
--
-- ObjC selector: @- attributedTitle@
attributedTitle :: IsNSButton nsButton => nsButton -> IO (Id NSAttributedString)
attributedTitle nsButton =
  sendMessage nsButton attributedTitleSelector

-- | The button's title, expressed as an attributed string.
--
-- ObjC selector: @- setAttributedTitle:@
setAttributedTitle :: (IsNSButton nsButton, IsNSAttributedString value) => nsButton -> value -> IO ()
setAttributedTitle nsButton value =
  sendMessage nsButton setAttributedTitleSelector (toNSAttributedString value)

-- | The title that the button displays when the button is in an on state, or an empty string if there is no such title. Note that some button types do not display an alternate title.
--
-- ObjC selector: @- alternateTitle@
alternateTitle :: IsNSButton nsButton => nsButton -> IO (Id NSString)
alternateTitle nsButton =
  sendMessage nsButton alternateTitleSelector

-- | The title that the button displays when the button is in an on state, or an empty string if there is no such title. Note that some button types do not display an alternate title.
--
-- ObjC selector: @- setAlternateTitle:@
setAlternateTitle :: (IsNSButton nsButton, IsNSString value) => nsButton -> value -> IO ()
setAlternateTitle nsButton value =
  sendMessage nsButton setAlternateTitleSelector (toNSString value)

-- | The alternate title, expressed as an attributed string.
--
-- ObjC selector: @- attributedAlternateTitle@
attributedAlternateTitle :: IsNSButton nsButton => nsButton -> IO (Id NSAttributedString)
attributedAlternateTitle nsButton =
  sendMessage nsButton attributedAlternateTitleSelector

-- | The alternate title, expressed as an attributed string.
--
-- ObjC selector: @- setAttributedAlternateTitle:@
setAttributedAlternateTitle :: (IsNSButton nsButton, IsNSAttributedString value) => nsButton -> value -> IO ()
setAttributedAlternateTitle nsButton value =
  sendMessage nsButton setAttributedAlternateTitleSelector (toNSAttributedString value)

-- | Indicates whether the button's action has a destructive effect on user data.  AppKit may guard a destructive-actioned button against accidental presses, and may give the button a special appearance in certain contexts to caution against unintentional use.  Defaults to NO.
--
-- ObjC selector: @- hasDestructiveAction@
hasDestructiveAction :: IsNSButton nsButton => nsButton -> IO Bool
hasDestructiveAction nsButton =
  sendMessage nsButton hasDestructiveActionSelector

-- | Indicates whether the button's action has a destructive effect on user data.  AppKit may guard a destructive-actioned button against accidental presses, and may give the button a special appearance in certain contexts to caution against unintentional use.  Defaults to NO.
--
-- ObjC selector: @- setHasDestructiveAction:@
setHasDestructiveAction :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setHasDestructiveAction nsButton value =
  sendMessage nsButton setHasDestructiveActionSelector value

-- | The sound that plays when the user clicks the button, or nil if the button should not play a sound. The default value is nil.
--
-- ObjC selector: @- sound@
sound :: IsNSButton nsButton => nsButton -> IO (Id NSSound)
sound nsButton =
  sendMessage nsButton soundSelector

-- | The sound that plays when the user clicks the button, or nil if the button should not play a sound. The default value is nil.
--
-- ObjC selector: @- setSound:@
setSound :: (IsNSButton nsButton, IsNSSound value) => nsButton -> value -> IO ()
setSound nsButton value =
  sendMessage nsButton setSoundSelector (toNSSound value)

-- | Sends action on deep-press or extended hover while dragging. Defaults to NO.
--
-- ObjC selector: @- springLoaded@
springLoaded :: IsNSButton nsButton => nsButton -> IO Bool
springLoaded nsButton =
  sendMessage nsButton springLoadedSelector

-- | Sends action on deep-press or extended hover while dragging. Defaults to NO.
--
-- ObjC selector: @- setSpringLoaded:@
setSpringLoaded :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setSpringLoaded nsButton value =
  sendMessage nsButton setSpringLoadedSelector value

-- | Configures the maximum allowed level for an NSMultiLevelAcceleratorButton, allowed values range from [1,5]. Defaults to 2.
--
-- ObjC selector: @- maxAcceleratorLevel@
maxAcceleratorLevel :: IsNSButton nsButton => nsButton -> IO CLong
maxAcceleratorLevel nsButton =
  sendMessage nsButton maxAcceleratorLevelSelector

-- | Configures the maximum allowed level for an NSMultiLevelAcceleratorButton, allowed values range from [1,5]. Defaults to 2.
--
-- ObjC selector: @- setMaxAcceleratorLevel:@
setMaxAcceleratorLevel :: IsNSButton nsButton => nsButton -> CLong -> IO ()
setMaxAcceleratorLevel nsButton value =
  sendMessage nsButton setMaxAcceleratorLevelSelector value

-- | The bezel style of the button, which provides a set of bezel artwork, layout metrics, and content styling from a set of system-provided styles. See the NSBezelStyle enumeration for a list of available styles. The bezel style is not used if the @bordered@ property is set to @NO@.
--
-- ObjC selector: @- bezelStyle@
bezelStyle :: IsNSButton nsButton => nsButton -> IO NSBezelStyle
bezelStyle nsButton =
  sendMessage nsButton bezelStyleSelector

-- | The bezel style of the button, which provides a set of bezel artwork, layout metrics, and content styling from a set of system-provided styles. See the NSBezelStyle enumeration for a list of available styles. The bezel style is not used if the @bordered@ property is set to @NO@.
--
-- ObjC selector: @- setBezelStyle:@
setBezelStyle :: IsNSButton nsButton => nsButton -> NSBezelStyle -> IO ()
setBezelStyle nsButton value =
  sendMessage nsButton setBezelStyleSelector value

-- | A Boolean value that determines whether the button draws a border.
--
-- ObjC selector: @- bordered@
bordered :: IsNSButton nsButton => nsButton -> IO Bool
bordered nsButton =
  sendMessage nsButton borderedSelector

-- | A Boolean value that determines whether the button draws a border.
--
-- ObjC selector: @- setBordered:@
setBordered :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setBordered nsButton value =
  sendMessage nsButton setBorderedSelector value

-- | A Boolean value that indicates whether the button is transparent. A transparent button never draws itself, but it receives mouse events, sends its action, and tracks the mouse properly.
--
-- ObjC selector: @- transparent@
transparent :: IsNSButton nsButton => nsButton -> IO Bool
transparent nsButton =
  sendMessage nsButton transparentSelector

-- | A Boolean value that indicates whether the button is transparent. A transparent button never draws itself, but it receives mouse events, sends its action, and tracks the mouse properly.
--
-- ObjC selector: @- setTransparent:@
setTransparent :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setTransparent nsButton value =
  sendMessage nsButton setTransparentSelector value

-- | @- showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInside :: IsNSButton nsButton => nsButton -> IO Bool
showsBorderOnlyWhileMouseInside nsButton =
  sendMessage nsButton showsBorderOnlyWhileMouseInsideSelector

-- | @- setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInside :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setShowsBorderOnlyWhileMouseInside nsButton value =
  sendMessage nsButton setShowsBorderOnlyWhileMouseInsideSelector value

-- | Applies a custom color to the button's bezel, in appearances that support it. A nil value indicates an unmodified button appearance. The default value is nil.
--
-- ObjC selector: @- bezelColor@
bezelColor :: IsNSButton nsButton => nsButton -> IO (Id NSColor)
bezelColor nsButton =
  sendMessage nsButton bezelColorSelector

-- | Applies a custom color to the button's bezel, in appearances that support it. A nil value indicates an unmodified button appearance. The default value is nil.
--
-- ObjC selector: @- setBezelColor:@
setBezelColor :: (IsNSButton nsButton, IsNSColor value) => nsButton -> value -> IO ()
setBezelColor nsButton value =
  sendMessage nsButton setBezelColorSelector (toNSColor value)

-- | Applies a tint color to template image and text content, in combination with other theme-appropriate effects. Only applicable to borderless buttons. A nil value indicates the standard set of effects without color modification. The default value is nil. Non-template images and attributed string values are not affected by the contentTintColor.
--
-- ObjC selector: @- contentTintColor@
contentTintColor :: IsNSButton nsButton => nsButton -> IO (Id NSColor)
contentTintColor nsButton =
  sendMessage nsButton contentTintColorSelector

-- | Applies a tint color to template image and text content, in combination with other theme-appropriate effects. Only applicable to borderless buttons. A nil value indicates the standard set of effects without color modification. The default value is nil. Non-template images and attributed string values are not affected by the contentTintColor.
--
-- ObjC selector: @- setContentTintColor:@
setContentTintColor :: (IsNSButton nsButton, IsNSColor value) => nsButton -> value -> IO ()
setContentTintColor nsButton value =
  sendMessage nsButton setContentTintColorSelector (toNSColor value)

-- | The tint prominence of the button. Use tint prominence to gently suggest a hierarchy when multiple buttons perform similar actions. A button with primary tint prominence suggests the most preferred option, while secondary prominence indicates a reasonable alternative. See ``NSTintProminence`` for a list of possible values.
--
-- ObjC selector: @- tintProminence@
tintProminence :: IsNSButton nsButton => nsButton -> IO NSTintProminence
tintProminence nsButton =
  sendMessage nsButton tintProminenceSelector

-- | The tint prominence of the button. Use tint prominence to gently suggest a hierarchy when multiple buttons perform similar actions. A button with primary tint prominence suggests the most preferred option, while secondary prominence indicates a reasonable alternative. See ``NSTintProminence`` for a list of possible values.
--
-- ObjC selector: @- setTintProminence:@
setTintProminence :: IsNSButton nsButton => nsButton -> NSTintProminence -> IO ()
setTintProminence nsButton value =
  sendMessage nsButton setTintProminenceSelector value

-- | The image that appears on the button when it’s in an off state, or nil if there is no such image.
--
-- ObjC selector: @- image@
image :: IsNSButton nsButton => nsButton -> IO (Id NSImage)
image nsButton =
  sendMessage nsButton imageSelector

-- | The image that appears on the button when it’s in an off state, or nil if there is no such image.
--
-- ObjC selector: @- setImage:@
setImage :: (IsNSButton nsButton, IsNSImage value) => nsButton -> value -> IO ()
setImage nsButton value =
  sendMessage nsButton setImageSelector (toNSImage value)

-- | An alternate image that appears on the button when the button is in an on state, or nil if there is no such image. Note that some button types do not display an alternate image.
--
-- ObjC selector: @- alternateImage@
alternateImage :: IsNSButton nsButton => nsButton -> IO (Id NSImage)
alternateImage nsButton =
  sendMessage nsButton alternateImageSelector

-- | An alternate image that appears on the button when the button is in an on state, or nil if there is no such image. Note that some button types do not display an alternate image.
--
-- ObjC selector: @- setAlternateImage:@
setAlternateImage :: (IsNSButton nsButton, IsNSImage value) => nsButton -> value -> IO ()
setAlternateImage nsButton value =
  sendMessage nsButton setAlternateImageSelector (toNSImage value)

-- | The position of the button's image relative to its title. See the NSCellImagePosition enumeration for possible values.
--
-- ObjC selector: @- imagePosition@
imagePosition :: IsNSButton nsButton => nsButton -> IO NSCellImagePosition
imagePosition nsButton =
  sendMessage nsButton imagePositionSelector

-- | The position of the button's image relative to its title. See the NSCellImagePosition enumeration for possible values.
--
-- ObjC selector: @- setImagePosition:@
setImagePosition :: IsNSButton nsButton => nsButton -> NSCellImagePosition -> IO ()
setImagePosition nsButton value =
  sendMessage nsButton setImagePositionSelector value

-- | The scaling mode applied to make the button's image fit within its bounds.
--
-- ObjC selector: @- imageScaling@
imageScaling :: IsNSButton nsButton => nsButton -> IO NSImageScaling
imageScaling nsButton =
  sendMessage nsButton imageScalingSelector

-- | The scaling mode applied to make the button's image fit within its bounds.
--
-- ObjC selector: @- setImageScaling:@
setImageScaling :: IsNSButton nsButton => nsButton -> NSImageScaling -> IO ()
setImageScaling nsButton value =
  sendMessage nsButton setImageScalingSelector value

-- | A Boolean value that determines how the button's image and title are positioned together within the button bezel. If false, the image is positioned according to the imagePosition property at the edge of the button bezel, and the title is positioned within the remaining space. If true, the button’s image is positioned directly adjacent to the title based on the imagePosition property, and the image and title are positioned within the button bezel as a single unit.
--
-- ObjC selector: @- imageHugsTitle@
imageHugsTitle :: IsNSButton nsButton => nsButton -> IO Bool
imageHugsTitle nsButton =
  sendMessage nsButton imageHugsTitleSelector

-- | A Boolean value that determines how the button's image and title are positioned together within the button bezel. If false, the image is positioned according to the imagePosition property at the edge of the button bezel, and the title is positioned within the remaining space. If true, the button’s image is positioned directly adjacent to the title based on the imagePosition property, and the image and title are positioned within the button bezel as a single unit.
--
-- ObjC selector: @- setImageHugsTitle:@
setImageHugsTitle :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setImageHugsTitle nsButton value =
  sendMessage nsButton setImageHugsTitleSelector value

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the symbol is matched to the button's @font@ property. The default value is nil.
--
-- ObjC selector: @- symbolConfiguration@
symbolConfiguration :: IsNSButton nsButton => nsButton -> IO (Id NSImageSymbolConfiguration)
symbolConfiguration nsButton =
  sendMessage nsButton symbolConfigurationSelector

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the symbol is matched to the button's @font@ property. The default value is nil.
--
-- ObjC selector: @- setSymbolConfiguration:@
setSymbolConfiguration :: (IsNSButton nsButton, IsNSImageSymbolConfiguration value) => nsButton -> value -> IO ()
setSymbolConfiguration nsButton value =
  sendMessage nsButton setSymbolConfigurationSelector (toNSImageSymbolConfiguration value)

-- | The button's state. Buttons support the off and on states, and an additional mixed state depending on the value of the @allowsMixedState@ property.
--
-- ObjC selector: @- state@
state :: IsNSButton nsButton => nsButton -> IO CLong
state nsButton =
  sendMessage nsButton stateSelector

-- | The button's state. Buttons support the off and on states, and an additional mixed state depending on the value of the @allowsMixedState@ property.
--
-- ObjC selector: @- setState:@
setState :: IsNSButton nsButton => nsButton -> CLong -> IO ()
setState nsButton value =
  sendMessage nsButton setStateSelector value

-- | A Boolean value that indicates whether the button allows a mixed state. If NO, the button has two states (on and off), and if YES, the button has three states (on, off, and mixed). The mixed state is commonly used with checkboxes and radio buttons to indicate a value which is partially on.
--
-- ObjC selector: @- allowsMixedState@
allowsMixedState :: IsNSButton nsButton => nsButton -> IO Bool
allowsMixedState nsButton =
  sendMessage nsButton allowsMixedStateSelector

-- | A Boolean value that indicates whether the button allows a mixed state. If NO, the button has two states (on and off), and if YES, the button has three states (on, off, and mixed). The mixed state is commonly used with checkboxes and radio buttons to indicate a value which is partially on.
--
-- ObjC selector: @- setAllowsMixedState:@
setAllowsMixedState :: IsNSButton nsButton => nsButton -> Bool -> IO ()
setAllowsMixedState nsButton value =
  sendMessage nsButton setAllowsMixedStateSelector value

-- | This property contains the button's key equivalent, or the empty string if no equivalent has been defined. Buttons don’t have a default key equivalent. Setting the key equivalent to the Return character causes it to act as the default button for its window.
--
-- ObjC selector: @- keyEquivalent@
keyEquivalent :: IsNSButton nsButton => nsButton -> IO (Id NSString)
keyEquivalent nsButton =
  sendMessage nsButton keyEquivalentSelector

-- | This property contains the button's key equivalent, or the empty string if no equivalent has been defined. Buttons don’t have a default key equivalent. Setting the key equivalent to the Return character causes it to act as the default button for its window.
--
-- ObjC selector: @- setKeyEquivalent:@
setKeyEquivalent :: (IsNSButton nsButton, IsNSString value) => nsButton -> value -> IO ()
setKeyEquivalent nsButton value =
  sendMessage nsButton setKeyEquivalentSelector (toNSString value)

-- | A bitmask specifying the modifier keys that are applied to the button's key equivalent. Mask bits are defined by the NSEventModifierFlags option set. The only mask bits relevant in button key-equivalent modifier masks are NSEventModifierFlagControl, NSEventModifierFlagOption, and NSEventModifierFlagCommand.
--
-- ObjC selector: @- keyEquivalentModifierMask@
keyEquivalentModifierMask :: IsNSButton nsButton => nsButton -> IO NSEventModifierFlags
keyEquivalentModifierMask nsButton =
  sendMessage nsButton keyEquivalentModifierMaskSelector

-- | A bitmask specifying the modifier keys that are applied to the button's key equivalent. Mask bits are defined by the NSEventModifierFlags option set. The only mask bits relevant in button key-equivalent modifier masks are NSEventModifierFlagControl, NSEventModifierFlagOption, and NSEventModifierFlagCommand.
--
-- ObjC selector: @- setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMask :: IsNSButton nsButton => nsButton -> NSEventModifierFlags -> IO ()
setKeyEquivalentModifierMask nsButton value =
  sendMessage nsButton setKeyEquivalentModifierMaskSelector value

-- | @- activeCompressionOptions@
activeCompressionOptions :: IsNSButton nsButton => nsButton -> IO (Id NSUserInterfaceCompressionOptions)
activeCompressionOptions nsButton =
  sendMessage nsButton activeCompressionOptionsSelector

-- | @- borderShape@
borderShape :: IsNSButton nsButton => nsButton -> IO NSControlBorderShape
borderShape nsButton =
  sendMessage nsButton borderShapeSelector

-- | @- setBorderShape:@
setBorderShape :: IsNSButton nsButton => nsButton -> NSControlBorderShape -> IO ()
setBorderShape nsButton value =
  sendMessage nsButton setBorderShapeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @buttonWithTitle:image:target:action:@
buttonWithTitle_image_target_actionSelector :: Selector '[Id NSString, Id NSImage, RawId, Sel] (Id NSButton)
buttonWithTitle_image_target_actionSelector = mkSelector "buttonWithTitle:image:target:action:"

-- | @Selector@ for @buttonWithTitle:target:action:@
buttonWithTitle_target_actionSelector :: Selector '[Id NSString, RawId, Sel] (Id NSButton)
buttonWithTitle_target_actionSelector = mkSelector "buttonWithTitle:target:action:"

-- | @Selector@ for @buttonWithImage:target:action:@
buttonWithImage_target_actionSelector :: Selector '[Id NSImage, RawId, Sel] (Id NSButton)
buttonWithImage_target_actionSelector = mkSelector "buttonWithImage:target:action:"

-- | @Selector@ for @checkboxWithTitle:target:action:@
checkboxWithTitle_target_actionSelector :: Selector '[Id NSString, RawId, Sel] (Id NSButton)
checkboxWithTitle_target_actionSelector = mkSelector "checkboxWithTitle:target:action:"

-- | @Selector@ for @radioButtonWithTitle:target:action:@
radioButtonWithTitle_target_actionSelector :: Selector '[Id NSString, RawId, Sel] (Id NSButton)
radioButtonWithTitle_target_actionSelector = mkSelector "radioButtonWithTitle:target:action:"

-- | @Selector@ for @setButtonType:@
setButtonTypeSelector :: Selector '[NSButtonType] ()
setButtonTypeSelector = mkSelector "setButtonType:"

-- | @Selector@ for @setPeriodicDelay:interval:@
setPeriodicDelay_intervalSelector :: Selector '[CFloat, CFloat] ()
setPeriodicDelay_intervalSelector = mkSelector "setPeriodicDelay:interval:"

-- | @Selector@ for @getPeriodicDelay:interval:@
getPeriodicDelay_intervalSelector :: Selector '[Ptr CFloat, Ptr CFloat] ()
getPeriodicDelay_intervalSelector = mkSelector "getPeriodicDelay:interval:"

-- | @Selector@ for @setNextState@
setNextStateSelector :: Selector '[] ()
setNextStateSelector = mkSelector "setNextState"

-- | @Selector@ for @highlight:@
highlightSelector :: Selector '[Bool] ()
highlightSelector = mkSelector "highlight:"

-- | @Selector@ for @performKeyEquivalent:@
performKeyEquivalentSelector :: Selector '[Id NSEvent] Bool
performKeyEquivalentSelector = mkSelector "performKeyEquivalent:"

-- | @Selector@ for @compressWithPrioritizedCompressionOptions:@
compressWithPrioritizedCompressionOptionsSelector :: Selector '[Id NSArray] ()
compressWithPrioritizedCompressionOptionsSelector = mkSelector "compressWithPrioritizedCompressionOptions:"

-- | @Selector@ for @minimumSizeWithPrioritizedCompressionOptions:@
minimumSizeWithPrioritizedCompressionOptionsSelector :: Selector '[Id NSArray] NSSize
minimumSizeWithPrioritizedCompressionOptionsSelector = mkSelector "minimumSizeWithPrioritizedCompressionOptions:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

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

-- | @Selector@ for @alternateTitle@
alternateTitleSelector :: Selector '[] (Id NSString)
alternateTitleSelector = mkSelector "alternateTitle"

-- | @Selector@ for @setAlternateTitle:@
setAlternateTitleSelector :: Selector '[Id NSString] ()
setAlternateTitleSelector = mkSelector "setAlternateTitle:"

-- | @Selector@ for @attributedAlternateTitle@
attributedAlternateTitleSelector :: Selector '[] (Id NSAttributedString)
attributedAlternateTitleSelector = mkSelector "attributedAlternateTitle"

-- | @Selector@ for @setAttributedAlternateTitle:@
setAttributedAlternateTitleSelector :: Selector '[Id NSAttributedString] ()
setAttributedAlternateTitleSelector = mkSelector "setAttributedAlternateTitle:"

-- | @Selector@ for @hasDestructiveAction@
hasDestructiveActionSelector :: Selector '[] Bool
hasDestructiveActionSelector = mkSelector "hasDestructiveAction"

-- | @Selector@ for @setHasDestructiveAction:@
setHasDestructiveActionSelector :: Selector '[Bool] ()
setHasDestructiveActionSelector = mkSelector "setHasDestructiveAction:"

-- | @Selector@ for @sound@
soundSelector :: Selector '[] (Id NSSound)
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector '[Id NSSound] ()
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @springLoaded@
springLoadedSelector :: Selector '[] Bool
springLoadedSelector = mkSelector "springLoaded"

-- | @Selector@ for @setSpringLoaded:@
setSpringLoadedSelector :: Selector '[Bool] ()
setSpringLoadedSelector = mkSelector "setSpringLoaded:"

-- | @Selector@ for @maxAcceleratorLevel@
maxAcceleratorLevelSelector :: Selector '[] CLong
maxAcceleratorLevelSelector = mkSelector "maxAcceleratorLevel"

-- | @Selector@ for @setMaxAcceleratorLevel:@
setMaxAcceleratorLevelSelector :: Selector '[CLong] ()
setMaxAcceleratorLevelSelector = mkSelector "setMaxAcceleratorLevel:"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector '[] NSBezelStyle
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector '[NSBezelStyle] ()
setBezelStyleSelector = mkSelector "setBezelStyle:"

-- | @Selector@ for @bordered@
borderedSelector :: Selector '[] Bool
borderedSelector = mkSelector "bordered"

-- | @Selector@ for @setBordered:@
setBorderedSelector :: Selector '[Bool] ()
setBorderedSelector = mkSelector "setBordered:"

-- | @Selector@ for @transparent@
transparentSelector :: Selector '[] Bool
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @setTransparent:@
setTransparentSelector :: Selector '[Bool] ()
setTransparentSelector = mkSelector "setTransparent:"

-- | @Selector@ for @showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInsideSelector :: Selector '[] Bool
showsBorderOnlyWhileMouseInsideSelector = mkSelector "showsBorderOnlyWhileMouseInside"

-- | @Selector@ for @setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInsideSelector :: Selector '[Bool] ()
setShowsBorderOnlyWhileMouseInsideSelector = mkSelector "setShowsBorderOnlyWhileMouseInside:"

-- | @Selector@ for @bezelColor@
bezelColorSelector :: Selector '[] (Id NSColor)
bezelColorSelector = mkSelector "bezelColor"

-- | @Selector@ for @setBezelColor:@
setBezelColorSelector :: Selector '[Id NSColor] ()
setBezelColorSelector = mkSelector "setBezelColor:"

-- | @Selector@ for @contentTintColor@
contentTintColorSelector :: Selector '[] (Id NSColor)
contentTintColorSelector = mkSelector "contentTintColor"

-- | @Selector@ for @setContentTintColor:@
setContentTintColorSelector :: Selector '[Id NSColor] ()
setContentTintColorSelector = mkSelector "setContentTintColor:"

-- | @Selector@ for @tintProminence@
tintProminenceSelector :: Selector '[] NSTintProminence
tintProminenceSelector = mkSelector "tintProminence"

-- | @Selector@ for @setTintProminence:@
setTintProminenceSelector :: Selector '[NSTintProminence] ()
setTintProminenceSelector = mkSelector "setTintProminence:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @alternateImage@
alternateImageSelector :: Selector '[] (Id NSImage)
alternateImageSelector = mkSelector "alternateImage"

-- | @Selector@ for @setAlternateImage:@
setAlternateImageSelector :: Selector '[Id NSImage] ()
setAlternateImageSelector = mkSelector "setAlternateImage:"

-- | @Selector@ for @imagePosition@
imagePositionSelector :: Selector '[] NSCellImagePosition
imagePositionSelector = mkSelector "imagePosition"

-- | @Selector@ for @setImagePosition:@
setImagePositionSelector :: Selector '[NSCellImagePosition] ()
setImagePositionSelector = mkSelector "setImagePosition:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector '[] NSImageScaling
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector '[NSImageScaling] ()
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @imageHugsTitle@
imageHugsTitleSelector :: Selector '[] Bool
imageHugsTitleSelector = mkSelector "imageHugsTitle"

-- | @Selector@ for @setImageHugsTitle:@
setImageHugsTitleSelector :: Selector '[Bool] ()
setImageHugsTitleSelector = mkSelector "setImageHugsTitle:"

-- | @Selector@ for @symbolConfiguration@
symbolConfigurationSelector :: Selector '[] (Id NSImageSymbolConfiguration)
symbolConfigurationSelector = mkSelector "symbolConfiguration"

-- | @Selector@ for @setSymbolConfiguration:@
setSymbolConfigurationSelector :: Selector '[Id NSImageSymbolConfiguration] ()
setSymbolConfigurationSelector = mkSelector "setSymbolConfiguration:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] CLong
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[CLong] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @allowsMixedState@
allowsMixedStateSelector :: Selector '[] Bool
allowsMixedStateSelector = mkSelector "allowsMixedState"

-- | @Selector@ for @setAllowsMixedState:@
setAllowsMixedStateSelector :: Selector '[Bool] ()
setAllowsMixedStateSelector = mkSelector "setAllowsMixedState:"

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

-- | @Selector@ for @activeCompressionOptions@
activeCompressionOptionsSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
activeCompressionOptionsSelector = mkSelector "activeCompressionOptions"

-- | @Selector@ for @borderShape@
borderShapeSelector :: Selector '[] NSControlBorderShape
borderShapeSelector = mkSelector "borderShape"

-- | @Selector@ for @setBorderShape:@
setBorderShapeSelector :: Selector '[NSControlBorderShape] ()
setBorderShapeSelector = mkSelector "setBorderShape:"

