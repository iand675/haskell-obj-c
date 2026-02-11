{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSButtonCell@.
module ObjC.AppKit.NSButtonCell
  ( NSButtonCell
  , IsNSButtonCell(..)
  , initTextCell
  , initImageCell
  , initWithCoder
  , setButtonType
  , setPeriodicDelay_interval
  , getPeriodicDelay_interval
  , performClick
  , mouseEntered
  , mouseExited
  , drawBezelWithFrame_inView
  , drawImage_withFrame_inView
  , drawTitle_withFrame_inView
  , setTitleWithMnemonic
  , setAlternateTitleWithMnemonic
  , setAlternateMnemonicLocation
  , alternateMnemonicLocation
  , alternateMnemonic
  , setKeyEquivalentFont_size
  , bezelStyle
  , setBezelStyle
  , highlightsBy
  , setHighlightsBy
  , showsStateBy
  , setShowsStateBy
  , title
  , setTitle
  , attributedTitle
  , setAttributedTitle
  , alternateTitle
  , setAlternateTitle
  , attributedAlternateTitle
  , setAttributedAlternateTitle
  , alternateImage
  , setAlternateImage
  , imagePosition
  , setImagePosition
  , imageScaling
  , setImageScaling
  , keyEquivalent
  , setKeyEquivalent
  , keyEquivalentModifierMask
  , setKeyEquivalentModifierMask
  , transparent
  , setTransparent
  , opaque
  , imageDimsWhenDisabled
  , setImageDimsWhenDisabled
  , showsBorderOnlyWhileMouseInside
  , setShowsBorderOnlyWhileMouseInside
  , sound
  , setSound
  , backgroundColor
  , setBackgroundColor
  , gradientType
  , setGradientType
  , keyEquivalentFont
  , setKeyEquivalentFont
  , initTextCellSelector
  , initImageCellSelector
  , initWithCoderSelector
  , setButtonTypeSelector
  , setPeriodicDelay_intervalSelector
  , getPeriodicDelay_intervalSelector
  , performClickSelector
  , mouseEnteredSelector
  , mouseExitedSelector
  , drawBezelWithFrame_inViewSelector
  , drawImage_withFrame_inViewSelector
  , drawTitle_withFrame_inViewSelector
  , setTitleWithMnemonicSelector
  , setAlternateTitleWithMnemonicSelector
  , setAlternateMnemonicLocationSelector
  , alternateMnemonicLocationSelector
  , alternateMnemonicSelector
  , setKeyEquivalentFont_sizeSelector
  , bezelStyleSelector
  , setBezelStyleSelector
  , highlightsBySelector
  , setHighlightsBySelector
  , showsStateBySelector
  , setShowsStateBySelector
  , titleSelector
  , setTitleSelector
  , attributedTitleSelector
  , setAttributedTitleSelector
  , alternateTitleSelector
  , setAlternateTitleSelector
  , attributedAlternateTitleSelector
  , setAttributedAlternateTitleSelector
  , alternateImageSelector
  , setAlternateImageSelector
  , imagePositionSelector
  , setImagePositionSelector
  , imageScalingSelector
  , setImageScalingSelector
  , keyEquivalentSelector
  , setKeyEquivalentSelector
  , keyEquivalentModifierMaskSelector
  , setKeyEquivalentModifierMaskSelector
  , transparentSelector
  , setTransparentSelector
  , opaqueSelector
  , imageDimsWhenDisabledSelector
  , setImageDimsWhenDisabledSelector
  , showsBorderOnlyWhileMouseInsideSelector
  , setShowsBorderOnlyWhileMouseInsideSelector
  , soundSelector
  , setSoundSelector
  , backgroundColorSelector
  , setBackgroundColorSelector
  , gradientTypeSelector
  , setGradientTypeSelector
  , keyEquivalentFontSelector
  , setKeyEquivalentFontSelector

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
  , NSCellStyleMask(NSCellStyleMask)
  , pattern NSNoCellMask
  , pattern NSContentsCellMask
  , pattern NSPushInCellMask
  , pattern NSChangeGrayCellMask
  , pattern NSChangeBackgroundCellMask
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
  , NSGradientType(NSGradientType)
  , pattern NSGradientNone
  , pattern NSGradientConcaveWeak
  , pattern NSGradientConcaveStrong
  , pattern NSGradientConvexWeak
  , pattern NSGradientConvexStrong
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
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- initTextCell:@
initTextCell :: (IsNSButtonCell nsButtonCell, IsNSString string) => nsButtonCell -> string -> IO (Id NSButtonCell)
initTextCell nsButtonCell  string =
  withObjCPtr string $ \raw_string ->
      sendMsg nsButtonCell (mkSelector "initTextCell:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= ownedObject . castPtr

-- | @- initImageCell:@
initImageCell :: (IsNSButtonCell nsButtonCell, IsNSImage image) => nsButtonCell -> image -> IO (Id NSButtonCell)
initImageCell nsButtonCell  image =
  withObjCPtr image $ \raw_image ->
      sendMsg nsButtonCell (mkSelector "initImageCell:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSButtonCell nsButtonCell, IsNSCoder coder) => nsButtonCell -> coder -> IO (Id NSButtonCell)
initWithCoder nsButtonCell  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsButtonCell (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- setButtonType:@
setButtonType :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSButtonType -> IO ()
setButtonType nsButtonCell  type_ =
    sendMsg nsButtonCell (mkSelector "setButtonType:") retVoid [argCULong (coerce type_)]

-- | @- setPeriodicDelay:interval:@
setPeriodicDelay_interval :: IsNSButtonCell nsButtonCell => nsButtonCell -> CFloat -> CFloat -> IO ()
setPeriodicDelay_interval nsButtonCell  delay interval =
    sendMsg nsButtonCell (mkSelector "setPeriodicDelay:interval:") retVoid [argCFloat delay, argCFloat interval]

-- | @- getPeriodicDelay:interval:@
getPeriodicDelay_interval :: IsNSButtonCell nsButtonCell => nsButtonCell -> Ptr CFloat -> Ptr CFloat -> IO ()
getPeriodicDelay_interval nsButtonCell  delay interval =
    sendMsg nsButtonCell (mkSelector "getPeriodicDelay:interval:") retVoid [argPtr delay, argPtr interval]

-- | @- performClick:@
performClick :: IsNSButtonCell nsButtonCell => nsButtonCell -> RawId -> IO ()
performClick nsButtonCell  sender =
    sendMsg nsButtonCell (mkSelector "performClick:") retVoid [argPtr (castPtr (unRawId sender) :: Ptr ())]

-- | @- mouseEntered:@
mouseEntered :: (IsNSButtonCell nsButtonCell, IsNSEvent event) => nsButtonCell -> event -> IO ()
mouseEntered nsButtonCell  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsButtonCell (mkSelector "mouseEntered:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseExited:@
mouseExited :: (IsNSButtonCell nsButtonCell, IsNSEvent event) => nsButtonCell -> event -> IO ()
mouseExited nsButtonCell  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsButtonCell (mkSelector "mouseExited:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- drawBezelWithFrame:inView:@
drawBezelWithFrame_inView :: (IsNSButtonCell nsButtonCell, IsNSView controlView) => nsButtonCell -> NSRect -> controlView -> IO ()
drawBezelWithFrame_inView nsButtonCell  frame controlView =
  withObjCPtr controlView $ \raw_controlView ->
      sendMsg nsButtonCell (mkSelector "drawBezelWithFrame:inView:") retVoid [argNSRect frame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawImage:withFrame:inView:@
drawImage_withFrame_inView :: (IsNSButtonCell nsButtonCell, IsNSImage image, IsNSView controlView) => nsButtonCell -> image -> NSRect -> controlView -> IO ()
drawImage_withFrame_inView nsButtonCell  image frame controlView =
  withObjCPtr image $ \raw_image ->
    withObjCPtr controlView $ \raw_controlView ->
        sendMsg nsButtonCell (mkSelector "drawImage:withFrame:inView:") retVoid [argPtr (castPtr raw_image :: Ptr ()), argNSRect frame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- drawTitle:withFrame:inView:@
drawTitle_withFrame_inView :: (IsNSButtonCell nsButtonCell, IsNSAttributedString title, IsNSView controlView) => nsButtonCell -> title -> NSRect -> controlView -> IO NSRect
drawTitle_withFrame_inView nsButtonCell  title frame controlView =
  withObjCPtr title $ \raw_title ->
    withObjCPtr controlView $ \raw_controlView ->
        sendMsgStret nsButtonCell (mkSelector "drawTitle:withFrame:inView:") retNSRect [argPtr (castPtr raw_title :: Ptr ()), argNSRect frame, argPtr (castPtr raw_controlView :: Ptr ())]

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSButtonCell nsButtonCell, IsNSString stringWithAmpersand) => nsButtonCell -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsButtonCell  stringWithAmpersand =
  withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
      sendMsg nsButtonCell (mkSelector "setTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | @- setAlternateTitleWithMnemonic:@
setAlternateTitleWithMnemonic :: (IsNSButtonCell nsButtonCell, IsNSString stringWithAmpersand) => nsButtonCell -> stringWithAmpersand -> IO ()
setAlternateTitleWithMnemonic nsButtonCell  stringWithAmpersand =
  withObjCPtr stringWithAmpersand $ \raw_stringWithAmpersand ->
      sendMsg nsButtonCell (mkSelector "setAlternateTitleWithMnemonic:") retVoid [argPtr (castPtr raw_stringWithAmpersand :: Ptr ())]

-- | @- setAlternateMnemonicLocation:@
setAlternateMnemonicLocation :: IsNSButtonCell nsButtonCell => nsButtonCell -> CULong -> IO ()
setAlternateMnemonicLocation nsButtonCell  location =
    sendMsg nsButtonCell (mkSelector "setAlternateMnemonicLocation:") retVoid [argCULong location]

-- | @- alternateMnemonicLocation@
alternateMnemonicLocation :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO CULong
alternateMnemonicLocation nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "alternateMnemonicLocation") retCULong []

-- | @- alternateMnemonic@
alternateMnemonic :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
alternateMnemonic nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "alternateMnemonic") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyEquivalentFont:size:@
setKeyEquivalentFont_size :: (IsNSButtonCell nsButtonCell, IsNSString fontName) => nsButtonCell -> fontName -> CDouble -> IO ()
setKeyEquivalentFont_size nsButtonCell  fontName fontSize =
  withObjCPtr fontName $ \raw_fontName ->
      sendMsg nsButtonCell (mkSelector "setKeyEquivalentFont:size:") retVoid [argPtr (castPtr raw_fontName :: Ptr ()), argCDouble fontSize]

-- | @- bezelStyle@
bezelStyle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSBezelStyle
bezelStyle nsButtonCell  =
    fmap (coerce :: CULong -> NSBezelStyle) $ sendMsg nsButtonCell (mkSelector "bezelStyle") retCULong []

-- | @- setBezelStyle:@
setBezelStyle :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSBezelStyle -> IO ()
setBezelStyle nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setBezelStyle:") retVoid [argCULong (coerce value)]

-- | @- highlightsBy@
highlightsBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSCellStyleMask
highlightsBy nsButtonCell  =
    fmap (coerce :: CULong -> NSCellStyleMask) $ sendMsg nsButtonCell (mkSelector "highlightsBy") retCULong []

-- | @- setHighlightsBy:@
setHighlightsBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSCellStyleMask -> IO ()
setHighlightsBy nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setHighlightsBy:") retVoid [argCULong (coerce value)]

-- | @- showsStateBy@
showsStateBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSCellStyleMask
showsStateBy nsButtonCell  =
    fmap (coerce :: CULong -> NSCellStyleMask) $ sendMsg nsButtonCell (mkSelector "showsStateBy") retCULong []

-- | @- setShowsStateBy:@
setShowsStateBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSCellStyleMask -> IO ()
setShowsStateBy nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setShowsStateBy:") retVoid [argCULong (coerce value)]

-- | @- title@
title :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
title nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSButtonCell nsButtonCell, IsNSString value) => nsButtonCell -> value -> IO ()
setTitle nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedTitle@
attributedTitle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSAttributedString)
attributedTitle nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "attributedTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSButtonCell nsButtonCell, IsNSAttributedString value) => nsButtonCell -> value -> IO ()
setAttributedTitle nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setAttributedTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alternateTitle@
alternateTitle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
alternateTitle nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "alternateTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlternateTitle:@
setAlternateTitle :: (IsNSButtonCell nsButtonCell, IsNSString value) => nsButtonCell -> value -> IO ()
setAlternateTitle nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setAlternateTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributedAlternateTitle@
attributedAlternateTitle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSAttributedString)
attributedAlternateTitle nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "attributedAlternateTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributedAlternateTitle:@
setAttributedAlternateTitle :: (IsNSButtonCell nsButtonCell, IsNSAttributedString value) => nsButtonCell -> value -> IO ()
setAttributedAlternateTitle nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setAttributedAlternateTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- alternateImage@
alternateImage :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSImage)
alternateImage nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "alternateImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAlternateImage:@
setAlternateImage :: (IsNSButtonCell nsButtonCell, IsNSImage value) => nsButtonCell -> value -> IO ()
setAlternateImage nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setAlternateImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- imagePosition@
imagePosition :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSCellImagePosition
imagePosition nsButtonCell  =
    fmap (coerce :: CULong -> NSCellImagePosition) $ sendMsg nsButtonCell (mkSelector "imagePosition") retCULong []

-- | @- setImagePosition:@
setImagePosition :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSCellImagePosition -> IO ()
setImagePosition nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setImagePosition:") retVoid [argCULong (coerce value)]

-- | @- imageScaling@
imageScaling :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSImageScaling
imageScaling nsButtonCell  =
    fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsButtonCell (mkSelector "imageScaling") retCULong []

-- | @- setImageScaling:@
setImageScaling :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSImageScaling -> IO ()
setImageScaling nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setImageScaling:") retVoid [argCULong (coerce value)]

-- | @- keyEquivalent@
keyEquivalent :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
keyEquivalent nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "keyEquivalent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyEquivalent:@
setKeyEquivalent :: (IsNSButtonCell nsButtonCell, IsNSString value) => nsButtonCell -> value -> IO ()
setKeyEquivalent nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setKeyEquivalent:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- keyEquivalentModifierMask@
keyEquivalentModifierMask :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSEventModifierFlags
keyEquivalentModifierMask nsButtonCell  =
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg nsButtonCell (mkSelector "keyEquivalentModifierMask") retCULong []

-- | @- setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMask :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSEventModifierFlags -> IO ()
setKeyEquivalentModifierMask nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setKeyEquivalentModifierMask:") retVoid [argCULong (coerce value)]

-- | @- transparent@
transparent :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
transparent nsButtonCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButtonCell (mkSelector "transparent") retCULong []

-- | @- setTransparent:@
setTransparent :: IsNSButtonCell nsButtonCell => nsButtonCell -> Bool -> IO ()
setTransparent nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setTransparent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- opaque@
opaque :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
opaque nsButtonCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButtonCell (mkSelector "opaque") retCULong []

-- | @- imageDimsWhenDisabled@
imageDimsWhenDisabled :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
imageDimsWhenDisabled nsButtonCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButtonCell (mkSelector "imageDimsWhenDisabled") retCULong []

-- | @- setImageDimsWhenDisabled:@
setImageDimsWhenDisabled :: IsNSButtonCell nsButtonCell => nsButtonCell -> Bool -> IO ()
setImageDimsWhenDisabled nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setImageDimsWhenDisabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInside :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
showsBorderOnlyWhileMouseInside nsButtonCell  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsButtonCell (mkSelector "showsBorderOnlyWhileMouseInside") retCULong []

-- | @- setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInside :: IsNSButtonCell nsButtonCell => nsButtonCell -> Bool -> IO ()
setShowsBorderOnlyWhileMouseInside nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setShowsBorderOnlyWhileMouseInside:") retVoid [argCULong (if value then 1 else 0)]

-- | @- sound@
sound :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSSound)
sound nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "sound") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSound:@
setSound :: (IsNSButtonCell nsButtonCell, IsNSSound value) => nsButtonCell -> value -> IO ()
setSound nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setSound:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- backgroundColor@
backgroundColor :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSColor)
backgroundColor nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "backgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSButtonCell nsButtonCell, IsNSColor value) => nsButtonCell -> value -> IO ()
setBackgroundColor nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setBackgroundColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- gradientType@
gradientType :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSGradientType
gradientType nsButtonCell  =
    fmap (coerce :: CULong -> NSGradientType) $ sendMsg nsButtonCell (mkSelector "gradientType") retCULong []

-- | @- setGradientType:@
setGradientType :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSGradientType -> IO ()
setGradientType nsButtonCell  value =
    sendMsg nsButtonCell (mkSelector "setGradientType:") retVoid [argCULong (coerce value)]

-- | @- keyEquivalentFont@
keyEquivalentFont :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSFont)
keyEquivalentFont nsButtonCell  =
    sendMsg nsButtonCell (mkSelector "keyEquivalentFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKeyEquivalentFont:@
setKeyEquivalentFont :: (IsNSButtonCell nsButtonCell, IsNSFont value) => nsButtonCell -> value -> IO ()
setKeyEquivalentFont nsButtonCell  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsButtonCell (mkSelector "setKeyEquivalentFont:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setButtonType:@
setButtonTypeSelector :: Selector
setButtonTypeSelector = mkSelector "setButtonType:"

-- | @Selector@ for @setPeriodicDelay:interval:@
setPeriodicDelay_intervalSelector :: Selector
setPeriodicDelay_intervalSelector = mkSelector "setPeriodicDelay:interval:"

-- | @Selector@ for @getPeriodicDelay:interval:@
getPeriodicDelay_intervalSelector :: Selector
getPeriodicDelay_intervalSelector = mkSelector "getPeriodicDelay:interval:"

-- | @Selector@ for @performClick:@
performClickSelector :: Selector
performClickSelector = mkSelector "performClick:"

-- | @Selector@ for @mouseEntered:@
mouseEnteredSelector :: Selector
mouseEnteredSelector = mkSelector "mouseEntered:"

-- | @Selector@ for @mouseExited:@
mouseExitedSelector :: Selector
mouseExitedSelector = mkSelector "mouseExited:"

-- | @Selector@ for @drawBezelWithFrame:inView:@
drawBezelWithFrame_inViewSelector :: Selector
drawBezelWithFrame_inViewSelector = mkSelector "drawBezelWithFrame:inView:"

-- | @Selector@ for @drawImage:withFrame:inView:@
drawImage_withFrame_inViewSelector :: Selector
drawImage_withFrame_inViewSelector = mkSelector "drawImage:withFrame:inView:"

-- | @Selector@ for @drawTitle:withFrame:inView:@
drawTitle_withFrame_inViewSelector :: Selector
drawTitle_withFrame_inViewSelector = mkSelector "drawTitle:withFrame:inView:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @setAlternateTitleWithMnemonic:@
setAlternateTitleWithMnemonicSelector :: Selector
setAlternateTitleWithMnemonicSelector = mkSelector "setAlternateTitleWithMnemonic:"

-- | @Selector@ for @setAlternateMnemonicLocation:@
setAlternateMnemonicLocationSelector :: Selector
setAlternateMnemonicLocationSelector = mkSelector "setAlternateMnemonicLocation:"

-- | @Selector@ for @alternateMnemonicLocation@
alternateMnemonicLocationSelector :: Selector
alternateMnemonicLocationSelector = mkSelector "alternateMnemonicLocation"

-- | @Selector@ for @alternateMnemonic@
alternateMnemonicSelector :: Selector
alternateMnemonicSelector = mkSelector "alternateMnemonic"

-- | @Selector@ for @setKeyEquivalentFont:size:@
setKeyEquivalentFont_sizeSelector :: Selector
setKeyEquivalentFont_sizeSelector = mkSelector "setKeyEquivalentFont:size:"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector
setBezelStyleSelector = mkSelector "setBezelStyle:"

-- | @Selector@ for @highlightsBy@
highlightsBySelector :: Selector
highlightsBySelector = mkSelector "highlightsBy"

-- | @Selector@ for @setHighlightsBy:@
setHighlightsBySelector :: Selector
setHighlightsBySelector = mkSelector "setHighlightsBy:"

-- | @Selector@ for @showsStateBy@
showsStateBySelector :: Selector
showsStateBySelector = mkSelector "showsStateBy"

-- | @Selector@ for @setShowsStateBy:@
setShowsStateBySelector :: Selector
setShowsStateBySelector = mkSelector "setShowsStateBy:"

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

-- | @Selector@ for @transparent@
transparentSelector :: Selector
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @setTransparent:@
setTransparentSelector :: Selector
setTransparentSelector = mkSelector "setTransparent:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @imageDimsWhenDisabled@
imageDimsWhenDisabledSelector :: Selector
imageDimsWhenDisabledSelector = mkSelector "imageDimsWhenDisabled"

-- | @Selector@ for @setImageDimsWhenDisabled:@
setImageDimsWhenDisabledSelector :: Selector
setImageDimsWhenDisabledSelector = mkSelector "setImageDimsWhenDisabled:"

-- | @Selector@ for @showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInsideSelector :: Selector
showsBorderOnlyWhileMouseInsideSelector = mkSelector "showsBorderOnlyWhileMouseInside"

-- | @Selector@ for @setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInsideSelector :: Selector
setShowsBorderOnlyWhileMouseInsideSelector = mkSelector "setShowsBorderOnlyWhileMouseInside:"

-- | @Selector@ for @sound@
soundSelector :: Selector
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @gradientType@
gradientTypeSelector :: Selector
gradientTypeSelector = mkSelector "gradientType"

-- | @Selector@ for @setGradientType:@
setGradientTypeSelector :: Selector
setGradientTypeSelector = mkSelector "setGradientType:"

-- | @Selector@ for @keyEquivalentFont@
keyEquivalentFontSelector :: Selector
keyEquivalentFontSelector = mkSelector "keyEquivalentFont"

-- | @Selector@ for @setKeyEquivalentFont:@
setKeyEquivalentFontSelector :: Selector
setKeyEquivalentFontSelector = mkSelector "setKeyEquivalentFont:"

