{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , alternateImageSelector
  , alternateMnemonicLocationSelector
  , alternateMnemonicSelector
  , alternateTitleSelector
  , attributedAlternateTitleSelector
  , attributedTitleSelector
  , backgroundColorSelector
  , bezelStyleSelector
  , drawBezelWithFrame_inViewSelector
  , drawImage_withFrame_inViewSelector
  , drawTitle_withFrame_inViewSelector
  , getPeriodicDelay_intervalSelector
  , gradientTypeSelector
  , highlightsBySelector
  , imageDimsWhenDisabledSelector
  , imagePositionSelector
  , imageScalingSelector
  , initImageCellSelector
  , initTextCellSelector
  , initWithCoderSelector
  , keyEquivalentFontSelector
  , keyEquivalentModifierMaskSelector
  , keyEquivalentSelector
  , mouseEnteredSelector
  , mouseExitedSelector
  , opaqueSelector
  , performClickSelector
  , setAlternateImageSelector
  , setAlternateMnemonicLocationSelector
  , setAlternateTitleSelector
  , setAlternateTitleWithMnemonicSelector
  , setAttributedAlternateTitleSelector
  , setAttributedTitleSelector
  , setBackgroundColorSelector
  , setBezelStyleSelector
  , setButtonTypeSelector
  , setGradientTypeSelector
  , setHighlightsBySelector
  , setImageDimsWhenDisabledSelector
  , setImagePositionSelector
  , setImageScalingSelector
  , setKeyEquivalentFontSelector
  , setKeyEquivalentFont_sizeSelector
  , setKeyEquivalentModifierMaskSelector
  , setKeyEquivalentSelector
  , setPeriodicDelay_intervalSelector
  , setShowsBorderOnlyWhileMouseInsideSelector
  , setShowsStateBySelector
  , setSoundSelector
  , setTitleSelector
  , setTitleWithMnemonicSelector
  , setTransparentSelector
  , showsBorderOnlyWhileMouseInsideSelector
  , showsStateBySelector
  , soundSelector
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

-- | @- initTextCell:@
initTextCell :: (IsNSButtonCell nsButtonCell, IsNSString string) => nsButtonCell -> string -> IO (Id NSButtonCell)
initTextCell nsButtonCell string =
  sendOwnedMessage nsButtonCell initTextCellSelector (toNSString string)

-- | @- initImageCell:@
initImageCell :: (IsNSButtonCell nsButtonCell, IsNSImage image) => nsButtonCell -> image -> IO (Id NSButtonCell)
initImageCell nsButtonCell image =
  sendOwnedMessage nsButtonCell initImageCellSelector (toNSImage image)

-- | @- initWithCoder:@
initWithCoder :: (IsNSButtonCell nsButtonCell, IsNSCoder coder) => nsButtonCell -> coder -> IO (Id NSButtonCell)
initWithCoder nsButtonCell coder =
  sendOwnedMessage nsButtonCell initWithCoderSelector (toNSCoder coder)

-- | @- setButtonType:@
setButtonType :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSButtonType -> IO ()
setButtonType nsButtonCell type_ =
  sendMessage nsButtonCell setButtonTypeSelector type_

-- | @- setPeriodicDelay:interval:@
setPeriodicDelay_interval :: IsNSButtonCell nsButtonCell => nsButtonCell -> CFloat -> CFloat -> IO ()
setPeriodicDelay_interval nsButtonCell delay interval =
  sendMessage nsButtonCell setPeriodicDelay_intervalSelector delay interval

-- | @- getPeriodicDelay:interval:@
getPeriodicDelay_interval :: IsNSButtonCell nsButtonCell => nsButtonCell -> Ptr CFloat -> Ptr CFloat -> IO ()
getPeriodicDelay_interval nsButtonCell delay interval =
  sendMessage nsButtonCell getPeriodicDelay_intervalSelector delay interval

-- | @- performClick:@
performClick :: IsNSButtonCell nsButtonCell => nsButtonCell -> RawId -> IO ()
performClick nsButtonCell sender =
  sendMessage nsButtonCell performClickSelector sender

-- | @- mouseEntered:@
mouseEntered :: (IsNSButtonCell nsButtonCell, IsNSEvent event) => nsButtonCell -> event -> IO ()
mouseEntered nsButtonCell event =
  sendMessage nsButtonCell mouseEnteredSelector (toNSEvent event)

-- | @- mouseExited:@
mouseExited :: (IsNSButtonCell nsButtonCell, IsNSEvent event) => nsButtonCell -> event -> IO ()
mouseExited nsButtonCell event =
  sendMessage nsButtonCell mouseExitedSelector (toNSEvent event)

-- | @- drawBezelWithFrame:inView:@
drawBezelWithFrame_inView :: (IsNSButtonCell nsButtonCell, IsNSView controlView) => nsButtonCell -> NSRect -> controlView -> IO ()
drawBezelWithFrame_inView nsButtonCell frame controlView =
  sendMessage nsButtonCell drawBezelWithFrame_inViewSelector frame (toNSView controlView)

-- | @- drawImage:withFrame:inView:@
drawImage_withFrame_inView :: (IsNSButtonCell nsButtonCell, IsNSImage image, IsNSView controlView) => nsButtonCell -> image -> NSRect -> controlView -> IO ()
drawImage_withFrame_inView nsButtonCell image frame controlView =
  sendMessage nsButtonCell drawImage_withFrame_inViewSelector (toNSImage image) frame (toNSView controlView)

-- | @- drawTitle:withFrame:inView:@
drawTitle_withFrame_inView :: (IsNSButtonCell nsButtonCell, IsNSAttributedString title, IsNSView controlView) => nsButtonCell -> title -> NSRect -> controlView -> IO NSRect
drawTitle_withFrame_inView nsButtonCell title frame controlView =
  sendMessage nsButtonCell drawTitle_withFrame_inViewSelector (toNSAttributedString title) frame (toNSView controlView)

-- | @- setTitleWithMnemonic:@
setTitleWithMnemonic :: (IsNSButtonCell nsButtonCell, IsNSString stringWithAmpersand) => nsButtonCell -> stringWithAmpersand -> IO ()
setTitleWithMnemonic nsButtonCell stringWithAmpersand =
  sendMessage nsButtonCell setTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | @- setAlternateTitleWithMnemonic:@
setAlternateTitleWithMnemonic :: (IsNSButtonCell nsButtonCell, IsNSString stringWithAmpersand) => nsButtonCell -> stringWithAmpersand -> IO ()
setAlternateTitleWithMnemonic nsButtonCell stringWithAmpersand =
  sendMessage nsButtonCell setAlternateTitleWithMnemonicSelector (toNSString stringWithAmpersand)

-- | @- setAlternateMnemonicLocation:@
setAlternateMnemonicLocation :: IsNSButtonCell nsButtonCell => nsButtonCell -> CULong -> IO ()
setAlternateMnemonicLocation nsButtonCell location =
  sendMessage nsButtonCell setAlternateMnemonicLocationSelector location

-- | @- alternateMnemonicLocation@
alternateMnemonicLocation :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO CULong
alternateMnemonicLocation nsButtonCell =
  sendMessage nsButtonCell alternateMnemonicLocationSelector

-- | @- alternateMnemonic@
alternateMnemonic :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
alternateMnemonic nsButtonCell =
  sendMessage nsButtonCell alternateMnemonicSelector

-- | @- setKeyEquivalentFont:size:@
setKeyEquivalentFont_size :: (IsNSButtonCell nsButtonCell, IsNSString fontName) => nsButtonCell -> fontName -> CDouble -> IO ()
setKeyEquivalentFont_size nsButtonCell fontName fontSize =
  sendMessage nsButtonCell setKeyEquivalentFont_sizeSelector (toNSString fontName) fontSize

-- | @- bezelStyle@
bezelStyle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSBezelStyle
bezelStyle nsButtonCell =
  sendMessage nsButtonCell bezelStyleSelector

-- | @- setBezelStyle:@
setBezelStyle :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSBezelStyle -> IO ()
setBezelStyle nsButtonCell value =
  sendMessage nsButtonCell setBezelStyleSelector value

-- | @- highlightsBy@
highlightsBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSCellStyleMask
highlightsBy nsButtonCell =
  sendMessage nsButtonCell highlightsBySelector

-- | @- setHighlightsBy:@
setHighlightsBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSCellStyleMask -> IO ()
setHighlightsBy nsButtonCell value =
  sendMessage nsButtonCell setHighlightsBySelector value

-- | @- showsStateBy@
showsStateBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSCellStyleMask
showsStateBy nsButtonCell =
  sendMessage nsButtonCell showsStateBySelector

-- | @- setShowsStateBy:@
setShowsStateBy :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSCellStyleMask -> IO ()
setShowsStateBy nsButtonCell value =
  sendMessage nsButtonCell setShowsStateBySelector value

-- | @- title@
title :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
title nsButtonCell =
  sendMessage nsButtonCell titleSelector

-- | @- setTitle:@
setTitle :: (IsNSButtonCell nsButtonCell, IsNSString value) => nsButtonCell -> value -> IO ()
setTitle nsButtonCell value =
  sendMessage nsButtonCell setTitleSelector (toNSString value)

-- | @- attributedTitle@
attributedTitle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSAttributedString)
attributedTitle nsButtonCell =
  sendMessage nsButtonCell attributedTitleSelector

-- | @- setAttributedTitle:@
setAttributedTitle :: (IsNSButtonCell nsButtonCell, IsNSAttributedString value) => nsButtonCell -> value -> IO ()
setAttributedTitle nsButtonCell value =
  sendMessage nsButtonCell setAttributedTitleSelector (toNSAttributedString value)

-- | @- alternateTitle@
alternateTitle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
alternateTitle nsButtonCell =
  sendMessage nsButtonCell alternateTitleSelector

-- | @- setAlternateTitle:@
setAlternateTitle :: (IsNSButtonCell nsButtonCell, IsNSString value) => nsButtonCell -> value -> IO ()
setAlternateTitle nsButtonCell value =
  sendMessage nsButtonCell setAlternateTitleSelector (toNSString value)

-- | @- attributedAlternateTitle@
attributedAlternateTitle :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSAttributedString)
attributedAlternateTitle nsButtonCell =
  sendMessage nsButtonCell attributedAlternateTitleSelector

-- | @- setAttributedAlternateTitle:@
setAttributedAlternateTitle :: (IsNSButtonCell nsButtonCell, IsNSAttributedString value) => nsButtonCell -> value -> IO ()
setAttributedAlternateTitle nsButtonCell value =
  sendMessage nsButtonCell setAttributedAlternateTitleSelector (toNSAttributedString value)

-- | @- alternateImage@
alternateImage :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSImage)
alternateImage nsButtonCell =
  sendMessage nsButtonCell alternateImageSelector

-- | @- setAlternateImage:@
setAlternateImage :: (IsNSButtonCell nsButtonCell, IsNSImage value) => nsButtonCell -> value -> IO ()
setAlternateImage nsButtonCell value =
  sendMessage nsButtonCell setAlternateImageSelector (toNSImage value)

-- | @- imagePosition@
imagePosition :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSCellImagePosition
imagePosition nsButtonCell =
  sendMessage nsButtonCell imagePositionSelector

-- | @- setImagePosition:@
setImagePosition :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSCellImagePosition -> IO ()
setImagePosition nsButtonCell value =
  sendMessage nsButtonCell setImagePositionSelector value

-- | @- imageScaling@
imageScaling :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSImageScaling
imageScaling nsButtonCell =
  sendMessage nsButtonCell imageScalingSelector

-- | @- setImageScaling:@
setImageScaling :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSImageScaling -> IO ()
setImageScaling nsButtonCell value =
  sendMessage nsButtonCell setImageScalingSelector value

-- | @- keyEquivalent@
keyEquivalent :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSString)
keyEquivalent nsButtonCell =
  sendMessage nsButtonCell keyEquivalentSelector

-- | @- setKeyEquivalent:@
setKeyEquivalent :: (IsNSButtonCell nsButtonCell, IsNSString value) => nsButtonCell -> value -> IO ()
setKeyEquivalent nsButtonCell value =
  sendMessage nsButtonCell setKeyEquivalentSelector (toNSString value)

-- | @- keyEquivalentModifierMask@
keyEquivalentModifierMask :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSEventModifierFlags
keyEquivalentModifierMask nsButtonCell =
  sendMessage nsButtonCell keyEquivalentModifierMaskSelector

-- | @- setKeyEquivalentModifierMask:@
setKeyEquivalentModifierMask :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSEventModifierFlags -> IO ()
setKeyEquivalentModifierMask nsButtonCell value =
  sendMessage nsButtonCell setKeyEquivalentModifierMaskSelector value

-- | @- transparent@
transparent :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
transparent nsButtonCell =
  sendMessage nsButtonCell transparentSelector

-- | @- setTransparent:@
setTransparent :: IsNSButtonCell nsButtonCell => nsButtonCell -> Bool -> IO ()
setTransparent nsButtonCell value =
  sendMessage nsButtonCell setTransparentSelector value

-- | @- opaque@
opaque :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
opaque nsButtonCell =
  sendMessage nsButtonCell opaqueSelector

-- | @- imageDimsWhenDisabled@
imageDimsWhenDisabled :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
imageDimsWhenDisabled nsButtonCell =
  sendMessage nsButtonCell imageDimsWhenDisabledSelector

-- | @- setImageDimsWhenDisabled:@
setImageDimsWhenDisabled :: IsNSButtonCell nsButtonCell => nsButtonCell -> Bool -> IO ()
setImageDimsWhenDisabled nsButtonCell value =
  sendMessage nsButtonCell setImageDimsWhenDisabledSelector value

-- | @- showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInside :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO Bool
showsBorderOnlyWhileMouseInside nsButtonCell =
  sendMessage nsButtonCell showsBorderOnlyWhileMouseInsideSelector

-- | @- setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInside :: IsNSButtonCell nsButtonCell => nsButtonCell -> Bool -> IO ()
setShowsBorderOnlyWhileMouseInside nsButtonCell value =
  sendMessage nsButtonCell setShowsBorderOnlyWhileMouseInsideSelector value

-- | @- sound@
sound :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSSound)
sound nsButtonCell =
  sendMessage nsButtonCell soundSelector

-- | @- setSound:@
setSound :: (IsNSButtonCell nsButtonCell, IsNSSound value) => nsButtonCell -> value -> IO ()
setSound nsButtonCell value =
  sendMessage nsButtonCell setSoundSelector (toNSSound value)

-- | @- backgroundColor@
backgroundColor :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSColor)
backgroundColor nsButtonCell =
  sendMessage nsButtonCell backgroundColorSelector

-- | @- setBackgroundColor:@
setBackgroundColor :: (IsNSButtonCell nsButtonCell, IsNSColor value) => nsButtonCell -> value -> IO ()
setBackgroundColor nsButtonCell value =
  sendMessage nsButtonCell setBackgroundColorSelector (toNSColor value)

-- | @- gradientType@
gradientType :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO NSGradientType
gradientType nsButtonCell =
  sendMessage nsButtonCell gradientTypeSelector

-- | @- setGradientType:@
setGradientType :: IsNSButtonCell nsButtonCell => nsButtonCell -> NSGradientType -> IO ()
setGradientType nsButtonCell value =
  sendMessage nsButtonCell setGradientTypeSelector value

-- | @- keyEquivalentFont@
keyEquivalentFont :: IsNSButtonCell nsButtonCell => nsButtonCell -> IO (Id NSFont)
keyEquivalentFont nsButtonCell =
  sendMessage nsButtonCell keyEquivalentFontSelector

-- | @- setKeyEquivalentFont:@
setKeyEquivalentFont :: (IsNSButtonCell nsButtonCell, IsNSFont value) => nsButtonCell -> value -> IO ()
setKeyEquivalentFont nsButtonCell value =
  sendMessage nsButtonCell setKeyEquivalentFontSelector (toNSFont value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initTextCell:@
initTextCellSelector :: Selector '[Id NSString] (Id NSButtonCell)
initTextCellSelector = mkSelector "initTextCell:"

-- | @Selector@ for @initImageCell:@
initImageCellSelector :: Selector '[Id NSImage] (Id NSButtonCell)
initImageCellSelector = mkSelector "initImageCell:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSButtonCell)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @setButtonType:@
setButtonTypeSelector :: Selector '[NSButtonType] ()
setButtonTypeSelector = mkSelector "setButtonType:"

-- | @Selector@ for @setPeriodicDelay:interval:@
setPeriodicDelay_intervalSelector :: Selector '[CFloat, CFloat] ()
setPeriodicDelay_intervalSelector = mkSelector "setPeriodicDelay:interval:"

-- | @Selector@ for @getPeriodicDelay:interval:@
getPeriodicDelay_intervalSelector :: Selector '[Ptr CFloat, Ptr CFloat] ()
getPeriodicDelay_intervalSelector = mkSelector "getPeriodicDelay:interval:"

-- | @Selector@ for @performClick:@
performClickSelector :: Selector '[RawId] ()
performClickSelector = mkSelector "performClick:"

-- | @Selector@ for @mouseEntered:@
mouseEnteredSelector :: Selector '[Id NSEvent] ()
mouseEnteredSelector = mkSelector "mouseEntered:"

-- | @Selector@ for @mouseExited:@
mouseExitedSelector :: Selector '[Id NSEvent] ()
mouseExitedSelector = mkSelector "mouseExited:"

-- | @Selector@ for @drawBezelWithFrame:inView:@
drawBezelWithFrame_inViewSelector :: Selector '[NSRect, Id NSView] ()
drawBezelWithFrame_inViewSelector = mkSelector "drawBezelWithFrame:inView:"

-- | @Selector@ for @drawImage:withFrame:inView:@
drawImage_withFrame_inViewSelector :: Selector '[Id NSImage, NSRect, Id NSView] ()
drawImage_withFrame_inViewSelector = mkSelector "drawImage:withFrame:inView:"

-- | @Selector@ for @drawTitle:withFrame:inView:@
drawTitle_withFrame_inViewSelector :: Selector '[Id NSAttributedString, NSRect, Id NSView] NSRect
drawTitle_withFrame_inViewSelector = mkSelector "drawTitle:withFrame:inView:"

-- | @Selector@ for @setTitleWithMnemonic:@
setTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setTitleWithMnemonicSelector = mkSelector "setTitleWithMnemonic:"

-- | @Selector@ for @setAlternateTitleWithMnemonic:@
setAlternateTitleWithMnemonicSelector :: Selector '[Id NSString] ()
setAlternateTitleWithMnemonicSelector = mkSelector "setAlternateTitleWithMnemonic:"

-- | @Selector@ for @setAlternateMnemonicLocation:@
setAlternateMnemonicLocationSelector :: Selector '[CULong] ()
setAlternateMnemonicLocationSelector = mkSelector "setAlternateMnemonicLocation:"

-- | @Selector@ for @alternateMnemonicLocation@
alternateMnemonicLocationSelector :: Selector '[] CULong
alternateMnemonicLocationSelector = mkSelector "alternateMnemonicLocation"

-- | @Selector@ for @alternateMnemonic@
alternateMnemonicSelector :: Selector '[] (Id NSString)
alternateMnemonicSelector = mkSelector "alternateMnemonic"

-- | @Selector@ for @setKeyEquivalentFont:size:@
setKeyEquivalentFont_sizeSelector :: Selector '[Id NSString, CDouble] ()
setKeyEquivalentFont_sizeSelector = mkSelector "setKeyEquivalentFont:size:"

-- | @Selector@ for @bezelStyle@
bezelStyleSelector :: Selector '[] NSBezelStyle
bezelStyleSelector = mkSelector "bezelStyle"

-- | @Selector@ for @setBezelStyle:@
setBezelStyleSelector :: Selector '[NSBezelStyle] ()
setBezelStyleSelector = mkSelector "setBezelStyle:"

-- | @Selector@ for @highlightsBy@
highlightsBySelector :: Selector '[] NSCellStyleMask
highlightsBySelector = mkSelector "highlightsBy"

-- | @Selector@ for @setHighlightsBy:@
setHighlightsBySelector :: Selector '[NSCellStyleMask] ()
setHighlightsBySelector = mkSelector "setHighlightsBy:"

-- | @Selector@ for @showsStateBy@
showsStateBySelector :: Selector '[] NSCellStyleMask
showsStateBySelector = mkSelector "showsStateBy"

-- | @Selector@ for @setShowsStateBy:@
setShowsStateBySelector :: Selector '[NSCellStyleMask] ()
setShowsStateBySelector = mkSelector "setShowsStateBy:"

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

-- | @Selector@ for @transparent@
transparentSelector :: Selector '[] Bool
transparentSelector = mkSelector "transparent"

-- | @Selector@ for @setTransparent:@
setTransparentSelector :: Selector '[Bool] ()
setTransparentSelector = mkSelector "setTransparent:"

-- | @Selector@ for @opaque@
opaqueSelector :: Selector '[] Bool
opaqueSelector = mkSelector "opaque"

-- | @Selector@ for @imageDimsWhenDisabled@
imageDimsWhenDisabledSelector :: Selector '[] Bool
imageDimsWhenDisabledSelector = mkSelector "imageDimsWhenDisabled"

-- | @Selector@ for @setImageDimsWhenDisabled:@
setImageDimsWhenDisabledSelector :: Selector '[Bool] ()
setImageDimsWhenDisabledSelector = mkSelector "setImageDimsWhenDisabled:"

-- | @Selector@ for @showsBorderOnlyWhileMouseInside@
showsBorderOnlyWhileMouseInsideSelector :: Selector '[] Bool
showsBorderOnlyWhileMouseInsideSelector = mkSelector "showsBorderOnlyWhileMouseInside"

-- | @Selector@ for @setShowsBorderOnlyWhileMouseInside:@
setShowsBorderOnlyWhileMouseInsideSelector :: Selector '[Bool] ()
setShowsBorderOnlyWhileMouseInsideSelector = mkSelector "setShowsBorderOnlyWhileMouseInside:"

-- | @Selector@ for @sound@
soundSelector :: Selector '[] (Id NSSound)
soundSelector = mkSelector "sound"

-- | @Selector@ for @setSound:@
setSoundSelector :: Selector '[Id NSSound] ()
setSoundSelector = mkSelector "setSound:"

-- | @Selector@ for @backgroundColor@
backgroundColorSelector :: Selector '[] (Id NSColor)
backgroundColorSelector = mkSelector "backgroundColor"

-- | @Selector@ for @setBackgroundColor:@
setBackgroundColorSelector :: Selector '[Id NSColor] ()
setBackgroundColorSelector = mkSelector "setBackgroundColor:"

-- | @Selector@ for @gradientType@
gradientTypeSelector :: Selector '[] NSGradientType
gradientTypeSelector = mkSelector "gradientType"

-- | @Selector@ for @setGradientType:@
setGradientTypeSelector :: Selector '[NSGradientType] ()
setGradientTypeSelector = mkSelector "setGradientType:"

-- | @Selector@ for @keyEquivalentFont@
keyEquivalentFontSelector :: Selector '[] (Id NSFont)
keyEquivalentFontSelector = mkSelector "keyEquivalentFont"

-- | @Selector@ for @setKeyEquivalentFont:@
setKeyEquivalentFontSelector :: Selector '[Id NSFont] ()
setKeyEquivalentFontSelector = mkSelector "setKeyEquivalentFont:"

