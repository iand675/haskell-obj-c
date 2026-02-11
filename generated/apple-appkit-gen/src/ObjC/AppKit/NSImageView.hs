{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSImageView@.
module ObjC.AppKit.NSImageView
  ( NSImageView
  , IsNSImageView(..)
  , imageViewWithImage
  , addSymbolEffect
  , addSymbolEffect_options
  , addSymbolEffect_options_animated
  , removeSymbolEffectOfType
  , removeSymbolEffectOfType_options
  , removeSymbolEffectOfType_options_animated
  , removeAllSymbolEffects
  , removeAllSymbolEffectsWithOptions
  , removeAllSymbolEffectsWithOptions_animated
  , setSymbolImage_withContentTransition
  , setSymbolImage_withContentTransition_options
  , image
  , setImage
  , editable
  , setEditable
  , imageAlignment
  , setImageAlignment
  , imageScaling
  , setImageScaling
  , imageFrameStyle
  , setImageFrameStyle
  , symbolConfiguration
  , setSymbolConfiguration
  , contentTintColor
  , setContentTintColor
  , animates
  , setAnimates
  , allowsCutCopyPaste
  , setAllowsCutCopyPaste
  , defaultPreferredImageDynamicRange
  , setDefaultPreferredImageDynamicRange
  , preferredImageDynamicRange
  , setPreferredImageDynamicRange
  , imageDynamicRange
  , imageViewWithImageSelector
  , addSymbolEffectSelector
  , addSymbolEffect_optionsSelector
  , addSymbolEffect_options_animatedSelector
  , removeSymbolEffectOfTypeSelector
  , removeSymbolEffectOfType_optionsSelector
  , removeSymbolEffectOfType_options_animatedSelector
  , removeAllSymbolEffectsSelector
  , removeAllSymbolEffectsWithOptionsSelector
  , removeAllSymbolEffectsWithOptions_animatedSelector
  , setSymbolImage_withContentTransitionSelector
  , setSymbolImage_withContentTransition_optionsSelector
  , imageSelector
  , setImageSelector
  , editableSelector
  , setEditableSelector
  , imageAlignmentSelector
  , setImageAlignmentSelector
  , imageScalingSelector
  , setImageScalingSelector
  , imageFrameStyleSelector
  , setImageFrameStyleSelector
  , symbolConfigurationSelector
  , setSymbolConfigurationSelector
  , contentTintColorSelector
  , setContentTintColorSelector
  , animatesSelector
  , setAnimatesSelector
  , allowsCutCopyPasteSelector
  , setAllowsCutCopyPasteSelector
  , defaultPreferredImageDynamicRangeSelector
  , setDefaultPreferredImageDynamicRangeSelector
  , preferredImageDynamicRangeSelector
  , setPreferredImageDynamicRangeSelector
  , imageDynamicRangeSelector

  -- * Enum types
  , NSImageAlignment(NSImageAlignment)
  , pattern NSImageAlignCenter
  , pattern NSImageAlignTop
  , pattern NSImageAlignTopLeft
  , pattern NSImageAlignTopRight
  , pattern NSImageAlignLeft
  , pattern NSImageAlignBottom
  , pattern NSImageAlignBottomLeft
  , pattern NSImageAlignBottomRight
  , pattern NSImageAlignRight
  , NSImageDynamicRange(NSImageDynamicRange)
  , pattern NSImageDynamicRangeUnspecified
  , pattern NSImageDynamicRangeStandard
  , pattern NSImageDynamicRangeConstrainedHigh
  , pattern NSImageDynamicRangeHigh
  , NSImageFrameStyle(NSImageFrameStyle)
  , pattern NSImageFrameNone
  , pattern NSImageFramePhoto
  , pattern NSImageFrameGrayBezel
  , pattern NSImageFrameGroove
  , pattern NSImageFrameButton
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
import ObjC.Symbols.Internal.Classes

-- | Creates a non-editable image view containing the provided image. The image is scaled proportionally down to fit the view, and is centered within the view.
--
-- @image@ — The image to display within the view.
--
-- Returns: An initialized image view.
--
-- ObjC selector: @+ imageViewWithImage:@
imageViewWithImage :: IsNSImage image => image -> IO (Id NSImageView)
imageViewWithImage image =
  do
    cls' <- getRequiredClass "NSImageView"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "imageViewWithImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | Adds a symbol effect to the image view with default options and animation.
--
-- ObjC selector: @- addSymbolEffect:@
addSymbolEffect :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect) => nsImageView -> symbolEffect -> IO ()
addSymbolEffect nsImageView  symbolEffect =
  withObjCPtr symbolEffect $ \raw_symbolEffect ->
      sendMsg nsImageView (mkSelector "addSymbolEffect:") retVoid [argPtr (castPtr raw_symbolEffect :: Ptr ())]

-- | Adds a symbol effect to the image view with specified options and default animation.
--
-- ObjC selector: @- addSymbolEffect:options:@
addSymbolEffect_options :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> IO ()
addSymbolEffect_options nsImageView  symbolEffect options =
  withObjCPtr symbolEffect $ \raw_symbolEffect ->
    withObjCPtr options $ \raw_options ->
        sendMsg nsImageView (mkSelector "addSymbolEffect:options:") retVoid [argPtr (castPtr raw_symbolEffect :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | Adds a symbol effect to the image view with specified options and animation.
--
-- ObjC selector: @- addSymbolEffect:options:animated:@
addSymbolEffect_options_animated :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> Bool -> IO ()
addSymbolEffect_options_animated nsImageView  symbolEffect options animated =
  withObjCPtr symbolEffect $ \raw_symbolEffect ->
    withObjCPtr options $ \raw_options ->
        sendMsg nsImageView (mkSelector "addSymbolEffect:options:animated:") retVoid [argPtr (castPtr raw_symbolEffect :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | Removes from the image view the symbol effect matching the type of effect passed in, with default options and animation.
--
-- ObjC selector: @- removeSymbolEffectOfType:@
removeSymbolEffectOfType :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect) => nsImageView -> symbolEffect -> IO ()
removeSymbolEffectOfType nsImageView  symbolEffect =
  withObjCPtr symbolEffect $ \raw_symbolEffect ->
      sendMsg nsImageView (mkSelector "removeSymbolEffectOfType:") retVoid [argPtr (castPtr raw_symbolEffect :: Ptr ())]

-- | Removes from the image view the symbol effect matching the type of effect passed in, with specified options and default animation.
--
-- ObjC selector: @- removeSymbolEffectOfType:options:@
removeSymbolEffectOfType_options :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> IO ()
removeSymbolEffectOfType_options nsImageView  symbolEffect options =
  withObjCPtr symbolEffect $ \raw_symbolEffect ->
    withObjCPtr options $ \raw_options ->
        sendMsg nsImageView (mkSelector "removeSymbolEffectOfType:options:") retVoid [argPtr (castPtr raw_symbolEffect :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | Removes from the image view the symbol effect matching the type of effect passed in, with specified options and animation.
--
-- ObjC selector: @- removeSymbolEffectOfType:options:animated:@
removeSymbolEffectOfType_options_animated :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> Bool -> IO ()
removeSymbolEffectOfType_options_animated nsImageView  symbolEffect options animated =
  withObjCPtr symbolEffect $ \raw_symbolEffect ->
    withObjCPtr options $ \raw_options ->
        sendMsg nsImageView (mkSelector "removeSymbolEffectOfType:options:animated:") retVoid [argPtr (castPtr raw_symbolEffect :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | Removes all symbol effects from the image view with default options and animation.
--
-- ObjC selector: @- removeAllSymbolEffects@
removeAllSymbolEffects :: IsNSImageView nsImageView => nsImageView -> IO ()
removeAllSymbolEffects nsImageView  =
    sendMsg nsImageView (mkSelector "removeAllSymbolEffects") retVoid []

-- | Removes all symbol effects from the image view with specified options and default animation.
--
-- ObjC selector: @- removeAllSymbolEffectsWithOptions:@
removeAllSymbolEffectsWithOptions :: (IsNSImageView nsImageView, IsNSSymbolEffectOptions options) => nsImageView -> options -> IO ()
removeAllSymbolEffectsWithOptions nsImageView  options =
  withObjCPtr options $ \raw_options ->
      sendMsg nsImageView (mkSelector "removeAllSymbolEffectsWithOptions:") retVoid [argPtr (castPtr raw_options :: Ptr ())]

-- | Removes all symbol effects from the image view with specified options and animation.
--
-- ObjC selector: @- removeAllSymbolEffectsWithOptions:animated:@
removeAllSymbolEffectsWithOptions_animated :: (IsNSImageView nsImageView, IsNSSymbolEffectOptions options) => nsImageView -> options -> Bool -> IO ()
removeAllSymbolEffectsWithOptions_animated nsImageView  options animated =
  withObjCPtr options $ \raw_options ->
      sendMsg nsImageView (mkSelector "removeAllSymbolEffectsWithOptions:animated:") retVoid [argPtr (castPtr raw_options :: Ptr ()), argCULong (if animated then 1 else 0)]

-- | Sets the symbol image on the image view with a symbol content transition and default options. Passing in a non-symbol image will result in undefined behavior.
--
-- ObjC selector: @- setSymbolImage:withContentTransition:@
setSymbolImage_withContentTransition :: (IsNSImageView nsImageView, IsNSImage symbolImage, IsNSSymbolContentTransition transition) => nsImageView -> symbolImage -> transition -> IO ()
setSymbolImage_withContentTransition nsImageView  symbolImage transition =
  withObjCPtr symbolImage $ \raw_symbolImage ->
    withObjCPtr transition $ \raw_transition ->
        sendMsg nsImageView (mkSelector "setSymbolImage:withContentTransition:") retVoid [argPtr (castPtr raw_symbolImage :: Ptr ()), argPtr (castPtr raw_transition :: Ptr ())]

-- | Sets the symbol image on the image view with a symbol content transition and specified options. Passing in a non-symbol image will result in undefined behavior.
--
-- ObjC selector: @- setSymbolImage:withContentTransition:options:@
setSymbolImage_withContentTransition_options :: (IsNSImageView nsImageView, IsNSImage symbolImage, IsNSSymbolContentTransition transition, IsNSSymbolEffectOptions options) => nsImageView -> symbolImage -> transition -> options -> IO ()
setSymbolImage_withContentTransition_options nsImageView  symbolImage transition options =
  withObjCPtr symbolImage $ \raw_symbolImage ->
    withObjCPtr transition $ \raw_transition ->
      withObjCPtr options $ \raw_options ->
          sendMsg nsImageView (mkSelector "setSymbolImage:withContentTransition:options:") retVoid [argPtr (castPtr raw_symbolImage :: Ptr ()), argPtr (castPtr raw_transition :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())]

-- | @- image@
image :: IsNSImageView nsImageView => nsImageView -> IO (Id NSImage)
image nsImageView  =
    sendMsg nsImageView (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setImage:@
setImage :: (IsNSImageView nsImageView, IsNSImage value) => nsImageView -> value -> IO ()
setImage nsImageView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsImageView (mkSelector "setImage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- editable@
editable :: IsNSImageView nsImageView => nsImageView -> IO Bool
editable nsImageView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageView (mkSelector "editable") retCULong []

-- | @- setEditable:@
setEditable :: IsNSImageView nsImageView => nsImageView -> Bool -> IO ()
setEditable nsImageView  value =
    sendMsg nsImageView (mkSelector "setEditable:") retVoid [argCULong (if value then 1 else 0)]

-- | @- imageAlignment@
imageAlignment :: IsNSImageView nsImageView => nsImageView -> IO NSImageAlignment
imageAlignment nsImageView  =
    fmap (coerce :: CULong -> NSImageAlignment) $ sendMsg nsImageView (mkSelector "imageAlignment") retCULong []

-- | @- setImageAlignment:@
setImageAlignment :: IsNSImageView nsImageView => nsImageView -> NSImageAlignment -> IO ()
setImageAlignment nsImageView  value =
    sendMsg nsImageView (mkSelector "setImageAlignment:") retVoid [argCULong (coerce value)]

-- | @- imageScaling@
imageScaling :: IsNSImageView nsImageView => nsImageView -> IO NSImageScaling
imageScaling nsImageView  =
    fmap (coerce :: CULong -> NSImageScaling) $ sendMsg nsImageView (mkSelector "imageScaling") retCULong []

-- | @- setImageScaling:@
setImageScaling :: IsNSImageView nsImageView => nsImageView -> NSImageScaling -> IO ()
setImageScaling nsImageView  value =
    sendMsg nsImageView (mkSelector "setImageScaling:") retVoid [argCULong (coerce value)]

-- | @- imageFrameStyle@
imageFrameStyle :: IsNSImageView nsImageView => nsImageView -> IO NSImageFrameStyle
imageFrameStyle nsImageView  =
    fmap (coerce :: CULong -> NSImageFrameStyle) $ sendMsg nsImageView (mkSelector "imageFrameStyle") retCULong []

-- | @- setImageFrameStyle:@
setImageFrameStyle :: IsNSImageView nsImageView => nsImageView -> NSImageFrameStyle -> IO ()
setImageFrameStyle nsImageView  value =
    sendMsg nsImageView (mkSelector "setImageFrameStyle:") retVoid [argCULong (coerce value)]

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the image view uses a default size, weight, and scale provided by the system. The default value is @nil@.
--
-- ObjC selector: @- symbolConfiguration@
symbolConfiguration :: IsNSImageView nsImageView => nsImageView -> IO (Id NSImageSymbolConfiguration)
symbolConfiguration nsImageView  =
    sendMsg nsImageView (mkSelector "symbolConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the image view uses a default size, weight, and scale provided by the system. The default value is @nil@.
--
-- ObjC selector: @- setSymbolConfiguration:@
setSymbolConfiguration :: (IsNSImageView nsImageView, IsNSImageSymbolConfiguration value) => nsImageView -> value -> IO ()
setSymbolConfiguration nsImageView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsImageView (mkSelector "setSymbolConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A tint color to be used when rendering template image content. This color may be combined with other effects to produce a theme-appropriate rendition of the template image. A nil value indicates the standard set of effects without color modification. The default value is nil.
--
-- ObjC selector: @- contentTintColor@
contentTintColor :: IsNSImageView nsImageView => nsImageView -> IO (Id NSColor)
contentTintColor nsImageView  =
    sendMsg nsImageView (mkSelector "contentTintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A tint color to be used when rendering template image content. This color may be combined with other effects to produce a theme-appropriate rendition of the template image. A nil value indicates the standard set of effects without color modification. The default value is nil.
--
-- ObjC selector: @- setContentTintColor:@
setContentTintColor :: (IsNSImageView nsImageView, IsNSColor value) => nsImageView -> value -> IO ()
setContentTintColor nsImageView  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsImageView (mkSelector "setContentTintColor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- animates@
animates :: IsNSImageView nsImageView => nsImageView -> IO Bool
animates nsImageView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageView (mkSelector "animates") retCULong []

-- | @- setAnimates:@
setAnimates :: IsNSImageView nsImageView => nsImageView -> Bool -> IO ()
setAnimates nsImageView  value =
    sendMsg nsImageView (mkSelector "setAnimates:") retVoid [argCULong (if value then 1 else 0)]

-- | @- allowsCutCopyPaste@
allowsCutCopyPaste :: IsNSImageView nsImageView => nsImageView -> IO Bool
allowsCutCopyPaste nsImageView  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsImageView (mkSelector "allowsCutCopyPaste") retCULong []

-- | @- setAllowsCutCopyPaste:@
setAllowsCutCopyPaste :: IsNSImageView nsImageView => nsImageView -> Bool -> IO ()
setAllowsCutCopyPaste nsImageView  value =
    sendMsg nsImageView (mkSelector "setAllowsCutCopyPaste:") retVoid [argCULong (if value then 1 else 0)]

-- | Default preferred image dynamic range. Defaults to @NSImageDynamicRangeConstrainedHigh@ on macOS 14 and higher, @NSImageDynamicRangeStandard@ otherwise. Set to another value to change the default for all subsequently created @NSImageView@s in your app.
--
-- ObjC selector: @+ defaultPreferredImageDynamicRange@
defaultPreferredImageDynamicRange :: IO NSImageDynamicRange
defaultPreferredImageDynamicRange  =
  do
    cls' <- getRequiredClass "NSImageView"
    fmap (coerce :: CLong -> NSImageDynamicRange) $ sendClassMsg cls' (mkSelector "defaultPreferredImageDynamicRange") retCLong []

-- | Default preferred image dynamic range. Defaults to @NSImageDynamicRangeConstrainedHigh@ on macOS 14 and higher, @NSImageDynamicRangeStandard@ otherwise. Set to another value to change the default for all subsequently created @NSImageView@s in your app.
--
-- ObjC selector: @+ setDefaultPreferredImageDynamicRange:@
setDefaultPreferredImageDynamicRange :: NSImageDynamicRange -> IO ()
setDefaultPreferredImageDynamicRange value =
  do
    cls' <- getRequiredClass "NSImageView"
    sendClassMsg cls' (mkSelector "setDefaultPreferredImageDynamicRange:") retVoid [argCLong (coerce value)]

-- | Preferred dynamic range when displaying an image in the receiving image view.
--
-- ObjC selector: @- preferredImageDynamicRange@
preferredImageDynamicRange :: IsNSImageView nsImageView => nsImageView -> IO NSImageDynamicRange
preferredImageDynamicRange nsImageView  =
    fmap (coerce :: CLong -> NSImageDynamicRange) $ sendMsg nsImageView (mkSelector "preferredImageDynamicRange") retCLong []

-- | Preferred dynamic range when displaying an image in the receiving image view.
--
-- ObjC selector: @- setPreferredImageDynamicRange:@
setPreferredImageDynamicRange :: IsNSImageView nsImageView => nsImageView -> NSImageDynamicRange -> IO ()
setPreferredImageDynamicRange nsImageView  value =
    sendMsg nsImageView (mkSelector "setPreferredImageDynamicRange:") retVoid [argCLong (coerce value)]

-- | Resolved dynamic range based on fully resolved image content. Note: this will return @NSImageDynamicRangeUnspecified@ if the image view has not or can not resolve the content (either because it has no resolvable image content or has not resolved because the image view hasn't displayed.)
--
-- ObjC selector: @- imageDynamicRange@
imageDynamicRange :: IsNSImageView nsImageView => nsImageView -> IO NSImageDynamicRange
imageDynamicRange nsImageView  =
    fmap (coerce :: CLong -> NSImageDynamicRange) $ sendMsg nsImageView (mkSelector "imageDynamicRange") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageViewWithImage:@
imageViewWithImageSelector :: Selector
imageViewWithImageSelector = mkSelector "imageViewWithImage:"

-- | @Selector@ for @addSymbolEffect:@
addSymbolEffectSelector :: Selector
addSymbolEffectSelector = mkSelector "addSymbolEffect:"

-- | @Selector@ for @addSymbolEffect:options:@
addSymbolEffect_optionsSelector :: Selector
addSymbolEffect_optionsSelector = mkSelector "addSymbolEffect:options:"

-- | @Selector@ for @addSymbolEffect:options:animated:@
addSymbolEffect_options_animatedSelector :: Selector
addSymbolEffect_options_animatedSelector = mkSelector "addSymbolEffect:options:animated:"

-- | @Selector@ for @removeSymbolEffectOfType:@
removeSymbolEffectOfTypeSelector :: Selector
removeSymbolEffectOfTypeSelector = mkSelector "removeSymbolEffectOfType:"

-- | @Selector@ for @removeSymbolEffectOfType:options:@
removeSymbolEffectOfType_optionsSelector :: Selector
removeSymbolEffectOfType_optionsSelector = mkSelector "removeSymbolEffectOfType:options:"

-- | @Selector@ for @removeSymbolEffectOfType:options:animated:@
removeSymbolEffectOfType_options_animatedSelector :: Selector
removeSymbolEffectOfType_options_animatedSelector = mkSelector "removeSymbolEffectOfType:options:animated:"

-- | @Selector@ for @removeAllSymbolEffects@
removeAllSymbolEffectsSelector :: Selector
removeAllSymbolEffectsSelector = mkSelector "removeAllSymbolEffects"

-- | @Selector@ for @removeAllSymbolEffectsWithOptions:@
removeAllSymbolEffectsWithOptionsSelector :: Selector
removeAllSymbolEffectsWithOptionsSelector = mkSelector "removeAllSymbolEffectsWithOptions:"

-- | @Selector@ for @removeAllSymbolEffectsWithOptions:animated:@
removeAllSymbolEffectsWithOptions_animatedSelector :: Selector
removeAllSymbolEffectsWithOptions_animatedSelector = mkSelector "removeAllSymbolEffectsWithOptions:animated:"

-- | @Selector@ for @setSymbolImage:withContentTransition:@
setSymbolImage_withContentTransitionSelector :: Selector
setSymbolImage_withContentTransitionSelector = mkSelector "setSymbolImage:withContentTransition:"

-- | @Selector@ for @setSymbolImage:withContentTransition:options:@
setSymbolImage_withContentTransition_optionsSelector :: Selector
setSymbolImage_withContentTransition_optionsSelector = mkSelector "setSymbolImage:withContentTransition:options:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @editable@
editableSelector :: Selector
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @setImageAlignment:@
setImageAlignmentSelector :: Selector
setImageAlignmentSelector = mkSelector "setImageAlignment:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @imageFrameStyle@
imageFrameStyleSelector :: Selector
imageFrameStyleSelector = mkSelector "imageFrameStyle"

-- | @Selector@ for @setImageFrameStyle:@
setImageFrameStyleSelector :: Selector
setImageFrameStyleSelector = mkSelector "setImageFrameStyle:"

-- | @Selector@ for @symbolConfiguration@
symbolConfigurationSelector :: Selector
symbolConfigurationSelector = mkSelector "symbolConfiguration"

-- | @Selector@ for @setSymbolConfiguration:@
setSymbolConfigurationSelector :: Selector
setSymbolConfigurationSelector = mkSelector "setSymbolConfiguration:"

-- | @Selector@ for @contentTintColor@
contentTintColorSelector :: Selector
contentTintColorSelector = mkSelector "contentTintColor"

-- | @Selector@ for @setContentTintColor:@
setContentTintColorSelector :: Selector
setContentTintColorSelector = mkSelector "setContentTintColor:"

-- | @Selector@ for @animates@
animatesSelector :: Selector
animatesSelector = mkSelector "animates"

-- | @Selector@ for @setAnimates:@
setAnimatesSelector :: Selector
setAnimatesSelector = mkSelector "setAnimates:"

-- | @Selector@ for @allowsCutCopyPaste@
allowsCutCopyPasteSelector :: Selector
allowsCutCopyPasteSelector = mkSelector "allowsCutCopyPaste"

-- | @Selector@ for @setAllowsCutCopyPaste:@
setAllowsCutCopyPasteSelector :: Selector
setAllowsCutCopyPasteSelector = mkSelector "setAllowsCutCopyPaste:"

-- | @Selector@ for @defaultPreferredImageDynamicRange@
defaultPreferredImageDynamicRangeSelector :: Selector
defaultPreferredImageDynamicRangeSelector = mkSelector "defaultPreferredImageDynamicRange"

-- | @Selector@ for @setDefaultPreferredImageDynamicRange:@
setDefaultPreferredImageDynamicRangeSelector :: Selector
setDefaultPreferredImageDynamicRangeSelector = mkSelector "setDefaultPreferredImageDynamicRange:"

-- | @Selector@ for @preferredImageDynamicRange@
preferredImageDynamicRangeSelector :: Selector
preferredImageDynamicRangeSelector = mkSelector "preferredImageDynamicRange"

-- | @Selector@ for @setPreferredImageDynamicRange:@
setPreferredImageDynamicRangeSelector :: Selector
setPreferredImageDynamicRangeSelector = mkSelector "setPreferredImageDynamicRange:"

-- | @Selector@ for @imageDynamicRange@
imageDynamicRangeSelector :: Selector
imageDynamicRangeSelector = mkSelector "imageDynamicRange"

