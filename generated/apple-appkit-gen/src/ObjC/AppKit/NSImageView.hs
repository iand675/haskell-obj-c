{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , addSymbolEffectSelector
  , addSymbolEffect_optionsSelector
  , addSymbolEffect_options_animatedSelector
  , allowsCutCopyPasteSelector
  , animatesSelector
  , contentTintColorSelector
  , defaultPreferredImageDynamicRangeSelector
  , editableSelector
  , imageAlignmentSelector
  , imageDynamicRangeSelector
  , imageFrameStyleSelector
  , imageScalingSelector
  , imageSelector
  , imageViewWithImageSelector
  , preferredImageDynamicRangeSelector
  , removeAllSymbolEffectsSelector
  , removeAllSymbolEffectsWithOptionsSelector
  , removeAllSymbolEffectsWithOptions_animatedSelector
  , removeSymbolEffectOfTypeSelector
  , removeSymbolEffectOfType_optionsSelector
  , removeSymbolEffectOfType_options_animatedSelector
  , setAllowsCutCopyPasteSelector
  , setAnimatesSelector
  , setContentTintColorSelector
  , setDefaultPreferredImageDynamicRangeSelector
  , setEditableSelector
  , setImageAlignmentSelector
  , setImageFrameStyleSelector
  , setImageScalingSelector
  , setImageSelector
  , setPreferredImageDynamicRangeSelector
  , setSymbolConfigurationSelector
  , setSymbolImage_withContentTransitionSelector
  , setSymbolImage_withContentTransition_optionsSelector
  , symbolConfigurationSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
    sendClassMessage cls' imageViewWithImageSelector (toNSImage image)

-- | Adds a symbol effect to the image view with default options and animation.
--
-- ObjC selector: @- addSymbolEffect:@
addSymbolEffect :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect) => nsImageView -> symbolEffect -> IO ()
addSymbolEffect nsImageView symbolEffect =
  sendMessage nsImageView addSymbolEffectSelector (toNSSymbolEffect symbolEffect)

-- | Adds a symbol effect to the image view with specified options and default animation.
--
-- ObjC selector: @- addSymbolEffect:options:@
addSymbolEffect_options :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> IO ()
addSymbolEffect_options nsImageView symbolEffect options =
  sendMessage nsImageView addSymbolEffect_optionsSelector (toNSSymbolEffect symbolEffect) (toNSSymbolEffectOptions options)

-- | Adds a symbol effect to the image view with specified options and animation.
--
-- ObjC selector: @- addSymbolEffect:options:animated:@
addSymbolEffect_options_animated :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> Bool -> IO ()
addSymbolEffect_options_animated nsImageView symbolEffect options animated =
  sendMessage nsImageView addSymbolEffect_options_animatedSelector (toNSSymbolEffect symbolEffect) (toNSSymbolEffectOptions options) animated

-- | Removes from the image view the symbol effect matching the type of effect passed in, with default options and animation.
--
-- ObjC selector: @- removeSymbolEffectOfType:@
removeSymbolEffectOfType :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect) => nsImageView -> symbolEffect -> IO ()
removeSymbolEffectOfType nsImageView symbolEffect =
  sendMessage nsImageView removeSymbolEffectOfTypeSelector (toNSSymbolEffect symbolEffect)

-- | Removes from the image view the symbol effect matching the type of effect passed in, with specified options and default animation.
--
-- ObjC selector: @- removeSymbolEffectOfType:options:@
removeSymbolEffectOfType_options :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> IO ()
removeSymbolEffectOfType_options nsImageView symbolEffect options =
  sendMessage nsImageView removeSymbolEffectOfType_optionsSelector (toNSSymbolEffect symbolEffect) (toNSSymbolEffectOptions options)

-- | Removes from the image view the symbol effect matching the type of effect passed in, with specified options and animation.
--
-- ObjC selector: @- removeSymbolEffectOfType:options:animated:@
removeSymbolEffectOfType_options_animated :: (IsNSImageView nsImageView, IsNSSymbolEffect symbolEffect, IsNSSymbolEffectOptions options) => nsImageView -> symbolEffect -> options -> Bool -> IO ()
removeSymbolEffectOfType_options_animated nsImageView symbolEffect options animated =
  sendMessage nsImageView removeSymbolEffectOfType_options_animatedSelector (toNSSymbolEffect symbolEffect) (toNSSymbolEffectOptions options) animated

-- | Removes all symbol effects from the image view with default options and animation.
--
-- ObjC selector: @- removeAllSymbolEffects@
removeAllSymbolEffects :: IsNSImageView nsImageView => nsImageView -> IO ()
removeAllSymbolEffects nsImageView =
  sendMessage nsImageView removeAllSymbolEffectsSelector

-- | Removes all symbol effects from the image view with specified options and default animation.
--
-- ObjC selector: @- removeAllSymbolEffectsWithOptions:@
removeAllSymbolEffectsWithOptions :: (IsNSImageView nsImageView, IsNSSymbolEffectOptions options) => nsImageView -> options -> IO ()
removeAllSymbolEffectsWithOptions nsImageView options =
  sendMessage nsImageView removeAllSymbolEffectsWithOptionsSelector (toNSSymbolEffectOptions options)

-- | Removes all symbol effects from the image view with specified options and animation.
--
-- ObjC selector: @- removeAllSymbolEffectsWithOptions:animated:@
removeAllSymbolEffectsWithOptions_animated :: (IsNSImageView nsImageView, IsNSSymbolEffectOptions options) => nsImageView -> options -> Bool -> IO ()
removeAllSymbolEffectsWithOptions_animated nsImageView options animated =
  sendMessage nsImageView removeAllSymbolEffectsWithOptions_animatedSelector (toNSSymbolEffectOptions options) animated

-- | Sets the symbol image on the image view with a symbol content transition and default options. Passing in a non-symbol image will result in undefined behavior.
--
-- ObjC selector: @- setSymbolImage:withContentTransition:@
setSymbolImage_withContentTransition :: (IsNSImageView nsImageView, IsNSImage symbolImage, IsNSSymbolContentTransition transition) => nsImageView -> symbolImage -> transition -> IO ()
setSymbolImage_withContentTransition nsImageView symbolImage transition =
  sendMessage nsImageView setSymbolImage_withContentTransitionSelector (toNSImage symbolImage) (toNSSymbolContentTransition transition)

-- | Sets the symbol image on the image view with a symbol content transition and specified options. Passing in a non-symbol image will result in undefined behavior.
--
-- ObjC selector: @- setSymbolImage:withContentTransition:options:@
setSymbolImage_withContentTransition_options :: (IsNSImageView nsImageView, IsNSImage symbolImage, IsNSSymbolContentTransition transition, IsNSSymbolEffectOptions options) => nsImageView -> symbolImage -> transition -> options -> IO ()
setSymbolImage_withContentTransition_options nsImageView symbolImage transition options =
  sendMessage nsImageView setSymbolImage_withContentTransition_optionsSelector (toNSImage symbolImage) (toNSSymbolContentTransition transition) (toNSSymbolEffectOptions options)

-- | @- image@
image :: IsNSImageView nsImageView => nsImageView -> IO (Id NSImage)
image nsImageView =
  sendMessage nsImageView imageSelector

-- | @- setImage:@
setImage :: (IsNSImageView nsImageView, IsNSImage value) => nsImageView -> value -> IO ()
setImage nsImageView value =
  sendMessage nsImageView setImageSelector (toNSImage value)

-- | @- editable@
editable :: IsNSImageView nsImageView => nsImageView -> IO Bool
editable nsImageView =
  sendMessage nsImageView editableSelector

-- | @- setEditable:@
setEditable :: IsNSImageView nsImageView => nsImageView -> Bool -> IO ()
setEditable nsImageView value =
  sendMessage nsImageView setEditableSelector value

-- | @- imageAlignment@
imageAlignment :: IsNSImageView nsImageView => nsImageView -> IO NSImageAlignment
imageAlignment nsImageView =
  sendMessage nsImageView imageAlignmentSelector

-- | @- setImageAlignment:@
setImageAlignment :: IsNSImageView nsImageView => nsImageView -> NSImageAlignment -> IO ()
setImageAlignment nsImageView value =
  sendMessage nsImageView setImageAlignmentSelector value

-- | @- imageScaling@
imageScaling :: IsNSImageView nsImageView => nsImageView -> IO NSImageScaling
imageScaling nsImageView =
  sendMessage nsImageView imageScalingSelector

-- | @- setImageScaling:@
setImageScaling :: IsNSImageView nsImageView => nsImageView -> NSImageScaling -> IO ()
setImageScaling nsImageView value =
  sendMessage nsImageView setImageScalingSelector value

-- | @- imageFrameStyle@
imageFrameStyle :: IsNSImageView nsImageView => nsImageView -> IO NSImageFrameStyle
imageFrameStyle nsImageView =
  sendMessage nsImageView imageFrameStyleSelector

-- | @- setImageFrameStyle:@
setImageFrameStyle :: IsNSImageView nsImageView => nsImageView -> NSImageFrameStyle -> IO ()
setImageFrameStyle nsImageView value =
  sendMessage nsImageView setImageFrameStyleSelector value

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the image view uses a default size, weight, and scale provided by the system. The default value is @nil@.
--
-- ObjC selector: @- symbolConfiguration@
symbolConfiguration :: IsNSImageView nsImageView => nsImageView -> IO (Id NSImageSymbolConfiguration)
symbolConfiguration nsImageView =
  sendMessage nsImageView symbolConfigurationSelector

-- | Specifies a combination of point size, weight, and scale to use when sizing and displaying symbol images. If a symbol configuration isn't provided, the image view uses a default size, weight, and scale provided by the system. The default value is @nil@.
--
-- ObjC selector: @- setSymbolConfiguration:@
setSymbolConfiguration :: (IsNSImageView nsImageView, IsNSImageSymbolConfiguration value) => nsImageView -> value -> IO ()
setSymbolConfiguration nsImageView value =
  sendMessage nsImageView setSymbolConfigurationSelector (toNSImageSymbolConfiguration value)

-- | A tint color to be used when rendering template image content. This color may be combined with other effects to produce a theme-appropriate rendition of the template image. A nil value indicates the standard set of effects without color modification. The default value is nil.
--
-- ObjC selector: @- contentTintColor@
contentTintColor :: IsNSImageView nsImageView => nsImageView -> IO (Id NSColor)
contentTintColor nsImageView =
  sendMessage nsImageView contentTintColorSelector

-- | A tint color to be used when rendering template image content. This color may be combined with other effects to produce a theme-appropriate rendition of the template image. A nil value indicates the standard set of effects without color modification. The default value is nil.
--
-- ObjC selector: @- setContentTintColor:@
setContentTintColor :: (IsNSImageView nsImageView, IsNSColor value) => nsImageView -> value -> IO ()
setContentTintColor nsImageView value =
  sendMessage nsImageView setContentTintColorSelector (toNSColor value)

-- | @- animates@
animates :: IsNSImageView nsImageView => nsImageView -> IO Bool
animates nsImageView =
  sendMessage nsImageView animatesSelector

-- | @- setAnimates:@
setAnimates :: IsNSImageView nsImageView => nsImageView -> Bool -> IO ()
setAnimates nsImageView value =
  sendMessage nsImageView setAnimatesSelector value

-- | @- allowsCutCopyPaste@
allowsCutCopyPaste :: IsNSImageView nsImageView => nsImageView -> IO Bool
allowsCutCopyPaste nsImageView =
  sendMessage nsImageView allowsCutCopyPasteSelector

-- | @- setAllowsCutCopyPaste:@
setAllowsCutCopyPaste :: IsNSImageView nsImageView => nsImageView -> Bool -> IO ()
setAllowsCutCopyPaste nsImageView value =
  sendMessage nsImageView setAllowsCutCopyPasteSelector value

-- | Default preferred image dynamic range. Defaults to @NSImageDynamicRangeConstrainedHigh@ on macOS 14 and higher, @NSImageDynamicRangeStandard@ otherwise. Set to another value to change the default for all subsequently created @NSImageView@s in your app.
--
-- ObjC selector: @+ defaultPreferredImageDynamicRange@
defaultPreferredImageDynamicRange :: IO NSImageDynamicRange
defaultPreferredImageDynamicRange  =
  do
    cls' <- getRequiredClass "NSImageView"
    sendClassMessage cls' defaultPreferredImageDynamicRangeSelector

-- | Default preferred image dynamic range. Defaults to @NSImageDynamicRangeConstrainedHigh@ on macOS 14 and higher, @NSImageDynamicRangeStandard@ otherwise. Set to another value to change the default for all subsequently created @NSImageView@s in your app.
--
-- ObjC selector: @+ setDefaultPreferredImageDynamicRange:@
setDefaultPreferredImageDynamicRange :: NSImageDynamicRange -> IO ()
setDefaultPreferredImageDynamicRange value =
  do
    cls' <- getRequiredClass "NSImageView"
    sendClassMessage cls' setDefaultPreferredImageDynamicRangeSelector value

-- | Preferred dynamic range when displaying an image in the receiving image view.
--
-- ObjC selector: @- preferredImageDynamicRange@
preferredImageDynamicRange :: IsNSImageView nsImageView => nsImageView -> IO NSImageDynamicRange
preferredImageDynamicRange nsImageView =
  sendMessage nsImageView preferredImageDynamicRangeSelector

-- | Preferred dynamic range when displaying an image in the receiving image view.
--
-- ObjC selector: @- setPreferredImageDynamicRange:@
setPreferredImageDynamicRange :: IsNSImageView nsImageView => nsImageView -> NSImageDynamicRange -> IO ()
setPreferredImageDynamicRange nsImageView value =
  sendMessage nsImageView setPreferredImageDynamicRangeSelector value

-- | Resolved dynamic range based on fully resolved image content. Note: this will return @NSImageDynamicRangeUnspecified@ if the image view has not or can not resolve the content (either because it has no resolvable image content or has not resolved because the image view hasn't displayed.)
--
-- ObjC selector: @- imageDynamicRange@
imageDynamicRange :: IsNSImageView nsImageView => nsImageView -> IO NSImageDynamicRange
imageDynamicRange nsImageView =
  sendMessage nsImageView imageDynamicRangeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageViewWithImage:@
imageViewWithImageSelector :: Selector '[Id NSImage] (Id NSImageView)
imageViewWithImageSelector = mkSelector "imageViewWithImage:"

-- | @Selector@ for @addSymbolEffect:@
addSymbolEffectSelector :: Selector '[Id NSSymbolEffect] ()
addSymbolEffectSelector = mkSelector "addSymbolEffect:"

-- | @Selector@ for @addSymbolEffect:options:@
addSymbolEffect_optionsSelector :: Selector '[Id NSSymbolEffect, Id NSSymbolEffectOptions] ()
addSymbolEffect_optionsSelector = mkSelector "addSymbolEffect:options:"

-- | @Selector@ for @addSymbolEffect:options:animated:@
addSymbolEffect_options_animatedSelector :: Selector '[Id NSSymbolEffect, Id NSSymbolEffectOptions, Bool] ()
addSymbolEffect_options_animatedSelector = mkSelector "addSymbolEffect:options:animated:"

-- | @Selector@ for @removeSymbolEffectOfType:@
removeSymbolEffectOfTypeSelector :: Selector '[Id NSSymbolEffect] ()
removeSymbolEffectOfTypeSelector = mkSelector "removeSymbolEffectOfType:"

-- | @Selector@ for @removeSymbolEffectOfType:options:@
removeSymbolEffectOfType_optionsSelector :: Selector '[Id NSSymbolEffect, Id NSSymbolEffectOptions] ()
removeSymbolEffectOfType_optionsSelector = mkSelector "removeSymbolEffectOfType:options:"

-- | @Selector@ for @removeSymbolEffectOfType:options:animated:@
removeSymbolEffectOfType_options_animatedSelector :: Selector '[Id NSSymbolEffect, Id NSSymbolEffectOptions, Bool] ()
removeSymbolEffectOfType_options_animatedSelector = mkSelector "removeSymbolEffectOfType:options:animated:"

-- | @Selector@ for @removeAllSymbolEffects@
removeAllSymbolEffectsSelector :: Selector '[] ()
removeAllSymbolEffectsSelector = mkSelector "removeAllSymbolEffects"

-- | @Selector@ for @removeAllSymbolEffectsWithOptions:@
removeAllSymbolEffectsWithOptionsSelector :: Selector '[Id NSSymbolEffectOptions] ()
removeAllSymbolEffectsWithOptionsSelector = mkSelector "removeAllSymbolEffectsWithOptions:"

-- | @Selector@ for @removeAllSymbolEffectsWithOptions:animated:@
removeAllSymbolEffectsWithOptions_animatedSelector :: Selector '[Id NSSymbolEffectOptions, Bool] ()
removeAllSymbolEffectsWithOptions_animatedSelector = mkSelector "removeAllSymbolEffectsWithOptions:animated:"

-- | @Selector@ for @setSymbolImage:withContentTransition:@
setSymbolImage_withContentTransitionSelector :: Selector '[Id NSImage, Id NSSymbolContentTransition] ()
setSymbolImage_withContentTransitionSelector = mkSelector "setSymbolImage:withContentTransition:"

-- | @Selector@ for @setSymbolImage:withContentTransition:options:@
setSymbolImage_withContentTransition_optionsSelector :: Selector '[Id NSImage, Id NSSymbolContentTransition, Id NSSymbolEffectOptions] ()
setSymbolImage_withContentTransition_optionsSelector = mkSelector "setSymbolImage:withContentTransition:options:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @editable@
editableSelector :: Selector '[] Bool
editableSelector = mkSelector "editable"

-- | @Selector@ for @setEditable:@
setEditableSelector :: Selector '[Bool] ()
setEditableSelector = mkSelector "setEditable:"

-- | @Selector@ for @imageAlignment@
imageAlignmentSelector :: Selector '[] NSImageAlignment
imageAlignmentSelector = mkSelector "imageAlignment"

-- | @Selector@ for @setImageAlignment:@
setImageAlignmentSelector :: Selector '[NSImageAlignment] ()
setImageAlignmentSelector = mkSelector "setImageAlignment:"

-- | @Selector@ for @imageScaling@
imageScalingSelector :: Selector '[] NSImageScaling
imageScalingSelector = mkSelector "imageScaling"

-- | @Selector@ for @setImageScaling:@
setImageScalingSelector :: Selector '[NSImageScaling] ()
setImageScalingSelector = mkSelector "setImageScaling:"

-- | @Selector@ for @imageFrameStyle@
imageFrameStyleSelector :: Selector '[] NSImageFrameStyle
imageFrameStyleSelector = mkSelector "imageFrameStyle"

-- | @Selector@ for @setImageFrameStyle:@
setImageFrameStyleSelector :: Selector '[NSImageFrameStyle] ()
setImageFrameStyleSelector = mkSelector "setImageFrameStyle:"

-- | @Selector@ for @symbolConfiguration@
symbolConfigurationSelector :: Selector '[] (Id NSImageSymbolConfiguration)
symbolConfigurationSelector = mkSelector "symbolConfiguration"

-- | @Selector@ for @setSymbolConfiguration:@
setSymbolConfigurationSelector :: Selector '[Id NSImageSymbolConfiguration] ()
setSymbolConfigurationSelector = mkSelector "setSymbolConfiguration:"

-- | @Selector@ for @contentTintColor@
contentTintColorSelector :: Selector '[] (Id NSColor)
contentTintColorSelector = mkSelector "contentTintColor"

-- | @Selector@ for @setContentTintColor:@
setContentTintColorSelector :: Selector '[Id NSColor] ()
setContentTintColorSelector = mkSelector "setContentTintColor:"

-- | @Selector@ for @animates@
animatesSelector :: Selector '[] Bool
animatesSelector = mkSelector "animates"

-- | @Selector@ for @setAnimates:@
setAnimatesSelector :: Selector '[Bool] ()
setAnimatesSelector = mkSelector "setAnimates:"

-- | @Selector@ for @allowsCutCopyPaste@
allowsCutCopyPasteSelector :: Selector '[] Bool
allowsCutCopyPasteSelector = mkSelector "allowsCutCopyPaste"

-- | @Selector@ for @setAllowsCutCopyPaste:@
setAllowsCutCopyPasteSelector :: Selector '[Bool] ()
setAllowsCutCopyPasteSelector = mkSelector "setAllowsCutCopyPaste:"

-- | @Selector@ for @defaultPreferredImageDynamicRange@
defaultPreferredImageDynamicRangeSelector :: Selector '[] NSImageDynamicRange
defaultPreferredImageDynamicRangeSelector = mkSelector "defaultPreferredImageDynamicRange"

-- | @Selector@ for @setDefaultPreferredImageDynamicRange:@
setDefaultPreferredImageDynamicRangeSelector :: Selector '[NSImageDynamicRange] ()
setDefaultPreferredImageDynamicRangeSelector = mkSelector "setDefaultPreferredImageDynamicRange:"

-- | @Selector@ for @preferredImageDynamicRange@
preferredImageDynamicRangeSelector :: Selector '[] NSImageDynamicRange
preferredImageDynamicRangeSelector = mkSelector "preferredImageDynamicRange"

-- | @Selector@ for @setPreferredImageDynamicRange:@
setPreferredImageDynamicRangeSelector :: Selector '[NSImageDynamicRange] ()
setPreferredImageDynamicRangeSelector = mkSelector "setPreferredImageDynamicRange:"

-- | @Selector@ for @imageDynamicRange@
imageDynamicRangeSelector :: Selector '[] NSImageDynamicRange
imageDynamicRangeSelector = mkSelector "imageDynamicRange"

