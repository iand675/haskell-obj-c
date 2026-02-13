{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSColor@.
module ObjC.AppKit.NSColor
  ( NSColor
  , IsNSColor(..)
  , init_
  , initWithCoder
  , colorWithColorSpace_components_count
  , colorWithSRGBRed_green_blue_alpha
  , colorWithGenericGamma22White_alpha
  , colorWithDisplayP3Red_green_blue_alpha
  , colorWithWhite_alpha
  , colorWithRed_green_blue_alpha
  , colorWithHue_saturation_brightness_alpha
  , colorWithColorSpace_hue_saturation_brightness_alpha
  , colorWithCatalogName_colorName
  , colorNamed_bundle
  , colorNamed
  , colorWithName_dynamicProvider
  , colorWithDeviceWhite_alpha
  , colorWithDeviceRed_green_blue_alpha
  , colorWithDeviceHue_saturation_brightness_alpha
  , colorWithDeviceCyan_magenta_yellow_black_alpha
  , colorWithCalibratedWhite_alpha
  , colorWithCalibratedRed_green_blue_alpha
  , colorWithCalibratedHue_saturation_brightness_alpha
  , colorWithPatternImage
  , colorUsingType
  , colorUsingColorSpace
  , colorWithRed_green_blue_alpha_exposure
  , colorWithRed_green_blue_alpha_linearExposure
  , colorByApplyingContentHeadroom
  , colorForControlTint
  , highlightWithLevel
  , shadowWithLevel
  , colorWithSystemEffect
  , set
  , setFill
  , setStroke
  , blendedColorWithFraction_ofColor
  , colorWithAlphaComponent
  , getRed_green_blue_alpha
  , getHue_saturation_brightness_alpha
  , getWhite_alpha
  , getCyan_magenta_yellow_black_alpha
  , getComponents
  , colorFromPasteboard
  , writeToPasteboard
  , drawSwatchInRect
  , colorWithCGColor
  , colorWithCIColor
  , colorUsingColorSpaceName_device
  , colorUsingColorSpaceName
  , type_
  , standardDynamicRangeColor
  , blackColor
  , darkGrayColor
  , lightGrayColor
  , whiteColor
  , grayColor
  , redColor
  , greenColor
  , blueColor
  , cyanColor
  , yellowColor
  , magentaColor
  , orangeColor
  , purpleColor
  , brownColor
  , clearColor
  , labelColor
  , secondaryLabelColor
  , tertiaryLabelColor
  , quaternaryLabelColor
  , quinaryLabelColor
  , linkColor
  , placeholderTextColor
  , windowFrameTextColor
  , selectedMenuItemTextColor
  , alternateSelectedControlTextColor
  , headerTextColor
  , separatorColor
  , gridColor
  , windowBackgroundColor
  , underPageBackgroundColor
  , controlBackgroundColor
  , selectedContentBackgroundColor
  , unemphasizedSelectedContentBackgroundColor
  , alternatingContentBackgroundColors
  , findHighlightColor
  , textColor
  , textBackgroundColor
  , textInsertionPointColor
  , selectedTextColor
  , selectedTextBackgroundColor
  , unemphasizedSelectedTextBackgroundColor
  , unemphasizedSelectedTextColor
  , controlColor
  , controlTextColor
  , selectedControlColor
  , selectedControlTextColor
  , disabledControlTextColor
  , keyboardFocusIndicatorColor
  , scrubberTexturedBackgroundColor
  , systemRedColor
  , systemGreenColor
  , systemBlueColor
  , systemOrangeColor
  , systemYellowColor
  , systemBrownColor
  , systemPinkColor
  , systemPurpleColor
  , systemGrayColor
  , systemTealColor
  , systemIndigoColor
  , systemMintColor
  , systemCyanColor
  , systemFillColor
  , secondarySystemFillColor
  , tertiarySystemFillColor
  , quaternarySystemFillColor
  , quinarySystemFillColor
  , controlAccentColor
  , currentControlTint
  , highlightColor
  , shadowColor
  , catalogNameComponent
  , colorNameComponent
  , localizedCatalogNameComponent
  , localizedColorNameComponent
  , redComponent
  , greenComponent
  , blueComponent
  , hueComponent
  , saturationComponent
  , brightnessComponent
  , whiteComponent
  , cyanComponent
  , magentaComponent
  , yellowComponent
  , blackComponent
  , colorSpace
  , numberOfComponents
  , patternImage
  , alphaComponent
  , linearExposure
  , cgColor
  , ignoresAlpha
  , setIgnoresAlpha
  , controlHighlightColor
  , controlLightHighlightColor
  , controlShadowColor
  , controlDarkShadowColor
  , scrollBarColor
  , knobColor
  , selectedKnobColor
  , windowFrameColor
  , selectedMenuItemColor
  , headerColor
  , secondarySelectedControlColor
  , alternateSelectedControlColor
  , controlAlternatingRowBackgroundColors
  , colorSpaceName
  , alphaComponentSelector
  , alternateSelectedControlColorSelector
  , alternateSelectedControlTextColorSelector
  , alternatingContentBackgroundColorsSelector
  , blackColorSelector
  , blackComponentSelector
  , blendedColorWithFraction_ofColorSelector
  , blueColorSelector
  , blueComponentSelector
  , brightnessComponentSelector
  , brownColorSelector
  , catalogNameComponentSelector
  , cgColorSelector
  , clearColorSelector
  , colorByApplyingContentHeadroomSelector
  , colorForControlTintSelector
  , colorFromPasteboardSelector
  , colorNameComponentSelector
  , colorNamedSelector
  , colorNamed_bundleSelector
  , colorSpaceNameSelector
  , colorSpaceSelector
  , colorUsingColorSpaceNameSelector
  , colorUsingColorSpaceName_deviceSelector
  , colorUsingColorSpaceSelector
  , colorUsingTypeSelector
  , colorWithAlphaComponentSelector
  , colorWithCGColorSelector
  , colorWithCIColorSelector
  , colorWithCalibratedHue_saturation_brightness_alphaSelector
  , colorWithCalibratedRed_green_blue_alphaSelector
  , colorWithCalibratedWhite_alphaSelector
  , colorWithCatalogName_colorNameSelector
  , colorWithColorSpace_components_countSelector
  , colorWithColorSpace_hue_saturation_brightness_alphaSelector
  , colorWithDeviceCyan_magenta_yellow_black_alphaSelector
  , colorWithDeviceHue_saturation_brightness_alphaSelector
  , colorWithDeviceRed_green_blue_alphaSelector
  , colorWithDeviceWhite_alphaSelector
  , colorWithDisplayP3Red_green_blue_alphaSelector
  , colorWithGenericGamma22White_alphaSelector
  , colorWithHue_saturation_brightness_alphaSelector
  , colorWithName_dynamicProviderSelector
  , colorWithPatternImageSelector
  , colorWithRed_green_blue_alphaSelector
  , colorWithRed_green_blue_alpha_exposureSelector
  , colorWithRed_green_blue_alpha_linearExposureSelector
  , colorWithSRGBRed_green_blue_alphaSelector
  , colorWithSystemEffectSelector
  , colorWithWhite_alphaSelector
  , controlAccentColorSelector
  , controlAlternatingRowBackgroundColorsSelector
  , controlBackgroundColorSelector
  , controlColorSelector
  , controlDarkShadowColorSelector
  , controlHighlightColorSelector
  , controlLightHighlightColorSelector
  , controlShadowColorSelector
  , controlTextColorSelector
  , currentControlTintSelector
  , cyanColorSelector
  , cyanComponentSelector
  , darkGrayColorSelector
  , disabledControlTextColorSelector
  , drawSwatchInRectSelector
  , findHighlightColorSelector
  , getComponentsSelector
  , getCyan_magenta_yellow_black_alphaSelector
  , getHue_saturation_brightness_alphaSelector
  , getRed_green_blue_alphaSelector
  , getWhite_alphaSelector
  , grayColorSelector
  , greenColorSelector
  , greenComponentSelector
  , gridColorSelector
  , headerColorSelector
  , headerTextColorSelector
  , highlightColorSelector
  , highlightWithLevelSelector
  , hueComponentSelector
  , ignoresAlphaSelector
  , initSelector
  , initWithCoderSelector
  , keyboardFocusIndicatorColorSelector
  , knobColorSelector
  , labelColorSelector
  , lightGrayColorSelector
  , linearExposureSelector
  , linkColorSelector
  , localizedCatalogNameComponentSelector
  , localizedColorNameComponentSelector
  , magentaColorSelector
  , magentaComponentSelector
  , numberOfComponentsSelector
  , orangeColorSelector
  , patternImageSelector
  , placeholderTextColorSelector
  , purpleColorSelector
  , quaternaryLabelColorSelector
  , quaternarySystemFillColorSelector
  , quinaryLabelColorSelector
  , quinarySystemFillColorSelector
  , redColorSelector
  , redComponentSelector
  , saturationComponentSelector
  , scrollBarColorSelector
  , scrubberTexturedBackgroundColorSelector
  , secondaryLabelColorSelector
  , secondarySelectedControlColorSelector
  , secondarySystemFillColorSelector
  , selectedContentBackgroundColorSelector
  , selectedControlColorSelector
  , selectedControlTextColorSelector
  , selectedKnobColorSelector
  , selectedMenuItemColorSelector
  , selectedMenuItemTextColorSelector
  , selectedTextBackgroundColorSelector
  , selectedTextColorSelector
  , separatorColorSelector
  , setFillSelector
  , setIgnoresAlphaSelector
  , setSelector
  , setStrokeSelector
  , shadowColorSelector
  , shadowWithLevelSelector
  , standardDynamicRangeColorSelector
  , systemBlueColorSelector
  , systemBrownColorSelector
  , systemCyanColorSelector
  , systemFillColorSelector
  , systemGrayColorSelector
  , systemGreenColorSelector
  , systemIndigoColorSelector
  , systemMintColorSelector
  , systemOrangeColorSelector
  , systemPinkColorSelector
  , systemPurpleColorSelector
  , systemRedColorSelector
  , systemTealColorSelector
  , systemYellowColorSelector
  , tertiaryLabelColorSelector
  , tertiarySystemFillColorSelector
  , textBackgroundColorSelector
  , textColorSelector
  , textInsertionPointColorSelector
  , typeSelector
  , underPageBackgroundColorSelector
  , unemphasizedSelectedContentBackgroundColorSelector
  , unemphasizedSelectedTextBackgroundColorSelector
  , unemphasizedSelectedTextColorSelector
  , whiteColorSelector
  , whiteComponentSelector
  , windowBackgroundColorSelector
  , windowFrameColorSelector
  , windowFrameTextColorSelector
  , writeToPasteboardSelector
  , yellowColorSelector
  , yellowComponentSelector

  -- * Enum types
  , NSColorSystemEffect(NSColorSystemEffect)
  , pattern NSColorSystemEffectNone
  , pattern NSColorSystemEffectPressed
  , pattern NSColorSystemEffectDeepPressed
  , pattern NSColorSystemEffectDisabled
  , pattern NSColorSystemEffectRollover
  , NSColorType(NSColorType)
  , pattern NSColorTypeComponentBased
  , pattern NSColorTypePattern
  , pattern NSColorTypeCatalog
  , NSControlTint(NSControlTint)
  , pattern NSDefaultControlTint
  , pattern NSBlueControlTint
  , pattern NSGraphiteControlTint
  , pattern NSClearControlTint

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
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSColor nsColor => nsColor -> IO (Id NSColor)
init_ nsColor =
  sendOwnedMessage nsColor initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSColor nsColor, IsNSCoder coder) => nsColor -> coder -> IO (Id NSColor)
initWithCoder nsColor coder =
  sendOwnedMessage nsColor initWithCoderSelector (toNSCoder coder)

-- | @+ colorWithColorSpace:components:count:@
colorWithColorSpace_components_count :: IsNSColorSpace space => space -> Const (Ptr CDouble) -> CLong -> IO (Id NSColor)
colorWithColorSpace_components_count space components numberOfComponents =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithColorSpace_components_countSelector (toNSColorSpace space) components numberOfComponents

-- | @+ colorWithSRGBRed:green:blue:alpha:@
colorWithSRGBRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithSRGBRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithSRGBRed_green_blue_alphaSelector red green blue alpha

-- | @+ colorWithGenericGamma22White:alpha:@
colorWithGenericGamma22White_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithGenericGamma22White_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithGenericGamma22White_alphaSelector white alpha

-- | @+ colorWithDisplayP3Red:green:blue:alpha:@
colorWithDisplayP3Red_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDisplayP3Red_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithDisplayP3Red_green_blue_alphaSelector red green blue alpha

-- | @+ colorWithWhite:alpha:@
colorWithWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithWhite_alphaSelector white alpha

-- | @+ colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithRed_green_blue_alphaSelector red green blue alpha

-- | @+ colorWithHue:saturation:brightness:alpha:@
colorWithHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithHue_saturation_brightness_alphaSelector hue saturation brightness alpha

-- | @+ colorWithColorSpace:hue:saturation:brightness:alpha:@
colorWithColorSpace_hue_saturation_brightness_alpha :: IsNSColorSpace space => space -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithColorSpace_hue_saturation_brightness_alpha space hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithColorSpace_hue_saturation_brightness_alphaSelector (toNSColorSpace space) hue saturation brightness alpha

-- | @+ colorWithCatalogName:colorName:@
colorWithCatalogName_colorName :: (IsNSString listName, IsNSString colorName) => listName -> colorName -> IO (Id NSColor)
colorWithCatalogName_colorName listName colorName =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithCatalogName_colorNameSelector (toNSString listName) (toNSString colorName)

-- | @+ colorNamed:bundle:@
colorNamed_bundle :: (IsNSString name, IsNSBundle bundle) => name -> bundle -> IO (Id NSColor)
colorNamed_bundle name bundle =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorNamed_bundleSelector (toNSString name) (toNSBundle bundle)

-- | @+ colorNamed:@
colorNamed :: IsNSString name => name -> IO (Id NSColor)
colorNamed name =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorNamedSelector (toNSString name)

-- | @+ colorWithName:dynamicProvider:@
colorWithName_dynamicProvider :: IsNSString colorName => colorName -> Ptr () -> IO (Id NSColor)
colorWithName_dynamicProvider colorName dynamicProvider =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithName_dynamicProviderSelector (toNSString colorName) dynamicProvider

-- | @+ colorWithDeviceWhite:alpha:@
colorWithDeviceWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithDeviceWhite_alphaSelector white alpha

-- | @+ colorWithDeviceRed:green:blue:alpha:@
colorWithDeviceRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithDeviceRed_green_blue_alphaSelector red green blue alpha

-- | @+ colorWithDeviceHue:saturation:brightness:alpha:@
colorWithDeviceHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithDeviceHue_saturation_brightness_alphaSelector hue saturation brightness alpha

-- | @+ colorWithDeviceCyan:magenta:yellow:black:alpha:@
colorWithDeviceCyan_magenta_yellow_black_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceCyan_magenta_yellow_black_alpha cyan magenta yellow black alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithDeviceCyan_magenta_yellow_black_alphaSelector cyan magenta yellow black alpha

-- | @+ colorWithCalibratedWhite:alpha:@
colorWithCalibratedWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithCalibratedWhite_alphaSelector white alpha

-- | @+ colorWithCalibratedRed:green:blue:alpha:@
colorWithCalibratedRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithCalibratedRed_green_blue_alphaSelector red green blue alpha

-- | @+ colorWithCalibratedHue:saturation:brightness:alpha:@
colorWithCalibratedHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithCalibratedHue_saturation_brightness_alphaSelector hue saturation brightness alpha

-- | @+ colorWithPatternImage:@
colorWithPatternImage :: IsNSImage image => image -> IO (Id NSColor)
colorWithPatternImage image =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithPatternImageSelector (toNSImage image)

-- | @- colorUsingType:@
colorUsingType :: IsNSColor nsColor => nsColor -> NSColorType -> IO (Id NSColor)
colorUsingType nsColor type_ =
  sendMessage nsColor colorUsingTypeSelector type_

-- | @- colorUsingColorSpace:@
colorUsingColorSpace :: (IsNSColor nsColor, IsNSColorSpace space) => nsColor -> space -> IO (Id NSColor)
colorUsingColorSpace nsColor space =
  sendMessage nsColor colorUsingColorSpaceSelector (toNSColorSpace space)

-- | Generates an HDR color in the extended sRGB colorspace by applying an exposure to the SDR color defined by the red, green, and blue components. The @red@, @green@, and @blue@ components have a nominal range of [0..1], @exposure@ is a value >= 0. To produce an HDR color, we process the given color in a linear color space, multiplying component values by @2^exposure@. The produced color will have a @contentHeadroom@ equal to the linearized exposure value. Each whole value of exposure produces a color that is twice as bright.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:exposure:@
colorWithRed_green_blue_alpha_exposure :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha_exposure red green blue alpha exposure =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithRed_green_blue_alpha_exposureSelector red green blue alpha exposure

-- | Generates an HDR color in the extended sRGB colorspace by applying an exposure to the SDR color defined by the red, green, and blue components. The @red@, @green@, and @blue@ components have a nominal range of [0..1], @linearExposure@ is a value >= 1. To produce an HDR color, we process the given color in a linear color space, multiplying component values by @linearExposure @. The produced color will have a @contentHeadroom@ equal to @linearExposure@. Each doubling of @linearExposure@ produces a color that is twice as bright.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:linearExposure:@
colorWithRed_green_blue_alpha_linearExposure :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha_linearExposure red green blue alpha linearExposure =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithRed_green_blue_alpha_linearExposureSelector red green blue alpha linearExposure

-- | Reinterpret the color by applying a new @contentHeadroom@ without changing the color components. Changing the @contentHeadroom@ redefines the color relative to a different peak white, changing its behavior under tone mapping and the result of calling @standardDynamicRangeColor@. The new color will have a @contentHeadroom@ >= 1.0. If called on a color with a color space that does not support extended range, or does not have an equivalent extended range counterpart, this will return @self@.
--
-- ObjC selector: @- colorByApplyingContentHeadroom:@
colorByApplyingContentHeadroom :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
colorByApplyingContentHeadroom nsColor contentHeadroom =
  sendMessage nsColor colorByApplyingContentHeadroomSelector contentHeadroom

-- | @+ colorForControlTint:@
colorForControlTint :: NSControlTint -> IO (Id NSColor)
colorForControlTint controlTint =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorForControlTintSelector controlTint

-- | @- highlightWithLevel:@
highlightWithLevel :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
highlightWithLevel nsColor val =
  sendMessage nsColor highlightWithLevelSelector val

-- | @- shadowWithLevel:@
shadowWithLevel :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
shadowWithLevel nsColor val =
  sendMessage nsColor shadowWithLevelSelector val

-- | Returns a color representing the base color with a system defined effect applied to it. This color is safe to create before draw time, as the resolution of the final color only happens when being @-set@, retrieving its @CGColor@, resolving with @-colorWithType:@, etc. The return color type is @.named@.
--
-- ObjC selector: @- colorWithSystemEffect:@
colorWithSystemEffect :: IsNSColor nsColor => nsColor -> NSColorSystemEffect -> IO (Id NSColor)
colorWithSystemEffect nsColor systemEffect =
  sendMessage nsColor colorWithSystemEffectSelector systemEffect

-- | @- set@
set :: IsNSColor nsColor => nsColor -> IO ()
set nsColor =
  sendMessage nsColor setSelector

-- | @- setFill@
setFill :: IsNSColor nsColor => nsColor -> IO ()
setFill nsColor =
  sendMessage nsColor setFillSelector

-- | @- setStroke@
setStroke :: IsNSColor nsColor => nsColor -> IO ()
setStroke nsColor =
  sendMessage nsColor setStrokeSelector

-- | @- blendedColorWithFraction:ofColor:@
blendedColorWithFraction_ofColor :: (IsNSColor nsColor, IsNSColor color) => nsColor -> CDouble -> color -> IO (Id NSColor)
blendedColorWithFraction_ofColor nsColor fraction color =
  sendMessage nsColor blendedColorWithFraction_ofColorSelector fraction (toNSColor color)

-- | @- colorWithAlphaComponent:@
colorWithAlphaComponent :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
colorWithAlphaComponent nsColor alpha =
  sendMessage nsColor colorWithAlphaComponentSelector alpha

-- | @- getRed:green:blue:alpha:@
getRed_green_blue_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
getRed_green_blue_alpha nsColor red green blue alpha =
  sendMessage nsColor getRed_green_blue_alphaSelector red green blue alpha

-- | @- getHue:saturation:brightness:alpha:@
getHue_saturation_brightness_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
getHue_saturation_brightness_alpha nsColor hue saturation brightness alpha =
  sendMessage nsColor getHue_saturation_brightness_alphaSelector hue saturation brightness alpha

-- | @- getWhite:alpha:@
getWhite_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> IO ()
getWhite_alpha nsColor white alpha =
  sendMessage nsColor getWhite_alphaSelector white alpha

-- | @- getCyan:magenta:yellow:black:alpha:@
getCyan_magenta_yellow_black_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
getCyan_magenta_yellow_black_alpha nsColor cyan magenta yellow black alpha =
  sendMessage nsColor getCyan_magenta_yellow_black_alphaSelector cyan magenta yellow black alpha

-- | @- getComponents:@
getComponents :: IsNSColor nsColor => nsColor -> Ptr CDouble -> IO ()
getComponents nsColor components =
  sendMessage nsColor getComponentsSelector components

-- | @+ colorFromPasteboard:@
colorFromPasteboard :: IsNSPasteboard pasteBoard => pasteBoard -> IO (Id NSColor)
colorFromPasteboard pasteBoard =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorFromPasteboardSelector (toNSPasteboard pasteBoard)

-- | @- writeToPasteboard:@
writeToPasteboard :: (IsNSColor nsColor, IsNSPasteboard pasteBoard) => nsColor -> pasteBoard -> IO ()
writeToPasteboard nsColor pasteBoard =
  sendMessage nsColor writeToPasteboardSelector (toNSPasteboard pasteBoard)

-- | @- drawSwatchInRect:@
drawSwatchInRect :: IsNSColor nsColor => nsColor -> NSRect -> IO ()
drawSwatchInRect nsColor rect =
  sendMessage nsColor drawSwatchInRectSelector rect

-- | @+ colorWithCGColor:@
colorWithCGColor :: Ptr () -> IO (Id NSColor)
colorWithCGColor cgColor =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithCGColorSelector cgColor

-- | @+ colorWithCIColor:@
colorWithCIColor :: IsCIColor color => color -> IO (Id NSColor)
colorWithCIColor color =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' colorWithCIColorSelector (toCIColor color)

-- | @- colorUsingColorSpaceName:device:@
colorUsingColorSpaceName_device :: (IsNSColor nsColor, IsNSString name, IsNSDictionary deviceDescription) => nsColor -> name -> deviceDescription -> IO (Id NSColor)
colorUsingColorSpaceName_device nsColor name deviceDescription =
  sendMessage nsColor colorUsingColorSpaceName_deviceSelector (toNSString name) (toNSDictionary deviceDescription)

-- | @- colorUsingColorSpaceName:@
colorUsingColorSpaceName :: (IsNSColor nsColor, IsNSString name) => nsColor -> name -> IO (Id NSColor)
colorUsingColorSpaceName nsColor name =
  sendMessage nsColor colorUsingColorSpaceNameSelector (toNSString name)

-- | @- type@
type_ :: IsNSColor nsColor => nsColor -> IO NSColorType
type_ nsColor =
  sendMessage nsColor typeSelector

-- | In some cases it is useful to recover the color that was base the SDR color that was exposed to generate an HDR color. If a color's @linearExposure@ is > 1, then this will return the base SDR color. If the color is not an HDR color, this will return @self@.
--
-- ObjC selector: @- standardDynamicRangeColor@
standardDynamicRangeColor :: IsNSColor nsColor => nsColor -> IO (Id NSColor)
standardDynamicRangeColor nsColor =
  sendMessage nsColor standardDynamicRangeColorSelector

-- | @+ blackColor@
blackColor :: IO (Id NSColor)
blackColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' blackColorSelector

-- | @+ darkGrayColor@
darkGrayColor :: IO (Id NSColor)
darkGrayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' darkGrayColorSelector

-- | @+ lightGrayColor@
lightGrayColor :: IO (Id NSColor)
lightGrayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' lightGrayColorSelector

-- | @+ whiteColor@
whiteColor :: IO (Id NSColor)
whiteColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' whiteColorSelector

-- | @+ grayColor@
grayColor :: IO (Id NSColor)
grayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' grayColorSelector

-- | @+ redColor@
redColor :: IO (Id NSColor)
redColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' redColorSelector

-- | @+ greenColor@
greenColor :: IO (Id NSColor)
greenColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' greenColorSelector

-- | @+ blueColor@
blueColor :: IO (Id NSColor)
blueColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' blueColorSelector

-- | @+ cyanColor@
cyanColor :: IO (Id NSColor)
cyanColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' cyanColorSelector

-- | @+ yellowColor@
yellowColor :: IO (Id NSColor)
yellowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' yellowColorSelector

-- | @+ magentaColor@
magentaColor :: IO (Id NSColor)
magentaColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' magentaColorSelector

-- | @+ orangeColor@
orangeColor :: IO (Id NSColor)
orangeColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' orangeColorSelector

-- | @+ purpleColor@
purpleColor :: IO (Id NSColor)
purpleColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' purpleColorSelector

-- | @+ brownColor@
brownColor :: IO (Id NSColor)
brownColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' brownColorSelector

-- | @+ clearColor@
clearColor :: IO (Id NSColor)
clearColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' clearColorSelector

-- | @+ labelColor@
labelColor :: IO (Id NSColor)
labelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' labelColorSelector

-- | @+ secondaryLabelColor@
secondaryLabelColor :: IO (Id NSColor)
secondaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' secondaryLabelColorSelector

-- | @+ tertiaryLabelColor@
tertiaryLabelColor :: IO (Id NSColor)
tertiaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' tertiaryLabelColorSelector

-- | @+ quaternaryLabelColor@
quaternaryLabelColor :: IO (Id NSColor)
quaternaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' quaternaryLabelColorSelector

-- | @+ quinaryLabelColor@
quinaryLabelColor :: IO (Id NSColor)
quinaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' quinaryLabelColorSelector

-- | Used for large scale images or subtle decorative elements; not for general foreground content.
--
-- ObjC selector: @+ linkColor@
linkColor :: IO (Id NSColor)
linkColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' linkColorSelector

-- | @+ placeholderTextColor@
placeholderTextColor :: IO (Id NSColor)
placeholderTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' placeholderTextColorSelector

-- | @+ windowFrameTextColor@
windowFrameTextColor :: IO (Id NSColor)
windowFrameTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' windowFrameTextColorSelector

-- | @+ selectedMenuItemTextColor@
selectedMenuItemTextColor :: IO (Id NSColor)
selectedMenuItemTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedMenuItemTextColorSelector

-- | @+ alternateSelectedControlTextColor@
alternateSelectedControlTextColor :: IO (Id NSColor)
alternateSelectedControlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' alternateSelectedControlTextColorSelector

-- | @+ headerTextColor@
headerTextColor :: IO (Id NSColor)
headerTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' headerTextColorSelector

-- | @+ separatorColor@
separatorColor :: IO (Id NSColor)
separatorColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' separatorColorSelector

-- | @+ gridColor@
gridColor :: IO (Id NSColor)
gridColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' gridColorSelector

-- | @+ windowBackgroundColor@
windowBackgroundColor :: IO (Id NSColor)
windowBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' windowBackgroundColorSelector

-- | @+ underPageBackgroundColor@
underPageBackgroundColor :: IO (Id NSColor)
underPageBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' underPageBackgroundColorSelector

-- | @+ controlBackgroundColor@
controlBackgroundColor :: IO (Id NSColor)
controlBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlBackgroundColorSelector

-- | @+ selectedContentBackgroundColor@
selectedContentBackgroundColor :: IO (Id NSColor)
selectedContentBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedContentBackgroundColorSelector

-- | @+ unemphasizedSelectedContentBackgroundColor@
unemphasizedSelectedContentBackgroundColor :: IO (Id NSColor)
unemphasizedSelectedContentBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' unemphasizedSelectedContentBackgroundColorSelector

-- | @+ alternatingContentBackgroundColors@
alternatingContentBackgroundColors :: IO (Id NSArray)
alternatingContentBackgroundColors  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' alternatingContentBackgroundColorsSelector

-- | @+ findHighlightColor@
findHighlightColor :: IO (Id NSColor)
findHighlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' findHighlightColorSelector

-- | @+ textColor@
textColor :: IO (Id NSColor)
textColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' textColorSelector

-- | @+ textBackgroundColor@
textBackgroundColor :: IO (Id NSColor)
textBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' textBackgroundColorSelector

-- | @+ textInsertionPointColor@
textInsertionPointColor :: IO (Id NSColor)
textInsertionPointColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' textInsertionPointColorSelector

-- | @+ selectedTextColor@
selectedTextColor :: IO (Id NSColor)
selectedTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedTextColorSelector

-- | @+ selectedTextBackgroundColor@
selectedTextBackgroundColor :: IO (Id NSColor)
selectedTextBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedTextBackgroundColorSelector

-- | @+ unemphasizedSelectedTextBackgroundColor@
unemphasizedSelectedTextBackgroundColor :: IO (Id NSColor)
unemphasizedSelectedTextBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' unemphasizedSelectedTextBackgroundColorSelector

-- | @+ unemphasizedSelectedTextColor@
unemphasizedSelectedTextColor :: IO (Id NSColor)
unemphasizedSelectedTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' unemphasizedSelectedTextColorSelector

-- | @+ controlColor@
controlColor :: IO (Id NSColor)
controlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlColorSelector

-- | @+ controlTextColor@
controlTextColor :: IO (Id NSColor)
controlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlTextColorSelector

-- | @+ selectedControlColor@
selectedControlColor :: IO (Id NSColor)
selectedControlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedControlColorSelector

-- | @+ selectedControlTextColor@
selectedControlTextColor :: IO (Id NSColor)
selectedControlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedControlTextColorSelector

-- | @+ disabledControlTextColor@
disabledControlTextColor :: IO (Id NSColor)
disabledControlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' disabledControlTextColorSelector

-- | @+ keyboardFocusIndicatorColor@
keyboardFocusIndicatorColor :: IO (Id NSColor)
keyboardFocusIndicatorColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' keyboardFocusIndicatorColorSelector

-- | @+ scrubberTexturedBackgroundColor@
scrubberTexturedBackgroundColor :: IO (Id NSColor)
scrubberTexturedBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' scrubberTexturedBackgroundColorSelector

-- | @+ systemRedColor@
systemRedColor :: IO (Id NSColor)
systemRedColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemRedColorSelector

-- | @+ systemGreenColor@
systemGreenColor :: IO (Id NSColor)
systemGreenColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemGreenColorSelector

-- | @+ systemBlueColor@
systemBlueColor :: IO (Id NSColor)
systemBlueColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemBlueColorSelector

-- | @+ systemOrangeColor@
systemOrangeColor :: IO (Id NSColor)
systemOrangeColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemOrangeColorSelector

-- | @+ systemYellowColor@
systemYellowColor :: IO (Id NSColor)
systemYellowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemYellowColorSelector

-- | @+ systemBrownColor@
systemBrownColor :: IO (Id NSColor)
systemBrownColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemBrownColorSelector

-- | @+ systemPinkColor@
systemPinkColor :: IO (Id NSColor)
systemPinkColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemPinkColorSelector

-- | @+ systemPurpleColor@
systemPurpleColor :: IO (Id NSColor)
systemPurpleColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemPurpleColorSelector

-- | @+ systemGrayColor@
systemGrayColor :: IO (Id NSColor)
systemGrayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemGrayColorSelector

-- | @+ systemTealColor@
systemTealColor :: IO (Id NSColor)
systemTealColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemTealColorSelector

-- | @+ systemIndigoColor@
systemIndigoColor :: IO (Id NSColor)
systemIndigoColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemIndigoColorSelector

-- | @+ systemMintColor@
systemMintColor :: IO (Id NSColor)
systemMintColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemMintColorSelector

-- | @+ systemCyanColor@
systemCyanColor :: IO (Id NSColor)
systemCyanColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemCyanColorSelector

-- | Fill colors for UI elements. These are meant to be used over the background colors, since their alpha component is less than 1.
--
-- systemFillColor is appropriate for filling thin shapes, such as the track of a slider.
--
-- ObjC selector: @+ systemFillColor@
systemFillColor :: IO (Id NSColor)
systemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' systemFillColorSelector

-- | secondarySystemFillColor is appropriate for filling small-size shapes, such as the backing of a progress indicator.
--
-- ObjC selector: @+ secondarySystemFillColor@
secondarySystemFillColor :: IO (Id NSColor)
secondarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' secondarySystemFillColorSelector

-- | tertiarySystemFillColor is appropriate for filling medium-size shapes,  such as the backing of a switch.
--
-- ObjC selector: @+ tertiarySystemFillColor@
tertiarySystemFillColor :: IO (Id NSColor)
tertiarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' tertiarySystemFillColorSelector

-- | quaternarySystemFillColor is appropriate for filling large areas, such as a group box or tab pane.
--
-- ObjC selector: @+ quaternarySystemFillColor@
quaternarySystemFillColor :: IO (Id NSColor)
quaternarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' quaternarySystemFillColorSelector

-- | quinarySystemFillColor is appropriate for filling large areas that require subtle emphasis, such as content of a form..
--
-- ObjC selector: @+ quinarySystemFillColor@
quinarySystemFillColor :: IO (Id NSColor)
quinarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' quinarySystemFillColorSelector

-- | A dynamic color that reflects the user's current preferred accent color. This color automatically updates when the accent color preference changes. Do not make assumptions about the color space of this color, which may change across releases.
--
-- ObjC selector: @+ controlAccentColor@
controlAccentColor :: IO (Id NSColor)
controlAccentColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlAccentColorSelector

-- | @+ currentControlTint@
currentControlTint :: IO NSControlTint
currentControlTint  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' currentControlTintSelector

-- | @+ highlightColor@
highlightColor :: IO (Id NSColor)
highlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' highlightColorSelector

-- | @+ shadowColor@
shadowColor :: IO (Id NSColor)
shadowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' shadowColorSelector

-- | * Methods to get various components of colors. Not all of the methods apply to all colors; if called, they raise. **
--
-- ObjC selector: @- catalogNameComponent@
catalogNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
catalogNameComponent nsColor =
  sendMessage nsColor catalogNameComponentSelector

-- | @- colorNameComponent@
colorNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
colorNameComponent nsColor =
  sendMessage nsColor colorNameComponentSelector

-- | @- localizedCatalogNameComponent@
localizedCatalogNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
localizedCatalogNameComponent nsColor =
  sendMessage nsColor localizedCatalogNameComponentSelector

-- | @- localizedColorNameComponent@
localizedColorNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
localizedColorNameComponent nsColor =
  sendMessage nsColor localizedColorNameComponentSelector

-- | @- redComponent@
redComponent :: IsNSColor nsColor => nsColor -> IO CDouble
redComponent nsColor =
  sendMessage nsColor redComponentSelector

-- | @- greenComponent@
greenComponent :: IsNSColor nsColor => nsColor -> IO CDouble
greenComponent nsColor =
  sendMessage nsColor greenComponentSelector

-- | @- blueComponent@
blueComponent :: IsNSColor nsColor => nsColor -> IO CDouble
blueComponent nsColor =
  sendMessage nsColor blueComponentSelector

-- | @- hueComponent@
hueComponent :: IsNSColor nsColor => nsColor -> IO CDouble
hueComponent nsColor =
  sendMessage nsColor hueComponentSelector

-- | @- saturationComponent@
saturationComponent :: IsNSColor nsColor => nsColor -> IO CDouble
saturationComponent nsColor =
  sendMessage nsColor saturationComponentSelector

-- | @- brightnessComponent@
brightnessComponent :: IsNSColor nsColor => nsColor -> IO CDouble
brightnessComponent nsColor =
  sendMessage nsColor brightnessComponentSelector

-- | @- whiteComponent@
whiteComponent :: IsNSColor nsColor => nsColor -> IO CDouble
whiteComponent nsColor =
  sendMessage nsColor whiteComponentSelector

-- | @- cyanComponent@
cyanComponent :: IsNSColor nsColor => nsColor -> IO CDouble
cyanComponent nsColor =
  sendMessage nsColor cyanComponentSelector

-- | @- magentaComponent@
magentaComponent :: IsNSColor nsColor => nsColor -> IO CDouble
magentaComponent nsColor =
  sendMessage nsColor magentaComponentSelector

-- | @- yellowComponent@
yellowComponent :: IsNSColor nsColor => nsColor -> IO CDouble
yellowComponent nsColor =
  sendMessage nsColor yellowComponentSelector

-- | @- blackComponent@
blackComponent :: IsNSColor nsColor => nsColor -> IO CDouble
blackComponent nsColor =
  sendMessage nsColor blackComponentSelector

-- | @- colorSpace@
colorSpace :: IsNSColor nsColor => nsColor -> IO (Id NSColorSpace)
colorSpace nsColor =
  sendMessage nsColor colorSpaceSelector

-- | @- numberOfComponents@
numberOfComponents :: IsNSColor nsColor => nsColor -> IO CLong
numberOfComponents nsColor =
  sendMessage nsColor numberOfComponentsSelector

-- | @- patternImage@
patternImage :: IsNSColor nsColor => nsColor -> IO (Id NSImage)
patternImage nsColor =
  sendMessage nsColor patternImageSelector

-- | @- alphaComponent@
alphaComponent :: IsNSColor nsColor => nsColor -> IO CDouble
alphaComponent nsColor =
  sendMessage nsColor alphaComponentSelector

-- | For HDR colors, the linear brightness multiplier that was applied when generating the color. Colors created with an exposure by NSColor create CGColors that are tagged with a contentHeadroom value. While CGColors created without a contentHeadroom tag will return 0 from CGColorGetHeadroom, NSColors generated in a similar fashion return a linearExposure of 1.0.
--
-- ObjC selector: @- linearExposure@
linearExposure :: IsNSColor nsColor => nsColor -> IO CDouble
linearExposure nsColor =
  sendMessage nsColor linearExposureSelector

-- | @- CGColor@
cgColor :: IsNSColor nsColor => nsColor -> IO (Ptr ())
cgColor nsColor =
  sendMessage nsColor cgColorSelector

-- | @+ ignoresAlpha@
ignoresAlpha :: IO Bool
ignoresAlpha  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' ignoresAlphaSelector

-- | @+ setIgnoresAlpha:@
setIgnoresAlpha :: Bool -> IO ()
setIgnoresAlpha value =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' setIgnoresAlphaSelector value

-- | Historically used as the inner border highlight color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlHighlightColor@
controlHighlightColor :: IO (Id NSColor)
controlHighlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlHighlightColorSelector

-- | Historically used as the outer border highlight color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlLightHighlightColor@
controlLightHighlightColor :: IO (Id NSColor)
controlLightHighlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlLightHighlightColorSelector

-- | Historically used as the inner border shadow color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlShadowColor@
controlShadowColor :: IO (Id NSColor)
controlShadowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlShadowColorSelector

-- | Historically used as the outer border shadow color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlDarkShadowColor@
controlDarkShadowColor :: IO (Id NSColor)
controlDarkShadowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlDarkShadowColorSelector

-- | Historically used as the color of scroll bars. No longer used.
--
-- ObjC selector: @+ scrollBarColor@
scrollBarColor :: IO (Id NSColor)
scrollBarColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' scrollBarColorSelector

-- | Historically used as the color of scroll bar knobs. No longer used.
--
-- ObjC selector: @+ knobColor@
knobColor :: IO (Id NSColor)
knobColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' knobColorSelector

-- | Historically used as the color of scroll bar knobs being dragged. No longer used.
--
-- ObjC selector: @+ selectedKnobColor@
selectedKnobColor :: IO (Id NSColor)
selectedKnobColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedKnobColorSelector

-- | Historically used as the color of the window chrome, which is no longer able to be represented by a color. No longer used.
--
-- ObjC selector: @+ windowFrameColor@
windowFrameColor :: IO (Id NSColor)
windowFrameColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' windowFrameColorSelector

-- | Historically used as the color of selected menu items, which is no longer a color but a tinted blur effect. No longer used.
--
-- ObjC selector: @+ selectedMenuItemColor@
selectedMenuItemColor :: IO (Id NSColor)
selectedMenuItemColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' selectedMenuItemColorSelector

-- | Historically used as the color of table headers, which is no longer a color but a tinted blur effect.
--
-- ObjC selector: @+ headerColor@
headerColor :: IO (Id NSColor)
headerColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' headerColorSelector

-- | The background color of selected content or text that is unemphasized. Older alias for +unemphasizedSelectedContentBackgroundColor and +unemphasizedSelectedTextBackgroundColor
--
-- ObjC selector: @+ secondarySelectedControlColor@
secondarySelectedControlColor :: IO (Id NSColor)
secondarySelectedControlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' secondarySelectedControlColorSelector

-- | The background color of selected and emphasized (focused) content: table views rows, collection views, etc. Older alias for +selectedContentBackgroundColor
--
-- ObjC selector: @+ alternateSelectedControlColor@
alternateSelectedControlColor :: IO (Id NSColor)
alternateSelectedControlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' alternateSelectedControlColorSelector

-- | The background colors for alternating content items: such as table view rows, collection view items. Older alias for +alternatingContentBackgroundColors
--
-- ObjC selector: @+ controlAlternatingRowBackgroundColors@
controlAlternatingRowBackgroundColors :: IO (Id NSArray)
controlAlternatingRowBackgroundColors  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMessage cls' controlAlternatingRowBackgroundColorsSelector

-- | @- colorSpaceName@
colorSpaceName :: IsNSColor nsColor => nsColor -> IO (Id NSString)
colorSpaceName nsColor =
  sendMessage nsColor colorSpaceNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSColor)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSColor)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @colorWithColorSpace:components:count:@
colorWithColorSpace_components_countSelector :: Selector '[Id NSColorSpace, Const (Ptr CDouble), CLong] (Id NSColor)
colorWithColorSpace_components_countSelector = mkSelector "colorWithColorSpace:components:count:"

-- | @Selector@ for @colorWithSRGBRed:green:blue:alpha:@
colorWithSRGBRed_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithSRGBRed_green_blue_alphaSelector = mkSelector "colorWithSRGBRed:green:blue:alpha:"

-- | @Selector@ for @colorWithGenericGamma22White:alpha:@
colorWithGenericGamma22White_alphaSelector :: Selector '[CDouble, CDouble] (Id NSColor)
colorWithGenericGamma22White_alphaSelector = mkSelector "colorWithGenericGamma22White:alpha:"

-- | @Selector@ for @colorWithDisplayP3Red:green:blue:alpha:@
colorWithDisplayP3Red_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithDisplayP3Red_green_blue_alphaSelector = mkSelector "colorWithDisplayP3Red:green:blue:alpha:"

-- | @Selector@ for @colorWithWhite:alpha:@
colorWithWhite_alphaSelector :: Selector '[CDouble, CDouble] (Id NSColor)
colorWithWhite_alphaSelector = mkSelector "colorWithWhite:alpha:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithRed_green_blue_alphaSelector = mkSelector "colorWithRed:green:blue:alpha:"

-- | @Selector@ for @colorWithHue:saturation:brightness:alpha:@
colorWithHue_saturation_brightness_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithHue_saturation_brightness_alphaSelector = mkSelector "colorWithHue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithColorSpace:hue:saturation:brightness:alpha:@
colorWithColorSpace_hue_saturation_brightness_alphaSelector :: Selector '[Id NSColorSpace, CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithColorSpace_hue_saturation_brightness_alphaSelector = mkSelector "colorWithColorSpace:hue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithCatalogName:colorName:@
colorWithCatalogName_colorNameSelector :: Selector '[Id NSString, Id NSString] (Id NSColor)
colorWithCatalogName_colorNameSelector = mkSelector "colorWithCatalogName:colorName:"

-- | @Selector@ for @colorNamed:bundle:@
colorNamed_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id NSColor)
colorNamed_bundleSelector = mkSelector "colorNamed:bundle:"

-- | @Selector@ for @colorNamed:@
colorNamedSelector :: Selector '[Id NSString] (Id NSColor)
colorNamedSelector = mkSelector "colorNamed:"

-- | @Selector@ for @colorWithName:dynamicProvider:@
colorWithName_dynamicProviderSelector :: Selector '[Id NSString, Ptr ()] (Id NSColor)
colorWithName_dynamicProviderSelector = mkSelector "colorWithName:dynamicProvider:"

-- | @Selector@ for @colorWithDeviceWhite:alpha:@
colorWithDeviceWhite_alphaSelector :: Selector '[CDouble, CDouble] (Id NSColor)
colorWithDeviceWhite_alphaSelector = mkSelector "colorWithDeviceWhite:alpha:"

-- | @Selector@ for @colorWithDeviceRed:green:blue:alpha:@
colorWithDeviceRed_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithDeviceRed_green_blue_alphaSelector = mkSelector "colorWithDeviceRed:green:blue:alpha:"

-- | @Selector@ for @colorWithDeviceHue:saturation:brightness:alpha:@
colorWithDeviceHue_saturation_brightness_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithDeviceHue_saturation_brightness_alphaSelector = mkSelector "colorWithDeviceHue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithDeviceCyan:magenta:yellow:black:alpha:@
colorWithDeviceCyan_magenta_yellow_black_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithDeviceCyan_magenta_yellow_black_alphaSelector = mkSelector "colorWithDeviceCyan:magenta:yellow:black:alpha:"

-- | @Selector@ for @colorWithCalibratedWhite:alpha:@
colorWithCalibratedWhite_alphaSelector :: Selector '[CDouble, CDouble] (Id NSColor)
colorWithCalibratedWhite_alphaSelector = mkSelector "colorWithCalibratedWhite:alpha:"

-- | @Selector@ for @colorWithCalibratedRed:green:blue:alpha:@
colorWithCalibratedRed_green_blue_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithCalibratedRed_green_blue_alphaSelector = mkSelector "colorWithCalibratedRed:green:blue:alpha:"

-- | @Selector@ for @colorWithCalibratedHue:saturation:brightness:alpha:@
colorWithCalibratedHue_saturation_brightness_alphaSelector :: Selector '[CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithCalibratedHue_saturation_brightness_alphaSelector = mkSelector "colorWithCalibratedHue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithPatternImage:@
colorWithPatternImageSelector :: Selector '[Id NSImage] (Id NSColor)
colorWithPatternImageSelector = mkSelector "colorWithPatternImage:"

-- | @Selector@ for @colorUsingType:@
colorUsingTypeSelector :: Selector '[NSColorType] (Id NSColor)
colorUsingTypeSelector = mkSelector "colorUsingType:"

-- | @Selector@ for @colorUsingColorSpace:@
colorUsingColorSpaceSelector :: Selector '[Id NSColorSpace] (Id NSColor)
colorUsingColorSpaceSelector = mkSelector "colorUsingColorSpace:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:exposure:@
colorWithRed_green_blue_alpha_exposureSelector :: Selector '[CDouble, CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithRed_green_blue_alpha_exposureSelector = mkSelector "colorWithRed:green:blue:alpha:exposure:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:linearExposure:@
colorWithRed_green_blue_alpha_linearExposureSelector :: Selector '[CDouble, CDouble, CDouble, CDouble, CDouble] (Id NSColor)
colorWithRed_green_blue_alpha_linearExposureSelector = mkSelector "colorWithRed:green:blue:alpha:linearExposure:"

-- | @Selector@ for @colorByApplyingContentHeadroom:@
colorByApplyingContentHeadroomSelector :: Selector '[CDouble] (Id NSColor)
colorByApplyingContentHeadroomSelector = mkSelector "colorByApplyingContentHeadroom:"

-- | @Selector@ for @colorForControlTint:@
colorForControlTintSelector :: Selector '[NSControlTint] (Id NSColor)
colorForControlTintSelector = mkSelector "colorForControlTint:"

-- | @Selector@ for @highlightWithLevel:@
highlightWithLevelSelector :: Selector '[CDouble] (Id NSColor)
highlightWithLevelSelector = mkSelector "highlightWithLevel:"

-- | @Selector@ for @shadowWithLevel:@
shadowWithLevelSelector :: Selector '[CDouble] (Id NSColor)
shadowWithLevelSelector = mkSelector "shadowWithLevel:"

-- | @Selector@ for @colorWithSystemEffect:@
colorWithSystemEffectSelector :: Selector '[NSColorSystemEffect] (Id NSColor)
colorWithSystemEffectSelector = mkSelector "colorWithSystemEffect:"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @setFill@
setFillSelector :: Selector '[] ()
setFillSelector = mkSelector "setFill"

-- | @Selector@ for @setStroke@
setStrokeSelector :: Selector '[] ()
setStrokeSelector = mkSelector "setStroke"

-- | @Selector@ for @blendedColorWithFraction:ofColor:@
blendedColorWithFraction_ofColorSelector :: Selector '[CDouble, Id NSColor] (Id NSColor)
blendedColorWithFraction_ofColorSelector = mkSelector "blendedColorWithFraction:ofColor:"

-- | @Selector@ for @colorWithAlphaComponent:@
colorWithAlphaComponentSelector :: Selector '[CDouble] (Id NSColor)
colorWithAlphaComponentSelector = mkSelector "colorWithAlphaComponent:"

-- | @Selector@ for @getRed:green:blue:alpha:@
getRed_green_blue_alphaSelector :: Selector '[Ptr CDouble, Ptr CDouble, Ptr CDouble, Ptr CDouble] ()
getRed_green_blue_alphaSelector = mkSelector "getRed:green:blue:alpha:"

-- | @Selector@ for @getHue:saturation:brightness:alpha:@
getHue_saturation_brightness_alphaSelector :: Selector '[Ptr CDouble, Ptr CDouble, Ptr CDouble, Ptr CDouble] ()
getHue_saturation_brightness_alphaSelector = mkSelector "getHue:saturation:brightness:alpha:"

-- | @Selector@ for @getWhite:alpha:@
getWhite_alphaSelector :: Selector '[Ptr CDouble, Ptr CDouble] ()
getWhite_alphaSelector = mkSelector "getWhite:alpha:"

-- | @Selector@ for @getCyan:magenta:yellow:black:alpha:@
getCyan_magenta_yellow_black_alphaSelector :: Selector '[Ptr CDouble, Ptr CDouble, Ptr CDouble, Ptr CDouble, Ptr CDouble] ()
getCyan_magenta_yellow_black_alphaSelector = mkSelector "getCyan:magenta:yellow:black:alpha:"

-- | @Selector@ for @getComponents:@
getComponentsSelector :: Selector '[Ptr CDouble] ()
getComponentsSelector = mkSelector "getComponents:"

-- | @Selector@ for @colorFromPasteboard:@
colorFromPasteboardSelector :: Selector '[Id NSPasteboard] (Id NSColor)
colorFromPasteboardSelector = mkSelector "colorFromPasteboard:"

-- | @Selector@ for @writeToPasteboard:@
writeToPasteboardSelector :: Selector '[Id NSPasteboard] ()
writeToPasteboardSelector = mkSelector "writeToPasteboard:"

-- | @Selector@ for @drawSwatchInRect:@
drawSwatchInRectSelector :: Selector '[NSRect] ()
drawSwatchInRectSelector = mkSelector "drawSwatchInRect:"

-- | @Selector@ for @colorWithCGColor:@
colorWithCGColorSelector :: Selector '[Ptr ()] (Id NSColor)
colorWithCGColorSelector = mkSelector "colorWithCGColor:"

-- | @Selector@ for @colorWithCIColor:@
colorWithCIColorSelector :: Selector '[Id CIColor] (Id NSColor)
colorWithCIColorSelector = mkSelector "colorWithCIColor:"

-- | @Selector@ for @colorUsingColorSpaceName:device:@
colorUsingColorSpaceName_deviceSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSColor)
colorUsingColorSpaceName_deviceSelector = mkSelector "colorUsingColorSpaceName:device:"

-- | @Selector@ for @colorUsingColorSpaceName:@
colorUsingColorSpaceNameSelector :: Selector '[Id NSString] (Id NSColor)
colorUsingColorSpaceNameSelector = mkSelector "colorUsingColorSpaceName:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSColorType
typeSelector = mkSelector "type"

-- | @Selector@ for @standardDynamicRangeColor@
standardDynamicRangeColorSelector :: Selector '[] (Id NSColor)
standardDynamicRangeColorSelector = mkSelector "standardDynamicRangeColor"

-- | @Selector@ for @blackColor@
blackColorSelector :: Selector '[] (Id NSColor)
blackColorSelector = mkSelector "blackColor"

-- | @Selector@ for @darkGrayColor@
darkGrayColorSelector :: Selector '[] (Id NSColor)
darkGrayColorSelector = mkSelector "darkGrayColor"

-- | @Selector@ for @lightGrayColor@
lightGrayColorSelector :: Selector '[] (Id NSColor)
lightGrayColorSelector = mkSelector "lightGrayColor"

-- | @Selector@ for @whiteColor@
whiteColorSelector :: Selector '[] (Id NSColor)
whiteColorSelector = mkSelector "whiteColor"

-- | @Selector@ for @grayColor@
grayColorSelector :: Selector '[] (Id NSColor)
grayColorSelector = mkSelector "grayColor"

-- | @Selector@ for @redColor@
redColorSelector :: Selector '[] (Id NSColor)
redColorSelector = mkSelector "redColor"

-- | @Selector@ for @greenColor@
greenColorSelector :: Selector '[] (Id NSColor)
greenColorSelector = mkSelector "greenColor"

-- | @Selector@ for @blueColor@
blueColorSelector :: Selector '[] (Id NSColor)
blueColorSelector = mkSelector "blueColor"

-- | @Selector@ for @cyanColor@
cyanColorSelector :: Selector '[] (Id NSColor)
cyanColorSelector = mkSelector "cyanColor"

-- | @Selector@ for @yellowColor@
yellowColorSelector :: Selector '[] (Id NSColor)
yellowColorSelector = mkSelector "yellowColor"

-- | @Selector@ for @magentaColor@
magentaColorSelector :: Selector '[] (Id NSColor)
magentaColorSelector = mkSelector "magentaColor"

-- | @Selector@ for @orangeColor@
orangeColorSelector :: Selector '[] (Id NSColor)
orangeColorSelector = mkSelector "orangeColor"

-- | @Selector@ for @purpleColor@
purpleColorSelector :: Selector '[] (Id NSColor)
purpleColorSelector = mkSelector "purpleColor"

-- | @Selector@ for @brownColor@
brownColorSelector :: Selector '[] (Id NSColor)
brownColorSelector = mkSelector "brownColor"

-- | @Selector@ for @clearColor@
clearColorSelector :: Selector '[] (Id NSColor)
clearColorSelector = mkSelector "clearColor"

-- | @Selector@ for @labelColor@
labelColorSelector :: Selector '[] (Id NSColor)
labelColorSelector = mkSelector "labelColor"

-- | @Selector@ for @secondaryLabelColor@
secondaryLabelColorSelector :: Selector '[] (Id NSColor)
secondaryLabelColorSelector = mkSelector "secondaryLabelColor"

-- | @Selector@ for @tertiaryLabelColor@
tertiaryLabelColorSelector :: Selector '[] (Id NSColor)
tertiaryLabelColorSelector = mkSelector "tertiaryLabelColor"

-- | @Selector@ for @quaternaryLabelColor@
quaternaryLabelColorSelector :: Selector '[] (Id NSColor)
quaternaryLabelColorSelector = mkSelector "quaternaryLabelColor"

-- | @Selector@ for @quinaryLabelColor@
quinaryLabelColorSelector :: Selector '[] (Id NSColor)
quinaryLabelColorSelector = mkSelector "quinaryLabelColor"

-- | @Selector@ for @linkColor@
linkColorSelector :: Selector '[] (Id NSColor)
linkColorSelector = mkSelector "linkColor"

-- | @Selector@ for @placeholderTextColor@
placeholderTextColorSelector :: Selector '[] (Id NSColor)
placeholderTextColorSelector = mkSelector "placeholderTextColor"

-- | @Selector@ for @windowFrameTextColor@
windowFrameTextColorSelector :: Selector '[] (Id NSColor)
windowFrameTextColorSelector = mkSelector "windowFrameTextColor"

-- | @Selector@ for @selectedMenuItemTextColor@
selectedMenuItemTextColorSelector :: Selector '[] (Id NSColor)
selectedMenuItemTextColorSelector = mkSelector "selectedMenuItemTextColor"

-- | @Selector@ for @alternateSelectedControlTextColor@
alternateSelectedControlTextColorSelector :: Selector '[] (Id NSColor)
alternateSelectedControlTextColorSelector = mkSelector "alternateSelectedControlTextColor"

-- | @Selector@ for @headerTextColor@
headerTextColorSelector :: Selector '[] (Id NSColor)
headerTextColorSelector = mkSelector "headerTextColor"

-- | @Selector@ for @separatorColor@
separatorColorSelector :: Selector '[] (Id NSColor)
separatorColorSelector = mkSelector "separatorColor"

-- | @Selector@ for @gridColor@
gridColorSelector :: Selector '[] (Id NSColor)
gridColorSelector = mkSelector "gridColor"

-- | @Selector@ for @windowBackgroundColor@
windowBackgroundColorSelector :: Selector '[] (Id NSColor)
windowBackgroundColorSelector = mkSelector "windowBackgroundColor"

-- | @Selector@ for @underPageBackgroundColor@
underPageBackgroundColorSelector :: Selector '[] (Id NSColor)
underPageBackgroundColorSelector = mkSelector "underPageBackgroundColor"

-- | @Selector@ for @controlBackgroundColor@
controlBackgroundColorSelector :: Selector '[] (Id NSColor)
controlBackgroundColorSelector = mkSelector "controlBackgroundColor"

-- | @Selector@ for @selectedContentBackgroundColor@
selectedContentBackgroundColorSelector :: Selector '[] (Id NSColor)
selectedContentBackgroundColorSelector = mkSelector "selectedContentBackgroundColor"

-- | @Selector@ for @unemphasizedSelectedContentBackgroundColor@
unemphasizedSelectedContentBackgroundColorSelector :: Selector '[] (Id NSColor)
unemphasizedSelectedContentBackgroundColorSelector = mkSelector "unemphasizedSelectedContentBackgroundColor"

-- | @Selector@ for @alternatingContentBackgroundColors@
alternatingContentBackgroundColorsSelector :: Selector '[] (Id NSArray)
alternatingContentBackgroundColorsSelector = mkSelector "alternatingContentBackgroundColors"

-- | @Selector@ for @findHighlightColor@
findHighlightColorSelector :: Selector '[] (Id NSColor)
findHighlightColorSelector = mkSelector "findHighlightColor"

-- | @Selector@ for @textColor@
textColorSelector :: Selector '[] (Id NSColor)
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @textBackgroundColor@
textBackgroundColorSelector :: Selector '[] (Id NSColor)
textBackgroundColorSelector = mkSelector "textBackgroundColor"

-- | @Selector@ for @textInsertionPointColor@
textInsertionPointColorSelector :: Selector '[] (Id NSColor)
textInsertionPointColorSelector = mkSelector "textInsertionPointColor"

-- | @Selector@ for @selectedTextColor@
selectedTextColorSelector :: Selector '[] (Id NSColor)
selectedTextColorSelector = mkSelector "selectedTextColor"

-- | @Selector@ for @selectedTextBackgroundColor@
selectedTextBackgroundColorSelector :: Selector '[] (Id NSColor)
selectedTextBackgroundColorSelector = mkSelector "selectedTextBackgroundColor"

-- | @Selector@ for @unemphasizedSelectedTextBackgroundColor@
unemphasizedSelectedTextBackgroundColorSelector :: Selector '[] (Id NSColor)
unemphasizedSelectedTextBackgroundColorSelector = mkSelector "unemphasizedSelectedTextBackgroundColor"

-- | @Selector@ for @unemphasizedSelectedTextColor@
unemphasizedSelectedTextColorSelector :: Selector '[] (Id NSColor)
unemphasizedSelectedTextColorSelector = mkSelector "unemphasizedSelectedTextColor"

-- | @Selector@ for @controlColor@
controlColorSelector :: Selector '[] (Id NSColor)
controlColorSelector = mkSelector "controlColor"

-- | @Selector@ for @controlTextColor@
controlTextColorSelector :: Selector '[] (Id NSColor)
controlTextColorSelector = mkSelector "controlTextColor"

-- | @Selector@ for @selectedControlColor@
selectedControlColorSelector :: Selector '[] (Id NSColor)
selectedControlColorSelector = mkSelector "selectedControlColor"

-- | @Selector@ for @selectedControlTextColor@
selectedControlTextColorSelector :: Selector '[] (Id NSColor)
selectedControlTextColorSelector = mkSelector "selectedControlTextColor"

-- | @Selector@ for @disabledControlTextColor@
disabledControlTextColorSelector :: Selector '[] (Id NSColor)
disabledControlTextColorSelector = mkSelector "disabledControlTextColor"

-- | @Selector@ for @keyboardFocusIndicatorColor@
keyboardFocusIndicatorColorSelector :: Selector '[] (Id NSColor)
keyboardFocusIndicatorColorSelector = mkSelector "keyboardFocusIndicatorColor"

-- | @Selector@ for @scrubberTexturedBackgroundColor@
scrubberTexturedBackgroundColorSelector :: Selector '[] (Id NSColor)
scrubberTexturedBackgroundColorSelector = mkSelector "scrubberTexturedBackgroundColor"

-- | @Selector@ for @systemRedColor@
systemRedColorSelector :: Selector '[] (Id NSColor)
systemRedColorSelector = mkSelector "systemRedColor"

-- | @Selector@ for @systemGreenColor@
systemGreenColorSelector :: Selector '[] (Id NSColor)
systemGreenColorSelector = mkSelector "systemGreenColor"

-- | @Selector@ for @systemBlueColor@
systemBlueColorSelector :: Selector '[] (Id NSColor)
systemBlueColorSelector = mkSelector "systemBlueColor"

-- | @Selector@ for @systemOrangeColor@
systemOrangeColorSelector :: Selector '[] (Id NSColor)
systemOrangeColorSelector = mkSelector "systemOrangeColor"

-- | @Selector@ for @systemYellowColor@
systemYellowColorSelector :: Selector '[] (Id NSColor)
systemYellowColorSelector = mkSelector "systemYellowColor"

-- | @Selector@ for @systemBrownColor@
systemBrownColorSelector :: Selector '[] (Id NSColor)
systemBrownColorSelector = mkSelector "systemBrownColor"

-- | @Selector@ for @systemPinkColor@
systemPinkColorSelector :: Selector '[] (Id NSColor)
systemPinkColorSelector = mkSelector "systemPinkColor"

-- | @Selector@ for @systemPurpleColor@
systemPurpleColorSelector :: Selector '[] (Id NSColor)
systemPurpleColorSelector = mkSelector "systemPurpleColor"

-- | @Selector@ for @systemGrayColor@
systemGrayColorSelector :: Selector '[] (Id NSColor)
systemGrayColorSelector = mkSelector "systemGrayColor"

-- | @Selector@ for @systemTealColor@
systemTealColorSelector :: Selector '[] (Id NSColor)
systemTealColorSelector = mkSelector "systemTealColor"

-- | @Selector@ for @systemIndigoColor@
systemIndigoColorSelector :: Selector '[] (Id NSColor)
systemIndigoColorSelector = mkSelector "systemIndigoColor"

-- | @Selector@ for @systemMintColor@
systemMintColorSelector :: Selector '[] (Id NSColor)
systemMintColorSelector = mkSelector "systemMintColor"

-- | @Selector@ for @systemCyanColor@
systemCyanColorSelector :: Selector '[] (Id NSColor)
systemCyanColorSelector = mkSelector "systemCyanColor"

-- | @Selector@ for @systemFillColor@
systemFillColorSelector :: Selector '[] (Id NSColor)
systemFillColorSelector = mkSelector "systemFillColor"

-- | @Selector@ for @secondarySystemFillColor@
secondarySystemFillColorSelector :: Selector '[] (Id NSColor)
secondarySystemFillColorSelector = mkSelector "secondarySystemFillColor"

-- | @Selector@ for @tertiarySystemFillColor@
tertiarySystemFillColorSelector :: Selector '[] (Id NSColor)
tertiarySystemFillColorSelector = mkSelector "tertiarySystemFillColor"

-- | @Selector@ for @quaternarySystemFillColor@
quaternarySystemFillColorSelector :: Selector '[] (Id NSColor)
quaternarySystemFillColorSelector = mkSelector "quaternarySystemFillColor"

-- | @Selector@ for @quinarySystemFillColor@
quinarySystemFillColorSelector :: Selector '[] (Id NSColor)
quinarySystemFillColorSelector = mkSelector "quinarySystemFillColor"

-- | @Selector@ for @controlAccentColor@
controlAccentColorSelector :: Selector '[] (Id NSColor)
controlAccentColorSelector = mkSelector "controlAccentColor"

-- | @Selector@ for @currentControlTint@
currentControlTintSelector :: Selector '[] NSControlTint
currentControlTintSelector = mkSelector "currentControlTint"

-- | @Selector@ for @highlightColor@
highlightColorSelector :: Selector '[] (Id NSColor)
highlightColorSelector = mkSelector "highlightColor"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector '[] (Id NSColor)
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @catalogNameComponent@
catalogNameComponentSelector :: Selector '[] (Id NSString)
catalogNameComponentSelector = mkSelector "catalogNameComponent"

-- | @Selector@ for @colorNameComponent@
colorNameComponentSelector :: Selector '[] (Id NSString)
colorNameComponentSelector = mkSelector "colorNameComponent"

-- | @Selector@ for @localizedCatalogNameComponent@
localizedCatalogNameComponentSelector :: Selector '[] (Id NSString)
localizedCatalogNameComponentSelector = mkSelector "localizedCatalogNameComponent"

-- | @Selector@ for @localizedColorNameComponent@
localizedColorNameComponentSelector :: Selector '[] (Id NSString)
localizedColorNameComponentSelector = mkSelector "localizedColorNameComponent"

-- | @Selector@ for @redComponent@
redComponentSelector :: Selector '[] CDouble
redComponentSelector = mkSelector "redComponent"

-- | @Selector@ for @greenComponent@
greenComponentSelector :: Selector '[] CDouble
greenComponentSelector = mkSelector "greenComponent"

-- | @Selector@ for @blueComponent@
blueComponentSelector :: Selector '[] CDouble
blueComponentSelector = mkSelector "blueComponent"

-- | @Selector@ for @hueComponent@
hueComponentSelector :: Selector '[] CDouble
hueComponentSelector = mkSelector "hueComponent"

-- | @Selector@ for @saturationComponent@
saturationComponentSelector :: Selector '[] CDouble
saturationComponentSelector = mkSelector "saturationComponent"

-- | @Selector@ for @brightnessComponent@
brightnessComponentSelector :: Selector '[] CDouble
brightnessComponentSelector = mkSelector "brightnessComponent"

-- | @Selector@ for @whiteComponent@
whiteComponentSelector :: Selector '[] CDouble
whiteComponentSelector = mkSelector "whiteComponent"

-- | @Selector@ for @cyanComponent@
cyanComponentSelector :: Selector '[] CDouble
cyanComponentSelector = mkSelector "cyanComponent"

-- | @Selector@ for @magentaComponent@
magentaComponentSelector :: Selector '[] CDouble
magentaComponentSelector = mkSelector "magentaComponent"

-- | @Selector@ for @yellowComponent@
yellowComponentSelector :: Selector '[] CDouble
yellowComponentSelector = mkSelector "yellowComponent"

-- | @Selector@ for @blackComponent@
blackComponentSelector :: Selector '[] CDouble
blackComponentSelector = mkSelector "blackComponent"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector '[] (Id NSColorSpace)
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @numberOfComponents@
numberOfComponentsSelector :: Selector '[] CLong
numberOfComponentsSelector = mkSelector "numberOfComponents"

-- | @Selector@ for @patternImage@
patternImageSelector :: Selector '[] (Id NSImage)
patternImageSelector = mkSelector "patternImage"

-- | @Selector@ for @alphaComponent@
alphaComponentSelector :: Selector '[] CDouble
alphaComponentSelector = mkSelector "alphaComponent"

-- | @Selector@ for @linearExposure@
linearExposureSelector :: Selector '[] CDouble
linearExposureSelector = mkSelector "linearExposure"

-- | @Selector@ for @CGColor@
cgColorSelector :: Selector '[] (Ptr ())
cgColorSelector = mkSelector "CGColor"

-- | @Selector@ for @ignoresAlpha@
ignoresAlphaSelector :: Selector '[] Bool
ignoresAlphaSelector = mkSelector "ignoresAlpha"

-- | @Selector@ for @setIgnoresAlpha:@
setIgnoresAlphaSelector :: Selector '[Bool] ()
setIgnoresAlphaSelector = mkSelector "setIgnoresAlpha:"

-- | @Selector@ for @controlHighlightColor@
controlHighlightColorSelector :: Selector '[] (Id NSColor)
controlHighlightColorSelector = mkSelector "controlHighlightColor"

-- | @Selector@ for @controlLightHighlightColor@
controlLightHighlightColorSelector :: Selector '[] (Id NSColor)
controlLightHighlightColorSelector = mkSelector "controlLightHighlightColor"

-- | @Selector@ for @controlShadowColor@
controlShadowColorSelector :: Selector '[] (Id NSColor)
controlShadowColorSelector = mkSelector "controlShadowColor"

-- | @Selector@ for @controlDarkShadowColor@
controlDarkShadowColorSelector :: Selector '[] (Id NSColor)
controlDarkShadowColorSelector = mkSelector "controlDarkShadowColor"

-- | @Selector@ for @scrollBarColor@
scrollBarColorSelector :: Selector '[] (Id NSColor)
scrollBarColorSelector = mkSelector "scrollBarColor"

-- | @Selector@ for @knobColor@
knobColorSelector :: Selector '[] (Id NSColor)
knobColorSelector = mkSelector "knobColor"

-- | @Selector@ for @selectedKnobColor@
selectedKnobColorSelector :: Selector '[] (Id NSColor)
selectedKnobColorSelector = mkSelector "selectedKnobColor"

-- | @Selector@ for @windowFrameColor@
windowFrameColorSelector :: Selector '[] (Id NSColor)
windowFrameColorSelector = mkSelector "windowFrameColor"

-- | @Selector@ for @selectedMenuItemColor@
selectedMenuItemColorSelector :: Selector '[] (Id NSColor)
selectedMenuItemColorSelector = mkSelector "selectedMenuItemColor"

-- | @Selector@ for @headerColor@
headerColorSelector :: Selector '[] (Id NSColor)
headerColorSelector = mkSelector "headerColor"

-- | @Selector@ for @secondarySelectedControlColor@
secondarySelectedControlColorSelector :: Selector '[] (Id NSColor)
secondarySelectedControlColorSelector = mkSelector "secondarySelectedControlColor"

-- | @Selector@ for @alternateSelectedControlColor@
alternateSelectedControlColorSelector :: Selector '[] (Id NSColor)
alternateSelectedControlColorSelector = mkSelector "alternateSelectedControlColor"

-- | @Selector@ for @controlAlternatingRowBackgroundColors@
controlAlternatingRowBackgroundColorsSelector :: Selector '[] (Id NSArray)
controlAlternatingRowBackgroundColorsSelector = mkSelector "controlAlternatingRowBackgroundColors"

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector '[] (Id NSString)
colorSpaceNameSelector = mkSelector "colorSpaceName"

