{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithCoderSelector
  , colorWithColorSpace_components_countSelector
  , colorWithSRGBRed_green_blue_alphaSelector
  , colorWithGenericGamma22White_alphaSelector
  , colorWithDisplayP3Red_green_blue_alphaSelector
  , colorWithWhite_alphaSelector
  , colorWithRed_green_blue_alphaSelector
  , colorWithHue_saturation_brightness_alphaSelector
  , colorWithColorSpace_hue_saturation_brightness_alphaSelector
  , colorWithCatalogName_colorNameSelector
  , colorNamed_bundleSelector
  , colorNamedSelector
  , colorWithName_dynamicProviderSelector
  , colorWithDeviceWhite_alphaSelector
  , colorWithDeviceRed_green_blue_alphaSelector
  , colorWithDeviceHue_saturation_brightness_alphaSelector
  , colorWithDeviceCyan_magenta_yellow_black_alphaSelector
  , colorWithCalibratedWhite_alphaSelector
  , colorWithCalibratedRed_green_blue_alphaSelector
  , colorWithCalibratedHue_saturation_brightness_alphaSelector
  , colorWithPatternImageSelector
  , colorUsingTypeSelector
  , colorUsingColorSpaceSelector
  , colorWithRed_green_blue_alpha_exposureSelector
  , colorWithRed_green_blue_alpha_linearExposureSelector
  , colorByApplyingContentHeadroomSelector
  , colorForControlTintSelector
  , highlightWithLevelSelector
  , shadowWithLevelSelector
  , colorWithSystemEffectSelector
  , setSelector
  , setFillSelector
  , setStrokeSelector
  , blendedColorWithFraction_ofColorSelector
  , colorWithAlphaComponentSelector
  , getRed_green_blue_alphaSelector
  , getHue_saturation_brightness_alphaSelector
  , getWhite_alphaSelector
  , getCyan_magenta_yellow_black_alphaSelector
  , getComponentsSelector
  , colorFromPasteboardSelector
  , writeToPasteboardSelector
  , drawSwatchInRectSelector
  , colorWithCGColorSelector
  , colorWithCIColorSelector
  , colorUsingColorSpaceName_deviceSelector
  , colorUsingColorSpaceNameSelector
  , typeSelector
  , standardDynamicRangeColorSelector
  , blackColorSelector
  , darkGrayColorSelector
  , lightGrayColorSelector
  , whiteColorSelector
  , grayColorSelector
  , redColorSelector
  , greenColorSelector
  , blueColorSelector
  , cyanColorSelector
  , yellowColorSelector
  , magentaColorSelector
  , orangeColorSelector
  , purpleColorSelector
  , brownColorSelector
  , clearColorSelector
  , labelColorSelector
  , secondaryLabelColorSelector
  , tertiaryLabelColorSelector
  , quaternaryLabelColorSelector
  , quinaryLabelColorSelector
  , linkColorSelector
  , placeholderTextColorSelector
  , windowFrameTextColorSelector
  , selectedMenuItemTextColorSelector
  , alternateSelectedControlTextColorSelector
  , headerTextColorSelector
  , separatorColorSelector
  , gridColorSelector
  , windowBackgroundColorSelector
  , underPageBackgroundColorSelector
  , controlBackgroundColorSelector
  , selectedContentBackgroundColorSelector
  , unemphasizedSelectedContentBackgroundColorSelector
  , alternatingContentBackgroundColorsSelector
  , findHighlightColorSelector
  , textColorSelector
  , textBackgroundColorSelector
  , textInsertionPointColorSelector
  , selectedTextColorSelector
  , selectedTextBackgroundColorSelector
  , unemphasizedSelectedTextBackgroundColorSelector
  , unemphasizedSelectedTextColorSelector
  , controlColorSelector
  , controlTextColorSelector
  , selectedControlColorSelector
  , selectedControlTextColorSelector
  , disabledControlTextColorSelector
  , keyboardFocusIndicatorColorSelector
  , scrubberTexturedBackgroundColorSelector
  , systemRedColorSelector
  , systemGreenColorSelector
  , systemBlueColorSelector
  , systemOrangeColorSelector
  , systemYellowColorSelector
  , systemBrownColorSelector
  , systemPinkColorSelector
  , systemPurpleColorSelector
  , systemGrayColorSelector
  , systemTealColorSelector
  , systemIndigoColorSelector
  , systemMintColorSelector
  , systemCyanColorSelector
  , systemFillColorSelector
  , secondarySystemFillColorSelector
  , tertiarySystemFillColorSelector
  , quaternarySystemFillColorSelector
  , quinarySystemFillColorSelector
  , controlAccentColorSelector
  , currentControlTintSelector
  , highlightColorSelector
  , shadowColorSelector
  , catalogNameComponentSelector
  , colorNameComponentSelector
  , localizedCatalogNameComponentSelector
  , localizedColorNameComponentSelector
  , redComponentSelector
  , greenComponentSelector
  , blueComponentSelector
  , hueComponentSelector
  , saturationComponentSelector
  , brightnessComponentSelector
  , whiteComponentSelector
  , cyanComponentSelector
  , magentaComponentSelector
  , yellowComponentSelector
  , blackComponentSelector
  , colorSpaceSelector
  , numberOfComponentsSelector
  , patternImageSelector
  , alphaComponentSelector
  , linearExposureSelector
  , cgColorSelector
  , ignoresAlphaSelector
  , setIgnoresAlphaSelector
  , controlHighlightColorSelector
  , controlLightHighlightColorSelector
  , controlShadowColorSelector
  , controlDarkShadowColorSelector
  , scrollBarColorSelector
  , knobColorSelector
  , selectedKnobColorSelector
  , windowFrameColorSelector
  , selectedMenuItemColorSelector
  , headerColorSelector
  , secondarySelectedControlColorSelector
  , alternateSelectedControlColorSelector
  , controlAlternatingRowBackgroundColorsSelector
  , colorSpaceNameSelector

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
import ObjC.Foundation.Internal.Structs
import ObjC.AppKit.Internal.Enums
import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSColor nsColor => nsColor -> IO (Id NSColor)
init_ nsColor  =
    sendMsg nsColor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSColor nsColor, IsNSCoder coder) => nsColor -> coder -> IO (Id NSColor)
initWithCoder nsColor  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsColor (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @+ colorWithColorSpace:components:count:@
colorWithColorSpace_components_count :: IsNSColorSpace space => space -> Const (Ptr CDouble) -> CLong -> IO (Id NSColor)
colorWithColorSpace_components_count space components numberOfComponents =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr space $ \raw_space ->
      sendClassMsg cls' (mkSelector "colorWithColorSpace:components:count:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ()), argPtr (unConst components), argCLong numberOfComponents] >>= retainedObject . castPtr

-- | @+ colorWithSRGBRed:green:blue:alpha:@
colorWithSRGBRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithSRGBRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithSRGBRed:green:blue:alpha:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithGenericGamma22White:alpha:@
colorWithGenericGamma22White_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithGenericGamma22White_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithGenericGamma22White:alpha:") (retPtr retVoid) [argCDouble white, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithDisplayP3Red:green:blue:alpha:@
colorWithDisplayP3Red_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDisplayP3Red_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDisplayP3Red:green:blue:alpha:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithWhite:alpha:@
colorWithWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithWhite:alpha:") (retPtr retVoid) [argCDouble white, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithHue:saturation:brightness:alpha:@
colorWithHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithHue:saturation:brightness:alpha:") (retPtr retVoid) [argCDouble hue, argCDouble saturation, argCDouble brightness, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithColorSpace:hue:saturation:brightness:alpha:@
colorWithColorSpace_hue_saturation_brightness_alpha :: IsNSColorSpace space => space -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithColorSpace_hue_saturation_brightness_alpha space hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr space $ \raw_space ->
      sendClassMsg cls' (mkSelector "colorWithColorSpace:hue:saturation:brightness:alpha:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ()), argCDouble hue, argCDouble saturation, argCDouble brightness, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithCatalogName:colorName:@
colorWithCatalogName_colorName :: (IsNSString listName, IsNSString colorName) => listName -> colorName -> IO (Id NSColor)
colorWithCatalogName_colorName listName colorName =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr listName $ \raw_listName ->
      withObjCPtr colorName $ \raw_colorName ->
        sendClassMsg cls' (mkSelector "colorWithCatalogName:colorName:") (retPtr retVoid) [argPtr (castPtr raw_listName :: Ptr ()), argPtr (castPtr raw_colorName :: Ptr ())] >>= retainedObject . castPtr

-- | @+ colorNamed:bundle:@
colorNamed_bundle :: (IsNSString name, IsNSBundle bundle) => name -> bundle -> IO (Id NSColor)
colorNamed_bundle name bundle =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr name $ \raw_name ->
      withObjCPtr bundle $ \raw_bundle ->
        sendClassMsg cls' (mkSelector "colorNamed:bundle:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_bundle :: Ptr ())] >>= retainedObject . castPtr

-- | @+ colorNamed:@
colorNamed :: IsNSString name => name -> IO (Id NSColor)
colorNamed name =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr name $ \raw_name ->
      sendClassMsg cls' (mkSelector "colorNamed:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @+ colorWithName:dynamicProvider:@
colorWithName_dynamicProvider :: IsNSString colorName => colorName -> Ptr () -> IO (Id NSColor)
colorWithName_dynamicProvider colorName dynamicProvider =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr colorName $ \raw_colorName ->
      sendClassMsg cls' (mkSelector "colorWithName:dynamicProvider:") (retPtr retVoid) [argPtr (castPtr raw_colorName :: Ptr ()), argPtr (castPtr dynamicProvider :: Ptr ())] >>= retainedObject . castPtr

-- | @+ colorWithDeviceWhite:alpha:@
colorWithDeviceWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceWhite:alpha:") (retPtr retVoid) [argCDouble white, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithDeviceRed:green:blue:alpha:@
colorWithDeviceRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceRed:green:blue:alpha:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithDeviceHue:saturation:brightness:alpha:@
colorWithDeviceHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceHue:saturation:brightness:alpha:") (retPtr retVoid) [argCDouble hue, argCDouble saturation, argCDouble brightness, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithDeviceCyan:magenta:yellow:black:alpha:@
colorWithDeviceCyan_magenta_yellow_black_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceCyan_magenta_yellow_black_alpha cyan magenta yellow black alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceCyan:magenta:yellow:black:alpha:") (retPtr retVoid) [argCDouble cyan, argCDouble magenta, argCDouble yellow, argCDouble black, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithCalibratedWhite:alpha:@
colorWithCalibratedWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCalibratedWhite:alpha:") (retPtr retVoid) [argCDouble white, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithCalibratedRed:green:blue:alpha:@
colorWithCalibratedRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCalibratedRed:green:blue:alpha:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithCalibratedHue:saturation:brightness:alpha:@
colorWithCalibratedHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCalibratedHue:saturation:brightness:alpha:") (retPtr retVoid) [argCDouble hue, argCDouble saturation, argCDouble brightness, argCDouble alpha] >>= retainedObject . castPtr

-- | @+ colorWithPatternImage:@
colorWithPatternImage :: IsNSImage image => image -> IO (Id NSColor)
colorWithPatternImage image =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr image $ \raw_image ->
      sendClassMsg cls' (mkSelector "colorWithPatternImage:") (retPtr retVoid) [argPtr (castPtr raw_image :: Ptr ())] >>= retainedObject . castPtr

-- | @- colorUsingType:@
colorUsingType :: IsNSColor nsColor => nsColor -> NSColorType -> IO (Id NSColor)
colorUsingType nsColor  type_ =
    sendMsg nsColor (mkSelector "colorUsingType:") (retPtr retVoid) [argCLong (coerce type_)] >>= retainedObject . castPtr

-- | @- colorUsingColorSpace:@
colorUsingColorSpace :: (IsNSColor nsColor, IsNSColorSpace space) => nsColor -> space -> IO (Id NSColor)
colorUsingColorSpace nsColor  space =
  withObjCPtr space $ \raw_space ->
      sendMsg nsColor (mkSelector "colorUsingColorSpace:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ())] >>= retainedObject . castPtr

-- | Generates an HDR color in the extended sRGB colorspace by applying an exposure to the SDR color defined by the red, green, and blue components. The @red@, @green@, and @blue@ components have a nominal range of [0..1], @exposure@ is a value >= 0. To produce an HDR color, we process the given color in a linear color space, multiplying component values by @2^exposure@. The produced color will have a @contentHeadroom@ equal to the linearized exposure value. Each whole value of exposure produces a color that is twice as bright.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:exposure:@
colorWithRed_green_blue_alpha_exposure :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha_exposure red green blue alpha exposure =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:exposure:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha, argCDouble exposure] >>= retainedObject . castPtr

-- | Generates an HDR color in the extended sRGB colorspace by applying an exposure to the SDR color defined by the red, green, and blue components. The @red@, @green@, and @blue@ components have a nominal range of [0..1], @linearExposure@ is a value >= 1. To produce an HDR color, we process the given color in a linear color space, multiplying component values by @linearExposure @. The produced color will have a @contentHeadroom@ equal to @linearExposure@. Each doubling of @linearExposure@ produces a color that is twice as bright.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:linearExposure:@
colorWithRed_green_blue_alpha_linearExposure :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha_linearExposure red green blue alpha linearExposure =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:linearExposure:") (retPtr retVoid) [argCDouble red, argCDouble green, argCDouble blue, argCDouble alpha, argCDouble linearExposure] >>= retainedObject . castPtr

-- | Reinterpret the color by applying a new @contentHeadroom@ without changing the color components. Changing the @contentHeadroom@ redefines the color relative to a different peak white, changing its behavior under tone mapping and the result of calling @standardDynamicRangeColor@. The new color will have a @contentHeadroom@ >= 1.0. If called on a color with a color space that does not support extended range, or does not have an equivalent extended range counterpart, this will return @self@.
--
-- ObjC selector: @- colorByApplyingContentHeadroom:@
colorByApplyingContentHeadroom :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
colorByApplyingContentHeadroom nsColor  contentHeadroom =
    sendMsg nsColor (mkSelector "colorByApplyingContentHeadroom:") (retPtr retVoid) [argCDouble contentHeadroom] >>= retainedObject . castPtr

-- | @+ colorForControlTint:@
colorForControlTint :: NSControlTint -> IO (Id NSColor)
colorForControlTint controlTint =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorForControlTint:") (retPtr retVoid) [argCULong (coerce controlTint)] >>= retainedObject . castPtr

-- | @- highlightWithLevel:@
highlightWithLevel :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
highlightWithLevel nsColor  val =
    sendMsg nsColor (mkSelector "highlightWithLevel:") (retPtr retVoid) [argCDouble val] >>= retainedObject . castPtr

-- | @- shadowWithLevel:@
shadowWithLevel :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
shadowWithLevel nsColor  val =
    sendMsg nsColor (mkSelector "shadowWithLevel:") (retPtr retVoid) [argCDouble val] >>= retainedObject . castPtr

-- | Returns a color representing the base color with a system defined effect applied to it. This color is safe to create before draw time, as the resolution of the final color only happens when being @-set@, retrieving its @CGColor@, resolving with @-colorWithType:@, etc. The return color type is @.named@.
--
-- ObjC selector: @- colorWithSystemEffect:@
colorWithSystemEffect :: IsNSColor nsColor => nsColor -> NSColorSystemEffect -> IO (Id NSColor)
colorWithSystemEffect nsColor  systemEffect =
    sendMsg nsColor (mkSelector "colorWithSystemEffect:") (retPtr retVoid) [argCLong (coerce systemEffect)] >>= retainedObject . castPtr

-- | @- set@
set :: IsNSColor nsColor => nsColor -> IO ()
set nsColor  =
    sendMsg nsColor (mkSelector "set") retVoid []

-- | @- setFill@
setFill :: IsNSColor nsColor => nsColor -> IO ()
setFill nsColor  =
    sendMsg nsColor (mkSelector "setFill") retVoid []

-- | @- setStroke@
setStroke :: IsNSColor nsColor => nsColor -> IO ()
setStroke nsColor  =
    sendMsg nsColor (mkSelector "setStroke") retVoid []

-- | @- blendedColorWithFraction:ofColor:@
blendedColorWithFraction_ofColor :: (IsNSColor nsColor, IsNSColor color) => nsColor -> CDouble -> color -> IO (Id NSColor)
blendedColorWithFraction_ofColor nsColor  fraction color =
  withObjCPtr color $ \raw_color ->
      sendMsg nsColor (mkSelector "blendedColorWithFraction:ofColor:") (retPtr retVoid) [argCDouble fraction, argPtr (castPtr raw_color :: Ptr ())] >>= retainedObject . castPtr

-- | @- colorWithAlphaComponent:@
colorWithAlphaComponent :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
colorWithAlphaComponent nsColor  alpha =
    sendMsg nsColor (mkSelector "colorWithAlphaComponent:") (retPtr retVoid) [argCDouble alpha] >>= retainedObject . castPtr

-- | @- getRed:green:blue:alpha:@
getRed_green_blue_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
getRed_green_blue_alpha nsColor  red green blue alpha =
    sendMsg nsColor (mkSelector "getRed:green:blue:alpha:") retVoid [argPtr red, argPtr green, argPtr blue, argPtr alpha]

-- | @- getHue:saturation:brightness:alpha:@
getHue_saturation_brightness_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
getHue_saturation_brightness_alpha nsColor  hue saturation brightness alpha =
    sendMsg nsColor (mkSelector "getHue:saturation:brightness:alpha:") retVoid [argPtr hue, argPtr saturation, argPtr brightness, argPtr alpha]

-- | @- getWhite:alpha:@
getWhite_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> IO ()
getWhite_alpha nsColor  white alpha =
    sendMsg nsColor (mkSelector "getWhite:alpha:") retVoid [argPtr white, argPtr alpha]

-- | @- getCyan:magenta:yellow:black:alpha:@
getCyan_magenta_yellow_black_alpha :: IsNSColor nsColor => nsColor -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()
getCyan_magenta_yellow_black_alpha nsColor  cyan magenta yellow black alpha =
    sendMsg nsColor (mkSelector "getCyan:magenta:yellow:black:alpha:") retVoid [argPtr cyan, argPtr magenta, argPtr yellow, argPtr black, argPtr alpha]

-- | @- getComponents:@
getComponents :: IsNSColor nsColor => nsColor -> Ptr CDouble -> IO ()
getComponents nsColor  components =
    sendMsg nsColor (mkSelector "getComponents:") retVoid [argPtr components]

-- | @+ colorFromPasteboard:@
colorFromPasteboard :: IsNSPasteboard pasteBoard => pasteBoard -> IO (Id NSColor)
colorFromPasteboard pasteBoard =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr pasteBoard $ \raw_pasteBoard ->
      sendClassMsg cls' (mkSelector "colorFromPasteboard:") (retPtr retVoid) [argPtr (castPtr raw_pasteBoard :: Ptr ())] >>= retainedObject . castPtr

-- | @- writeToPasteboard:@
writeToPasteboard :: (IsNSColor nsColor, IsNSPasteboard pasteBoard) => nsColor -> pasteBoard -> IO ()
writeToPasteboard nsColor  pasteBoard =
  withObjCPtr pasteBoard $ \raw_pasteBoard ->
      sendMsg nsColor (mkSelector "writeToPasteboard:") retVoid [argPtr (castPtr raw_pasteBoard :: Ptr ())]

-- | @- drawSwatchInRect:@
drawSwatchInRect :: IsNSColor nsColor => nsColor -> NSRect -> IO ()
drawSwatchInRect nsColor  rect =
    sendMsg nsColor (mkSelector "drawSwatchInRect:") retVoid [argNSRect rect]

-- | @+ colorWithCGColor:@
colorWithCGColor :: Ptr () -> IO (Id NSColor)
colorWithCGColor cgColor =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCGColor:") (retPtr retVoid) [argPtr cgColor] >>= retainedObject . castPtr

-- | @+ colorWithCIColor:@
colorWithCIColor :: IsCIColor color => color -> IO (Id NSColor)
colorWithCIColor color =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr color $ \raw_color ->
      sendClassMsg cls' (mkSelector "colorWithCIColor:") (retPtr retVoid) [argPtr (castPtr raw_color :: Ptr ())] >>= retainedObject . castPtr

-- | @- colorUsingColorSpaceName:device:@
colorUsingColorSpaceName_device :: (IsNSColor nsColor, IsNSString name, IsNSDictionary deviceDescription) => nsColor -> name -> deviceDescription -> IO (Id NSColor)
colorUsingColorSpaceName_device nsColor  name deviceDescription =
  withObjCPtr name $ \raw_name ->
    withObjCPtr deviceDescription $ \raw_deviceDescription ->
        sendMsg nsColor (mkSelector "colorUsingColorSpaceName:device:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ()), argPtr (castPtr raw_deviceDescription :: Ptr ())] >>= retainedObject . castPtr

-- | @- colorUsingColorSpaceName:@
colorUsingColorSpaceName :: (IsNSColor nsColor, IsNSString name) => nsColor -> name -> IO (Id NSColor)
colorUsingColorSpaceName nsColor  name =
  withObjCPtr name $ \raw_name ->
      sendMsg nsColor (mkSelector "colorUsingColorSpaceName:") (retPtr retVoid) [argPtr (castPtr raw_name :: Ptr ())] >>= retainedObject . castPtr

-- | @- type@
type_ :: IsNSColor nsColor => nsColor -> IO NSColorType
type_ nsColor  =
    fmap (coerce :: CLong -> NSColorType) $ sendMsg nsColor (mkSelector "type") retCLong []

-- | In some cases it is useful to recover the color that was base the SDR color that was exposed to generate an HDR color. If a color's @linearExposure@ is > 1, then this will return the base SDR color. If the color is not an HDR color, this will return @self@.
--
-- ObjC selector: @- standardDynamicRangeColor@
standardDynamicRangeColor :: IsNSColor nsColor => nsColor -> IO (Id NSColor)
standardDynamicRangeColor nsColor  =
    sendMsg nsColor (mkSelector "standardDynamicRangeColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ blackColor@
blackColor :: IO (Id NSColor)
blackColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "blackColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ darkGrayColor@
darkGrayColor :: IO (Id NSColor)
darkGrayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "darkGrayColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ lightGrayColor@
lightGrayColor :: IO (Id NSColor)
lightGrayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "lightGrayColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ whiteColor@
whiteColor :: IO (Id NSColor)
whiteColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "whiteColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ grayColor@
grayColor :: IO (Id NSColor)
grayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "grayColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ redColor@
redColor :: IO (Id NSColor)
redColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "redColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ greenColor@
greenColor :: IO (Id NSColor)
greenColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "greenColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ blueColor@
blueColor :: IO (Id NSColor)
blueColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "blueColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cyanColor@
cyanColor :: IO (Id NSColor)
cyanColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "cyanColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yellowColor@
yellowColor :: IO (Id NSColor)
yellowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "yellowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ magentaColor@
magentaColor :: IO (Id NSColor)
magentaColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "magentaColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ orangeColor@
orangeColor :: IO (Id NSColor)
orangeColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "orangeColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ purpleColor@
purpleColor :: IO (Id NSColor)
purpleColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "purpleColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ brownColor@
brownColor :: IO (Id NSColor)
brownColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "brownColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ clearColor@
clearColor :: IO (Id NSColor)
clearColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "clearColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ labelColor@
labelColor :: IO (Id NSColor)
labelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "labelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ secondaryLabelColor@
secondaryLabelColor :: IO (Id NSColor)
secondaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "secondaryLabelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ tertiaryLabelColor@
tertiaryLabelColor :: IO (Id NSColor)
tertiaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "tertiaryLabelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ quaternaryLabelColor@
quaternaryLabelColor :: IO (Id NSColor)
quaternaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "quaternaryLabelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ quinaryLabelColor@
quinaryLabelColor :: IO (Id NSColor)
quinaryLabelColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "quinaryLabelColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Used for large scale images or subtle decorative elements; not for general foreground content.
--
-- ObjC selector: @+ linkColor@
linkColor :: IO (Id NSColor)
linkColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "linkColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ placeholderTextColor@
placeholderTextColor :: IO (Id NSColor)
placeholderTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "placeholderTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ windowFrameTextColor@
windowFrameTextColor :: IO (Id NSColor)
windowFrameTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "windowFrameTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ selectedMenuItemTextColor@
selectedMenuItemTextColor :: IO (Id NSColor)
selectedMenuItemTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedMenuItemTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ alternateSelectedControlTextColor@
alternateSelectedControlTextColor :: IO (Id NSColor)
alternateSelectedControlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "alternateSelectedControlTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ headerTextColor@
headerTextColor :: IO (Id NSColor)
headerTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "headerTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ separatorColor@
separatorColor :: IO (Id NSColor)
separatorColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "separatorColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gridColor@
gridColor :: IO (Id NSColor)
gridColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "gridColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ windowBackgroundColor@
windowBackgroundColor :: IO (Id NSColor)
windowBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "windowBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ underPageBackgroundColor@
underPageBackgroundColor :: IO (Id NSColor)
underPageBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "underPageBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ controlBackgroundColor@
controlBackgroundColor :: IO (Id NSColor)
controlBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ selectedContentBackgroundColor@
selectedContentBackgroundColor :: IO (Id NSColor)
selectedContentBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedContentBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ unemphasizedSelectedContentBackgroundColor@
unemphasizedSelectedContentBackgroundColor :: IO (Id NSColor)
unemphasizedSelectedContentBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "unemphasizedSelectedContentBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ alternatingContentBackgroundColors@
alternatingContentBackgroundColors :: IO (Id NSArray)
alternatingContentBackgroundColors  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "alternatingContentBackgroundColors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ findHighlightColor@
findHighlightColor :: IO (Id NSColor)
findHighlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "findHighlightColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ textColor@
textColor :: IO (Id NSColor)
textColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "textColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ textBackgroundColor@
textBackgroundColor :: IO (Id NSColor)
textBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "textBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ textInsertionPointColor@
textInsertionPointColor :: IO (Id NSColor)
textInsertionPointColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "textInsertionPointColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ selectedTextColor@
selectedTextColor :: IO (Id NSColor)
selectedTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ selectedTextBackgroundColor@
selectedTextBackgroundColor :: IO (Id NSColor)
selectedTextBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedTextBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ unemphasizedSelectedTextBackgroundColor@
unemphasizedSelectedTextBackgroundColor :: IO (Id NSColor)
unemphasizedSelectedTextBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "unemphasizedSelectedTextBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ unemphasizedSelectedTextColor@
unemphasizedSelectedTextColor :: IO (Id NSColor)
unemphasizedSelectedTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "unemphasizedSelectedTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ controlColor@
controlColor :: IO (Id NSColor)
controlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ controlTextColor@
controlTextColor :: IO (Id NSColor)
controlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ selectedControlColor@
selectedControlColor :: IO (Id NSColor)
selectedControlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedControlColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ selectedControlTextColor@
selectedControlTextColor :: IO (Id NSColor)
selectedControlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedControlTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ disabledControlTextColor@
disabledControlTextColor :: IO (Id NSColor)
disabledControlTextColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "disabledControlTextColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ keyboardFocusIndicatorColor@
keyboardFocusIndicatorColor :: IO (Id NSColor)
keyboardFocusIndicatorColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "keyboardFocusIndicatorColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ scrubberTexturedBackgroundColor@
scrubberTexturedBackgroundColor :: IO (Id NSColor)
scrubberTexturedBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "scrubberTexturedBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemRedColor@
systemRedColor :: IO (Id NSColor)
systemRedColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemRedColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemGreenColor@
systemGreenColor :: IO (Id NSColor)
systemGreenColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemGreenColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemBlueColor@
systemBlueColor :: IO (Id NSColor)
systemBlueColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemBlueColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemOrangeColor@
systemOrangeColor :: IO (Id NSColor)
systemOrangeColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemOrangeColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemYellowColor@
systemYellowColor :: IO (Id NSColor)
systemYellowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemYellowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemBrownColor@
systemBrownColor :: IO (Id NSColor)
systemBrownColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemBrownColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemPinkColor@
systemPinkColor :: IO (Id NSColor)
systemPinkColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemPinkColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemPurpleColor@
systemPurpleColor :: IO (Id NSColor)
systemPurpleColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemPurpleColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemGrayColor@
systemGrayColor :: IO (Id NSColor)
systemGrayColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemGrayColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemTealColor@
systemTealColor :: IO (Id NSColor)
systemTealColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemTealColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemIndigoColor@
systemIndigoColor :: IO (Id NSColor)
systemIndigoColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemIndigoColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemMintColor@
systemMintColor :: IO (Id NSColor)
systemMintColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemMintColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ systemCyanColor@
systemCyanColor :: IO (Id NSColor)
systemCyanColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemCyanColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Fill colors for UI elements. These are meant to be used over the background colors, since their alpha component is less than 1.
--
-- systemFillColor is appropriate for filling thin shapes, such as the track of a slider.
--
-- ObjC selector: @+ systemFillColor@
systemFillColor :: IO (Id NSColor)
systemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "systemFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | secondarySystemFillColor is appropriate for filling small-size shapes, such as the backing of a progress indicator.
--
-- ObjC selector: @+ secondarySystemFillColor@
secondarySystemFillColor :: IO (Id NSColor)
secondarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "secondarySystemFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | tertiarySystemFillColor is appropriate for filling medium-size shapes,  such as the backing of a switch.
--
-- ObjC selector: @+ tertiarySystemFillColor@
tertiarySystemFillColor :: IO (Id NSColor)
tertiarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "tertiarySystemFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | quaternarySystemFillColor is appropriate for filling large areas, such as a group box or tab pane.
--
-- ObjC selector: @+ quaternarySystemFillColor@
quaternarySystemFillColor :: IO (Id NSColor)
quaternarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "quaternarySystemFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | quinarySystemFillColor is appropriate for filling large areas that require subtle emphasis, such as content of a form..
--
-- ObjC selector: @+ quinarySystemFillColor@
quinarySystemFillColor :: IO (Id NSColor)
quinarySystemFillColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "quinarySystemFillColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A dynamic color that reflects the user's current preferred accent color. This color automatically updates when the accent color preference changes. Do not make assumptions about the color space of this color, which may change across releases.
--
-- ObjC selector: @+ controlAccentColor@
controlAccentColor :: IO (Id NSColor)
controlAccentColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlAccentColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ currentControlTint@
currentControlTint :: IO NSControlTint
currentControlTint  =
  do
    cls' <- getRequiredClass "NSColor"
    fmap (coerce :: CULong -> NSControlTint) $ sendClassMsg cls' (mkSelector "currentControlTint") retCULong []

-- | @+ highlightColor@
highlightColor :: IO (Id NSColor)
highlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "highlightColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ shadowColor@
shadowColor :: IO (Id NSColor)
shadowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "shadowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | * Methods to get various components of colors. Not all of the methods apply to all colors; if called, they raise. **
--
-- ObjC selector: @- catalogNameComponent@
catalogNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
catalogNameComponent nsColor  =
    sendMsg nsColor (mkSelector "catalogNameComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- colorNameComponent@
colorNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
colorNameComponent nsColor  =
    sendMsg nsColor (mkSelector "colorNameComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedCatalogNameComponent@
localizedCatalogNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
localizedCatalogNameComponent nsColor  =
    sendMsg nsColor (mkSelector "localizedCatalogNameComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- localizedColorNameComponent@
localizedColorNameComponent :: IsNSColor nsColor => nsColor -> IO (Id NSString)
localizedColorNameComponent nsColor  =
    sendMsg nsColor (mkSelector "localizedColorNameComponent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- redComponent@
redComponent :: IsNSColor nsColor => nsColor -> IO CDouble
redComponent nsColor  =
    sendMsg nsColor (mkSelector "redComponent") retCDouble []

-- | @- greenComponent@
greenComponent :: IsNSColor nsColor => nsColor -> IO CDouble
greenComponent nsColor  =
    sendMsg nsColor (mkSelector "greenComponent") retCDouble []

-- | @- blueComponent@
blueComponent :: IsNSColor nsColor => nsColor -> IO CDouble
blueComponent nsColor  =
    sendMsg nsColor (mkSelector "blueComponent") retCDouble []

-- | @- hueComponent@
hueComponent :: IsNSColor nsColor => nsColor -> IO CDouble
hueComponent nsColor  =
    sendMsg nsColor (mkSelector "hueComponent") retCDouble []

-- | @- saturationComponent@
saturationComponent :: IsNSColor nsColor => nsColor -> IO CDouble
saturationComponent nsColor  =
    sendMsg nsColor (mkSelector "saturationComponent") retCDouble []

-- | @- brightnessComponent@
brightnessComponent :: IsNSColor nsColor => nsColor -> IO CDouble
brightnessComponent nsColor  =
    sendMsg nsColor (mkSelector "brightnessComponent") retCDouble []

-- | @- whiteComponent@
whiteComponent :: IsNSColor nsColor => nsColor -> IO CDouble
whiteComponent nsColor  =
    sendMsg nsColor (mkSelector "whiteComponent") retCDouble []

-- | @- cyanComponent@
cyanComponent :: IsNSColor nsColor => nsColor -> IO CDouble
cyanComponent nsColor  =
    sendMsg nsColor (mkSelector "cyanComponent") retCDouble []

-- | @- magentaComponent@
magentaComponent :: IsNSColor nsColor => nsColor -> IO CDouble
magentaComponent nsColor  =
    sendMsg nsColor (mkSelector "magentaComponent") retCDouble []

-- | @- yellowComponent@
yellowComponent :: IsNSColor nsColor => nsColor -> IO CDouble
yellowComponent nsColor  =
    sendMsg nsColor (mkSelector "yellowComponent") retCDouble []

-- | @- blackComponent@
blackComponent :: IsNSColor nsColor => nsColor -> IO CDouble
blackComponent nsColor  =
    sendMsg nsColor (mkSelector "blackComponent") retCDouble []

-- | @- colorSpace@
colorSpace :: IsNSColor nsColor => nsColor -> IO (Id NSColorSpace)
colorSpace nsColor  =
    sendMsg nsColor (mkSelector "colorSpace") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- numberOfComponents@
numberOfComponents :: IsNSColor nsColor => nsColor -> IO CLong
numberOfComponents nsColor  =
    sendMsg nsColor (mkSelector "numberOfComponents") retCLong []

-- | @- patternImage@
patternImage :: IsNSColor nsColor => nsColor -> IO (Id NSImage)
patternImage nsColor  =
    sendMsg nsColor (mkSelector "patternImage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- alphaComponent@
alphaComponent :: IsNSColor nsColor => nsColor -> IO CDouble
alphaComponent nsColor  =
    sendMsg nsColor (mkSelector "alphaComponent") retCDouble []

-- | For HDR colors, the linear brightness multiplier that was applied when generating the color. Colors created with an exposure by NSColor create CGColors that are tagged with a contentHeadroom value. While CGColors created without a contentHeadroom tag will return 0 from CGColorGetHeadroom, NSColors generated in a similar fashion return a linearExposure of 1.0.
--
-- ObjC selector: @- linearExposure@
linearExposure :: IsNSColor nsColor => nsColor -> IO CDouble
linearExposure nsColor  =
    sendMsg nsColor (mkSelector "linearExposure") retCDouble []

-- | @- CGColor@
cgColor :: IsNSColor nsColor => nsColor -> IO (Ptr ())
cgColor nsColor  =
    fmap castPtr $ sendMsg nsColor (mkSelector "CGColor") (retPtr retVoid) []

-- | @+ ignoresAlpha@
ignoresAlpha :: IO Bool
ignoresAlpha  =
  do
    cls' <- getRequiredClass "NSColor"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "ignoresAlpha") retCULong []

-- | @+ setIgnoresAlpha:@
setIgnoresAlpha :: Bool -> IO ()
setIgnoresAlpha value =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "setIgnoresAlpha:") retVoid [argCULong (if value then 1 else 0)]

-- | Historically used as the inner border highlight color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlHighlightColor@
controlHighlightColor :: IO (Id NSColor)
controlHighlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlHighlightColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the outer border highlight color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlLightHighlightColor@
controlLightHighlightColor :: IO (Id NSColor)
controlLightHighlightColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlLightHighlightColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the inner border shadow color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlShadowColor@
controlShadowColor :: IO (Id NSColor)
controlShadowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlShadowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the outer border shadow color for beveled buttons. No longer used.
--
-- ObjC selector: @+ controlDarkShadowColor@
controlDarkShadowColor :: IO (Id NSColor)
controlDarkShadowColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlDarkShadowColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the color of scroll bars. No longer used.
--
-- ObjC selector: @+ scrollBarColor@
scrollBarColor :: IO (Id NSColor)
scrollBarColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "scrollBarColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the color of scroll bar knobs. No longer used.
--
-- ObjC selector: @+ knobColor@
knobColor :: IO (Id NSColor)
knobColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "knobColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the color of scroll bar knobs being dragged. No longer used.
--
-- ObjC selector: @+ selectedKnobColor@
selectedKnobColor :: IO (Id NSColor)
selectedKnobColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedKnobColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the color of the window chrome, which is no longer able to be represented by a color. No longer used.
--
-- ObjC selector: @+ windowFrameColor@
windowFrameColor :: IO (Id NSColor)
windowFrameColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "windowFrameColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the color of selected menu items, which is no longer a color but a tinted blur effect. No longer used.
--
-- ObjC selector: @+ selectedMenuItemColor@
selectedMenuItemColor :: IO (Id NSColor)
selectedMenuItemColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "selectedMenuItemColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Historically used as the color of table headers, which is no longer a color but a tinted blur effect.
--
-- ObjC selector: @+ headerColor@
headerColor :: IO (Id NSColor)
headerColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "headerColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The background color of selected content or text that is unemphasized. Older alias for +unemphasizedSelectedContentBackgroundColor and +unemphasizedSelectedTextBackgroundColor
--
-- ObjC selector: @+ secondarySelectedControlColor@
secondarySelectedControlColor :: IO (Id NSColor)
secondarySelectedControlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "secondarySelectedControlColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The background color of selected and emphasized (focused) content: table views rows, collection views, etc. Older alias for +selectedContentBackgroundColor
--
-- ObjC selector: @+ alternateSelectedControlColor@
alternateSelectedControlColor :: IO (Id NSColor)
alternateSelectedControlColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "alternateSelectedControlColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The background colors for alternating content items: such as table view rows, collection view items. Older alias for +alternatingContentBackgroundColors
--
-- ObjC selector: @+ controlAlternatingRowBackgroundColors@
controlAlternatingRowBackgroundColors :: IO (Id NSArray)
controlAlternatingRowBackgroundColors  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlAlternatingRowBackgroundColors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- colorSpaceName@
colorSpaceName :: IsNSColor nsColor => nsColor -> IO (Id NSString)
colorSpaceName nsColor  =
    sendMsg nsColor (mkSelector "colorSpaceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @colorWithColorSpace:components:count:@
colorWithColorSpace_components_countSelector :: Selector
colorWithColorSpace_components_countSelector = mkSelector "colorWithColorSpace:components:count:"

-- | @Selector@ for @colorWithSRGBRed:green:blue:alpha:@
colorWithSRGBRed_green_blue_alphaSelector :: Selector
colorWithSRGBRed_green_blue_alphaSelector = mkSelector "colorWithSRGBRed:green:blue:alpha:"

-- | @Selector@ for @colorWithGenericGamma22White:alpha:@
colorWithGenericGamma22White_alphaSelector :: Selector
colorWithGenericGamma22White_alphaSelector = mkSelector "colorWithGenericGamma22White:alpha:"

-- | @Selector@ for @colorWithDisplayP3Red:green:blue:alpha:@
colorWithDisplayP3Red_green_blue_alphaSelector :: Selector
colorWithDisplayP3Red_green_blue_alphaSelector = mkSelector "colorWithDisplayP3Red:green:blue:alpha:"

-- | @Selector@ for @colorWithWhite:alpha:@
colorWithWhite_alphaSelector :: Selector
colorWithWhite_alphaSelector = mkSelector "colorWithWhite:alpha:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alphaSelector :: Selector
colorWithRed_green_blue_alphaSelector = mkSelector "colorWithRed:green:blue:alpha:"

-- | @Selector@ for @colorWithHue:saturation:brightness:alpha:@
colorWithHue_saturation_brightness_alphaSelector :: Selector
colorWithHue_saturation_brightness_alphaSelector = mkSelector "colorWithHue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithColorSpace:hue:saturation:brightness:alpha:@
colorWithColorSpace_hue_saturation_brightness_alphaSelector :: Selector
colorWithColorSpace_hue_saturation_brightness_alphaSelector = mkSelector "colorWithColorSpace:hue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithCatalogName:colorName:@
colorWithCatalogName_colorNameSelector :: Selector
colorWithCatalogName_colorNameSelector = mkSelector "colorWithCatalogName:colorName:"

-- | @Selector@ for @colorNamed:bundle:@
colorNamed_bundleSelector :: Selector
colorNamed_bundleSelector = mkSelector "colorNamed:bundle:"

-- | @Selector@ for @colorNamed:@
colorNamedSelector :: Selector
colorNamedSelector = mkSelector "colorNamed:"

-- | @Selector@ for @colorWithName:dynamicProvider:@
colorWithName_dynamicProviderSelector :: Selector
colorWithName_dynamicProviderSelector = mkSelector "colorWithName:dynamicProvider:"

-- | @Selector@ for @colorWithDeviceWhite:alpha:@
colorWithDeviceWhite_alphaSelector :: Selector
colorWithDeviceWhite_alphaSelector = mkSelector "colorWithDeviceWhite:alpha:"

-- | @Selector@ for @colorWithDeviceRed:green:blue:alpha:@
colorWithDeviceRed_green_blue_alphaSelector :: Selector
colorWithDeviceRed_green_blue_alphaSelector = mkSelector "colorWithDeviceRed:green:blue:alpha:"

-- | @Selector@ for @colorWithDeviceHue:saturation:brightness:alpha:@
colorWithDeviceHue_saturation_brightness_alphaSelector :: Selector
colorWithDeviceHue_saturation_brightness_alphaSelector = mkSelector "colorWithDeviceHue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithDeviceCyan:magenta:yellow:black:alpha:@
colorWithDeviceCyan_magenta_yellow_black_alphaSelector :: Selector
colorWithDeviceCyan_magenta_yellow_black_alphaSelector = mkSelector "colorWithDeviceCyan:magenta:yellow:black:alpha:"

-- | @Selector@ for @colorWithCalibratedWhite:alpha:@
colorWithCalibratedWhite_alphaSelector :: Selector
colorWithCalibratedWhite_alphaSelector = mkSelector "colorWithCalibratedWhite:alpha:"

-- | @Selector@ for @colorWithCalibratedRed:green:blue:alpha:@
colorWithCalibratedRed_green_blue_alphaSelector :: Selector
colorWithCalibratedRed_green_blue_alphaSelector = mkSelector "colorWithCalibratedRed:green:blue:alpha:"

-- | @Selector@ for @colorWithCalibratedHue:saturation:brightness:alpha:@
colorWithCalibratedHue_saturation_brightness_alphaSelector :: Selector
colorWithCalibratedHue_saturation_brightness_alphaSelector = mkSelector "colorWithCalibratedHue:saturation:brightness:alpha:"

-- | @Selector@ for @colorWithPatternImage:@
colorWithPatternImageSelector :: Selector
colorWithPatternImageSelector = mkSelector "colorWithPatternImage:"

-- | @Selector@ for @colorUsingType:@
colorUsingTypeSelector :: Selector
colorUsingTypeSelector = mkSelector "colorUsingType:"

-- | @Selector@ for @colorUsingColorSpace:@
colorUsingColorSpaceSelector :: Selector
colorUsingColorSpaceSelector = mkSelector "colorUsingColorSpace:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:exposure:@
colorWithRed_green_blue_alpha_exposureSelector :: Selector
colorWithRed_green_blue_alpha_exposureSelector = mkSelector "colorWithRed:green:blue:alpha:exposure:"

-- | @Selector@ for @colorWithRed:green:blue:alpha:linearExposure:@
colorWithRed_green_blue_alpha_linearExposureSelector :: Selector
colorWithRed_green_blue_alpha_linearExposureSelector = mkSelector "colorWithRed:green:blue:alpha:linearExposure:"

-- | @Selector@ for @colorByApplyingContentHeadroom:@
colorByApplyingContentHeadroomSelector :: Selector
colorByApplyingContentHeadroomSelector = mkSelector "colorByApplyingContentHeadroom:"

-- | @Selector@ for @colorForControlTint:@
colorForControlTintSelector :: Selector
colorForControlTintSelector = mkSelector "colorForControlTint:"

-- | @Selector@ for @highlightWithLevel:@
highlightWithLevelSelector :: Selector
highlightWithLevelSelector = mkSelector "highlightWithLevel:"

-- | @Selector@ for @shadowWithLevel:@
shadowWithLevelSelector :: Selector
shadowWithLevelSelector = mkSelector "shadowWithLevel:"

-- | @Selector@ for @colorWithSystemEffect:@
colorWithSystemEffectSelector :: Selector
colorWithSystemEffectSelector = mkSelector "colorWithSystemEffect:"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @setFill@
setFillSelector :: Selector
setFillSelector = mkSelector "setFill"

-- | @Selector@ for @setStroke@
setStrokeSelector :: Selector
setStrokeSelector = mkSelector "setStroke"

-- | @Selector@ for @blendedColorWithFraction:ofColor:@
blendedColorWithFraction_ofColorSelector :: Selector
blendedColorWithFraction_ofColorSelector = mkSelector "blendedColorWithFraction:ofColor:"

-- | @Selector@ for @colorWithAlphaComponent:@
colorWithAlphaComponentSelector :: Selector
colorWithAlphaComponentSelector = mkSelector "colorWithAlphaComponent:"

-- | @Selector@ for @getRed:green:blue:alpha:@
getRed_green_blue_alphaSelector :: Selector
getRed_green_blue_alphaSelector = mkSelector "getRed:green:blue:alpha:"

-- | @Selector@ for @getHue:saturation:brightness:alpha:@
getHue_saturation_brightness_alphaSelector :: Selector
getHue_saturation_brightness_alphaSelector = mkSelector "getHue:saturation:brightness:alpha:"

-- | @Selector@ for @getWhite:alpha:@
getWhite_alphaSelector :: Selector
getWhite_alphaSelector = mkSelector "getWhite:alpha:"

-- | @Selector@ for @getCyan:magenta:yellow:black:alpha:@
getCyan_magenta_yellow_black_alphaSelector :: Selector
getCyan_magenta_yellow_black_alphaSelector = mkSelector "getCyan:magenta:yellow:black:alpha:"

-- | @Selector@ for @getComponents:@
getComponentsSelector :: Selector
getComponentsSelector = mkSelector "getComponents:"

-- | @Selector@ for @colorFromPasteboard:@
colorFromPasteboardSelector :: Selector
colorFromPasteboardSelector = mkSelector "colorFromPasteboard:"

-- | @Selector@ for @writeToPasteboard:@
writeToPasteboardSelector :: Selector
writeToPasteboardSelector = mkSelector "writeToPasteboard:"

-- | @Selector@ for @drawSwatchInRect:@
drawSwatchInRectSelector :: Selector
drawSwatchInRectSelector = mkSelector "drawSwatchInRect:"

-- | @Selector@ for @colorWithCGColor:@
colorWithCGColorSelector :: Selector
colorWithCGColorSelector = mkSelector "colorWithCGColor:"

-- | @Selector@ for @colorWithCIColor:@
colorWithCIColorSelector :: Selector
colorWithCIColorSelector = mkSelector "colorWithCIColor:"

-- | @Selector@ for @colorUsingColorSpaceName:device:@
colorUsingColorSpaceName_deviceSelector :: Selector
colorUsingColorSpaceName_deviceSelector = mkSelector "colorUsingColorSpaceName:device:"

-- | @Selector@ for @colorUsingColorSpaceName:@
colorUsingColorSpaceNameSelector :: Selector
colorUsingColorSpaceNameSelector = mkSelector "colorUsingColorSpaceName:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @standardDynamicRangeColor@
standardDynamicRangeColorSelector :: Selector
standardDynamicRangeColorSelector = mkSelector "standardDynamicRangeColor"

-- | @Selector@ for @blackColor@
blackColorSelector :: Selector
blackColorSelector = mkSelector "blackColor"

-- | @Selector@ for @darkGrayColor@
darkGrayColorSelector :: Selector
darkGrayColorSelector = mkSelector "darkGrayColor"

-- | @Selector@ for @lightGrayColor@
lightGrayColorSelector :: Selector
lightGrayColorSelector = mkSelector "lightGrayColor"

-- | @Selector@ for @whiteColor@
whiteColorSelector :: Selector
whiteColorSelector = mkSelector "whiteColor"

-- | @Selector@ for @grayColor@
grayColorSelector :: Selector
grayColorSelector = mkSelector "grayColor"

-- | @Selector@ for @redColor@
redColorSelector :: Selector
redColorSelector = mkSelector "redColor"

-- | @Selector@ for @greenColor@
greenColorSelector :: Selector
greenColorSelector = mkSelector "greenColor"

-- | @Selector@ for @blueColor@
blueColorSelector :: Selector
blueColorSelector = mkSelector "blueColor"

-- | @Selector@ for @cyanColor@
cyanColorSelector :: Selector
cyanColorSelector = mkSelector "cyanColor"

-- | @Selector@ for @yellowColor@
yellowColorSelector :: Selector
yellowColorSelector = mkSelector "yellowColor"

-- | @Selector@ for @magentaColor@
magentaColorSelector :: Selector
magentaColorSelector = mkSelector "magentaColor"

-- | @Selector@ for @orangeColor@
orangeColorSelector :: Selector
orangeColorSelector = mkSelector "orangeColor"

-- | @Selector@ for @purpleColor@
purpleColorSelector :: Selector
purpleColorSelector = mkSelector "purpleColor"

-- | @Selector@ for @brownColor@
brownColorSelector :: Selector
brownColorSelector = mkSelector "brownColor"

-- | @Selector@ for @clearColor@
clearColorSelector :: Selector
clearColorSelector = mkSelector "clearColor"

-- | @Selector@ for @labelColor@
labelColorSelector :: Selector
labelColorSelector = mkSelector "labelColor"

-- | @Selector@ for @secondaryLabelColor@
secondaryLabelColorSelector :: Selector
secondaryLabelColorSelector = mkSelector "secondaryLabelColor"

-- | @Selector@ for @tertiaryLabelColor@
tertiaryLabelColorSelector :: Selector
tertiaryLabelColorSelector = mkSelector "tertiaryLabelColor"

-- | @Selector@ for @quaternaryLabelColor@
quaternaryLabelColorSelector :: Selector
quaternaryLabelColorSelector = mkSelector "quaternaryLabelColor"

-- | @Selector@ for @quinaryLabelColor@
quinaryLabelColorSelector :: Selector
quinaryLabelColorSelector = mkSelector "quinaryLabelColor"

-- | @Selector@ for @linkColor@
linkColorSelector :: Selector
linkColorSelector = mkSelector "linkColor"

-- | @Selector@ for @placeholderTextColor@
placeholderTextColorSelector :: Selector
placeholderTextColorSelector = mkSelector "placeholderTextColor"

-- | @Selector@ for @windowFrameTextColor@
windowFrameTextColorSelector :: Selector
windowFrameTextColorSelector = mkSelector "windowFrameTextColor"

-- | @Selector@ for @selectedMenuItemTextColor@
selectedMenuItemTextColorSelector :: Selector
selectedMenuItemTextColorSelector = mkSelector "selectedMenuItemTextColor"

-- | @Selector@ for @alternateSelectedControlTextColor@
alternateSelectedControlTextColorSelector :: Selector
alternateSelectedControlTextColorSelector = mkSelector "alternateSelectedControlTextColor"

-- | @Selector@ for @headerTextColor@
headerTextColorSelector :: Selector
headerTextColorSelector = mkSelector "headerTextColor"

-- | @Selector@ for @separatorColor@
separatorColorSelector :: Selector
separatorColorSelector = mkSelector "separatorColor"

-- | @Selector@ for @gridColor@
gridColorSelector :: Selector
gridColorSelector = mkSelector "gridColor"

-- | @Selector@ for @windowBackgroundColor@
windowBackgroundColorSelector :: Selector
windowBackgroundColorSelector = mkSelector "windowBackgroundColor"

-- | @Selector@ for @underPageBackgroundColor@
underPageBackgroundColorSelector :: Selector
underPageBackgroundColorSelector = mkSelector "underPageBackgroundColor"

-- | @Selector@ for @controlBackgroundColor@
controlBackgroundColorSelector :: Selector
controlBackgroundColorSelector = mkSelector "controlBackgroundColor"

-- | @Selector@ for @selectedContentBackgroundColor@
selectedContentBackgroundColorSelector :: Selector
selectedContentBackgroundColorSelector = mkSelector "selectedContentBackgroundColor"

-- | @Selector@ for @unemphasizedSelectedContentBackgroundColor@
unemphasizedSelectedContentBackgroundColorSelector :: Selector
unemphasizedSelectedContentBackgroundColorSelector = mkSelector "unemphasizedSelectedContentBackgroundColor"

-- | @Selector@ for @alternatingContentBackgroundColors@
alternatingContentBackgroundColorsSelector :: Selector
alternatingContentBackgroundColorsSelector = mkSelector "alternatingContentBackgroundColors"

-- | @Selector@ for @findHighlightColor@
findHighlightColorSelector :: Selector
findHighlightColorSelector = mkSelector "findHighlightColor"

-- | @Selector@ for @textColor@
textColorSelector :: Selector
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @textBackgroundColor@
textBackgroundColorSelector :: Selector
textBackgroundColorSelector = mkSelector "textBackgroundColor"

-- | @Selector@ for @textInsertionPointColor@
textInsertionPointColorSelector :: Selector
textInsertionPointColorSelector = mkSelector "textInsertionPointColor"

-- | @Selector@ for @selectedTextColor@
selectedTextColorSelector :: Selector
selectedTextColorSelector = mkSelector "selectedTextColor"

-- | @Selector@ for @selectedTextBackgroundColor@
selectedTextBackgroundColorSelector :: Selector
selectedTextBackgroundColorSelector = mkSelector "selectedTextBackgroundColor"

-- | @Selector@ for @unemphasizedSelectedTextBackgroundColor@
unemphasizedSelectedTextBackgroundColorSelector :: Selector
unemphasizedSelectedTextBackgroundColorSelector = mkSelector "unemphasizedSelectedTextBackgroundColor"

-- | @Selector@ for @unemphasizedSelectedTextColor@
unemphasizedSelectedTextColorSelector :: Selector
unemphasizedSelectedTextColorSelector = mkSelector "unemphasizedSelectedTextColor"

-- | @Selector@ for @controlColor@
controlColorSelector :: Selector
controlColorSelector = mkSelector "controlColor"

-- | @Selector@ for @controlTextColor@
controlTextColorSelector :: Selector
controlTextColorSelector = mkSelector "controlTextColor"

-- | @Selector@ for @selectedControlColor@
selectedControlColorSelector :: Selector
selectedControlColorSelector = mkSelector "selectedControlColor"

-- | @Selector@ for @selectedControlTextColor@
selectedControlTextColorSelector :: Selector
selectedControlTextColorSelector = mkSelector "selectedControlTextColor"

-- | @Selector@ for @disabledControlTextColor@
disabledControlTextColorSelector :: Selector
disabledControlTextColorSelector = mkSelector "disabledControlTextColor"

-- | @Selector@ for @keyboardFocusIndicatorColor@
keyboardFocusIndicatorColorSelector :: Selector
keyboardFocusIndicatorColorSelector = mkSelector "keyboardFocusIndicatorColor"

-- | @Selector@ for @scrubberTexturedBackgroundColor@
scrubberTexturedBackgroundColorSelector :: Selector
scrubberTexturedBackgroundColorSelector = mkSelector "scrubberTexturedBackgroundColor"

-- | @Selector@ for @systemRedColor@
systemRedColorSelector :: Selector
systemRedColorSelector = mkSelector "systemRedColor"

-- | @Selector@ for @systemGreenColor@
systemGreenColorSelector :: Selector
systemGreenColorSelector = mkSelector "systemGreenColor"

-- | @Selector@ for @systemBlueColor@
systemBlueColorSelector :: Selector
systemBlueColorSelector = mkSelector "systemBlueColor"

-- | @Selector@ for @systemOrangeColor@
systemOrangeColorSelector :: Selector
systemOrangeColorSelector = mkSelector "systemOrangeColor"

-- | @Selector@ for @systemYellowColor@
systemYellowColorSelector :: Selector
systemYellowColorSelector = mkSelector "systemYellowColor"

-- | @Selector@ for @systemBrownColor@
systemBrownColorSelector :: Selector
systemBrownColorSelector = mkSelector "systemBrownColor"

-- | @Selector@ for @systemPinkColor@
systemPinkColorSelector :: Selector
systemPinkColorSelector = mkSelector "systemPinkColor"

-- | @Selector@ for @systemPurpleColor@
systemPurpleColorSelector :: Selector
systemPurpleColorSelector = mkSelector "systemPurpleColor"

-- | @Selector@ for @systemGrayColor@
systemGrayColorSelector :: Selector
systemGrayColorSelector = mkSelector "systemGrayColor"

-- | @Selector@ for @systemTealColor@
systemTealColorSelector :: Selector
systemTealColorSelector = mkSelector "systemTealColor"

-- | @Selector@ for @systemIndigoColor@
systemIndigoColorSelector :: Selector
systemIndigoColorSelector = mkSelector "systemIndigoColor"

-- | @Selector@ for @systemMintColor@
systemMintColorSelector :: Selector
systemMintColorSelector = mkSelector "systemMintColor"

-- | @Selector@ for @systemCyanColor@
systemCyanColorSelector :: Selector
systemCyanColorSelector = mkSelector "systemCyanColor"

-- | @Selector@ for @systemFillColor@
systemFillColorSelector :: Selector
systemFillColorSelector = mkSelector "systemFillColor"

-- | @Selector@ for @secondarySystemFillColor@
secondarySystemFillColorSelector :: Selector
secondarySystemFillColorSelector = mkSelector "secondarySystemFillColor"

-- | @Selector@ for @tertiarySystemFillColor@
tertiarySystemFillColorSelector :: Selector
tertiarySystemFillColorSelector = mkSelector "tertiarySystemFillColor"

-- | @Selector@ for @quaternarySystemFillColor@
quaternarySystemFillColorSelector :: Selector
quaternarySystemFillColorSelector = mkSelector "quaternarySystemFillColor"

-- | @Selector@ for @quinarySystemFillColor@
quinarySystemFillColorSelector :: Selector
quinarySystemFillColorSelector = mkSelector "quinarySystemFillColor"

-- | @Selector@ for @controlAccentColor@
controlAccentColorSelector :: Selector
controlAccentColorSelector = mkSelector "controlAccentColor"

-- | @Selector@ for @currentControlTint@
currentControlTintSelector :: Selector
currentControlTintSelector = mkSelector "currentControlTint"

-- | @Selector@ for @highlightColor@
highlightColorSelector :: Selector
highlightColorSelector = mkSelector "highlightColor"

-- | @Selector@ for @shadowColor@
shadowColorSelector :: Selector
shadowColorSelector = mkSelector "shadowColor"

-- | @Selector@ for @catalogNameComponent@
catalogNameComponentSelector :: Selector
catalogNameComponentSelector = mkSelector "catalogNameComponent"

-- | @Selector@ for @colorNameComponent@
colorNameComponentSelector :: Selector
colorNameComponentSelector = mkSelector "colorNameComponent"

-- | @Selector@ for @localizedCatalogNameComponent@
localizedCatalogNameComponentSelector :: Selector
localizedCatalogNameComponentSelector = mkSelector "localizedCatalogNameComponent"

-- | @Selector@ for @localizedColorNameComponent@
localizedColorNameComponentSelector :: Selector
localizedColorNameComponentSelector = mkSelector "localizedColorNameComponent"

-- | @Selector@ for @redComponent@
redComponentSelector :: Selector
redComponentSelector = mkSelector "redComponent"

-- | @Selector@ for @greenComponent@
greenComponentSelector :: Selector
greenComponentSelector = mkSelector "greenComponent"

-- | @Selector@ for @blueComponent@
blueComponentSelector :: Selector
blueComponentSelector = mkSelector "blueComponent"

-- | @Selector@ for @hueComponent@
hueComponentSelector :: Selector
hueComponentSelector = mkSelector "hueComponent"

-- | @Selector@ for @saturationComponent@
saturationComponentSelector :: Selector
saturationComponentSelector = mkSelector "saturationComponent"

-- | @Selector@ for @brightnessComponent@
brightnessComponentSelector :: Selector
brightnessComponentSelector = mkSelector "brightnessComponent"

-- | @Selector@ for @whiteComponent@
whiteComponentSelector :: Selector
whiteComponentSelector = mkSelector "whiteComponent"

-- | @Selector@ for @cyanComponent@
cyanComponentSelector :: Selector
cyanComponentSelector = mkSelector "cyanComponent"

-- | @Selector@ for @magentaComponent@
magentaComponentSelector :: Selector
magentaComponentSelector = mkSelector "magentaComponent"

-- | @Selector@ for @yellowComponent@
yellowComponentSelector :: Selector
yellowComponentSelector = mkSelector "yellowComponent"

-- | @Selector@ for @blackComponent@
blackComponentSelector :: Selector
blackComponentSelector = mkSelector "blackComponent"

-- | @Selector@ for @colorSpace@
colorSpaceSelector :: Selector
colorSpaceSelector = mkSelector "colorSpace"

-- | @Selector@ for @numberOfComponents@
numberOfComponentsSelector :: Selector
numberOfComponentsSelector = mkSelector "numberOfComponents"

-- | @Selector@ for @patternImage@
patternImageSelector :: Selector
patternImageSelector = mkSelector "patternImage"

-- | @Selector@ for @alphaComponent@
alphaComponentSelector :: Selector
alphaComponentSelector = mkSelector "alphaComponent"

-- | @Selector@ for @linearExposure@
linearExposureSelector :: Selector
linearExposureSelector = mkSelector "linearExposure"

-- | @Selector@ for @CGColor@
cgColorSelector :: Selector
cgColorSelector = mkSelector "CGColor"

-- | @Selector@ for @ignoresAlpha@
ignoresAlphaSelector :: Selector
ignoresAlphaSelector = mkSelector "ignoresAlpha"

-- | @Selector@ for @setIgnoresAlpha:@
setIgnoresAlphaSelector :: Selector
setIgnoresAlphaSelector = mkSelector "setIgnoresAlpha:"

-- | @Selector@ for @controlHighlightColor@
controlHighlightColorSelector :: Selector
controlHighlightColorSelector = mkSelector "controlHighlightColor"

-- | @Selector@ for @controlLightHighlightColor@
controlLightHighlightColorSelector :: Selector
controlLightHighlightColorSelector = mkSelector "controlLightHighlightColor"

-- | @Selector@ for @controlShadowColor@
controlShadowColorSelector :: Selector
controlShadowColorSelector = mkSelector "controlShadowColor"

-- | @Selector@ for @controlDarkShadowColor@
controlDarkShadowColorSelector :: Selector
controlDarkShadowColorSelector = mkSelector "controlDarkShadowColor"

-- | @Selector@ for @scrollBarColor@
scrollBarColorSelector :: Selector
scrollBarColorSelector = mkSelector "scrollBarColor"

-- | @Selector@ for @knobColor@
knobColorSelector :: Selector
knobColorSelector = mkSelector "knobColor"

-- | @Selector@ for @selectedKnobColor@
selectedKnobColorSelector :: Selector
selectedKnobColorSelector = mkSelector "selectedKnobColor"

-- | @Selector@ for @windowFrameColor@
windowFrameColorSelector :: Selector
windowFrameColorSelector = mkSelector "windowFrameColor"

-- | @Selector@ for @selectedMenuItemColor@
selectedMenuItemColorSelector :: Selector
selectedMenuItemColorSelector = mkSelector "selectedMenuItemColor"

-- | @Selector@ for @headerColor@
headerColorSelector :: Selector
headerColorSelector = mkSelector "headerColor"

-- | @Selector@ for @secondarySelectedControlColor@
secondarySelectedControlColorSelector :: Selector
secondarySelectedControlColorSelector = mkSelector "secondarySelectedControlColor"

-- | @Selector@ for @alternateSelectedControlColor@
alternateSelectedControlColorSelector :: Selector
alternateSelectedControlColorSelector = mkSelector "alternateSelectedControlColor"

-- | @Selector@ for @controlAlternatingRowBackgroundColors@
controlAlternatingRowBackgroundColorsSelector :: Selector
controlAlternatingRowBackgroundColorsSelector = mkSelector "controlAlternatingRowBackgroundColors"

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector
colorSpaceNameSelector = mkSelector "colorSpaceName"

