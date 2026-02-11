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
  , windowFrameTextColor
  , selectedMenuItemTextColor
  , alternateSelectedControlTextColor
  , headerTextColor
  , gridColor
  , windowBackgroundColor
  , controlBackgroundColor
  , textColor
  , textBackgroundColor
  , selectedTextColor
  , selectedTextBackgroundColor
  , controlColor
  , controlTextColor
  , selectedControlColor
  , selectedControlTextColor
  , disabledControlTextColor
  , keyboardFocusIndicatorColor
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
  , windowFrameTextColorSelector
  , selectedMenuItemTextColorSelector
  , alternateSelectedControlTextColorSelector
  , headerTextColorSelector
  , gridColorSelector
  , windowBackgroundColorSelector
  , controlBackgroundColorSelector
  , textColorSelector
  , textBackgroundColorSelector
  , selectedTextColorSelector
  , selectedTextBackgroundColorSelector
  , controlColorSelector
  , controlTextColorSelector
  , selectedControlColorSelector
  , selectedControlTextColorSelector
  , disabledControlTextColorSelector
  , keyboardFocusIndicatorColorSelector
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
      sendClassMsg cls' (mkSelector "colorWithColorSpace:components:count:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ()), argPtr (unConst components), argCLong (fromIntegral numberOfComponents)] >>= retainedObject . castPtr

-- | @+ colorWithSRGBRed:green:blue:alpha:@
colorWithSRGBRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithSRGBRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithSRGBRed:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithGenericGamma22White:alpha:@
colorWithGenericGamma22White_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithGenericGamma22White_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithGenericGamma22White:alpha:") (retPtr retVoid) [argCDouble (fromIntegral white), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithDisplayP3Red:green:blue:alpha:@
colorWithDisplayP3Red_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDisplayP3Red_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDisplayP3Red:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithWhite:alpha:@
colorWithWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithWhite:alpha:") (retPtr retVoid) [argCDouble (fromIntegral white), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithRed:green:blue:alpha:@
colorWithRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithHue:saturation:brightness:alpha:@
colorWithHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithHue:saturation:brightness:alpha:") (retPtr retVoid) [argCDouble (fromIntegral hue), argCDouble (fromIntegral saturation), argCDouble (fromIntegral brightness), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithColorSpace:hue:saturation:brightness:alpha:@
colorWithColorSpace_hue_saturation_brightness_alpha :: IsNSColorSpace space => space -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithColorSpace_hue_saturation_brightness_alpha space hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    withObjCPtr space $ \raw_space ->
      sendClassMsg cls' (mkSelector "colorWithColorSpace:hue:saturation:brightness:alpha:") (retPtr retVoid) [argPtr (castPtr raw_space :: Ptr ()), argCDouble (fromIntegral hue), argCDouble (fromIntegral saturation), argCDouble (fromIntegral brightness), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "colorWithDeviceWhite:alpha:") (retPtr retVoid) [argCDouble (fromIntegral white), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithDeviceRed:green:blue:alpha:@
colorWithDeviceRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceRed:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithDeviceHue:saturation:brightness:alpha:@
colorWithDeviceHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceHue:saturation:brightness:alpha:") (retPtr retVoid) [argCDouble (fromIntegral hue), argCDouble (fromIntegral saturation), argCDouble (fromIntegral brightness), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithDeviceCyan:magenta:yellow:black:alpha:@
colorWithDeviceCyan_magenta_yellow_black_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithDeviceCyan_magenta_yellow_black_alpha cyan magenta yellow black alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithDeviceCyan:magenta:yellow:black:alpha:") (retPtr retVoid) [argCDouble (fromIntegral cyan), argCDouble (fromIntegral magenta), argCDouble (fromIntegral yellow), argCDouble (fromIntegral black), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithCalibratedWhite:alpha:@
colorWithCalibratedWhite_alpha :: CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedWhite_alpha white alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCalibratedWhite:alpha:") (retPtr retVoid) [argCDouble (fromIntegral white), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithCalibratedRed:green:blue:alpha:@
colorWithCalibratedRed_green_blue_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedRed_green_blue_alpha red green blue alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCalibratedRed:green:blue:alpha:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

-- | @+ colorWithCalibratedHue:saturation:brightness:alpha:@
colorWithCalibratedHue_saturation_brightness_alpha :: CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithCalibratedHue_saturation_brightness_alpha hue saturation brightness alpha =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithCalibratedHue:saturation:brightness:alpha:") (retPtr retVoid) [argCDouble (fromIntegral hue), argCDouble (fromIntegral saturation), argCDouble (fromIntegral brightness), argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:exposure:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha), argCDouble (fromIntegral exposure)] >>= retainedObject . castPtr

-- | Generates an HDR color in the extended sRGB colorspace by applying an exposure to the SDR color defined by the red, green, and blue components. The @red@, @green@, and @blue@ components have a nominal range of [0..1], @linearExposure@ is a value >= 1. To produce an HDR color, we process the given color in a linear color space, multiplying component values by @linearExposure @. The produced color will have a @contentHeadroom@ equal to @linearExposure@. Each doubling of @linearExposure@ produces a color that is twice as bright.
--
-- ObjC selector: @+ colorWithRed:green:blue:alpha:linearExposure:@
colorWithRed_green_blue_alpha_linearExposure :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO (Id NSColor)
colorWithRed_green_blue_alpha_linearExposure red green blue alpha linearExposure =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorWithRed:green:blue:alpha:linearExposure:") (retPtr retVoid) [argCDouble (fromIntegral red), argCDouble (fromIntegral green), argCDouble (fromIntegral blue), argCDouble (fromIntegral alpha), argCDouble (fromIntegral linearExposure)] >>= retainedObject . castPtr

-- | Reinterpret the color by applying a new @contentHeadroom@ without changing the color components. Changing the @contentHeadroom@ redefines the color relative to a different peak white, changing its behavior under tone mapping and the result of calling @standardDynamicRangeColor@. The new color will have a @contentHeadroom@ >= 1.0. If called on a color with a color space that does not support extended range, or does not have an equivalent extended range counterpart, this will return @self@.
--
-- ObjC selector: @- colorByApplyingContentHeadroom:@
colorByApplyingContentHeadroom :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
colorByApplyingContentHeadroom nsColor  contentHeadroom =
  sendMsg nsColor (mkSelector "colorByApplyingContentHeadroom:") (retPtr retVoid) [argCDouble (fromIntegral contentHeadroom)] >>= retainedObject . castPtr

-- | @+ colorForControlTint:@
colorForControlTint :: NSControlTint -> IO (Id NSColor)
colorForControlTint controlTint =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "colorForControlTint:") (retPtr retVoid) [argCULong (coerce controlTint)] >>= retainedObject . castPtr

-- | @- highlightWithLevel:@
highlightWithLevel :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
highlightWithLevel nsColor  val =
  sendMsg nsColor (mkSelector "highlightWithLevel:") (retPtr retVoid) [argCDouble (fromIntegral val)] >>= retainedObject . castPtr

-- | @- shadowWithLevel:@
shadowWithLevel :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
shadowWithLevel nsColor  val =
  sendMsg nsColor (mkSelector "shadowWithLevel:") (retPtr retVoid) [argCDouble (fromIntegral val)] >>= retainedObject . castPtr

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
    sendMsg nsColor (mkSelector "blendedColorWithFraction:ofColor:") (retPtr retVoid) [argCDouble (fromIntegral fraction), argPtr (castPtr raw_color :: Ptr ())] >>= retainedObject . castPtr

-- | @- colorWithAlphaComponent:@
colorWithAlphaComponent :: IsNSColor nsColor => nsColor -> CDouble -> IO (Id NSColor)
colorWithAlphaComponent nsColor  alpha =
  sendMsg nsColor (mkSelector "colorWithAlphaComponent:") (retPtr retVoid) [argCDouble (fromIntegral alpha)] >>= retainedObject . castPtr

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

-- | @+ controlBackgroundColor@
controlBackgroundColor :: IO (Id NSColor)
controlBackgroundColor  =
  do
    cls' <- getRequiredClass "NSColor"
    sendClassMsg cls' (mkSelector "controlBackgroundColor") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @gridColor@
gridColorSelector :: Selector
gridColorSelector = mkSelector "gridColor"

-- | @Selector@ for @windowBackgroundColor@
windowBackgroundColorSelector :: Selector
windowBackgroundColorSelector = mkSelector "windowBackgroundColor"

-- | @Selector@ for @controlBackgroundColor@
controlBackgroundColorSelector :: Selector
controlBackgroundColorSelector = mkSelector "controlBackgroundColor"

-- | @Selector@ for @textColor@
textColorSelector :: Selector
textColorSelector = mkSelector "textColor"

-- | @Selector@ for @textBackgroundColor@
textBackgroundColorSelector :: Selector
textBackgroundColorSelector = mkSelector "textBackgroundColor"

-- | @Selector@ for @selectedTextColor@
selectedTextColorSelector :: Selector
selectedTextColorSelector = mkSelector "selectedTextColor"

-- | @Selector@ for @selectedTextBackgroundColor@
selectedTextBackgroundColorSelector :: Selector
selectedTextBackgroundColorSelector = mkSelector "selectedTextBackgroundColor"

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

-- | @Selector@ for @colorSpaceName@
colorSpaceNameSelector :: Selector
colorSpaceNameSelector = mkSelector "colorSpaceName"

