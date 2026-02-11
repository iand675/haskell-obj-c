{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFont@.
module ObjC.AppKit.NSFont
  ( NSFont
  , IsNSFont(..)
  , fontWithName_size
  , fontWithName_matrix
  , fontWithDescriptor_size
  , fontWithDescriptor_textTransform
  , userFontOfSize
  , userFixedPitchFontOfSize
  , setUserFont
  , setUserFixedPitchFont
  , systemFontOfSize
  , boldSystemFontOfSize
  , labelFontOfSize
  , titleBarFontOfSize
  , menuFontOfSize
  , menuBarFontOfSize
  , messageFontOfSize
  , paletteFontOfSize
  , toolTipsFontOfSize
  , controlContentFontOfSize
  , systemFontOfSize_weight
  , monospacedDigitSystemFontOfSize_weight
  , systemFontOfSize_weight_width
  , monospacedSystemFontOfSize_weight
  , fontWithSize
  , systemFontSizeForControlSize
  , boundingRectForCGGlyph
  , advancementForCGGlyph
  , set
  , setInContext
  , preferredFontForTextStyle_options
  , glyphWithName
  , boundingRectForGlyph
  , advancementForGlyph
  , getAdvancements_forPackedGlyphs_length
  , screenFontWithRenderingMode
  , systemFontSize
  , smallSystemFontSize
  , labelFontSize
  , fontName
  , pointSize
  , familyName
  , displayName
  , fontDescriptor
  , textTransform
  , numberOfGlyphs
  , mostCompatibleStringEncoding
  , coveredCharacterSet
  , boundingRectForFont
  , maximumAdvancement
  , ascender
  , descender
  , leading
  , underlinePosition
  , underlineThickness
  , italicAngle
  , capHeight
  , xHeight
  , fixedPitch
  , vertical
  , printerFont
  , screenFont
  , renderingMode
  , fontWithName_sizeSelector
  , fontWithName_matrixSelector
  , fontWithDescriptor_sizeSelector
  , fontWithDescriptor_textTransformSelector
  , userFontOfSizeSelector
  , userFixedPitchFontOfSizeSelector
  , setUserFontSelector
  , setUserFixedPitchFontSelector
  , systemFontOfSizeSelector
  , boldSystemFontOfSizeSelector
  , labelFontOfSizeSelector
  , titleBarFontOfSizeSelector
  , menuFontOfSizeSelector
  , menuBarFontOfSizeSelector
  , messageFontOfSizeSelector
  , paletteFontOfSizeSelector
  , toolTipsFontOfSizeSelector
  , controlContentFontOfSizeSelector
  , systemFontOfSize_weightSelector
  , monospacedDigitSystemFontOfSize_weightSelector
  , systemFontOfSize_weight_widthSelector
  , monospacedSystemFontOfSize_weightSelector
  , fontWithSizeSelector
  , systemFontSizeForControlSizeSelector
  , boundingRectForCGGlyphSelector
  , advancementForCGGlyphSelector
  , setSelector
  , setInContextSelector
  , preferredFontForTextStyle_optionsSelector
  , glyphWithNameSelector
  , boundingRectForGlyphSelector
  , advancementForGlyphSelector
  , getAdvancements_forPackedGlyphs_lengthSelector
  , screenFontWithRenderingModeSelector
  , systemFontSizeSelector
  , smallSystemFontSizeSelector
  , labelFontSizeSelector
  , fontNameSelector
  , pointSizeSelector
  , familyNameSelector
  , displayNameSelector
  , fontDescriptorSelector
  , textTransformSelector
  , numberOfGlyphsSelector
  , mostCompatibleStringEncodingSelector
  , coveredCharacterSetSelector
  , boundingRectForFontSelector
  , maximumAdvancementSelector
  , ascenderSelector
  , descenderSelector
  , leadingSelector
  , underlinePositionSelector
  , underlineThicknessSelector
  , italicAngleSelector
  , capHeightSelector
  , xHeightSelector
  , fixedPitchSelector
  , verticalSelector
  , printerFontSelector
  , screenFontSelector
  , renderingModeSelector

  -- * Enum types
  , NSControlSize(NSControlSize)
  , pattern NSControlSizeRegular
  , pattern NSControlSizeSmall
  , pattern NSControlSizeMini
  , pattern NSControlSizeLarge
  , pattern NSControlSizeExtraLarge
  , NSFontRenderingMode(NSFontRenderingMode)
  , pattern NSFontDefaultRenderingMode
  , pattern NSFontAntialiasedRenderingMode
  , pattern NSFontIntegerAdvancementsRenderingMode
  , pattern NSFontAntialiasedIntegerAdvancementsRenderingMode

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

-- | ******* Factory ********
--
-- ObjC selector: @+ fontWithName:size:@
fontWithName_size :: IsNSString fontName => fontName -> CDouble -> IO (Id NSFont)
fontWithName_size fontName fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr fontName $ \raw_fontName ->
      sendClassMsg cls' (mkSelector "fontWithName:size:") (retPtr retVoid) [argPtr (castPtr raw_fontName :: Ptr ()), argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ fontWithName:matrix:@
fontWithName_matrix :: IsNSString fontName => fontName -> Const (Ptr CDouble) -> IO (Id NSFont)
fontWithName_matrix fontName fontMatrix =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr fontName $ \raw_fontName ->
      sendClassMsg cls' (mkSelector "fontWithName:matrix:") (retPtr retVoid) [argPtr (castPtr raw_fontName :: Ptr ()), argPtr (unConst fontMatrix)] >>= retainedObject . castPtr

-- | @+ fontWithDescriptor:size:@
fontWithDescriptor_size :: IsNSFontDescriptor fontDescriptor => fontDescriptor -> CDouble -> IO (Id NSFont)
fontWithDescriptor_size fontDescriptor fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr fontDescriptor $ \raw_fontDescriptor ->
      sendClassMsg cls' (mkSelector "fontWithDescriptor:size:") (retPtr retVoid) [argPtr (castPtr raw_fontDescriptor :: Ptr ()), argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ fontWithDescriptor:textTransform:@
fontWithDescriptor_textTransform :: (IsNSFontDescriptor fontDescriptor, IsNSAffineTransform textTransform) => fontDescriptor -> textTransform -> IO (Id NSFont)
fontWithDescriptor_textTransform fontDescriptor textTransform =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr fontDescriptor $ \raw_fontDescriptor ->
      withObjCPtr textTransform $ \raw_textTransform ->
        sendClassMsg cls' (mkSelector "fontWithDescriptor:textTransform:") (retPtr retVoid) [argPtr (castPtr raw_fontDescriptor :: Ptr ()), argPtr (castPtr raw_textTransform :: Ptr ())] >>= retainedObject . castPtr

-- | ******* Meta Font ********
--
-- ObjC selector: @+ userFontOfSize:@
userFontOfSize :: CDouble -> IO (Id NSFont)
userFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "userFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ userFixedPitchFontOfSize:@
userFixedPitchFontOfSize :: CDouble -> IO (Id NSFont)
userFixedPitchFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "userFixedPitchFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ setUserFont:@
setUserFont :: IsNSFont font => font -> IO ()
setUserFont font =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr font $ \raw_font ->
      sendClassMsg cls' (mkSelector "setUserFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @+ setUserFixedPitchFont:@
setUserFixedPitchFont :: IsNSFont font => font -> IO ()
setUserFixedPitchFont font =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr font $ \raw_font ->
      sendClassMsg cls' (mkSelector "setUserFixedPitchFont:") retVoid [argPtr (castPtr raw_font :: Ptr ())]

-- | @+ systemFontOfSize:@
systemFontOfSize :: CDouble -> IO (Id NSFont)
systemFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "systemFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ boldSystemFontOfSize:@
boldSystemFontOfSize :: CDouble -> IO (Id NSFont)
boldSystemFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "boldSystemFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ labelFontOfSize:@
labelFontOfSize :: CDouble -> IO (Id NSFont)
labelFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "labelFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ titleBarFontOfSize:@
titleBarFontOfSize :: CDouble -> IO (Id NSFont)
titleBarFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "titleBarFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ menuFontOfSize:@
menuFontOfSize :: CDouble -> IO (Id NSFont)
menuFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "menuFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ menuBarFontOfSize:@
menuBarFontOfSize :: CDouble -> IO (Id NSFont)
menuBarFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "menuBarFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ messageFontOfSize:@
messageFontOfSize :: CDouble -> IO (Id NSFont)
messageFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "messageFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ paletteFontOfSize:@
paletteFontOfSize :: CDouble -> IO (Id NSFont)
paletteFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "paletteFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ toolTipsFontOfSize:@
toolTipsFontOfSize :: CDouble -> IO (Id NSFont)
toolTipsFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "toolTipsFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ controlContentFontOfSize:@
controlContentFontOfSize :: CDouble -> IO (Id NSFont)
controlContentFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "controlContentFontOfSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ systemFontOfSize:weight:@
systemFontOfSize_weight :: CDouble -> CDouble -> IO (Id NSFont)
systemFontOfSize_weight fontSize weight =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "systemFontOfSize:weight:") (retPtr retVoid) [argCDouble (fromIntegral fontSize), argCDouble (fromIntegral weight)] >>= retainedObject . castPtr

-- | @+ monospacedDigitSystemFontOfSize:weight:@
monospacedDigitSystemFontOfSize_weight :: CDouble -> CDouble -> IO (Id NSFont)
monospacedDigitSystemFontOfSize_weight fontSize weight =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "monospacedDigitSystemFontOfSize:weight:") (retPtr retVoid) [argCDouble (fromIntegral fontSize), argCDouble (fromIntegral weight)] >>= retainedObject . castPtr

-- | @+ systemFontOfSize:weight:width:@
systemFontOfSize_weight_width :: CDouble -> CDouble -> CDouble -> IO (Id NSFont)
systemFontOfSize_weight_width fontSize weight width =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "systemFontOfSize:weight:width:") (retPtr retVoid) [argCDouble (fromIntegral fontSize), argCDouble (fromIntegral weight), argCDouble (fromIntegral width)] >>= retainedObject . castPtr

-- | @+ monospacedSystemFontOfSize:weight:@
monospacedSystemFontOfSize_weight :: CDouble -> CDouble -> IO (Id NSFont)
monospacedSystemFontOfSize_weight fontSize weight =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "monospacedSystemFontOfSize:weight:") (retPtr retVoid) [argCDouble (fromIntegral fontSize), argCDouble (fromIntegral weight)] >>= retainedObject . castPtr

-- | @- fontWithSize:@
fontWithSize :: IsNSFont nsFont => nsFont -> CDouble -> IO (Id NSFont)
fontWithSize nsFont  fontSize =
  sendMsg nsFont (mkSelector "fontWithSize:") (retPtr retVoid) [argCDouble (fromIntegral fontSize)] >>= retainedObject . castPtr

-- | @+ systemFontSizeForControlSize:@
systemFontSizeForControlSize :: NSControlSize -> IO CDouble
systemFontSizeForControlSize controlSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "systemFontSizeForControlSize:") retCDouble [argCULong (coerce controlSize)]

-- | ******* Glyph metrics ********
--
-- ******* Glyph metrics ********
--
-- ObjC selector: @- boundingRectForCGGlyph:@
boundingRectForCGGlyph :: IsNSFont nsFont => nsFont -> CUShort -> IO NSRect
boundingRectForCGGlyph nsFont  glyph =
  sendMsgStret nsFont (mkSelector "boundingRectForCGGlyph:") retNSRect [argCUInt (fromIntegral glyph)]

-- | @- advancementForCGGlyph:@
advancementForCGGlyph :: IsNSFont nsFont => nsFont -> CUShort -> IO NSSize
advancementForCGGlyph nsFont  glyph =
  sendMsgStret nsFont (mkSelector "advancementForCGGlyph:") retNSSize [argCUInt (fromIntegral glyph)]

-- | ******* NSGraphicsContext-related ********
--
-- ObjC selector: @- set@
set :: IsNSFont nsFont => nsFont -> IO ()
set nsFont  =
  sendMsg nsFont (mkSelector "set") retVoid []

-- | @- setInContext:@
setInContext :: (IsNSFont nsFont, IsNSGraphicsContext graphicsContext) => nsFont -> graphicsContext -> IO ()
setInContext nsFont  graphicsContext =
withObjCPtr graphicsContext $ \raw_graphicsContext ->
    sendMsg nsFont (mkSelector "setInContext:") retVoid [argPtr (castPtr raw_graphicsContext :: Ptr ())]

-- | @+ preferredFontForTextStyle:options:@
preferredFontForTextStyle_options :: (IsNSString style, IsNSDictionary options) => style -> options -> IO (Id NSFont)
preferredFontForTextStyle_options style options =
  do
    cls' <- getRequiredClass "NSFont"
    withObjCPtr style $ \raw_style ->
      withObjCPtr options $ \raw_options ->
        sendClassMsg cls' (mkSelector "preferredFontForTextStyle:options:") (retPtr retVoid) [argPtr (castPtr raw_style :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- glyphWithName:@
glyphWithName :: (IsNSFont nsFont, IsNSString name) => nsFont -> name -> IO CUInt
glyphWithName nsFont  name =
withObjCPtr name $ \raw_name ->
    sendMsg nsFont (mkSelector "glyphWithName:") retCUInt [argPtr (castPtr raw_name :: Ptr ())]

-- | @- boundingRectForGlyph:@
boundingRectForGlyph :: IsNSFont nsFont => nsFont -> CUInt -> IO NSRect
boundingRectForGlyph nsFont  glyph =
  sendMsgStret nsFont (mkSelector "boundingRectForGlyph:") retNSRect [argCUInt (fromIntegral glyph)]

-- | @- advancementForGlyph:@
advancementForGlyph :: IsNSFont nsFont => nsFont -> CUInt -> IO NSSize
advancementForGlyph nsFont  glyph =
  sendMsgStret nsFont (mkSelector "advancementForGlyph:") retNSSize [argCUInt (fromIntegral glyph)]

-- | @- getAdvancements:forPackedGlyphs:length:@
getAdvancements_forPackedGlyphs_length :: IsNSFont nsFont => nsFont -> Ptr NSSize -> Const (Ptr ()) -> CULong -> IO ()
getAdvancements_forPackedGlyphs_length nsFont  advancements packedGlyphs length_ =
  sendMsg nsFont (mkSelector "getAdvancements:forPackedGlyphs:length:") retVoid [argPtr advancements, argPtr (unConst packedGlyphs), argCULong (fromIntegral length_)]

-- | @- screenFontWithRenderingMode:@
screenFontWithRenderingMode :: IsNSFont nsFont => nsFont -> NSFontRenderingMode -> IO (Id NSFont)
screenFontWithRenderingMode nsFont  renderingMode =
  sendMsg nsFont (mkSelector "screenFontWithRenderingMode:") (retPtr retVoid) [argCULong (coerce renderingMode)] >>= retainedObject . castPtr

-- | @+ systemFontSize@
systemFontSize :: IO CDouble
systemFontSize  =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "systemFontSize") retCDouble []

-- | @+ smallSystemFontSize@
smallSystemFontSize :: IO CDouble
smallSystemFontSize  =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "smallSystemFontSize") retCDouble []

-- | @+ labelFontSize@
labelFontSize :: IO CDouble
labelFontSize  =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMsg cls' (mkSelector "labelFontSize") retCDouble []

-- | ******* Core font attribute ********
--
-- ObjC selector: @- fontName@
fontName :: IsNSFont nsFont => nsFont -> IO (Id NSString)
fontName nsFont  =
  sendMsg nsFont (mkSelector "fontName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pointSize@
pointSize :: IsNSFont nsFont => nsFont -> IO CDouble
pointSize nsFont  =
  sendMsg nsFont (mkSelector "pointSize") retCDouble []

-- | @- familyName@
familyName :: IsNSFont nsFont => nsFont -> IO (Id NSString)
familyName nsFont  =
  sendMsg nsFont (mkSelector "familyName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- displayName@
displayName :: IsNSFont nsFont => nsFont -> IO (Id NSString)
displayName nsFont  =
  sendMsg nsFont (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- fontDescriptor@
fontDescriptor :: IsNSFont nsFont => nsFont -> IO (Id NSFontDescriptor)
fontDescriptor nsFont  =
  sendMsg nsFont (mkSelector "fontDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- textTransform@
textTransform :: IsNSFont nsFont => nsFont -> IO (Id NSAffineTransform)
textTransform nsFont  =
  sendMsg nsFont (mkSelector "textTransform") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ******* Glyph coverage ********
--
-- ObjC selector: @- numberOfGlyphs@
numberOfGlyphs :: IsNSFont nsFont => nsFont -> IO CULong
numberOfGlyphs nsFont  =
  sendMsg nsFont (mkSelector "numberOfGlyphs") retCULong []

-- | @- mostCompatibleStringEncoding@
mostCompatibleStringEncoding :: IsNSFont nsFont => nsFont -> IO CULong
mostCompatibleStringEncoding nsFont  =
  sendMsg nsFont (mkSelector "mostCompatibleStringEncoding") retCULong []

-- | @- coveredCharacterSet@
coveredCharacterSet :: IsNSFont nsFont => nsFont -> IO (Id NSCharacterSet)
coveredCharacterSet nsFont  =
  sendMsg nsFont (mkSelector "coveredCharacterSet") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- boundingRectForFont@
boundingRectForFont :: IsNSFont nsFont => nsFont -> IO NSRect
boundingRectForFont nsFont  =
  sendMsgStret nsFont (mkSelector "boundingRectForFont") retNSRect []

-- | @- maximumAdvancement@
maximumAdvancement :: IsNSFont nsFont => nsFont -> IO NSSize
maximumAdvancement nsFont  =
  sendMsgStret nsFont (mkSelector "maximumAdvancement") retNSSize []

-- | @- ascender@
ascender :: IsNSFont nsFont => nsFont -> IO CDouble
ascender nsFont  =
  sendMsg nsFont (mkSelector "ascender") retCDouble []

-- | @- descender@
descender :: IsNSFont nsFont => nsFont -> IO CDouble
descender nsFont  =
  sendMsg nsFont (mkSelector "descender") retCDouble []

-- | @- leading@
leading :: IsNSFont nsFont => nsFont -> IO CDouble
leading nsFont  =
  sendMsg nsFont (mkSelector "leading") retCDouble []

-- | @- underlinePosition@
underlinePosition :: IsNSFont nsFont => nsFont -> IO CDouble
underlinePosition nsFont  =
  sendMsg nsFont (mkSelector "underlinePosition") retCDouble []

-- | @- underlineThickness@
underlineThickness :: IsNSFont nsFont => nsFont -> IO CDouble
underlineThickness nsFont  =
  sendMsg nsFont (mkSelector "underlineThickness") retCDouble []

-- | @- italicAngle@
italicAngle :: IsNSFont nsFont => nsFont -> IO CDouble
italicAngle nsFont  =
  sendMsg nsFont (mkSelector "italicAngle") retCDouble []

-- | @- capHeight@
capHeight :: IsNSFont nsFont => nsFont -> IO CDouble
capHeight nsFont  =
  sendMsg nsFont (mkSelector "capHeight") retCDouble []

-- | @- xHeight@
xHeight :: IsNSFont nsFont => nsFont -> IO CDouble
xHeight nsFont  =
  sendMsg nsFont (mkSelector "xHeight") retCDouble []

-- | @- fixedPitch@
fixedPitch :: IsNSFont nsFont => nsFont -> IO Bool
fixedPitch nsFont  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFont (mkSelector "fixedPitch") retCULong []

-- | @- vertical@
vertical :: IsNSFont nsFont => nsFont -> IO Bool
vertical nsFont  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFont (mkSelector "vertical") retCULong []

-- | ******* Rendering mode ********
--
-- ObjC selector: @- printerFont@
printerFont :: IsNSFont nsFont => nsFont -> IO (Id NSFont)
printerFont nsFont  =
  sendMsg nsFont (mkSelector "printerFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- screenFont@
screenFont :: IsNSFont nsFont => nsFont -> IO (Id NSFont)
screenFont nsFont  =
  sendMsg nsFont (mkSelector "screenFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- renderingMode@
renderingMode :: IsNSFont nsFont => nsFont -> IO NSFontRenderingMode
renderingMode nsFont  =
  fmap (coerce :: CULong -> NSFontRenderingMode) $ sendMsg nsFont (mkSelector "renderingMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fontWithName:size:@
fontWithName_sizeSelector :: Selector
fontWithName_sizeSelector = mkSelector "fontWithName:size:"

-- | @Selector@ for @fontWithName:matrix:@
fontWithName_matrixSelector :: Selector
fontWithName_matrixSelector = mkSelector "fontWithName:matrix:"

-- | @Selector@ for @fontWithDescriptor:size:@
fontWithDescriptor_sizeSelector :: Selector
fontWithDescriptor_sizeSelector = mkSelector "fontWithDescriptor:size:"

-- | @Selector@ for @fontWithDescriptor:textTransform:@
fontWithDescriptor_textTransformSelector :: Selector
fontWithDescriptor_textTransformSelector = mkSelector "fontWithDescriptor:textTransform:"

-- | @Selector@ for @userFontOfSize:@
userFontOfSizeSelector :: Selector
userFontOfSizeSelector = mkSelector "userFontOfSize:"

-- | @Selector@ for @userFixedPitchFontOfSize:@
userFixedPitchFontOfSizeSelector :: Selector
userFixedPitchFontOfSizeSelector = mkSelector "userFixedPitchFontOfSize:"

-- | @Selector@ for @setUserFont:@
setUserFontSelector :: Selector
setUserFontSelector = mkSelector "setUserFont:"

-- | @Selector@ for @setUserFixedPitchFont:@
setUserFixedPitchFontSelector :: Selector
setUserFixedPitchFontSelector = mkSelector "setUserFixedPitchFont:"

-- | @Selector@ for @systemFontOfSize:@
systemFontOfSizeSelector :: Selector
systemFontOfSizeSelector = mkSelector "systemFontOfSize:"

-- | @Selector@ for @boldSystemFontOfSize:@
boldSystemFontOfSizeSelector :: Selector
boldSystemFontOfSizeSelector = mkSelector "boldSystemFontOfSize:"

-- | @Selector@ for @labelFontOfSize:@
labelFontOfSizeSelector :: Selector
labelFontOfSizeSelector = mkSelector "labelFontOfSize:"

-- | @Selector@ for @titleBarFontOfSize:@
titleBarFontOfSizeSelector :: Selector
titleBarFontOfSizeSelector = mkSelector "titleBarFontOfSize:"

-- | @Selector@ for @menuFontOfSize:@
menuFontOfSizeSelector :: Selector
menuFontOfSizeSelector = mkSelector "menuFontOfSize:"

-- | @Selector@ for @menuBarFontOfSize:@
menuBarFontOfSizeSelector :: Selector
menuBarFontOfSizeSelector = mkSelector "menuBarFontOfSize:"

-- | @Selector@ for @messageFontOfSize:@
messageFontOfSizeSelector :: Selector
messageFontOfSizeSelector = mkSelector "messageFontOfSize:"

-- | @Selector@ for @paletteFontOfSize:@
paletteFontOfSizeSelector :: Selector
paletteFontOfSizeSelector = mkSelector "paletteFontOfSize:"

-- | @Selector@ for @toolTipsFontOfSize:@
toolTipsFontOfSizeSelector :: Selector
toolTipsFontOfSizeSelector = mkSelector "toolTipsFontOfSize:"

-- | @Selector@ for @controlContentFontOfSize:@
controlContentFontOfSizeSelector :: Selector
controlContentFontOfSizeSelector = mkSelector "controlContentFontOfSize:"

-- | @Selector@ for @systemFontOfSize:weight:@
systemFontOfSize_weightSelector :: Selector
systemFontOfSize_weightSelector = mkSelector "systemFontOfSize:weight:"

-- | @Selector@ for @monospacedDigitSystemFontOfSize:weight:@
monospacedDigitSystemFontOfSize_weightSelector :: Selector
monospacedDigitSystemFontOfSize_weightSelector = mkSelector "monospacedDigitSystemFontOfSize:weight:"

-- | @Selector@ for @systemFontOfSize:weight:width:@
systemFontOfSize_weight_widthSelector :: Selector
systemFontOfSize_weight_widthSelector = mkSelector "systemFontOfSize:weight:width:"

-- | @Selector@ for @monospacedSystemFontOfSize:weight:@
monospacedSystemFontOfSize_weightSelector :: Selector
monospacedSystemFontOfSize_weightSelector = mkSelector "monospacedSystemFontOfSize:weight:"

-- | @Selector@ for @fontWithSize:@
fontWithSizeSelector :: Selector
fontWithSizeSelector = mkSelector "fontWithSize:"

-- | @Selector@ for @systemFontSizeForControlSize:@
systemFontSizeForControlSizeSelector :: Selector
systemFontSizeForControlSizeSelector = mkSelector "systemFontSizeForControlSize:"

-- | @Selector@ for @boundingRectForCGGlyph:@
boundingRectForCGGlyphSelector :: Selector
boundingRectForCGGlyphSelector = mkSelector "boundingRectForCGGlyph:"

-- | @Selector@ for @advancementForCGGlyph:@
advancementForCGGlyphSelector :: Selector
advancementForCGGlyphSelector = mkSelector "advancementForCGGlyph:"

-- | @Selector@ for @set@
setSelector :: Selector
setSelector = mkSelector "set"

-- | @Selector@ for @setInContext:@
setInContextSelector :: Selector
setInContextSelector = mkSelector "setInContext:"

-- | @Selector@ for @preferredFontForTextStyle:options:@
preferredFontForTextStyle_optionsSelector :: Selector
preferredFontForTextStyle_optionsSelector = mkSelector "preferredFontForTextStyle:options:"

-- | @Selector@ for @glyphWithName:@
glyphWithNameSelector :: Selector
glyphWithNameSelector = mkSelector "glyphWithName:"

-- | @Selector@ for @boundingRectForGlyph:@
boundingRectForGlyphSelector :: Selector
boundingRectForGlyphSelector = mkSelector "boundingRectForGlyph:"

-- | @Selector@ for @advancementForGlyph:@
advancementForGlyphSelector :: Selector
advancementForGlyphSelector = mkSelector "advancementForGlyph:"

-- | @Selector@ for @getAdvancements:forPackedGlyphs:length:@
getAdvancements_forPackedGlyphs_lengthSelector :: Selector
getAdvancements_forPackedGlyphs_lengthSelector = mkSelector "getAdvancements:forPackedGlyphs:length:"

-- | @Selector@ for @screenFontWithRenderingMode:@
screenFontWithRenderingModeSelector :: Selector
screenFontWithRenderingModeSelector = mkSelector "screenFontWithRenderingMode:"

-- | @Selector@ for @systemFontSize@
systemFontSizeSelector :: Selector
systemFontSizeSelector = mkSelector "systemFontSize"

-- | @Selector@ for @smallSystemFontSize@
smallSystemFontSizeSelector :: Selector
smallSystemFontSizeSelector = mkSelector "smallSystemFontSize"

-- | @Selector@ for @labelFontSize@
labelFontSizeSelector :: Selector
labelFontSizeSelector = mkSelector "labelFontSize"

-- | @Selector@ for @fontName@
fontNameSelector :: Selector
fontNameSelector = mkSelector "fontName"

-- | @Selector@ for @pointSize@
pointSizeSelector :: Selector
pointSizeSelector = mkSelector "pointSize"

-- | @Selector@ for @familyName@
familyNameSelector :: Selector
familyNameSelector = mkSelector "familyName"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @fontDescriptor@
fontDescriptorSelector :: Selector
fontDescriptorSelector = mkSelector "fontDescriptor"

-- | @Selector@ for @textTransform@
textTransformSelector :: Selector
textTransformSelector = mkSelector "textTransform"

-- | @Selector@ for @numberOfGlyphs@
numberOfGlyphsSelector :: Selector
numberOfGlyphsSelector = mkSelector "numberOfGlyphs"

-- | @Selector@ for @mostCompatibleStringEncoding@
mostCompatibleStringEncodingSelector :: Selector
mostCompatibleStringEncodingSelector = mkSelector "mostCompatibleStringEncoding"

-- | @Selector@ for @coveredCharacterSet@
coveredCharacterSetSelector :: Selector
coveredCharacterSetSelector = mkSelector "coveredCharacterSet"

-- | @Selector@ for @boundingRectForFont@
boundingRectForFontSelector :: Selector
boundingRectForFontSelector = mkSelector "boundingRectForFont"

-- | @Selector@ for @maximumAdvancement@
maximumAdvancementSelector :: Selector
maximumAdvancementSelector = mkSelector "maximumAdvancement"

-- | @Selector@ for @ascender@
ascenderSelector :: Selector
ascenderSelector = mkSelector "ascender"

-- | @Selector@ for @descender@
descenderSelector :: Selector
descenderSelector = mkSelector "descender"

-- | @Selector@ for @leading@
leadingSelector :: Selector
leadingSelector = mkSelector "leading"

-- | @Selector@ for @underlinePosition@
underlinePositionSelector :: Selector
underlinePositionSelector = mkSelector "underlinePosition"

-- | @Selector@ for @underlineThickness@
underlineThicknessSelector :: Selector
underlineThicknessSelector = mkSelector "underlineThickness"

-- | @Selector@ for @italicAngle@
italicAngleSelector :: Selector
italicAngleSelector = mkSelector "italicAngle"

-- | @Selector@ for @capHeight@
capHeightSelector :: Selector
capHeightSelector = mkSelector "capHeight"

-- | @Selector@ for @xHeight@
xHeightSelector :: Selector
xHeightSelector = mkSelector "xHeight"

-- | @Selector@ for @fixedPitch@
fixedPitchSelector :: Selector
fixedPitchSelector = mkSelector "fixedPitch"

-- | @Selector@ for @vertical@
verticalSelector :: Selector
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @printerFont@
printerFontSelector :: Selector
printerFontSelector = mkSelector "printerFont"

-- | @Selector@ for @screenFont@
screenFontSelector :: Selector
screenFontSelector = mkSelector "screenFont"

-- | @Selector@ for @renderingMode@
renderingModeSelector :: Selector
renderingModeSelector = mkSelector "renderingMode"

