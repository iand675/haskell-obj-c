{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , getBoundingRects_forCGGlyphs_count
  , getAdvancements_forCGGlyphs_count
  , set
  , setInContext
  , preferredFontForTextStyle_options
  , glyphWithName
  , boundingRectForGlyph
  , advancementForGlyph
  , getBoundingRects_forGlyphs_count
  , getAdvancements_forGlyphs_count
  , getAdvancements_forPackedGlyphs_length
  , screenFontWithRenderingMode
  , systemFontSize
  , smallSystemFontSize
  , labelFontSize
  , fontName
  , pointSize
  , matrix
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
  , verticalFont
  , vertical
  , printerFont
  , screenFont
  , renderingMode
  , advancementForCGGlyphSelector
  , advancementForGlyphSelector
  , ascenderSelector
  , boldSystemFontOfSizeSelector
  , boundingRectForCGGlyphSelector
  , boundingRectForFontSelector
  , boundingRectForGlyphSelector
  , capHeightSelector
  , controlContentFontOfSizeSelector
  , coveredCharacterSetSelector
  , descenderSelector
  , displayNameSelector
  , familyNameSelector
  , fixedPitchSelector
  , fontDescriptorSelector
  , fontNameSelector
  , fontWithDescriptor_sizeSelector
  , fontWithDescriptor_textTransformSelector
  , fontWithName_matrixSelector
  , fontWithName_sizeSelector
  , fontWithSizeSelector
  , getAdvancements_forCGGlyphs_countSelector
  , getAdvancements_forGlyphs_countSelector
  , getAdvancements_forPackedGlyphs_lengthSelector
  , getBoundingRects_forCGGlyphs_countSelector
  , getBoundingRects_forGlyphs_countSelector
  , glyphWithNameSelector
  , italicAngleSelector
  , labelFontOfSizeSelector
  , labelFontSizeSelector
  , leadingSelector
  , matrixSelector
  , maximumAdvancementSelector
  , menuBarFontOfSizeSelector
  , menuFontOfSizeSelector
  , messageFontOfSizeSelector
  , monospacedDigitSystemFontOfSize_weightSelector
  , monospacedSystemFontOfSize_weightSelector
  , mostCompatibleStringEncodingSelector
  , numberOfGlyphsSelector
  , paletteFontOfSizeSelector
  , pointSizeSelector
  , preferredFontForTextStyle_optionsSelector
  , printerFontSelector
  , renderingModeSelector
  , screenFontSelector
  , screenFontWithRenderingModeSelector
  , setInContextSelector
  , setSelector
  , setUserFixedPitchFontSelector
  , setUserFontSelector
  , smallSystemFontSizeSelector
  , systemFontOfSizeSelector
  , systemFontOfSize_weightSelector
  , systemFontOfSize_weight_widthSelector
  , systemFontSizeForControlSizeSelector
  , systemFontSizeSelector
  , textTransformSelector
  , titleBarFontOfSizeSelector
  , toolTipsFontOfSizeSelector
  , underlinePositionSelector
  , underlineThicknessSelector
  , userFixedPitchFontOfSizeSelector
  , userFontOfSizeSelector
  , verticalFontSelector
  , verticalSelector
  , xHeightSelector

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

-- | ******* Factory ********
--
-- ObjC selector: @+ fontWithName:size:@
fontWithName_size :: IsNSString fontName => fontName -> CDouble -> IO (Id NSFont)
fontWithName_size fontName fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' fontWithName_sizeSelector (toNSString fontName) fontSize

-- | @+ fontWithName:matrix:@
fontWithName_matrix :: IsNSString fontName => fontName -> Const (Ptr CDouble) -> IO (Id NSFont)
fontWithName_matrix fontName fontMatrix =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' fontWithName_matrixSelector (toNSString fontName) fontMatrix

-- | @+ fontWithDescriptor:size:@
fontWithDescriptor_size :: IsNSFontDescriptor fontDescriptor => fontDescriptor -> CDouble -> IO (Id NSFont)
fontWithDescriptor_size fontDescriptor fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' fontWithDescriptor_sizeSelector (toNSFontDescriptor fontDescriptor) fontSize

-- | @+ fontWithDescriptor:textTransform:@
fontWithDescriptor_textTransform :: (IsNSFontDescriptor fontDescriptor, IsNSAffineTransform textTransform) => fontDescriptor -> textTransform -> IO (Id NSFont)
fontWithDescriptor_textTransform fontDescriptor textTransform =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' fontWithDescriptor_textTransformSelector (toNSFontDescriptor fontDescriptor) (toNSAffineTransform textTransform)

-- | ******* Meta Font ********
--
-- ObjC selector: @+ userFontOfSize:@
userFontOfSize :: CDouble -> IO (Id NSFont)
userFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' userFontOfSizeSelector fontSize

-- | @+ userFixedPitchFontOfSize:@
userFixedPitchFontOfSize :: CDouble -> IO (Id NSFont)
userFixedPitchFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' userFixedPitchFontOfSizeSelector fontSize

-- | @+ setUserFont:@
setUserFont :: IsNSFont font => font -> IO ()
setUserFont font =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' setUserFontSelector (toNSFont font)

-- | @+ setUserFixedPitchFont:@
setUserFixedPitchFont :: IsNSFont font => font -> IO ()
setUserFixedPitchFont font =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' setUserFixedPitchFontSelector (toNSFont font)

-- | @+ systemFontOfSize:@
systemFontOfSize :: CDouble -> IO (Id NSFont)
systemFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' systemFontOfSizeSelector fontSize

-- | @+ boldSystemFontOfSize:@
boldSystemFontOfSize :: CDouble -> IO (Id NSFont)
boldSystemFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' boldSystemFontOfSizeSelector fontSize

-- | @+ labelFontOfSize:@
labelFontOfSize :: CDouble -> IO (Id NSFont)
labelFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' labelFontOfSizeSelector fontSize

-- | @+ titleBarFontOfSize:@
titleBarFontOfSize :: CDouble -> IO (Id NSFont)
titleBarFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' titleBarFontOfSizeSelector fontSize

-- | @+ menuFontOfSize:@
menuFontOfSize :: CDouble -> IO (Id NSFont)
menuFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' menuFontOfSizeSelector fontSize

-- | @+ menuBarFontOfSize:@
menuBarFontOfSize :: CDouble -> IO (Id NSFont)
menuBarFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' menuBarFontOfSizeSelector fontSize

-- | @+ messageFontOfSize:@
messageFontOfSize :: CDouble -> IO (Id NSFont)
messageFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' messageFontOfSizeSelector fontSize

-- | @+ paletteFontOfSize:@
paletteFontOfSize :: CDouble -> IO (Id NSFont)
paletteFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' paletteFontOfSizeSelector fontSize

-- | @+ toolTipsFontOfSize:@
toolTipsFontOfSize :: CDouble -> IO (Id NSFont)
toolTipsFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' toolTipsFontOfSizeSelector fontSize

-- | @+ controlContentFontOfSize:@
controlContentFontOfSize :: CDouble -> IO (Id NSFont)
controlContentFontOfSize fontSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' controlContentFontOfSizeSelector fontSize

-- | @+ systemFontOfSize:weight:@
systemFontOfSize_weight :: CDouble -> CDouble -> IO (Id NSFont)
systemFontOfSize_weight fontSize weight =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' systemFontOfSize_weightSelector fontSize weight

-- | @+ monospacedDigitSystemFontOfSize:weight:@
monospacedDigitSystemFontOfSize_weight :: CDouble -> CDouble -> IO (Id NSFont)
monospacedDigitSystemFontOfSize_weight fontSize weight =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' monospacedDigitSystemFontOfSize_weightSelector fontSize weight

-- | @+ systemFontOfSize:weight:width:@
systemFontOfSize_weight_width :: CDouble -> CDouble -> CDouble -> IO (Id NSFont)
systemFontOfSize_weight_width fontSize weight width =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' systemFontOfSize_weight_widthSelector fontSize weight width

-- | @+ monospacedSystemFontOfSize:weight:@
monospacedSystemFontOfSize_weight :: CDouble -> CDouble -> IO (Id NSFont)
monospacedSystemFontOfSize_weight fontSize weight =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' monospacedSystemFontOfSize_weightSelector fontSize weight

-- | @- fontWithSize:@
fontWithSize :: IsNSFont nsFont => nsFont -> CDouble -> IO (Id NSFont)
fontWithSize nsFont fontSize =
  sendMessage nsFont fontWithSizeSelector fontSize

-- | @+ systemFontSizeForControlSize:@
systemFontSizeForControlSize :: NSControlSize -> IO CDouble
systemFontSizeForControlSize controlSize =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' systemFontSizeForControlSizeSelector controlSize

-- | ******* Glyph metrics ********
--
-- ******* Glyph metrics ********
--
-- ObjC selector: @- boundingRectForCGGlyph:@
boundingRectForCGGlyph :: IsNSFont nsFont => nsFont -> CUShort -> IO NSRect
boundingRectForCGGlyph nsFont glyph =
  sendMessage nsFont boundingRectForCGGlyphSelector glyph

-- | @- advancementForCGGlyph:@
advancementForCGGlyph :: IsNSFont nsFont => nsFont -> CUShort -> IO NSSize
advancementForCGGlyph nsFont glyph =
  sendMessage nsFont advancementForCGGlyphSelector glyph

-- | @- getBoundingRects:forCGGlyphs:count:@
getBoundingRects_forCGGlyphs_count :: IsNSFont nsFont => nsFont -> Ptr NSRect -> Const RawId -> CULong -> IO ()
getBoundingRects_forCGGlyphs_count nsFont bounds glyphs glyphCount =
  sendMessage nsFont getBoundingRects_forCGGlyphs_countSelector bounds glyphs glyphCount

-- | @- getAdvancements:forCGGlyphs:count:@
getAdvancements_forCGGlyphs_count :: IsNSFont nsFont => nsFont -> Ptr NSSize -> Const RawId -> CULong -> IO ()
getAdvancements_forCGGlyphs_count nsFont advancements glyphs glyphCount =
  sendMessage nsFont getAdvancements_forCGGlyphs_countSelector advancements glyphs glyphCount

-- | ******* NSGraphicsContext-related ********
--
-- ObjC selector: @- set@
set :: IsNSFont nsFont => nsFont -> IO ()
set nsFont =
  sendMessage nsFont setSelector

-- | @- setInContext:@
setInContext :: (IsNSFont nsFont, IsNSGraphicsContext graphicsContext) => nsFont -> graphicsContext -> IO ()
setInContext nsFont graphicsContext =
  sendMessage nsFont setInContextSelector (toNSGraphicsContext graphicsContext)

-- | @+ preferredFontForTextStyle:options:@
preferredFontForTextStyle_options :: (IsNSString style, IsNSDictionary options) => style -> options -> IO (Id NSFont)
preferredFontForTextStyle_options style options =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' preferredFontForTextStyle_optionsSelector (toNSString style) (toNSDictionary options)

-- | @- glyphWithName:@
glyphWithName :: (IsNSFont nsFont, IsNSString name) => nsFont -> name -> IO CUInt
glyphWithName nsFont name =
  sendMessage nsFont glyphWithNameSelector (toNSString name)

-- | @- boundingRectForGlyph:@
boundingRectForGlyph :: IsNSFont nsFont => nsFont -> CUInt -> IO NSRect
boundingRectForGlyph nsFont glyph =
  sendMessage nsFont boundingRectForGlyphSelector glyph

-- | @- advancementForGlyph:@
advancementForGlyph :: IsNSFont nsFont => nsFont -> CUInt -> IO NSSize
advancementForGlyph nsFont glyph =
  sendMessage nsFont advancementForGlyphSelector glyph

-- | @- getBoundingRects:forGlyphs:count:@
getBoundingRects_forGlyphs_count :: IsNSFont nsFont => nsFont -> Ptr NSRect -> Const RawId -> CULong -> IO ()
getBoundingRects_forGlyphs_count nsFont bounds glyphs glyphCount =
  sendMessage nsFont getBoundingRects_forGlyphs_countSelector bounds glyphs glyphCount

-- | @- getAdvancements:forGlyphs:count:@
getAdvancements_forGlyphs_count :: IsNSFont nsFont => nsFont -> Ptr NSSize -> Const RawId -> CULong -> IO ()
getAdvancements_forGlyphs_count nsFont advancements glyphs glyphCount =
  sendMessage nsFont getAdvancements_forGlyphs_countSelector advancements glyphs glyphCount

-- | @- getAdvancements:forPackedGlyphs:length:@
getAdvancements_forPackedGlyphs_length :: IsNSFont nsFont => nsFont -> Ptr NSSize -> Const (Ptr ()) -> CULong -> IO ()
getAdvancements_forPackedGlyphs_length nsFont advancements packedGlyphs length_ =
  sendMessage nsFont getAdvancements_forPackedGlyphs_lengthSelector advancements packedGlyphs length_

-- | @- screenFontWithRenderingMode:@
screenFontWithRenderingMode :: IsNSFont nsFont => nsFont -> NSFontRenderingMode -> IO (Id NSFont)
screenFontWithRenderingMode nsFont renderingMode =
  sendMessage nsFont screenFontWithRenderingModeSelector renderingMode

-- | @+ systemFontSize@
systemFontSize :: IO CDouble
systemFontSize  =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' systemFontSizeSelector

-- | @+ smallSystemFontSize@
smallSystemFontSize :: IO CDouble
smallSystemFontSize  =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' smallSystemFontSizeSelector

-- | @+ labelFontSize@
labelFontSize :: IO CDouble
labelFontSize  =
  do
    cls' <- getRequiredClass "NSFont"
    sendClassMessage cls' labelFontSizeSelector

-- | ******* Core font attribute ********
--
-- ObjC selector: @- fontName@
fontName :: IsNSFont nsFont => nsFont -> IO (Id NSString)
fontName nsFont =
  sendMessage nsFont fontNameSelector

-- | @- pointSize@
pointSize :: IsNSFont nsFont => nsFont -> IO CDouble
pointSize nsFont =
  sendMessage nsFont pointSizeSelector

-- | @- matrix@
matrix :: IsNSFont nsFont => nsFont -> IO RawId
matrix nsFont =
  sendMessage nsFont matrixSelector

-- | @- familyName@
familyName :: IsNSFont nsFont => nsFont -> IO (Id NSString)
familyName nsFont =
  sendMessage nsFont familyNameSelector

-- | @- displayName@
displayName :: IsNSFont nsFont => nsFont -> IO (Id NSString)
displayName nsFont =
  sendMessage nsFont displayNameSelector

-- | @- fontDescriptor@
fontDescriptor :: IsNSFont nsFont => nsFont -> IO (Id NSFontDescriptor)
fontDescriptor nsFont =
  sendMessage nsFont fontDescriptorSelector

-- | @- textTransform@
textTransform :: IsNSFont nsFont => nsFont -> IO (Id NSAffineTransform)
textTransform nsFont =
  sendMessage nsFont textTransformSelector

-- | ******* Glyph coverage ********
--
-- ObjC selector: @- numberOfGlyphs@
numberOfGlyphs :: IsNSFont nsFont => nsFont -> IO CULong
numberOfGlyphs nsFont =
  sendMessage nsFont numberOfGlyphsSelector

-- | @- mostCompatibleStringEncoding@
mostCompatibleStringEncoding :: IsNSFont nsFont => nsFont -> IO CULong
mostCompatibleStringEncoding nsFont =
  sendMessage nsFont mostCompatibleStringEncodingSelector

-- | @- coveredCharacterSet@
coveredCharacterSet :: IsNSFont nsFont => nsFont -> IO (Id NSCharacterSet)
coveredCharacterSet nsFont =
  sendMessage nsFont coveredCharacterSetSelector

-- | @- boundingRectForFont@
boundingRectForFont :: IsNSFont nsFont => nsFont -> IO NSRect
boundingRectForFont nsFont =
  sendMessage nsFont boundingRectForFontSelector

-- | @- maximumAdvancement@
maximumAdvancement :: IsNSFont nsFont => nsFont -> IO NSSize
maximumAdvancement nsFont =
  sendMessage nsFont maximumAdvancementSelector

-- | @- ascender@
ascender :: IsNSFont nsFont => nsFont -> IO CDouble
ascender nsFont =
  sendMessage nsFont ascenderSelector

-- | @- descender@
descender :: IsNSFont nsFont => nsFont -> IO CDouble
descender nsFont =
  sendMessage nsFont descenderSelector

-- | @- leading@
leading :: IsNSFont nsFont => nsFont -> IO CDouble
leading nsFont =
  sendMessage nsFont leadingSelector

-- | @- underlinePosition@
underlinePosition :: IsNSFont nsFont => nsFont -> IO CDouble
underlinePosition nsFont =
  sendMessage nsFont underlinePositionSelector

-- | @- underlineThickness@
underlineThickness :: IsNSFont nsFont => nsFont -> IO CDouble
underlineThickness nsFont =
  sendMessage nsFont underlineThicknessSelector

-- | @- italicAngle@
italicAngle :: IsNSFont nsFont => nsFont -> IO CDouble
italicAngle nsFont =
  sendMessage nsFont italicAngleSelector

-- | @- capHeight@
capHeight :: IsNSFont nsFont => nsFont -> IO CDouble
capHeight nsFont =
  sendMessage nsFont capHeightSelector

-- | @- xHeight@
xHeight :: IsNSFont nsFont => nsFont -> IO CDouble
xHeight nsFont =
  sendMessage nsFont xHeightSelector

-- | @- fixedPitch@
fixedPitch :: IsNSFont nsFont => nsFont -> IO Bool
fixedPitch nsFont =
  sendMessage nsFont fixedPitchSelector

-- | ******* Vertical mode ********
--
-- ObjC selector: @- verticalFont@
verticalFont :: IsNSFont nsFont => nsFont -> IO (Id NSFont)
verticalFont nsFont =
  sendMessage nsFont verticalFontSelector

-- | @- vertical@
vertical :: IsNSFont nsFont => nsFont -> IO Bool
vertical nsFont =
  sendMessage nsFont verticalSelector

-- | ******* Rendering mode ********
--
-- ObjC selector: @- printerFont@
printerFont :: IsNSFont nsFont => nsFont -> IO (Id NSFont)
printerFont nsFont =
  sendMessage nsFont printerFontSelector

-- | @- screenFont@
screenFont :: IsNSFont nsFont => nsFont -> IO (Id NSFont)
screenFont nsFont =
  sendMessage nsFont screenFontSelector

-- | @- renderingMode@
renderingMode :: IsNSFont nsFont => nsFont -> IO NSFontRenderingMode
renderingMode nsFont =
  sendMessage nsFont renderingModeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fontWithName:size:@
fontWithName_sizeSelector :: Selector '[Id NSString, CDouble] (Id NSFont)
fontWithName_sizeSelector = mkSelector "fontWithName:size:"

-- | @Selector@ for @fontWithName:matrix:@
fontWithName_matrixSelector :: Selector '[Id NSString, Const (Ptr CDouble)] (Id NSFont)
fontWithName_matrixSelector = mkSelector "fontWithName:matrix:"

-- | @Selector@ for @fontWithDescriptor:size:@
fontWithDescriptor_sizeSelector :: Selector '[Id NSFontDescriptor, CDouble] (Id NSFont)
fontWithDescriptor_sizeSelector = mkSelector "fontWithDescriptor:size:"

-- | @Selector@ for @fontWithDescriptor:textTransform:@
fontWithDescriptor_textTransformSelector :: Selector '[Id NSFontDescriptor, Id NSAffineTransform] (Id NSFont)
fontWithDescriptor_textTransformSelector = mkSelector "fontWithDescriptor:textTransform:"

-- | @Selector@ for @userFontOfSize:@
userFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
userFontOfSizeSelector = mkSelector "userFontOfSize:"

-- | @Selector@ for @userFixedPitchFontOfSize:@
userFixedPitchFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
userFixedPitchFontOfSizeSelector = mkSelector "userFixedPitchFontOfSize:"

-- | @Selector@ for @setUserFont:@
setUserFontSelector :: Selector '[Id NSFont] ()
setUserFontSelector = mkSelector "setUserFont:"

-- | @Selector@ for @setUserFixedPitchFont:@
setUserFixedPitchFontSelector :: Selector '[Id NSFont] ()
setUserFixedPitchFontSelector = mkSelector "setUserFixedPitchFont:"

-- | @Selector@ for @systemFontOfSize:@
systemFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
systemFontOfSizeSelector = mkSelector "systemFontOfSize:"

-- | @Selector@ for @boldSystemFontOfSize:@
boldSystemFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
boldSystemFontOfSizeSelector = mkSelector "boldSystemFontOfSize:"

-- | @Selector@ for @labelFontOfSize:@
labelFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
labelFontOfSizeSelector = mkSelector "labelFontOfSize:"

-- | @Selector@ for @titleBarFontOfSize:@
titleBarFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
titleBarFontOfSizeSelector = mkSelector "titleBarFontOfSize:"

-- | @Selector@ for @menuFontOfSize:@
menuFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
menuFontOfSizeSelector = mkSelector "menuFontOfSize:"

-- | @Selector@ for @menuBarFontOfSize:@
menuBarFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
menuBarFontOfSizeSelector = mkSelector "menuBarFontOfSize:"

-- | @Selector@ for @messageFontOfSize:@
messageFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
messageFontOfSizeSelector = mkSelector "messageFontOfSize:"

-- | @Selector@ for @paletteFontOfSize:@
paletteFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
paletteFontOfSizeSelector = mkSelector "paletteFontOfSize:"

-- | @Selector@ for @toolTipsFontOfSize:@
toolTipsFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
toolTipsFontOfSizeSelector = mkSelector "toolTipsFontOfSize:"

-- | @Selector@ for @controlContentFontOfSize:@
controlContentFontOfSizeSelector :: Selector '[CDouble] (Id NSFont)
controlContentFontOfSizeSelector = mkSelector "controlContentFontOfSize:"

-- | @Selector@ for @systemFontOfSize:weight:@
systemFontOfSize_weightSelector :: Selector '[CDouble, CDouble] (Id NSFont)
systemFontOfSize_weightSelector = mkSelector "systemFontOfSize:weight:"

-- | @Selector@ for @monospacedDigitSystemFontOfSize:weight:@
monospacedDigitSystemFontOfSize_weightSelector :: Selector '[CDouble, CDouble] (Id NSFont)
monospacedDigitSystemFontOfSize_weightSelector = mkSelector "monospacedDigitSystemFontOfSize:weight:"

-- | @Selector@ for @systemFontOfSize:weight:width:@
systemFontOfSize_weight_widthSelector :: Selector '[CDouble, CDouble, CDouble] (Id NSFont)
systemFontOfSize_weight_widthSelector = mkSelector "systemFontOfSize:weight:width:"

-- | @Selector@ for @monospacedSystemFontOfSize:weight:@
monospacedSystemFontOfSize_weightSelector :: Selector '[CDouble, CDouble] (Id NSFont)
monospacedSystemFontOfSize_weightSelector = mkSelector "monospacedSystemFontOfSize:weight:"

-- | @Selector@ for @fontWithSize:@
fontWithSizeSelector :: Selector '[CDouble] (Id NSFont)
fontWithSizeSelector = mkSelector "fontWithSize:"

-- | @Selector@ for @systemFontSizeForControlSize:@
systemFontSizeForControlSizeSelector :: Selector '[NSControlSize] CDouble
systemFontSizeForControlSizeSelector = mkSelector "systemFontSizeForControlSize:"

-- | @Selector@ for @boundingRectForCGGlyph:@
boundingRectForCGGlyphSelector :: Selector '[CUShort] NSRect
boundingRectForCGGlyphSelector = mkSelector "boundingRectForCGGlyph:"

-- | @Selector@ for @advancementForCGGlyph:@
advancementForCGGlyphSelector :: Selector '[CUShort] NSSize
advancementForCGGlyphSelector = mkSelector "advancementForCGGlyph:"

-- | @Selector@ for @getBoundingRects:forCGGlyphs:count:@
getBoundingRects_forCGGlyphs_countSelector :: Selector '[Ptr NSRect, Const RawId, CULong] ()
getBoundingRects_forCGGlyphs_countSelector = mkSelector "getBoundingRects:forCGGlyphs:count:"

-- | @Selector@ for @getAdvancements:forCGGlyphs:count:@
getAdvancements_forCGGlyphs_countSelector :: Selector '[Ptr NSSize, Const RawId, CULong] ()
getAdvancements_forCGGlyphs_countSelector = mkSelector "getAdvancements:forCGGlyphs:count:"

-- | @Selector@ for @set@
setSelector :: Selector '[] ()
setSelector = mkSelector "set"

-- | @Selector@ for @setInContext:@
setInContextSelector :: Selector '[Id NSGraphicsContext] ()
setInContextSelector = mkSelector "setInContext:"

-- | @Selector@ for @preferredFontForTextStyle:options:@
preferredFontForTextStyle_optionsSelector :: Selector '[Id NSString, Id NSDictionary] (Id NSFont)
preferredFontForTextStyle_optionsSelector = mkSelector "preferredFontForTextStyle:options:"

-- | @Selector@ for @glyphWithName:@
glyphWithNameSelector :: Selector '[Id NSString] CUInt
glyphWithNameSelector = mkSelector "glyphWithName:"

-- | @Selector@ for @boundingRectForGlyph:@
boundingRectForGlyphSelector :: Selector '[CUInt] NSRect
boundingRectForGlyphSelector = mkSelector "boundingRectForGlyph:"

-- | @Selector@ for @advancementForGlyph:@
advancementForGlyphSelector :: Selector '[CUInt] NSSize
advancementForGlyphSelector = mkSelector "advancementForGlyph:"

-- | @Selector@ for @getBoundingRects:forGlyphs:count:@
getBoundingRects_forGlyphs_countSelector :: Selector '[Ptr NSRect, Const RawId, CULong] ()
getBoundingRects_forGlyphs_countSelector = mkSelector "getBoundingRects:forGlyphs:count:"

-- | @Selector@ for @getAdvancements:forGlyphs:count:@
getAdvancements_forGlyphs_countSelector :: Selector '[Ptr NSSize, Const RawId, CULong] ()
getAdvancements_forGlyphs_countSelector = mkSelector "getAdvancements:forGlyphs:count:"

-- | @Selector@ for @getAdvancements:forPackedGlyphs:length:@
getAdvancements_forPackedGlyphs_lengthSelector :: Selector '[Ptr NSSize, Const (Ptr ()), CULong] ()
getAdvancements_forPackedGlyphs_lengthSelector = mkSelector "getAdvancements:forPackedGlyphs:length:"

-- | @Selector@ for @screenFontWithRenderingMode:@
screenFontWithRenderingModeSelector :: Selector '[NSFontRenderingMode] (Id NSFont)
screenFontWithRenderingModeSelector = mkSelector "screenFontWithRenderingMode:"

-- | @Selector@ for @systemFontSize@
systemFontSizeSelector :: Selector '[] CDouble
systemFontSizeSelector = mkSelector "systemFontSize"

-- | @Selector@ for @smallSystemFontSize@
smallSystemFontSizeSelector :: Selector '[] CDouble
smallSystemFontSizeSelector = mkSelector "smallSystemFontSize"

-- | @Selector@ for @labelFontSize@
labelFontSizeSelector :: Selector '[] CDouble
labelFontSizeSelector = mkSelector "labelFontSize"

-- | @Selector@ for @fontName@
fontNameSelector :: Selector '[] (Id NSString)
fontNameSelector = mkSelector "fontName"

-- | @Selector@ for @pointSize@
pointSizeSelector :: Selector '[] CDouble
pointSizeSelector = mkSelector "pointSize"

-- | @Selector@ for @matrix@
matrixSelector :: Selector '[] RawId
matrixSelector = mkSelector "matrix"

-- | @Selector@ for @familyName@
familyNameSelector :: Selector '[] (Id NSString)
familyNameSelector = mkSelector "familyName"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @fontDescriptor@
fontDescriptorSelector :: Selector '[] (Id NSFontDescriptor)
fontDescriptorSelector = mkSelector "fontDescriptor"

-- | @Selector@ for @textTransform@
textTransformSelector :: Selector '[] (Id NSAffineTransform)
textTransformSelector = mkSelector "textTransform"

-- | @Selector@ for @numberOfGlyphs@
numberOfGlyphsSelector :: Selector '[] CULong
numberOfGlyphsSelector = mkSelector "numberOfGlyphs"

-- | @Selector@ for @mostCompatibleStringEncoding@
mostCompatibleStringEncodingSelector :: Selector '[] CULong
mostCompatibleStringEncodingSelector = mkSelector "mostCompatibleStringEncoding"

-- | @Selector@ for @coveredCharacterSet@
coveredCharacterSetSelector :: Selector '[] (Id NSCharacterSet)
coveredCharacterSetSelector = mkSelector "coveredCharacterSet"

-- | @Selector@ for @boundingRectForFont@
boundingRectForFontSelector :: Selector '[] NSRect
boundingRectForFontSelector = mkSelector "boundingRectForFont"

-- | @Selector@ for @maximumAdvancement@
maximumAdvancementSelector :: Selector '[] NSSize
maximumAdvancementSelector = mkSelector "maximumAdvancement"

-- | @Selector@ for @ascender@
ascenderSelector :: Selector '[] CDouble
ascenderSelector = mkSelector "ascender"

-- | @Selector@ for @descender@
descenderSelector :: Selector '[] CDouble
descenderSelector = mkSelector "descender"

-- | @Selector@ for @leading@
leadingSelector :: Selector '[] CDouble
leadingSelector = mkSelector "leading"

-- | @Selector@ for @underlinePosition@
underlinePositionSelector :: Selector '[] CDouble
underlinePositionSelector = mkSelector "underlinePosition"

-- | @Selector@ for @underlineThickness@
underlineThicknessSelector :: Selector '[] CDouble
underlineThicknessSelector = mkSelector "underlineThickness"

-- | @Selector@ for @italicAngle@
italicAngleSelector :: Selector '[] CDouble
italicAngleSelector = mkSelector "italicAngle"

-- | @Selector@ for @capHeight@
capHeightSelector :: Selector '[] CDouble
capHeightSelector = mkSelector "capHeight"

-- | @Selector@ for @xHeight@
xHeightSelector :: Selector '[] CDouble
xHeightSelector = mkSelector "xHeight"

-- | @Selector@ for @fixedPitch@
fixedPitchSelector :: Selector '[] Bool
fixedPitchSelector = mkSelector "fixedPitch"

-- | @Selector@ for @verticalFont@
verticalFontSelector :: Selector '[] (Id NSFont)
verticalFontSelector = mkSelector "verticalFont"

-- | @Selector@ for @vertical@
verticalSelector :: Selector '[] Bool
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @printerFont@
printerFontSelector :: Selector '[] (Id NSFont)
printerFontSelector = mkSelector "printerFont"

-- | @Selector@ for @screenFont@
screenFontSelector :: Selector '[] (Id NSFont)
screenFontSelector = mkSelector "screenFont"

-- | @Selector@ for @renderingMode@
renderingModeSelector :: Selector '[] NSFontRenderingMode
renderingModeSelector = mkSelector "renderingMode"

