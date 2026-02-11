{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaption
--
-- An instance of AVCaption represents a unit of text that is active at a particular time range.
--
-- A caption contains one meaningful sentence, paragraph, or otherwise known as a caption cue. Within the active time range, it may perform animation (e.g. Karaoke), rolling-up, changes the visibility, or any other dynamic styling.
--
-- Generated bindings for @AVCaption@.
module ObjC.AVFoundation.AVCaption
  ( AVCaption
  , IsAVCaption(..)
  , init_
  , new
  , textColorAtIndex_range
  , backgroundColorAtIndex_range
  , fontWeightAtIndex_range
  , fontStyleAtIndex_range
  , decorationAtIndex_range
  , textCombineAtIndex_range
  , rubyAtIndex_range
  , text
  , animation
  , region
  , textAlignment
  , initSelector
  , newSelector
  , textColorAtIndex_rangeSelector
  , backgroundColorAtIndex_rangeSelector
  , fontWeightAtIndex_rangeSelector
  , fontStyleAtIndex_rangeSelector
  , decorationAtIndex_rangeSelector
  , textCombineAtIndex_rangeSelector
  , rubyAtIndex_rangeSelector
  , textSelector
  , animationSelector
  , regionSelector
  , textAlignmentSelector

  -- * Enum types
  , AVCaptionAnimation(AVCaptionAnimation)
  , pattern AVCaptionAnimationNone
  , pattern AVCaptionAnimationCharacterReveal
  , AVCaptionDecoration(AVCaptionDecoration)
  , pattern AVCaptionDecorationNone
  , pattern AVCaptionDecorationUnderline
  , pattern AVCaptionDecorationLineThrough
  , pattern AVCaptionDecorationOverline
  , AVCaptionFontStyle(AVCaptionFontStyle)
  , pattern AVCaptionFontStyleUnknown
  , pattern AVCaptionFontStyleNormal
  , pattern AVCaptionFontStyleItalic
  , AVCaptionFontWeight(AVCaptionFontWeight)
  , pattern AVCaptionFontWeightUnknown
  , pattern AVCaptionFontWeightNormal
  , pattern AVCaptionFontWeightBold
  , AVCaptionTextAlignment(AVCaptionTextAlignment)
  , pattern AVCaptionTextAlignmentStart
  , pattern AVCaptionTextAlignmentEnd
  , pattern AVCaptionTextAlignmentCenter
  , pattern AVCaptionTextAlignmentLeft
  , pattern AVCaptionTextAlignmentRight
  , AVCaptionTextCombine(AVCaptionTextCombine)
  , pattern AVCaptionTextCombineAll
  , pattern AVCaptionTextCombineNone
  , pattern AVCaptionTextCombineOneDigit
  , pattern AVCaptionTextCombineTwoDigits
  , pattern AVCaptionTextCombineThreeDigits
  , pattern AVCaptionTextCombineFourDigits

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Structs
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaption avCaption => avCaption -> IO (Id AVCaption)
init_ avCaption  =
  sendMsg avCaption (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaption)
new  =
  do
    cls' <- getRequiredClass "AVCaption"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | textColorAtIndex:range:
--
-- The color of the character(s).
--
-- A value of NULL means that the color is not specified. The caller must release the returned object via CGColorRelease. The range parameter receives UTF-16 code unit index range the style is effective. The range will indicate where the current style returned (including NULL) will be active and allows discovery of the next change in the style.
--
-- CEA608 closed captions support the following 8 colors with 1.0 alpha value.		White	(RGB:1.0, 1.0, 1.0)		Red		(RGB:1.0, 0.0, 0.0)		Blue	(RGB:0.0, 0.0, 1.0)		Green	(RGB:0.0, 1.0, 0.0)		Yellow	(RGB:1.0, 1.0, 0.0)		Cyan	(RGB:0.0, 1.0, 1.0)		Magenta	(RGB:1.0, 0.0, 1.0)		Black	(RGB:0.0, 0.0, 0.0)
--
-- ObjC selector: @- textColorAtIndex:range:@
textColorAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO (Ptr ())
textColorAtIndex_range avCaption  index outRange =
  fmap castPtr $ sendMsg avCaption (mkSelector "textColorAtIndex:range:") (retPtr retVoid) [argCLong (fromIntegral index), argPtr outRange]

-- | backgroundColorAtIndex:range:
--
-- The background color of the character(s).
--
-- A value of NULL means that the color is not specified. The caller must release the returned object via CGColorRelease. The range parameter receives UTF-16 code unit index range the style is effective. After returning from the call, the range will indicate where the current style returned (including NULL) will be active and allows discovery of the next change in the style.
--
-- iTT format ignores this property.
--
-- CEA608 closed captions support the following 8 colors with 1.0, 0.5 and 0.0 alpha values.		White	(RGB:1.0, 1.0, 1.0)		Red		(RGB:1.0, 0.0, 0.0)		Blue	(RGB:0.0, 0.0, 1.0)		Green	(RGB:0.0, 1.0, 0.0)		Yellow	(RGB:1.0, 1.0, 0.0)		Cyan	(RGB:0.0, 1.0, 1.0)		Magenta	(RGB:1.0, 0.0, 1.0)		Black	(RGB:0.0, 0.0, 0.0)
--
-- ObjC selector: @- backgroundColorAtIndex:range:@
backgroundColorAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO (Ptr ())
backgroundColorAtIndex_range avCaption  index outRange =
  fmap castPtr $ sendMsg avCaption (mkSelector "backgroundColorAtIndex:range:") (retPtr retVoid) [argCLong (fromIntegral index), argPtr outRange]

-- | fontWeightAtIndex:range:
--
-- Indicates the font weight of the character(s).
--
-- The range parameter receives UTF-16 code unit index range the style is effective. After returning from the call, the range will indicate where the current style returned will be active and allows discovery of the next change in the style.
--
-- CEA608 closed captions ignore this property.
--
-- A visible distinction between AVCaptionFontWeightNormal and AVCaptionFontWeightBold may not exist if the font used has only one weight. This can be more common with CJK fonts where individual fonts can be quite large in terms of storage. Nevertheless, AVCaption still carries the font weight semantics so if the same AVCaption is applied to a different font having multiple weights, the distinction will become visible.
--
-- ObjC selector: @- fontWeightAtIndex:range:@
fontWeightAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO AVCaptionFontWeight
fontWeightAtIndex_range avCaption  index outRange =
  fmap (coerce :: CLong -> AVCaptionFontWeight) $ sendMsg avCaption (mkSelector "fontWeightAtIndex:range:") retCLong [argCLong (fromIntegral index), argPtr outRange]

-- | fontStyleAtIndex:range:
--
-- Indicates the font style of the character(s).
--
-- The range parameter receives UTF-16 code unit index range the style is effective. After returning from the call, the range will indicate where the current style returned will be active and allows discovery of the next change in the style.
--
-- Some writing systems may not have italic glyphs for characters and so fonts with italic forms are not available. For example, Japanese fonts do not typically have italic forms for most characters although there may be special cases for Latin characters. Nevertheless, AVCaption still carries the font style semantics even though there may be no visible rendering distinction between using AVCaptionFontStyleNormal and AVCaptionFontStyleItalic with that language.
--
-- ObjC selector: @- fontStyleAtIndex:range:@
fontStyleAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO AVCaptionFontStyle
fontStyleAtIndex_range avCaption  index outRange =
  fmap (coerce :: CLong -> AVCaptionFontStyle) $ sendMsg avCaption (mkSelector "fontStyleAtIndex:range:") retCLong [argCLong (fromIntegral index), argPtr outRange]

-- | decorationAtIndex:range:
--
-- Character decoration
--
-- The value of OR-ed value of AVCaptionDecoration as NSInteger. The range parameter receives UTF-16 code unit index range the style is effective. After returning from the call, the range will indicate where the current style returned will be active and allows discovery of the next change in the style.
--
-- CEA608 closed captions support only AVCaptionDecorationNone and AVCaptionDecorationUnderline.
--
-- ObjC selector: @- decorationAtIndex:range:@
decorationAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO AVCaptionDecoration
decorationAtIndex_range avCaption  index outRange =
  fmap (coerce :: CULong -> AVCaptionDecoration) $ sendMsg avCaption (mkSelector "decorationAtIndex:range:") retCULong [argCLong (fromIntegral index), argPtr outRange]

-- | textCombineAtIndex:range:
--
-- Text combine (Tate-Chu-Yoko)
--
-- The style is effective only in a vertical text region.
--
-- When specified, the renderer combines all the characters in the style range so that their glyph areas consume the nominal bounding box of a single em square of the surrounding vertical text.
--
-- ObjC selector: @- textCombineAtIndex:range:@
textCombineAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO AVCaptionTextCombine
textCombineAtIndex_range avCaption  index outRange =
  fmap (coerce :: CLong -> AVCaptionTextCombine) $ sendMsg avCaption (mkSelector "textCombineAtIndex:range:") retCLong [argCLong (fromIntegral index), argPtr outRange]

-- | rubyAtIndex:range:
--
-- Get Ruby associated with the characters.
--
-- The range parameter receives UTF-16 code unit index range where the ruby text is applied. After returning from the call, the range will indicate where the current style returned (including NULL) will be active and allows discovery of the next change in the style.
--
-- It returns nil when the text doesn't have a ruby at the position.
--
-- CEA608 closed captions ignore this property.
--
-- ObjC selector: @- rubyAtIndex:range:@
rubyAtIndex_range :: IsAVCaption avCaption => avCaption -> CLong -> Ptr NSRange -> IO (Id AVCaptionRuby)
rubyAtIndex_range avCaption  index outRange =
  sendMsg avCaption (mkSelector "rubyAtIndex:range:") (retPtr retVoid) [argCLong (fromIntegral index), argPtr outRange] >>= retainedObject . castPtr

-- | text
--
-- The text content of the caption.
--
-- The text may contain any of the line breaking character sequences (LF, CR, or CF+LF) and separating the lines in the presentation.
--
-- The Apple iTT format supports all Unicode code points allowed in a XML document. Any XML special characters such as '&' are converted to a corresponding character reference syntax when written to the destination file.
--
-- CEA608 closed captions support the following Unicode characters.		Range: U+0020 - U+005F		Range: U+0061 - U+007E		Range: U+00A1 - U+00A5		Characters: U+00A9, U+00AB, U+00AE, U+00B0, U+00BB, U+00BD, U+00BF		Range: U+00C0-U+00C5		Range: U+00C7-U+00CF		Range: U+00D1-U+00D6		Range: U+00D8-U+00DC		Range: U+00DF-U+00E5		Range: U+00E7-U+00EF		Range: U+00F1-U+00FC		Range: U+2018-U+2019		Range: U+2018-U+201D		Character: U+2022		Range: U+2120-U+2122		Characters: U+2501, U+2503, U+250F, U+2513, U+2517, U+251B, U+2588, U+266A
--
-- CEA608 closed captions don't support the line breaking character sequences (LF, CR, or CF+LF).
--
-- ObjC selector: @- text@
text :: IsAVCaption avCaption => avCaption -> IO (Id NSString)
text avCaption  =
  sendMsg avCaption (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- animation@
animation :: IsAVCaption avCaption => avCaption -> IO AVCaptionAnimation
animation avCaption  =
  fmap (coerce :: CLong -> AVCaptionAnimation) $ sendMsg avCaption (mkSelector "animation") retCLong []

-- | region
--
-- The region where the caption is placed.
--
-- It can be nil when the underlying caption format doesn't support or use regions.
--
-- ObjC selector: @- region@
region :: IsAVCaption avCaption => avCaption -> IO (Id AVCaptionRegion)
region avCaption  =
  sendMsg avCaption (mkSelector "region") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | textAlignment
--
-- The text alignment within the containing region.
--
-- This property throws an exception if a value is set which is not a valid AVCaptionTextAlignment.
--
-- ObjC selector: @- textAlignment@
textAlignment :: IsAVCaption avCaption => avCaption -> IO AVCaptionTextAlignment
textAlignment avCaption  =
  fmap (coerce :: CLong -> AVCaptionTextAlignment) $ sendMsg avCaption (mkSelector "textAlignment") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @textColorAtIndex:range:@
textColorAtIndex_rangeSelector :: Selector
textColorAtIndex_rangeSelector = mkSelector "textColorAtIndex:range:"

-- | @Selector@ for @backgroundColorAtIndex:range:@
backgroundColorAtIndex_rangeSelector :: Selector
backgroundColorAtIndex_rangeSelector = mkSelector "backgroundColorAtIndex:range:"

-- | @Selector@ for @fontWeightAtIndex:range:@
fontWeightAtIndex_rangeSelector :: Selector
fontWeightAtIndex_rangeSelector = mkSelector "fontWeightAtIndex:range:"

-- | @Selector@ for @fontStyleAtIndex:range:@
fontStyleAtIndex_rangeSelector :: Selector
fontStyleAtIndex_rangeSelector = mkSelector "fontStyleAtIndex:range:"

-- | @Selector@ for @decorationAtIndex:range:@
decorationAtIndex_rangeSelector :: Selector
decorationAtIndex_rangeSelector = mkSelector "decorationAtIndex:range:"

-- | @Selector@ for @textCombineAtIndex:range:@
textCombineAtIndex_rangeSelector :: Selector
textCombineAtIndex_rangeSelector = mkSelector "textCombineAtIndex:range:"

-- | @Selector@ for @rubyAtIndex:range:@
rubyAtIndex_rangeSelector :: Selector
rubyAtIndex_rangeSelector = mkSelector "rubyAtIndex:range:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @animation@
animationSelector :: Selector
animationSelector = mkSelector "animation"

-- | @Selector@ for @region@
regionSelector :: Selector
regionSelector = mkSelector "region"

-- | @Selector@ for @textAlignment@
textAlignmentSelector :: Selector
textAlignmentSelector = mkSelector "textAlignment"

