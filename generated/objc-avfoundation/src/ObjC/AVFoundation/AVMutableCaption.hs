{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMutableCaption
--
-- Mutable subclass of AVCaption.
--
-- Generated bindings for @AVMutableCaption@.
module ObjC.AVFoundation.AVMutableCaption
  ( AVMutableCaption
  , IsAVMutableCaption(..)
  , setTextColor_inRange
  , setBackgroundColor_inRange
  , setFontWeight_inRange
  , setFontStyle_inRange
  , setDecoration_inRange
  , setTextCombine_inRange
  , setRuby_inRange
  , removeTextColorInRange
  , removeBackgroundColorInRange
  , removeFontWeightInRange
  , removeFontStyleInRange
  , removeDecorationInRange
  , removeTextCombineInRange
  , removeRubyInRange
  , text
  , setText
  , animation
  , setAnimation
  , region
  , setRegion
  , textAlignment
  , setTextAlignment
  , setTextColor_inRangeSelector
  , setBackgroundColor_inRangeSelector
  , setFontWeight_inRangeSelector
  , setFontStyle_inRangeSelector
  , setDecoration_inRangeSelector
  , setTextCombine_inRangeSelector
  , setRuby_inRangeSelector
  , removeTextColorInRangeSelector
  , removeBackgroundColorInRangeSelector
  , removeFontWeightInRangeSelector
  , removeFontStyleInRangeSelector
  , removeDecorationInRangeSelector
  , removeTextCombineInRangeSelector
  , removeRubyInRangeSelector
  , textSelector
  , setTextSelector
  , animationSelector
  , setAnimationSelector
  , regionSelector
  , setRegionSelector
  , textAlignmentSelector
  , setTextAlignmentSelector

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

-- | setTextColor:inRange:
--
-- Set text color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setTextColor:inRange:@
setTextColor_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> Ptr () -> NSRange -> IO ()
setTextColor_inRange avMutableCaption  color range =
  sendMsg avMutableCaption (mkSelector "setTextColor:inRange:") retVoid [argPtr color, argNSRange range]

-- | setBackgroundColor:inRange:
--
-- Set background color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setBackgroundColor:inRange:@
setBackgroundColor_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> Ptr () -> NSRange -> IO ()
setBackgroundColor_inRange avMutableCaption  color range =
  sendMsg avMutableCaption (mkSelector "setBackgroundColor:inRange:") retVoid [argPtr color, argNSRange range]

-- | setFontWeight:inRange:
--
-- Set font weight for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setFontWeight:inRange:@
setFontWeight_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionFontWeight -> NSRange -> IO ()
setFontWeight_inRange avMutableCaption  fontWeight range =
  sendMsg avMutableCaption (mkSelector "setFontWeight:inRange:") retVoid [argCLong (coerce fontWeight), argNSRange range]

-- | setFontStyle:inRange:
--
-- Set font style for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setFontStyle:inRange:@
setFontStyle_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionFontStyle -> NSRange -> IO ()
setFontStyle_inRange avMutableCaption  fontStyle range =
  sendMsg avMutableCaption (mkSelector "setFontStyle:inRange:") retVoid [argCLong (coerce fontStyle), argNSRange range]

-- | setDecoration:inRange:
--
-- Set text decoration for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setDecoration:inRange:@
setDecoration_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionDecoration -> NSRange -> IO ()
setDecoration_inRange avMutableCaption  decoration range =
  sendMsg avMutableCaption (mkSelector "setDecoration:inRange:") retVoid [argCULong (coerce decoration), argNSRange range]

-- | setTextCombine:inRange:
--
-- Set text combine for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setTextCombine:inRange:@
setTextCombine_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionTextCombine -> NSRange -> IO ()
setTextCombine_inRange avMutableCaption  textCombine range =
  sendMsg avMutableCaption (mkSelector "setTextCombine:inRange:") retVoid [argCLong (coerce textCombine), argNSRange range]

-- | setRuby:inRange:
--
-- Set ruby text  for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setRuby:inRange:@
setRuby_inRange :: (IsAVMutableCaption avMutableCaption, IsAVCaptionRuby ruby) => avMutableCaption -> ruby -> NSRange -> IO ()
setRuby_inRange avMutableCaption  ruby range =
withObjCPtr ruby $ \raw_ruby ->
    sendMsg avMutableCaption (mkSelector "setRuby:inRange:") retVoid [argPtr (castPtr raw_ruby :: Ptr ()), argNSRange range]

-- | removeTextColorInRange:
--
-- Remove text color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeTextColorInRange:@
removeTextColorInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeTextColorInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeTextColorInRange:") retVoid [argNSRange range]

-- | removeBackgroundColorInRange:
--
-- Remove background color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeBackgroundColorInRange:@
removeBackgroundColorInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeBackgroundColorInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeBackgroundColorInRange:") retVoid [argNSRange range]

-- | removeFontWeightInRange:
--
-- Remove font weight for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeFontWeightInRange:@
removeFontWeightInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeFontWeightInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeFontWeightInRange:") retVoid [argNSRange range]

-- | removeFontStyleInRange:
--
-- Remove font style for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeFontStyleInRange:@
removeFontStyleInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeFontStyleInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeFontStyleInRange:") retVoid [argNSRange range]

-- | removeDecorationInRange:
--
-- Remove text decoration for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeDecorationInRange:@
removeDecorationInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeDecorationInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeDecorationInRange:") retVoid [argNSRange range]

-- | removeTextCombineInRange:
--
-- Remove text combine for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeTextCombineInRange:@
removeTextCombineInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeTextCombineInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeTextCombineInRange:") retVoid [argNSRange range]

-- | removeRubyInRange:
--
-- Remove ruby text  for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeRubyInRange:@
removeRubyInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeRubyInRange avMutableCaption  range =
  sendMsg avMutableCaption (mkSelector "removeRubyInRange:") retVoid [argNSRange range]

-- | text
--
-- The text content of the caption.
--
-- All styling information is cleared on setting this property.
--
-- ObjC selector: @- text@
text :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO (Id NSString)
text avMutableCaption  =
  sendMsg avMutableCaption (mkSelector "text") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | text
--
-- The text content of the caption.
--
-- All styling information is cleared on setting this property.
--
-- ObjC selector: @- setText:@
setText :: (IsAVMutableCaption avMutableCaption, IsNSString value) => avMutableCaption -> value -> IO ()
setText avMutableCaption  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableCaption (mkSelector "setText:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- animation@
animation :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO AVCaptionAnimation
animation avMutableCaption  =
  fmap (coerce :: CLong -> AVCaptionAnimation) $ sendMsg avMutableCaption (mkSelector "animation") retCLong []

-- | @- setAnimation:@
setAnimation :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionAnimation -> IO ()
setAnimation avMutableCaption  value =
  sendMsg avMutableCaption (mkSelector "setAnimation:") retVoid [argCLong (coerce value)]

-- | region
--
-- The region where the caption is placed.
--
-- It can be nil when the underlying caption format doesn't support or use regions.	This property throws an exception if region has unrecognizeable units.
--
-- ObjC selector: @- region@
region :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO (Id AVCaptionRegion)
region avMutableCaption  =
  sendMsg avMutableCaption (mkSelector "region") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | region
--
-- The region where the caption is placed.
--
-- It can be nil when the underlying caption format doesn't support or use regions.	This property throws an exception if region has unrecognizeable units.
--
-- ObjC selector: @- setRegion:@
setRegion :: (IsAVMutableCaption avMutableCaption, IsAVCaptionRegion value) => avMutableCaption -> value -> IO ()
setRegion avMutableCaption  value =
withObjCPtr value $ \raw_value ->
    sendMsg avMutableCaption (mkSelector "setRegion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | textAlignment
--
-- The text alignemnt within the containing region.
--
-- ObjC selector: @- textAlignment@
textAlignment :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO AVCaptionTextAlignment
textAlignment avMutableCaption  =
  fmap (coerce :: CLong -> AVCaptionTextAlignment) $ sendMsg avMutableCaption (mkSelector "textAlignment") retCLong []

-- | textAlignment
--
-- The text alignemnt within the containing region.
--
-- ObjC selector: @- setTextAlignment:@
setTextAlignment :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionTextAlignment -> IO ()
setTextAlignment avMutableCaption  value =
  sendMsg avMutableCaption (mkSelector "setTextAlignment:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTextColor:inRange:@
setTextColor_inRangeSelector :: Selector
setTextColor_inRangeSelector = mkSelector "setTextColor:inRange:"

-- | @Selector@ for @setBackgroundColor:inRange:@
setBackgroundColor_inRangeSelector :: Selector
setBackgroundColor_inRangeSelector = mkSelector "setBackgroundColor:inRange:"

-- | @Selector@ for @setFontWeight:inRange:@
setFontWeight_inRangeSelector :: Selector
setFontWeight_inRangeSelector = mkSelector "setFontWeight:inRange:"

-- | @Selector@ for @setFontStyle:inRange:@
setFontStyle_inRangeSelector :: Selector
setFontStyle_inRangeSelector = mkSelector "setFontStyle:inRange:"

-- | @Selector@ for @setDecoration:inRange:@
setDecoration_inRangeSelector :: Selector
setDecoration_inRangeSelector = mkSelector "setDecoration:inRange:"

-- | @Selector@ for @setTextCombine:inRange:@
setTextCombine_inRangeSelector :: Selector
setTextCombine_inRangeSelector = mkSelector "setTextCombine:inRange:"

-- | @Selector@ for @setRuby:inRange:@
setRuby_inRangeSelector :: Selector
setRuby_inRangeSelector = mkSelector "setRuby:inRange:"

-- | @Selector@ for @removeTextColorInRange:@
removeTextColorInRangeSelector :: Selector
removeTextColorInRangeSelector = mkSelector "removeTextColorInRange:"

-- | @Selector@ for @removeBackgroundColorInRange:@
removeBackgroundColorInRangeSelector :: Selector
removeBackgroundColorInRangeSelector = mkSelector "removeBackgroundColorInRange:"

-- | @Selector@ for @removeFontWeightInRange:@
removeFontWeightInRangeSelector :: Selector
removeFontWeightInRangeSelector = mkSelector "removeFontWeightInRange:"

-- | @Selector@ for @removeFontStyleInRange:@
removeFontStyleInRangeSelector :: Selector
removeFontStyleInRangeSelector = mkSelector "removeFontStyleInRange:"

-- | @Selector@ for @removeDecorationInRange:@
removeDecorationInRangeSelector :: Selector
removeDecorationInRangeSelector = mkSelector "removeDecorationInRange:"

-- | @Selector@ for @removeTextCombineInRange:@
removeTextCombineInRangeSelector :: Selector
removeTextCombineInRangeSelector = mkSelector "removeTextCombineInRange:"

-- | @Selector@ for @removeRubyInRange:@
removeRubyInRangeSelector :: Selector
removeRubyInRangeSelector = mkSelector "removeRubyInRange:"

-- | @Selector@ for @text@
textSelector :: Selector
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @animation@
animationSelector :: Selector
animationSelector = mkSelector "animation"

-- | @Selector@ for @setAnimation:@
setAnimationSelector :: Selector
setAnimationSelector = mkSelector "setAnimation:"

-- | @Selector@ for @region@
regionSelector :: Selector
regionSelector = mkSelector "region"

-- | @Selector@ for @setRegion:@
setRegionSelector :: Selector
setRegionSelector = mkSelector "setRegion:"

-- | @Selector@ for @textAlignment@
textAlignmentSelector :: Selector
textAlignmentSelector = mkSelector "textAlignment"

-- | @Selector@ for @setTextAlignment:@
setTextAlignmentSelector :: Selector
setTextAlignmentSelector = mkSelector "setTextAlignment:"

