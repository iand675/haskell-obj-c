{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , animationSelector
  , regionSelector
  , removeBackgroundColorInRangeSelector
  , removeDecorationInRangeSelector
  , removeFontStyleInRangeSelector
  , removeFontWeightInRangeSelector
  , removeRubyInRangeSelector
  , removeTextColorInRangeSelector
  , removeTextCombineInRangeSelector
  , setAnimationSelector
  , setBackgroundColor_inRangeSelector
  , setDecoration_inRangeSelector
  , setFontStyle_inRangeSelector
  , setFontWeight_inRangeSelector
  , setRegionSelector
  , setRuby_inRangeSelector
  , setTextAlignmentSelector
  , setTextColor_inRangeSelector
  , setTextCombine_inRangeSelector
  , setTextSelector
  , textAlignmentSelector
  , textSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
setTextColor_inRange avMutableCaption color range =
  sendMessage avMutableCaption setTextColor_inRangeSelector color range

-- | setBackgroundColor:inRange:
--
-- Set background color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setBackgroundColor:inRange:@
setBackgroundColor_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> Ptr () -> NSRange -> IO ()
setBackgroundColor_inRange avMutableCaption color range =
  sendMessage avMutableCaption setBackgroundColor_inRangeSelector color range

-- | setFontWeight:inRange:
--
-- Set font weight for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setFontWeight:inRange:@
setFontWeight_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionFontWeight -> NSRange -> IO ()
setFontWeight_inRange avMutableCaption fontWeight range =
  sendMessage avMutableCaption setFontWeight_inRangeSelector fontWeight range

-- | setFontStyle:inRange:
--
-- Set font style for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setFontStyle:inRange:@
setFontStyle_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionFontStyle -> NSRange -> IO ()
setFontStyle_inRange avMutableCaption fontStyle range =
  sendMessage avMutableCaption setFontStyle_inRangeSelector fontStyle range

-- | setDecoration:inRange:
--
-- Set text decoration for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setDecoration:inRange:@
setDecoration_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionDecoration -> NSRange -> IO ()
setDecoration_inRange avMutableCaption decoration range =
  sendMessage avMutableCaption setDecoration_inRangeSelector decoration range

-- | setTextCombine:inRange:
--
-- Set text combine for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setTextCombine:inRange:@
setTextCombine_inRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionTextCombine -> NSRange -> IO ()
setTextCombine_inRange avMutableCaption textCombine range =
  sendMessage avMutableCaption setTextCombine_inRangeSelector textCombine range

-- | setRuby:inRange:
--
-- Set ruby text  for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- setRuby:inRange:@
setRuby_inRange :: (IsAVMutableCaption avMutableCaption, IsAVCaptionRuby ruby) => avMutableCaption -> ruby -> NSRange -> IO ()
setRuby_inRange avMutableCaption ruby range =
  sendMessage avMutableCaption setRuby_inRangeSelector (toAVCaptionRuby ruby) range

-- | removeTextColorInRange:
--
-- Remove text color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeTextColorInRange:@
removeTextColorInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeTextColorInRange avMutableCaption range =
  sendMessage avMutableCaption removeTextColorInRangeSelector range

-- | removeBackgroundColorInRange:
--
-- Remove background color for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeBackgroundColorInRange:@
removeBackgroundColorInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeBackgroundColorInRange avMutableCaption range =
  sendMessage avMutableCaption removeBackgroundColorInRangeSelector range

-- | removeFontWeightInRange:
--
-- Remove font weight for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeFontWeightInRange:@
removeFontWeightInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeFontWeightInRange avMutableCaption range =
  sendMessage avMutableCaption removeFontWeightInRangeSelector range

-- | removeFontStyleInRange:
--
-- Remove font style for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeFontStyleInRange:@
removeFontStyleInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeFontStyleInRange avMutableCaption range =
  sendMessage avMutableCaption removeFontStyleInRangeSelector range

-- | removeDecorationInRange:
--
-- Remove text decoration for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeDecorationInRange:@
removeDecorationInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeDecorationInRange avMutableCaption range =
  sendMessage avMutableCaption removeDecorationInRangeSelector range

-- | removeTextCombineInRange:
--
-- Remove text combine for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeTextCombineInRange:@
removeTextCombineInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeTextCombineInRange avMutableCaption range =
  sendMessage avMutableCaption removeTextCombineInRangeSelector range

-- | removeRubyInRange:
--
-- Remove ruby text  for the range.
--
-- The range parameter uses UTF-16 code unit index range.
--
-- ObjC selector: @- removeRubyInRange:@
removeRubyInRange :: IsAVMutableCaption avMutableCaption => avMutableCaption -> NSRange -> IO ()
removeRubyInRange avMutableCaption range =
  sendMessage avMutableCaption removeRubyInRangeSelector range

-- | text
--
-- The text content of the caption.
--
-- All styling information is cleared on setting this property.
--
-- ObjC selector: @- text@
text :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO (Id NSString)
text avMutableCaption =
  sendMessage avMutableCaption textSelector

-- | text
--
-- The text content of the caption.
--
-- All styling information is cleared on setting this property.
--
-- ObjC selector: @- setText:@
setText :: (IsAVMutableCaption avMutableCaption, IsNSString value) => avMutableCaption -> value -> IO ()
setText avMutableCaption value =
  sendMessage avMutableCaption setTextSelector (toNSString value)

-- | @- animation@
animation :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO AVCaptionAnimation
animation avMutableCaption =
  sendMessage avMutableCaption animationSelector

-- | @- setAnimation:@
setAnimation :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionAnimation -> IO ()
setAnimation avMutableCaption value =
  sendMessage avMutableCaption setAnimationSelector value

-- | region
--
-- The region where the caption is placed.
--
-- It can be nil when the underlying caption format doesn't support or use regions.	This property throws an exception if region has unrecognizeable units.
--
-- ObjC selector: @- region@
region :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO (Id AVCaptionRegion)
region avMutableCaption =
  sendMessage avMutableCaption regionSelector

-- | region
--
-- The region where the caption is placed.
--
-- It can be nil when the underlying caption format doesn't support or use regions.	This property throws an exception if region has unrecognizeable units.
--
-- ObjC selector: @- setRegion:@
setRegion :: (IsAVMutableCaption avMutableCaption, IsAVCaptionRegion value) => avMutableCaption -> value -> IO ()
setRegion avMutableCaption value =
  sendMessage avMutableCaption setRegionSelector (toAVCaptionRegion value)

-- | textAlignment
--
-- The text alignemnt within the containing region.
--
-- ObjC selector: @- textAlignment@
textAlignment :: IsAVMutableCaption avMutableCaption => avMutableCaption -> IO AVCaptionTextAlignment
textAlignment avMutableCaption =
  sendMessage avMutableCaption textAlignmentSelector

-- | textAlignment
--
-- The text alignemnt within the containing region.
--
-- ObjC selector: @- setTextAlignment:@
setTextAlignment :: IsAVMutableCaption avMutableCaption => avMutableCaption -> AVCaptionTextAlignment -> IO ()
setTextAlignment avMutableCaption value =
  sendMessage avMutableCaption setTextAlignmentSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setTextColor:inRange:@
setTextColor_inRangeSelector :: Selector '[Ptr (), NSRange] ()
setTextColor_inRangeSelector = mkSelector "setTextColor:inRange:"

-- | @Selector@ for @setBackgroundColor:inRange:@
setBackgroundColor_inRangeSelector :: Selector '[Ptr (), NSRange] ()
setBackgroundColor_inRangeSelector = mkSelector "setBackgroundColor:inRange:"

-- | @Selector@ for @setFontWeight:inRange:@
setFontWeight_inRangeSelector :: Selector '[AVCaptionFontWeight, NSRange] ()
setFontWeight_inRangeSelector = mkSelector "setFontWeight:inRange:"

-- | @Selector@ for @setFontStyle:inRange:@
setFontStyle_inRangeSelector :: Selector '[AVCaptionFontStyle, NSRange] ()
setFontStyle_inRangeSelector = mkSelector "setFontStyle:inRange:"

-- | @Selector@ for @setDecoration:inRange:@
setDecoration_inRangeSelector :: Selector '[AVCaptionDecoration, NSRange] ()
setDecoration_inRangeSelector = mkSelector "setDecoration:inRange:"

-- | @Selector@ for @setTextCombine:inRange:@
setTextCombine_inRangeSelector :: Selector '[AVCaptionTextCombine, NSRange] ()
setTextCombine_inRangeSelector = mkSelector "setTextCombine:inRange:"

-- | @Selector@ for @setRuby:inRange:@
setRuby_inRangeSelector :: Selector '[Id AVCaptionRuby, NSRange] ()
setRuby_inRangeSelector = mkSelector "setRuby:inRange:"

-- | @Selector@ for @removeTextColorInRange:@
removeTextColorInRangeSelector :: Selector '[NSRange] ()
removeTextColorInRangeSelector = mkSelector "removeTextColorInRange:"

-- | @Selector@ for @removeBackgroundColorInRange:@
removeBackgroundColorInRangeSelector :: Selector '[NSRange] ()
removeBackgroundColorInRangeSelector = mkSelector "removeBackgroundColorInRange:"

-- | @Selector@ for @removeFontWeightInRange:@
removeFontWeightInRangeSelector :: Selector '[NSRange] ()
removeFontWeightInRangeSelector = mkSelector "removeFontWeightInRange:"

-- | @Selector@ for @removeFontStyleInRange:@
removeFontStyleInRangeSelector :: Selector '[NSRange] ()
removeFontStyleInRangeSelector = mkSelector "removeFontStyleInRange:"

-- | @Selector@ for @removeDecorationInRange:@
removeDecorationInRangeSelector :: Selector '[NSRange] ()
removeDecorationInRangeSelector = mkSelector "removeDecorationInRange:"

-- | @Selector@ for @removeTextCombineInRange:@
removeTextCombineInRangeSelector :: Selector '[NSRange] ()
removeTextCombineInRangeSelector = mkSelector "removeTextCombineInRange:"

-- | @Selector@ for @removeRubyInRange:@
removeRubyInRangeSelector :: Selector '[NSRange] ()
removeRubyInRangeSelector = mkSelector "removeRubyInRange:"

-- | @Selector@ for @text@
textSelector :: Selector '[] (Id NSString)
textSelector = mkSelector "text"

-- | @Selector@ for @setText:@
setTextSelector :: Selector '[Id NSString] ()
setTextSelector = mkSelector "setText:"

-- | @Selector@ for @animation@
animationSelector :: Selector '[] AVCaptionAnimation
animationSelector = mkSelector "animation"

-- | @Selector@ for @setAnimation:@
setAnimationSelector :: Selector '[AVCaptionAnimation] ()
setAnimationSelector = mkSelector "setAnimation:"

-- | @Selector@ for @region@
regionSelector :: Selector '[] (Id AVCaptionRegion)
regionSelector = mkSelector "region"

-- | @Selector@ for @setRegion:@
setRegionSelector :: Selector '[Id AVCaptionRegion] ()
setRegionSelector = mkSelector "setRegion:"

-- | @Selector@ for @textAlignment@
textAlignmentSelector :: Selector '[] AVCaptionTextAlignment
textAlignmentSelector = mkSelector "textAlignment"

-- | @Selector@ for @setTextAlignment:@
setTextAlignmentSelector :: Selector '[AVCaptionTextAlignment] ()
setTextAlignmentSelector = mkSelector "setTextAlignment:"

