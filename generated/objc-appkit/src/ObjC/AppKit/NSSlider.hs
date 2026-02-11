{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSlider@.
module ObjC.AppKit.NSSlider
  ( NSSlider
  , IsNSSlider(..)
  , acceptsFirstMouse
  , setTitleCell
  , titleCell
  , setTitleColor
  , titleColor
  , setTitleFont
  , titleFont
  , title
  , setTitle
  , setKnobThickness
  , setImage
  , image
  , sliderWithTarget_action
  , sliderWithValue_minValue_maxValue_target_action
  , tickMarkValueAtIndex
  , rectOfTickMarkAtIndex
  , indexOfTickMarkAtPoint
  , closestTickMarkValueToValue
  , sliderType
  , setSliderType
  , minValue
  , setMinValue
  , maxValue
  , setMaxValue
  , neutralValue
  , setNeutralValue
  , altIncrementValue
  , setAltIncrementValue
  , knobThickness
  , vertical
  , setVertical
  , tintProminence
  , setTintProminence
  , numberOfTickMarks
  , setNumberOfTickMarks
  , tickMarkPosition
  , setTickMarkPosition
  , allowsTickMarkValuesOnly
  , setAllowsTickMarkValuesOnly
  , acceptsFirstMouseSelector
  , setTitleCellSelector
  , titleCellSelector
  , setTitleColorSelector
  , titleColorSelector
  , setTitleFontSelector
  , titleFontSelector
  , titleSelector
  , setTitleSelector
  , setKnobThicknessSelector
  , setImageSelector
  , imageSelector
  , sliderWithTarget_actionSelector
  , sliderWithValue_minValue_maxValue_target_actionSelector
  , tickMarkValueAtIndexSelector
  , rectOfTickMarkAtIndexSelector
  , indexOfTickMarkAtPointSelector
  , closestTickMarkValueToValueSelector
  , sliderTypeSelector
  , setSliderTypeSelector
  , minValueSelector
  , setMinValueSelector
  , maxValueSelector
  , setMaxValueSelector
  , neutralValueSelector
  , setNeutralValueSelector
  , altIncrementValueSelector
  , setAltIncrementValueSelector
  , knobThicknessSelector
  , verticalSelector
  , setVerticalSelector
  , tintProminenceSelector
  , setTintProminenceSelector
  , numberOfTickMarksSelector
  , setNumberOfTickMarksSelector
  , tickMarkPositionSelector
  , setTickMarkPositionSelector
  , allowsTickMarkValuesOnlySelector
  , setAllowsTickMarkValuesOnlySelector

  -- * Enum types
  , NSSliderType(NSSliderType)
  , pattern NSSliderTypeLinear
  , pattern NSSliderTypeCircular
  , NSTickMarkPosition(NSTickMarkPosition)
  , pattern NSTickMarkPositionBelow
  , pattern NSTickMarkPositionAbove
  , pattern NSTickMarkPositionLeading
  , pattern NSTickMarkPositionTrailing
  , NSTintProminence(NSTintProminence)
  , pattern NSTintProminenceAutomatic
  , pattern NSTintProminenceNone
  , pattern NSTintProminencePrimary
  , pattern NSTintProminenceSecondary

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

-- | @- acceptsFirstMouse:@
acceptsFirstMouse :: (IsNSSlider nsSlider, IsNSEvent event) => nsSlider -> event -> IO Bool
acceptsFirstMouse nsSlider  event =
withObjCPtr event $ \raw_event ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSlider (mkSelector "acceptsFirstMouse:") retCULong [argPtr (castPtr raw_event :: Ptr ())]

-- | @- setTitleCell:@
setTitleCell :: (IsNSSlider nsSlider, IsNSCell cell) => nsSlider -> cell -> IO ()
setTitleCell nsSlider  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsSlider (mkSelector "setTitleCell:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- titleCell@
titleCell :: IsNSSlider nsSlider => nsSlider -> IO RawId
titleCell nsSlider  =
  fmap (RawId . castPtr) $ sendMsg nsSlider (mkSelector "titleCell") (retPtr retVoid) []

-- | @- setTitleColor:@
setTitleColor :: (IsNSSlider nsSlider, IsNSColor newColor) => nsSlider -> newColor -> IO ()
setTitleColor nsSlider  newColor =
withObjCPtr newColor $ \raw_newColor ->
    sendMsg nsSlider (mkSelector "setTitleColor:") retVoid [argPtr (castPtr raw_newColor :: Ptr ())]

-- | @- titleColor@
titleColor :: IsNSSlider nsSlider => nsSlider -> IO (Id NSColor)
titleColor nsSlider  =
  sendMsg nsSlider (mkSelector "titleColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitleFont:@
setTitleFont :: (IsNSSlider nsSlider, IsNSFont fontObj) => nsSlider -> fontObj -> IO ()
setTitleFont nsSlider  fontObj =
withObjCPtr fontObj $ \raw_fontObj ->
    sendMsg nsSlider (mkSelector "setTitleFont:") retVoid [argPtr (castPtr raw_fontObj :: Ptr ())]

-- | @- titleFont@
titleFont :: IsNSSlider nsSlider => nsSlider -> IO (Id NSFont)
titleFont nsSlider  =
  sendMsg nsSlider (mkSelector "titleFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSSlider nsSlider => nsSlider -> IO (Id NSString)
title nsSlider  =
  sendMsg nsSlider (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSSlider nsSlider, IsNSString string) => nsSlider -> string -> IO ()
setTitle nsSlider  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsSlider (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- setKnobThickness:@
setKnobThickness :: IsNSSlider nsSlider => nsSlider -> CDouble -> IO ()
setKnobThickness nsSlider  thickness =
  sendMsg nsSlider (mkSelector "setKnobThickness:") retVoid [argCDouble (fromIntegral thickness)]

-- | @- setImage:@
setImage :: (IsNSSlider nsSlider, IsNSImage backgroundImage) => nsSlider -> backgroundImage -> IO ()
setImage nsSlider  backgroundImage =
withObjCPtr backgroundImage $ \raw_backgroundImage ->
    sendMsg nsSlider (mkSelector "setImage:") retVoid [argPtr (castPtr raw_backgroundImage :: Ptr ())]

-- | @- image@
image :: IsNSSlider nsSlider => nsSlider -> IO (Id NSImage)
image nsSlider  =
  sendMsg nsSlider (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Creates a continuous horizontal slider over the range 0.0 to 1.0. The default value is 0.0.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized slider control.
--
-- ObjC selector: @+ sliderWithTarget:action:@
sliderWithTarget_action :: RawId -> Selector -> IO (Id NSSlider)
sliderWithTarget_action target action =
  do
    cls' <- getRequiredClass "NSSlider"
    sendClassMsg cls' (mkSelector "sliderWithTarget:action:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | Creates a continuous horizontal slider that represents values over a specified range.
--
-- @value@ — The initial value displayed by the control.
--
-- @minValue@ — The minimum value represented by the control.
--
-- @maxValue@ — The maximum value represented by the control.
--
-- @target@ — The target object that receives action messages from the control.
--
-- @action@ — The action message sent by the control.
--
-- Returns: An initialized slider control.
--
-- ObjC selector: @+ sliderWithValue:minValue:maxValue:target:action:@
sliderWithValue_minValue_maxValue_target_action :: CDouble -> CDouble -> CDouble -> RawId -> Selector -> IO (Id NSSlider)
sliderWithValue_minValue_maxValue_target_action value minValue maxValue target action =
  do
    cls' <- getRequiredClass "NSSlider"
    sendClassMsg cls' (mkSelector "sliderWithValue:minValue:maxValue:target:action:") (retPtr retVoid) [argCDouble (fromIntegral value), argCDouble (fromIntegral minValue), argCDouble (fromIntegral maxValue), argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= retainedObject . castPtr

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSSlider nsSlider => nsSlider -> CLong -> IO CDouble
tickMarkValueAtIndex nsSlider  index =
  sendMsg nsSlider (mkSelector "tickMarkValueAtIndex:") retCDouble [argCLong (fromIntegral index)]

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSSlider nsSlider => nsSlider -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsSlider  index =
  sendMsgStret nsSlider (mkSelector "rectOfTickMarkAtIndex:") retNSRect [argCLong (fromIntegral index)]

-- | @- indexOfTickMarkAtPoint:@
indexOfTickMarkAtPoint :: IsNSSlider nsSlider => nsSlider -> NSPoint -> IO CLong
indexOfTickMarkAtPoint nsSlider  point =
  sendMsg nsSlider (mkSelector "indexOfTickMarkAtPoint:") retCLong [argNSPoint point]

-- | @- closestTickMarkValueToValue:@
closestTickMarkValueToValue :: IsNSSlider nsSlider => nsSlider -> CDouble -> IO CDouble
closestTickMarkValueToValue nsSlider  value =
  sendMsg nsSlider (mkSelector "closestTickMarkValueToValue:") retCDouble [argCDouble (fromIntegral value)]

-- | @- sliderType@
sliderType :: IsNSSlider nsSlider => nsSlider -> IO NSSliderType
sliderType nsSlider  =
  fmap (coerce :: CULong -> NSSliderType) $ sendMsg nsSlider (mkSelector "sliderType") retCULong []

-- | @- setSliderType:@
setSliderType :: IsNSSlider nsSlider => nsSlider -> NSSliderType -> IO ()
setSliderType nsSlider  value =
  sendMsg nsSlider (mkSelector "setSliderType:") retVoid [argCULong (coerce value)]

-- | @- minValue@
minValue :: IsNSSlider nsSlider => nsSlider -> IO CDouble
minValue nsSlider  =
  sendMsg nsSlider (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSSlider nsSlider => nsSlider -> CDouble -> IO ()
setMinValue nsSlider  value =
  sendMsg nsSlider (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxValue@
maxValue :: IsNSSlider nsSlider => nsSlider -> IO CDouble
maxValue nsSlider  =
  sendMsg nsSlider (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSSlider nsSlider => nsSlider -> CDouble -> IO ()
setMaxValue nsSlider  value =
  sendMsg nsSlider (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | The value this slider will be filled from. This slider will be filled from its @neutralValue@ to its current value. If @neutralValue@ has not been explicitly set before, access to @neutralValue@ will return @minValue@.
--
-- ObjC selector: @- neutralValue@
neutralValue :: IsNSSlider nsSlider => nsSlider -> IO CDouble
neutralValue nsSlider  =
  sendMsg nsSlider (mkSelector "neutralValue") retCDouble []

-- | The value this slider will be filled from. This slider will be filled from its @neutralValue@ to its current value. If @neutralValue@ has not been explicitly set before, access to @neutralValue@ will return @minValue@.
--
-- ObjC selector: @- setNeutralValue:@
setNeutralValue :: IsNSSlider nsSlider => nsSlider -> CDouble -> IO ()
setNeutralValue nsSlider  value =
  sendMsg nsSlider (mkSelector "setNeutralValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- altIncrementValue@
altIncrementValue :: IsNSSlider nsSlider => nsSlider -> IO CDouble
altIncrementValue nsSlider  =
  sendMsg nsSlider (mkSelector "altIncrementValue") retCDouble []

-- | @- setAltIncrementValue:@
setAltIncrementValue :: IsNSSlider nsSlider => nsSlider -> CDouble -> IO ()
setAltIncrementValue nsSlider  value =
  sendMsg nsSlider (mkSelector "setAltIncrementValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- knobThickness@
knobThickness :: IsNSSlider nsSlider => nsSlider -> IO CDouble
knobThickness nsSlider  =
  sendMsg nsSlider (mkSelector "knobThickness") retCDouble []

-- | @- vertical@
vertical :: IsNSSlider nsSlider => nsSlider -> IO Bool
vertical nsSlider  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSlider (mkSelector "vertical") retCULong []

-- | @- setVertical:@
setVertical :: IsNSSlider nsSlider => nsSlider -> Bool -> IO ()
setVertical nsSlider  value =
  sendMsg nsSlider (mkSelector "setVertical:") retVoid [argCULong (if value then 1 else 0)]

-- | The tint prominence of the slider. The automatic behavior for a regular slider tints its track fill, while a slider with tick marks is untinted. Setting the tint prominence will override this default behavior and choose an explicit track fill tint behavior. See ``NSTintProminence`` for a list of possible values.
--
-- ObjC selector: @- tintProminence@
tintProminence :: IsNSSlider nsSlider => nsSlider -> IO NSTintProminence
tintProminence nsSlider  =
  fmap (coerce :: CLong -> NSTintProminence) $ sendMsg nsSlider (mkSelector "tintProminence") retCLong []

-- | The tint prominence of the slider. The automatic behavior for a regular slider tints its track fill, while a slider with tick marks is untinted. Setting the tint prominence will override this default behavior and choose an explicit track fill tint behavior. See ``NSTintProminence`` for a list of possible values.
--
-- ObjC selector: @- setTintProminence:@
setTintProminence :: IsNSSlider nsSlider => nsSlider -> NSTintProminence -> IO ()
setTintProminence nsSlider  value =
  sendMsg nsSlider (mkSelector "setTintProminence:") retVoid [argCLong (coerce value)]

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSSlider nsSlider => nsSlider -> IO CLong
numberOfTickMarks nsSlider  =
  sendMsg nsSlider (mkSelector "numberOfTickMarks") retCLong []

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSSlider nsSlider => nsSlider -> CLong -> IO ()
setNumberOfTickMarks nsSlider  value =
  sendMsg nsSlider (mkSelector "setNumberOfTickMarks:") retVoid [argCLong (fromIntegral value)]

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSSlider nsSlider => nsSlider -> IO NSTickMarkPosition
tickMarkPosition nsSlider  =
  fmap (coerce :: CULong -> NSTickMarkPosition) $ sendMsg nsSlider (mkSelector "tickMarkPosition") retCULong []

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSSlider nsSlider => nsSlider -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsSlider  value =
  sendMsg nsSlider (mkSelector "setTickMarkPosition:") retVoid [argCULong (coerce value)]

-- | @- allowsTickMarkValuesOnly@
allowsTickMarkValuesOnly :: IsNSSlider nsSlider => nsSlider -> IO Bool
allowsTickMarkValuesOnly nsSlider  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSlider (mkSelector "allowsTickMarkValuesOnly") retCULong []

-- | @- setAllowsTickMarkValuesOnly:@
setAllowsTickMarkValuesOnly :: IsNSSlider nsSlider => nsSlider -> Bool -> IO ()
setAllowsTickMarkValuesOnly nsSlider  value =
  sendMsg nsSlider (mkSelector "setAllowsTickMarkValuesOnly:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @acceptsFirstMouse:@
acceptsFirstMouseSelector :: Selector
acceptsFirstMouseSelector = mkSelector "acceptsFirstMouse:"

-- | @Selector@ for @setTitleCell:@
setTitleCellSelector :: Selector
setTitleCellSelector = mkSelector "setTitleCell:"

-- | @Selector@ for @titleCell@
titleCellSelector :: Selector
titleCellSelector = mkSelector "titleCell"

-- | @Selector@ for @setTitleColor:@
setTitleColorSelector :: Selector
setTitleColorSelector = mkSelector "setTitleColor:"

-- | @Selector@ for @titleColor@
titleColorSelector :: Selector
titleColorSelector = mkSelector "titleColor"

-- | @Selector@ for @setTitleFont:@
setTitleFontSelector :: Selector
setTitleFontSelector = mkSelector "setTitleFont:"

-- | @Selector@ for @titleFont@
titleFontSelector :: Selector
titleFontSelector = mkSelector "titleFont"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @setKnobThickness:@
setKnobThicknessSelector :: Selector
setKnobThicknessSelector = mkSelector "setKnobThickness:"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @image@
imageSelector :: Selector
imageSelector = mkSelector "image"

-- | @Selector@ for @sliderWithTarget:action:@
sliderWithTarget_actionSelector :: Selector
sliderWithTarget_actionSelector = mkSelector "sliderWithTarget:action:"

-- | @Selector@ for @sliderWithValue:minValue:maxValue:target:action:@
sliderWithValue_minValue_maxValue_target_actionSelector :: Selector
sliderWithValue_minValue_maxValue_target_actionSelector = mkSelector "sliderWithValue:minValue:maxValue:target:action:"

-- | @Selector@ for @tickMarkValueAtIndex:@
tickMarkValueAtIndexSelector :: Selector
tickMarkValueAtIndexSelector = mkSelector "tickMarkValueAtIndex:"

-- | @Selector@ for @rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndexSelector :: Selector
rectOfTickMarkAtIndexSelector = mkSelector "rectOfTickMarkAtIndex:"

-- | @Selector@ for @indexOfTickMarkAtPoint:@
indexOfTickMarkAtPointSelector :: Selector
indexOfTickMarkAtPointSelector = mkSelector "indexOfTickMarkAtPoint:"

-- | @Selector@ for @closestTickMarkValueToValue:@
closestTickMarkValueToValueSelector :: Selector
closestTickMarkValueToValueSelector = mkSelector "closestTickMarkValueToValue:"

-- | @Selector@ for @sliderType@
sliderTypeSelector :: Selector
sliderTypeSelector = mkSelector "sliderType"

-- | @Selector@ for @setSliderType:@
setSliderTypeSelector :: Selector
setSliderTypeSelector = mkSelector "setSliderType:"

-- | @Selector@ for @minValue@
minValueSelector :: Selector
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @neutralValue@
neutralValueSelector :: Selector
neutralValueSelector = mkSelector "neutralValue"

-- | @Selector@ for @setNeutralValue:@
setNeutralValueSelector :: Selector
setNeutralValueSelector = mkSelector "setNeutralValue:"

-- | @Selector@ for @altIncrementValue@
altIncrementValueSelector :: Selector
altIncrementValueSelector = mkSelector "altIncrementValue"

-- | @Selector@ for @setAltIncrementValue:@
setAltIncrementValueSelector :: Selector
setAltIncrementValueSelector = mkSelector "setAltIncrementValue:"

-- | @Selector@ for @knobThickness@
knobThicknessSelector :: Selector
knobThicknessSelector = mkSelector "knobThickness"

-- | @Selector@ for @vertical@
verticalSelector :: Selector
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @setVertical:@
setVerticalSelector :: Selector
setVerticalSelector = mkSelector "setVertical:"

-- | @Selector@ for @tintProminence@
tintProminenceSelector :: Selector
tintProminenceSelector = mkSelector "tintProminence"

-- | @Selector@ for @setTintProminence:@
setTintProminenceSelector :: Selector
setTintProminenceSelector = mkSelector "setTintProminence:"

-- | @Selector@ for @numberOfTickMarks@
numberOfTickMarksSelector :: Selector
numberOfTickMarksSelector = mkSelector "numberOfTickMarks"

-- | @Selector@ for @setNumberOfTickMarks:@
setNumberOfTickMarksSelector :: Selector
setNumberOfTickMarksSelector = mkSelector "setNumberOfTickMarks:"

-- | @Selector@ for @tickMarkPosition@
tickMarkPositionSelector :: Selector
tickMarkPositionSelector = mkSelector "tickMarkPosition"

-- | @Selector@ for @setTickMarkPosition:@
setTickMarkPositionSelector :: Selector
setTickMarkPositionSelector = mkSelector "setTickMarkPosition:"

-- | @Selector@ for @allowsTickMarkValuesOnly@
allowsTickMarkValuesOnlySelector :: Selector
allowsTickMarkValuesOnlySelector = mkSelector "allowsTickMarkValuesOnly"

-- | @Selector@ for @setAllowsTickMarkValuesOnly:@
setAllowsTickMarkValuesOnlySelector :: Selector
setAllowsTickMarkValuesOnlySelector = mkSelector "setAllowsTickMarkValuesOnly:"

