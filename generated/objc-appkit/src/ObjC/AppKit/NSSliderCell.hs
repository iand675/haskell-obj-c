{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSSliderCell@.
module ObjC.AppKit.NSSliderCell
  ( NSSliderCell
  , IsNSSliderCell(..)
  , knobRectFlipped
  , barRectFlipped
  , drawKnob
  , drawBarInside_flipped
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
  , tickMarkValueAtIndex
  , rectOfTickMarkAtIndex
  , indexOfTickMarkAtPoint
  , closestTickMarkValueToValue
  , drawTickMarks
  , prefersTrackingUntilMouseUp
  , minValue
  , setMinValue
  , maxValue
  , setMaxValue
  , altIncrementValue
  , setAltIncrementValue
  , sliderType
  , setSliderType
  , vertical
  , setVertical
  , trackRect
  , knobThickness
  , numberOfTickMarks
  , setNumberOfTickMarks
  , tickMarkPosition
  , setTickMarkPosition
  , allowsTickMarkValuesOnly
  , setAllowsTickMarkValuesOnly
  , knobRectFlippedSelector
  , barRectFlippedSelector
  , drawKnobSelector
  , drawBarInside_flippedSelector
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
  , tickMarkValueAtIndexSelector
  , rectOfTickMarkAtIndexSelector
  , indexOfTickMarkAtPointSelector
  , closestTickMarkValueToValueSelector
  , drawTickMarksSelector
  , prefersTrackingUntilMouseUpSelector
  , minValueSelector
  , setMinValueSelector
  , maxValueSelector
  , setMaxValueSelector
  , altIncrementValueSelector
  , setAltIncrementValueSelector
  , sliderTypeSelector
  , setSliderTypeSelector
  , verticalSelector
  , setVerticalSelector
  , trackRectSelector
  , knobThicknessSelector
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

-- | @- knobRectFlipped:@
knobRectFlipped :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO NSRect
knobRectFlipped nsSliderCell  flipped =
  sendMsgStret nsSliderCell (mkSelector "knobRectFlipped:") retNSRect [argCULong (if flipped then 1 else 0)]

-- | @- barRectFlipped:@
barRectFlipped :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO NSRect
barRectFlipped nsSliderCell  flipped =
  sendMsgStret nsSliderCell (mkSelector "barRectFlipped:") retNSRect [argCULong (if flipped then 1 else 0)]

-- | @- drawKnob:@
drawKnob :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSRect -> IO ()
drawKnob nsSliderCell  knobRect =
  sendMsg nsSliderCell (mkSelector "drawKnob:") retVoid [argNSRect knobRect]

-- | @- drawBarInside:flipped:@
drawBarInside_flipped :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSRect -> Bool -> IO ()
drawBarInside_flipped nsSliderCell  rect flipped =
  sendMsg nsSliderCell (mkSelector "drawBarInside:flipped:") retVoid [argNSRect rect, argCULong (if flipped then 1 else 0)]

-- | @- setTitleCell:@
setTitleCell :: (IsNSSliderCell nsSliderCell, IsNSCell cell) => nsSliderCell -> cell -> IO ()
setTitleCell nsSliderCell  cell =
withObjCPtr cell $ \raw_cell ->
    sendMsg nsSliderCell (mkSelector "setTitleCell:") retVoid [argPtr (castPtr raw_cell :: Ptr ())]

-- | @- titleCell@
titleCell :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO RawId
titleCell nsSliderCell  =
  fmap (RawId . castPtr) $ sendMsg nsSliderCell (mkSelector "titleCell") (retPtr retVoid) []

-- | @- setTitleColor:@
setTitleColor :: (IsNSSliderCell nsSliderCell, IsNSColor newColor) => nsSliderCell -> newColor -> IO ()
setTitleColor nsSliderCell  newColor =
withObjCPtr newColor $ \raw_newColor ->
    sendMsg nsSliderCell (mkSelector "setTitleColor:") retVoid [argPtr (castPtr raw_newColor :: Ptr ())]

-- | @- titleColor@
titleColor :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSColor)
titleColor nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "titleColor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitleFont:@
setTitleFont :: (IsNSSliderCell nsSliderCell, IsNSFont fontObj) => nsSliderCell -> fontObj -> IO ()
setTitleFont nsSliderCell  fontObj =
withObjCPtr fontObj $ \raw_fontObj ->
    sendMsg nsSliderCell (mkSelector "setTitleFont:") retVoid [argPtr (castPtr raw_fontObj :: Ptr ())]

-- | @- titleFont@
titleFont :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSFont)
titleFont nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "titleFont") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- title@
title :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSString)
title nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsNSSliderCell nsSliderCell, IsNSString string) => nsSliderCell -> string -> IO ()
setTitle nsSliderCell  string =
withObjCPtr string $ \raw_string ->
    sendMsg nsSliderCell (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_string :: Ptr ())]

-- | @- setKnobThickness:@
setKnobThickness :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setKnobThickness nsSliderCell  thickness =
  sendMsg nsSliderCell (mkSelector "setKnobThickness:") retVoid [argCDouble (fromIntegral thickness)]

-- | @- setImage:@
setImage :: (IsNSSliderCell nsSliderCell, IsNSImage backgroundImage) => nsSliderCell -> backgroundImage -> IO ()
setImage nsSliderCell  backgroundImage =
withObjCPtr backgroundImage $ \raw_backgroundImage ->
    sendMsg nsSliderCell (mkSelector "setImage:") retVoid [argPtr (castPtr raw_backgroundImage :: Ptr ())]

-- | @- image@
image :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSImage)
image nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "image") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSSliderCell nsSliderCell => nsSliderCell -> CLong -> IO CDouble
tickMarkValueAtIndex nsSliderCell  index =
  sendMsg nsSliderCell (mkSelector "tickMarkValueAtIndex:") retCDouble [argCLong (fromIntegral index)]

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSSliderCell nsSliderCell => nsSliderCell -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsSliderCell  index =
  sendMsgStret nsSliderCell (mkSelector "rectOfTickMarkAtIndex:") retNSRect [argCLong (fromIntegral index)]

-- | @- indexOfTickMarkAtPoint:@
indexOfTickMarkAtPoint :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSPoint -> IO CLong
indexOfTickMarkAtPoint nsSliderCell  point =
  sendMsg nsSliderCell (mkSelector "indexOfTickMarkAtPoint:") retCLong [argNSPoint point]

-- | @- closestTickMarkValueToValue:@
closestTickMarkValueToValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO CDouble
closestTickMarkValueToValue nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "closestTickMarkValueToValue:") retCDouble [argCDouble (fromIntegral value)]

-- | @- drawTickMarks@
drawTickMarks :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO ()
drawTickMarks nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "drawTickMarks") retVoid []

-- | @+ prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUp :: IO Bool
prefersTrackingUntilMouseUp  =
  do
    cls' <- getRequiredClass "NSSliderCell"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "prefersTrackingUntilMouseUp") retCULong []

-- | @- minValue@
minValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
minValue nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "minValue") retCDouble []

-- | @- setMinValue:@
setMinValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setMinValue nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setMinValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- maxValue@
maxValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
maxValue nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "maxValue") retCDouble []

-- | @- setMaxValue:@
setMaxValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setMaxValue nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setMaxValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- altIncrementValue@
altIncrementValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
altIncrementValue nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "altIncrementValue") retCDouble []

-- | @- setAltIncrementValue:@
setAltIncrementValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setAltIncrementValue nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setAltIncrementValue:") retVoid [argCDouble (fromIntegral value)]

-- | @- sliderType@
sliderType :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO NSSliderType
sliderType nsSliderCell  =
  fmap (coerce :: CULong -> NSSliderType) $ sendMsg nsSliderCell (mkSelector "sliderType") retCULong []

-- | @- setSliderType:@
setSliderType :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSSliderType -> IO ()
setSliderType nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setSliderType:") retVoid [argCULong (coerce value)]

-- | @- vertical@
vertical :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO Bool
vertical nsSliderCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSliderCell (mkSelector "vertical") retCULong []

-- | @- setVertical:@
setVertical :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO ()
setVertical nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setVertical:") retVoid [argCULong (if value then 1 else 0)]

-- | @- trackRect@
trackRect :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO NSRect
trackRect nsSliderCell  =
  sendMsgStret nsSliderCell (mkSelector "trackRect") retNSRect []

-- | @- knobThickness@
knobThickness :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
knobThickness nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "knobThickness") retCDouble []

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CLong
numberOfTickMarks nsSliderCell  =
  sendMsg nsSliderCell (mkSelector "numberOfTickMarks") retCLong []

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSSliderCell nsSliderCell => nsSliderCell -> CLong -> IO ()
setNumberOfTickMarks nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setNumberOfTickMarks:") retVoid [argCLong (fromIntegral value)]

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO NSTickMarkPosition
tickMarkPosition nsSliderCell  =
  fmap (coerce :: CULong -> NSTickMarkPosition) $ sendMsg nsSliderCell (mkSelector "tickMarkPosition") retCULong []

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setTickMarkPosition:") retVoid [argCULong (coerce value)]

-- | @- allowsTickMarkValuesOnly@
allowsTickMarkValuesOnly :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO Bool
allowsTickMarkValuesOnly nsSliderCell  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsSliderCell (mkSelector "allowsTickMarkValuesOnly") retCULong []

-- | @- setAllowsTickMarkValuesOnly:@
setAllowsTickMarkValuesOnly :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO ()
setAllowsTickMarkValuesOnly nsSliderCell  value =
  sendMsg nsSliderCell (mkSelector "setAllowsTickMarkValuesOnly:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @knobRectFlipped:@
knobRectFlippedSelector :: Selector
knobRectFlippedSelector = mkSelector "knobRectFlipped:"

-- | @Selector@ for @barRectFlipped:@
barRectFlippedSelector :: Selector
barRectFlippedSelector = mkSelector "barRectFlipped:"

-- | @Selector@ for @drawKnob:@
drawKnobSelector :: Selector
drawKnobSelector = mkSelector "drawKnob:"

-- | @Selector@ for @drawBarInside:flipped:@
drawBarInside_flippedSelector :: Selector
drawBarInside_flippedSelector = mkSelector "drawBarInside:flipped:"

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

-- | @Selector@ for @drawTickMarks@
drawTickMarksSelector :: Selector
drawTickMarksSelector = mkSelector "drawTickMarks"

-- | @Selector@ for @prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUpSelector :: Selector
prefersTrackingUntilMouseUpSelector = mkSelector "prefersTrackingUntilMouseUp"

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

-- | @Selector@ for @altIncrementValue@
altIncrementValueSelector :: Selector
altIncrementValueSelector = mkSelector "altIncrementValue"

-- | @Selector@ for @setAltIncrementValue:@
setAltIncrementValueSelector :: Selector
setAltIncrementValueSelector = mkSelector "setAltIncrementValue:"

-- | @Selector@ for @sliderType@
sliderTypeSelector :: Selector
sliderTypeSelector = mkSelector "sliderType"

-- | @Selector@ for @setSliderType:@
setSliderTypeSelector :: Selector
setSliderTypeSelector = mkSelector "setSliderType:"

-- | @Selector@ for @vertical@
verticalSelector :: Selector
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @setVertical:@
setVerticalSelector :: Selector
setVerticalSelector = mkSelector "setVertical:"

-- | @Selector@ for @trackRect@
trackRectSelector :: Selector
trackRectSelector = mkSelector "trackRect"

-- | @Selector@ for @knobThickness@
knobThicknessSelector :: Selector
knobThicknessSelector = mkSelector "knobThickness"

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

