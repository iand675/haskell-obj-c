{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , allowsTickMarkValuesOnlySelector
  , altIncrementValueSelector
  , barRectFlippedSelector
  , closestTickMarkValueToValueSelector
  , drawBarInside_flippedSelector
  , drawKnobSelector
  , drawTickMarksSelector
  , imageSelector
  , indexOfTickMarkAtPointSelector
  , knobRectFlippedSelector
  , knobThicknessSelector
  , maxValueSelector
  , minValueSelector
  , numberOfTickMarksSelector
  , prefersTrackingUntilMouseUpSelector
  , rectOfTickMarkAtIndexSelector
  , setAllowsTickMarkValuesOnlySelector
  , setAltIncrementValueSelector
  , setImageSelector
  , setKnobThicknessSelector
  , setMaxValueSelector
  , setMinValueSelector
  , setNumberOfTickMarksSelector
  , setSliderTypeSelector
  , setTickMarkPositionSelector
  , setTitleCellSelector
  , setTitleColorSelector
  , setTitleFontSelector
  , setTitleSelector
  , setVerticalSelector
  , sliderTypeSelector
  , tickMarkPositionSelector
  , tickMarkValueAtIndexSelector
  , titleCellSelector
  , titleColorSelector
  , titleFontSelector
  , titleSelector
  , trackRectSelector
  , verticalSelector

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

-- | @- knobRectFlipped:@
knobRectFlipped :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO NSRect
knobRectFlipped nsSliderCell flipped =
  sendMessage nsSliderCell knobRectFlippedSelector flipped

-- | @- barRectFlipped:@
barRectFlipped :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO NSRect
barRectFlipped nsSliderCell flipped =
  sendMessage nsSliderCell barRectFlippedSelector flipped

-- | @- drawKnob:@
drawKnob :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSRect -> IO ()
drawKnob nsSliderCell knobRect =
  sendMessage nsSliderCell drawKnobSelector knobRect

-- | @- drawBarInside:flipped:@
drawBarInside_flipped :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSRect -> Bool -> IO ()
drawBarInside_flipped nsSliderCell rect flipped =
  sendMessage nsSliderCell drawBarInside_flippedSelector rect flipped

-- | @- setTitleCell:@
setTitleCell :: (IsNSSliderCell nsSliderCell, IsNSCell cell) => nsSliderCell -> cell -> IO ()
setTitleCell nsSliderCell cell =
  sendMessage nsSliderCell setTitleCellSelector (toNSCell cell)

-- | @- titleCell@
titleCell :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO RawId
titleCell nsSliderCell =
  sendMessage nsSliderCell titleCellSelector

-- | @- setTitleColor:@
setTitleColor :: (IsNSSliderCell nsSliderCell, IsNSColor newColor) => nsSliderCell -> newColor -> IO ()
setTitleColor nsSliderCell newColor =
  sendMessage nsSliderCell setTitleColorSelector (toNSColor newColor)

-- | @- titleColor@
titleColor :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSColor)
titleColor nsSliderCell =
  sendMessage nsSliderCell titleColorSelector

-- | @- setTitleFont:@
setTitleFont :: (IsNSSliderCell nsSliderCell, IsNSFont fontObj) => nsSliderCell -> fontObj -> IO ()
setTitleFont nsSliderCell fontObj =
  sendMessage nsSliderCell setTitleFontSelector (toNSFont fontObj)

-- | @- titleFont@
titleFont :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSFont)
titleFont nsSliderCell =
  sendMessage nsSliderCell titleFontSelector

-- | @- title@
title :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSString)
title nsSliderCell =
  sendMessage nsSliderCell titleSelector

-- | @- setTitle:@
setTitle :: (IsNSSliderCell nsSliderCell, IsNSString string) => nsSliderCell -> string -> IO ()
setTitle nsSliderCell string =
  sendMessage nsSliderCell setTitleSelector (toNSString string)

-- | @- setKnobThickness:@
setKnobThickness :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setKnobThickness nsSliderCell thickness =
  sendMessage nsSliderCell setKnobThicknessSelector thickness

-- | @- setImage:@
setImage :: (IsNSSliderCell nsSliderCell, IsNSImage backgroundImage) => nsSliderCell -> backgroundImage -> IO ()
setImage nsSliderCell backgroundImage =
  sendMessage nsSliderCell setImageSelector (toNSImage backgroundImage)

-- | @- image@
image :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO (Id NSImage)
image nsSliderCell =
  sendMessage nsSliderCell imageSelector

-- | @- tickMarkValueAtIndex:@
tickMarkValueAtIndex :: IsNSSliderCell nsSliderCell => nsSliderCell -> CLong -> IO CDouble
tickMarkValueAtIndex nsSliderCell index =
  sendMessage nsSliderCell tickMarkValueAtIndexSelector index

-- | @- rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndex :: IsNSSliderCell nsSliderCell => nsSliderCell -> CLong -> IO NSRect
rectOfTickMarkAtIndex nsSliderCell index =
  sendMessage nsSliderCell rectOfTickMarkAtIndexSelector index

-- | @- indexOfTickMarkAtPoint:@
indexOfTickMarkAtPoint :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSPoint -> IO CLong
indexOfTickMarkAtPoint nsSliderCell point =
  sendMessage nsSliderCell indexOfTickMarkAtPointSelector point

-- | @- closestTickMarkValueToValue:@
closestTickMarkValueToValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO CDouble
closestTickMarkValueToValue nsSliderCell value =
  sendMessage nsSliderCell closestTickMarkValueToValueSelector value

-- | @- drawTickMarks@
drawTickMarks :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO ()
drawTickMarks nsSliderCell =
  sendMessage nsSliderCell drawTickMarksSelector

-- | @+ prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUp :: IO Bool
prefersTrackingUntilMouseUp  =
  do
    cls' <- getRequiredClass "NSSliderCell"
    sendClassMessage cls' prefersTrackingUntilMouseUpSelector

-- | @- minValue@
minValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
minValue nsSliderCell =
  sendMessage nsSliderCell minValueSelector

-- | @- setMinValue:@
setMinValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setMinValue nsSliderCell value =
  sendMessage nsSliderCell setMinValueSelector value

-- | @- maxValue@
maxValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
maxValue nsSliderCell =
  sendMessage nsSliderCell maxValueSelector

-- | @- setMaxValue:@
setMaxValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setMaxValue nsSliderCell value =
  sendMessage nsSliderCell setMaxValueSelector value

-- | @- altIncrementValue@
altIncrementValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
altIncrementValue nsSliderCell =
  sendMessage nsSliderCell altIncrementValueSelector

-- | @- setAltIncrementValue:@
setAltIncrementValue :: IsNSSliderCell nsSliderCell => nsSliderCell -> CDouble -> IO ()
setAltIncrementValue nsSliderCell value =
  sendMessage nsSliderCell setAltIncrementValueSelector value

-- | @- sliderType@
sliderType :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO NSSliderType
sliderType nsSliderCell =
  sendMessage nsSliderCell sliderTypeSelector

-- | @- setSliderType:@
setSliderType :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSSliderType -> IO ()
setSliderType nsSliderCell value =
  sendMessage nsSliderCell setSliderTypeSelector value

-- | @- vertical@
vertical :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO Bool
vertical nsSliderCell =
  sendMessage nsSliderCell verticalSelector

-- | @- setVertical:@
setVertical :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO ()
setVertical nsSliderCell value =
  sendMessage nsSliderCell setVerticalSelector value

-- | @- trackRect@
trackRect :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO NSRect
trackRect nsSliderCell =
  sendMessage nsSliderCell trackRectSelector

-- | @- knobThickness@
knobThickness :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CDouble
knobThickness nsSliderCell =
  sendMessage nsSliderCell knobThicknessSelector

-- | @- numberOfTickMarks@
numberOfTickMarks :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO CLong
numberOfTickMarks nsSliderCell =
  sendMessage nsSliderCell numberOfTickMarksSelector

-- | @- setNumberOfTickMarks:@
setNumberOfTickMarks :: IsNSSliderCell nsSliderCell => nsSliderCell -> CLong -> IO ()
setNumberOfTickMarks nsSliderCell value =
  sendMessage nsSliderCell setNumberOfTickMarksSelector value

-- | @- tickMarkPosition@
tickMarkPosition :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO NSTickMarkPosition
tickMarkPosition nsSliderCell =
  sendMessage nsSliderCell tickMarkPositionSelector

-- | @- setTickMarkPosition:@
setTickMarkPosition :: IsNSSliderCell nsSliderCell => nsSliderCell -> NSTickMarkPosition -> IO ()
setTickMarkPosition nsSliderCell value =
  sendMessage nsSliderCell setTickMarkPositionSelector value

-- | @- allowsTickMarkValuesOnly@
allowsTickMarkValuesOnly :: IsNSSliderCell nsSliderCell => nsSliderCell -> IO Bool
allowsTickMarkValuesOnly nsSliderCell =
  sendMessage nsSliderCell allowsTickMarkValuesOnlySelector

-- | @- setAllowsTickMarkValuesOnly:@
setAllowsTickMarkValuesOnly :: IsNSSliderCell nsSliderCell => nsSliderCell -> Bool -> IO ()
setAllowsTickMarkValuesOnly nsSliderCell value =
  sendMessage nsSliderCell setAllowsTickMarkValuesOnlySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @knobRectFlipped:@
knobRectFlippedSelector :: Selector '[Bool] NSRect
knobRectFlippedSelector = mkSelector "knobRectFlipped:"

-- | @Selector@ for @barRectFlipped:@
barRectFlippedSelector :: Selector '[Bool] NSRect
barRectFlippedSelector = mkSelector "barRectFlipped:"

-- | @Selector@ for @drawKnob:@
drawKnobSelector :: Selector '[NSRect] ()
drawKnobSelector = mkSelector "drawKnob:"

-- | @Selector@ for @drawBarInside:flipped:@
drawBarInside_flippedSelector :: Selector '[NSRect, Bool] ()
drawBarInside_flippedSelector = mkSelector "drawBarInside:flipped:"

-- | @Selector@ for @setTitleCell:@
setTitleCellSelector :: Selector '[Id NSCell] ()
setTitleCellSelector = mkSelector "setTitleCell:"

-- | @Selector@ for @titleCell@
titleCellSelector :: Selector '[] RawId
titleCellSelector = mkSelector "titleCell"

-- | @Selector@ for @setTitleColor:@
setTitleColorSelector :: Selector '[Id NSColor] ()
setTitleColorSelector = mkSelector "setTitleColor:"

-- | @Selector@ for @titleColor@
titleColorSelector :: Selector '[] (Id NSColor)
titleColorSelector = mkSelector "titleColor"

-- | @Selector@ for @setTitleFont:@
setTitleFontSelector :: Selector '[Id NSFont] ()
setTitleFontSelector = mkSelector "setTitleFont:"

-- | @Selector@ for @titleFont@
titleFontSelector :: Selector '[] (Id NSFont)
titleFontSelector = mkSelector "titleFont"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @setKnobThickness:@
setKnobThicknessSelector :: Selector '[CDouble] ()
setKnobThicknessSelector = mkSelector "setKnobThickness:"

-- | @Selector@ for @setImage:@
setImageSelector :: Selector '[Id NSImage] ()
setImageSelector = mkSelector "setImage:"

-- | @Selector@ for @image@
imageSelector :: Selector '[] (Id NSImage)
imageSelector = mkSelector "image"

-- | @Selector@ for @tickMarkValueAtIndex:@
tickMarkValueAtIndexSelector :: Selector '[CLong] CDouble
tickMarkValueAtIndexSelector = mkSelector "tickMarkValueAtIndex:"

-- | @Selector@ for @rectOfTickMarkAtIndex:@
rectOfTickMarkAtIndexSelector :: Selector '[CLong] NSRect
rectOfTickMarkAtIndexSelector = mkSelector "rectOfTickMarkAtIndex:"

-- | @Selector@ for @indexOfTickMarkAtPoint:@
indexOfTickMarkAtPointSelector :: Selector '[NSPoint] CLong
indexOfTickMarkAtPointSelector = mkSelector "indexOfTickMarkAtPoint:"

-- | @Selector@ for @closestTickMarkValueToValue:@
closestTickMarkValueToValueSelector :: Selector '[CDouble] CDouble
closestTickMarkValueToValueSelector = mkSelector "closestTickMarkValueToValue:"

-- | @Selector@ for @drawTickMarks@
drawTickMarksSelector :: Selector '[] ()
drawTickMarksSelector = mkSelector "drawTickMarks"

-- | @Selector@ for @prefersTrackingUntilMouseUp@
prefersTrackingUntilMouseUpSelector :: Selector '[] Bool
prefersTrackingUntilMouseUpSelector = mkSelector "prefersTrackingUntilMouseUp"

-- | @Selector@ for @minValue@
minValueSelector :: Selector '[] CDouble
minValueSelector = mkSelector "minValue"

-- | @Selector@ for @setMinValue:@
setMinValueSelector :: Selector '[CDouble] ()
setMinValueSelector = mkSelector "setMinValue:"

-- | @Selector@ for @maxValue@
maxValueSelector :: Selector '[] CDouble
maxValueSelector = mkSelector "maxValue"

-- | @Selector@ for @setMaxValue:@
setMaxValueSelector :: Selector '[CDouble] ()
setMaxValueSelector = mkSelector "setMaxValue:"

-- | @Selector@ for @altIncrementValue@
altIncrementValueSelector :: Selector '[] CDouble
altIncrementValueSelector = mkSelector "altIncrementValue"

-- | @Selector@ for @setAltIncrementValue:@
setAltIncrementValueSelector :: Selector '[CDouble] ()
setAltIncrementValueSelector = mkSelector "setAltIncrementValue:"

-- | @Selector@ for @sliderType@
sliderTypeSelector :: Selector '[] NSSliderType
sliderTypeSelector = mkSelector "sliderType"

-- | @Selector@ for @setSliderType:@
setSliderTypeSelector :: Selector '[NSSliderType] ()
setSliderTypeSelector = mkSelector "setSliderType:"

-- | @Selector@ for @vertical@
verticalSelector :: Selector '[] Bool
verticalSelector = mkSelector "vertical"

-- | @Selector@ for @setVertical:@
setVerticalSelector :: Selector '[Bool] ()
setVerticalSelector = mkSelector "setVertical:"

-- | @Selector@ for @trackRect@
trackRectSelector :: Selector '[] NSRect
trackRectSelector = mkSelector "trackRect"

-- | @Selector@ for @knobThickness@
knobThicknessSelector :: Selector '[] CDouble
knobThicknessSelector = mkSelector "knobThickness"

-- | @Selector@ for @numberOfTickMarks@
numberOfTickMarksSelector :: Selector '[] CLong
numberOfTickMarksSelector = mkSelector "numberOfTickMarks"

-- | @Selector@ for @setNumberOfTickMarks:@
setNumberOfTickMarksSelector :: Selector '[CLong] ()
setNumberOfTickMarksSelector = mkSelector "setNumberOfTickMarks:"

-- | @Selector@ for @tickMarkPosition@
tickMarkPositionSelector :: Selector '[] NSTickMarkPosition
tickMarkPositionSelector = mkSelector "tickMarkPosition"

-- | @Selector@ for @setTickMarkPosition:@
setTickMarkPositionSelector :: Selector '[NSTickMarkPosition] ()
setTickMarkPositionSelector = mkSelector "setTickMarkPosition:"

-- | @Selector@ for @allowsTickMarkValuesOnly@
allowsTickMarkValuesOnlySelector :: Selector '[] Bool
allowsTickMarkValuesOnlySelector = mkSelector "allowsTickMarkValuesOnly"

-- | @Selector@ for @setAllowsTickMarkValuesOnly:@
setAllowsTickMarkValuesOnlySelector :: Selector '[Bool] ()
setAllowsTickMarkValuesOnlySelector = mkSelector "setAllowsTickMarkValuesOnly:"

