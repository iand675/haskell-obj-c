{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSRulerView@.
module ObjC.AppKit.NSRulerView
  ( NSRulerView
  , IsNSRulerView(..)
  , registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle
  , initWithCoder
  , initWithScrollView_orientation
  , addMarker
  , removeMarker
  , trackMarker_withMouseEvent
  , moveRulerlineFromLocation_toLocation
  , invalidateHashMarks
  , drawHashMarksAndLabelsInRect
  , drawMarkersInRect
  , scrollView
  , setScrollView
  , orientation
  , setOrientation
  , baselineLocation
  , requiredThickness
  , ruleThickness
  , setRuleThickness
  , reservedThicknessForMarkers
  , setReservedThicknessForMarkers
  , reservedThicknessForAccessoryView
  , setReservedThicknessForAccessoryView
  , measurementUnits
  , setMeasurementUnits
  , originOffset
  , setOriginOffset
  , clientView
  , setClientView
  , markers
  , setMarkers
  , accessoryView
  , setAccessoryView
  , flipped
  , accessoryViewSelector
  , addMarkerSelector
  , baselineLocationSelector
  , clientViewSelector
  , drawHashMarksAndLabelsInRectSelector
  , drawMarkersInRectSelector
  , flippedSelector
  , initWithCoderSelector
  , initWithScrollView_orientationSelector
  , invalidateHashMarksSelector
  , markersSelector
  , measurementUnitsSelector
  , moveRulerlineFromLocation_toLocationSelector
  , orientationSelector
  , originOffsetSelector
  , registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector
  , removeMarkerSelector
  , requiredThicknessSelector
  , reservedThicknessForAccessoryViewSelector
  , reservedThicknessForMarkersSelector
  , ruleThicknessSelector
  , scrollViewSelector
  , setAccessoryViewSelector
  , setClientViewSelector
  , setMarkersSelector
  , setMeasurementUnitsSelector
  , setOrientationSelector
  , setOriginOffsetSelector
  , setReservedThicknessForAccessoryViewSelector
  , setReservedThicknessForMarkersSelector
  , setRuleThicknessSelector
  , setScrollViewSelector
  , trackMarker_withMouseEventSelector

  -- * Enum types
  , NSRulerOrientation(NSRulerOrientation)
  , pattern NSHorizontalRuler
  , pattern NSVerticalRuler

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

-- | *********************** Registering new units ************************
--
-- ObjC selector: @+ registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:@
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle :: (IsNSString unitName, IsNSString abbreviation, IsNSArray stepUpCycle, IsNSArray stepDownCycle) => unitName -> abbreviation -> CDouble -> stepUpCycle -> stepDownCycle -> IO ()
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle unitName abbreviation conversionFactor stepUpCycle stepDownCycle =
  do
    cls' <- getRequiredClass "NSRulerView"
    sendClassMessage cls' registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector (toNSString unitName) (toNSString abbreviation) conversionFactor (toNSArray stepUpCycle) (toNSArray stepDownCycle)

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsNSRulerView nsRulerView, IsNSCoder coder) => nsRulerView -> coder -> IO (Id NSRulerView)
initWithCoder nsRulerView coder =
  sendOwnedMessage nsRulerView initWithCoderSelector (toNSCoder coder)

-- | @- initWithScrollView:orientation:@
initWithScrollView_orientation :: (IsNSRulerView nsRulerView, IsNSScrollView scrollView) => nsRulerView -> scrollView -> NSRulerOrientation -> IO (Id NSRulerView)
initWithScrollView_orientation nsRulerView scrollView orientation =
  sendOwnedMessage nsRulerView initWithScrollView_orientationSelector (toNSScrollView scrollView) orientation

-- | @- addMarker:@
addMarker :: (IsNSRulerView nsRulerView, IsNSRulerMarker marker) => nsRulerView -> marker -> IO ()
addMarker nsRulerView marker =
  sendMessage nsRulerView addMarkerSelector (toNSRulerMarker marker)

-- | @- removeMarker:@
removeMarker :: (IsNSRulerView nsRulerView, IsNSRulerMarker marker) => nsRulerView -> marker -> IO ()
removeMarker nsRulerView marker =
  sendMessage nsRulerView removeMarkerSelector (toNSRulerMarker marker)

-- | @- trackMarker:withMouseEvent:@
trackMarker_withMouseEvent :: (IsNSRulerView nsRulerView, IsNSRulerMarker marker, IsNSEvent event) => nsRulerView -> marker -> event -> IO Bool
trackMarker_withMouseEvent nsRulerView marker event =
  sendMessage nsRulerView trackMarker_withMouseEventSelector (toNSRulerMarker marker) (toNSEvent event)

-- | @- moveRulerlineFromLocation:toLocation:@
moveRulerlineFromLocation_toLocation :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> CDouble -> IO ()
moveRulerlineFromLocation_toLocation nsRulerView oldLocation newLocation =
  sendMessage nsRulerView moveRulerlineFromLocation_toLocationSelector oldLocation newLocation

-- | ********************* Drawing and hash invalidation **********************
--
-- ObjC selector: @- invalidateHashMarks@
invalidateHashMarks :: IsNSRulerView nsRulerView => nsRulerView -> IO ()
invalidateHashMarks nsRulerView =
  sendMessage nsRulerView invalidateHashMarksSelector

-- | @- drawHashMarksAndLabelsInRect:@
drawHashMarksAndLabelsInRect :: IsNSRulerView nsRulerView => nsRulerView -> NSRect -> IO ()
drawHashMarksAndLabelsInRect nsRulerView rect =
  sendMessage nsRulerView drawHashMarksAndLabelsInRectSelector rect

-- | @- drawMarkersInRect:@
drawMarkersInRect :: IsNSRulerView nsRulerView => nsRulerView -> NSRect -> IO ()
drawMarkersInRect nsRulerView rect =
  sendMessage nsRulerView drawMarkersInRectSelector rect

-- | ************************** Basic setup ***************************
--
-- ObjC selector: @- scrollView@
scrollView :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSScrollView)
scrollView nsRulerView =
  sendMessage nsRulerView scrollViewSelector

-- | ************************** Basic setup ***************************
--
-- ObjC selector: @- setScrollView:@
setScrollView :: (IsNSRulerView nsRulerView, IsNSScrollView value) => nsRulerView -> value -> IO ()
setScrollView nsRulerView value =
  sendMessage nsRulerView setScrollViewSelector (toNSScrollView value)

-- | @- orientation@
orientation :: IsNSRulerView nsRulerView => nsRulerView -> IO NSRulerOrientation
orientation nsRulerView =
  sendMessage nsRulerView orientationSelector

-- | @- setOrientation:@
setOrientation :: IsNSRulerView nsRulerView => nsRulerView -> NSRulerOrientation -> IO ()
setOrientation nsRulerView value =
  sendMessage nsRulerView setOrientationSelector value

-- | ************************** Ruler geometry ***************************
--
-- ObjC selector: @- baselineLocation@
baselineLocation :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
baselineLocation nsRulerView =
  sendMessage nsRulerView baselineLocationSelector

-- | @- requiredThickness@
requiredThickness :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
requiredThickness nsRulerView =
  sendMessage nsRulerView requiredThicknessSelector

-- | @- ruleThickness@
ruleThickness :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
ruleThickness nsRulerView =
  sendMessage nsRulerView ruleThicknessSelector

-- | @- setRuleThickness:@
setRuleThickness :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setRuleThickness nsRulerView value =
  sendMessage nsRulerView setRuleThicknessSelector value

-- | @- reservedThicknessForMarkers@
reservedThicknessForMarkers :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
reservedThicknessForMarkers nsRulerView =
  sendMessage nsRulerView reservedThicknessForMarkersSelector

-- | @- setReservedThicknessForMarkers:@
setReservedThicknessForMarkers :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setReservedThicknessForMarkers nsRulerView value =
  sendMessage nsRulerView setReservedThicknessForMarkersSelector value

-- | @- reservedThicknessForAccessoryView@
reservedThicknessForAccessoryView :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
reservedThicknessForAccessoryView nsRulerView =
  sendMessage nsRulerView reservedThicknessForAccessoryViewSelector

-- | @- setReservedThicknessForAccessoryView:@
setReservedThicknessForAccessoryView :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setReservedThicknessForAccessoryView nsRulerView value =
  sendMessage nsRulerView setReservedThicknessForAccessoryViewSelector value

-- | ************************** Rule configuration ***************************
--
-- ObjC selector: @- measurementUnits@
measurementUnits :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSString)
measurementUnits nsRulerView =
  sendMessage nsRulerView measurementUnitsSelector

-- | ************************** Rule configuration ***************************
--
-- ObjC selector: @- setMeasurementUnits:@
setMeasurementUnits :: (IsNSRulerView nsRulerView, IsNSString value) => nsRulerView -> value -> IO ()
setMeasurementUnits nsRulerView value =
  sendMessage nsRulerView setMeasurementUnitsSelector (toNSString value)

-- | @- originOffset@
originOffset :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
originOffset nsRulerView =
  sendMessage nsRulerView originOffsetSelector

-- | @- setOriginOffset:@
setOriginOffset :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setOriginOffset nsRulerView value =
  sendMessage nsRulerView setOriginOffsetSelector value

-- | ************************** Client view setup ***************************
--
-- ObjC selector: @- clientView@
clientView :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSView)
clientView nsRulerView =
  sendMessage nsRulerView clientViewSelector

-- | ************************** Client view setup ***************************
--
-- ObjC selector: @- setClientView:@
setClientView :: (IsNSRulerView nsRulerView, IsNSView value) => nsRulerView -> value -> IO ()
setClientView nsRulerView value =
  sendMessage nsRulerView setClientViewSelector (toNSView value)

-- | @- markers@
markers :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSArray)
markers nsRulerView =
  sendMessage nsRulerView markersSelector

-- | @- setMarkers:@
setMarkers :: (IsNSRulerView nsRulerView, IsNSArray value) => nsRulerView -> value -> IO ()
setMarkers nsRulerView value =
  sendMessage nsRulerView setMarkersSelector (toNSArray value)

-- | @- accessoryView@
accessoryView :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSView)
accessoryView nsRulerView =
  sendMessage nsRulerView accessoryViewSelector

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSRulerView nsRulerView, IsNSView value) => nsRulerView -> value -> IO ()
setAccessoryView nsRulerView value =
  sendMessage nsRulerView setAccessoryViewSelector (toNSView value)

-- | ************************** Key overrides ***************************
--
-- ObjC selector: @- flipped@
flipped :: IsNSRulerView nsRulerView => nsRulerView -> IO Bool
flipped nsRulerView =
  sendMessage nsRulerView flippedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:@
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector :: Selector '[Id NSString, Id NSString, CDouble, Id NSArray, Id NSArray] ()
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector = mkSelector "registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSRulerView)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithScrollView:orientation:@
initWithScrollView_orientationSelector :: Selector '[Id NSScrollView, NSRulerOrientation] (Id NSRulerView)
initWithScrollView_orientationSelector = mkSelector "initWithScrollView:orientation:"

-- | @Selector@ for @addMarker:@
addMarkerSelector :: Selector '[Id NSRulerMarker] ()
addMarkerSelector = mkSelector "addMarker:"

-- | @Selector@ for @removeMarker:@
removeMarkerSelector :: Selector '[Id NSRulerMarker] ()
removeMarkerSelector = mkSelector "removeMarker:"

-- | @Selector@ for @trackMarker:withMouseEvent:@
trackMarker_withMouseEventSelector :: Selector '[Id NSRulerMarker, Id NSEvent] Bool
trackMarker_withMouseEventSelector = mkSelector "trackMarker:withMouseEvent:"

-- | @Selector@ for @moveRulerlineFromLocation:toLocation:@
moveRulerlineFromLocation_toLocationSelector :: Selector '[CDouble, CDouble] ()
moveRulerlineFromLocation_toLocationSelector = mkSelector "moveRulerlineFromLocation:toLocation:"

-- | @Selector@ for @invalidateHashMarks@
invalidateHashMarksSelector :: Selector '[] ()
invalidateHashMarksSelector = mkSelector "invalidateHashMarks"

-- | @Selector@ for @drawHashMarksAndLabelsInRect:@
drawHashMarksAndLabelsInRectSelector :: Selector '[NSRect] ()
drawHashMarksAndLabelsInRectSelector = mkSelector "drawHashMarksAndLabelsInRect:"

-- | @Selector@ for @drawMarkersInRect:@
drawMarkersInRectSelector :: Selector '[NSRect] ()
drawMarkersInRectSelector = mkSelector "drawMarkersInRect:"

-- | @Selector@ for @scrollView@
scrollViewSelector :: Selector '[] (Id NSScrollView)
scrollViewSelector = mkSelector "scrollView"

-- | @Selector@ for @setScrollView:@
setScrollViewSelector :: Selector '[Id NSScrollView] ()
setScrollViewSelector = mkSelector "setScrollView:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector '[] NSRulerOrientation
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector '[NSRulerOrientation] ()
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @baselineLocation@
baselineLocationSelector :: Selector '[] CDouble
baselineLocationSelector = mkSelector "baselineLocation"

-- | @Selector@ for @requiredThickness@
requiredThicknessSelector :: Selector '[] CDouble
requiredThicknessSelector = mkSelector "requiredThickness"

-- | @Selector@ for @ruleThickness@
ruleThicknessSelector :: Selector '[] CDouble
ruleThicknessSelector = mkSelector "ruleThickness"

-- | @Selector@ for @setRuleThickness:@
setRuleThicknessSelector :: Selector '[CDouble] ()
setRuleThicknessSelector = mkSelector "setRuleThickness:"

-- | @Selector@ for @reservedThicknessForMarkers@
reservedThicknessForMarkersSelector :: Selector '[] CDouble
reservedThicknessForMarkersSelector = mkSelector "reservedThicknessForMarkers"

-- | @Selector@ for @setReservedThicknessForMarkers:@
setReservedThicknessForMarkersSelector :: Selector '[CDouble] ()
setReservedThicknessForMarkersSelector = mkSelector "setReservedThicknessForMarkers:"

-- | @Selector@ for @reservedThicknessForAccessoryView@
reservedThicknessForAccessoryViewSelector :: Selector '[] CDouble
reservedThicknessForAccessoryViewSelector = mkSelector "reservedThicknessForAccessoryView"

-- | @Selector@ for @setReservedThicknessForAccessoryView:@
setReservedThicknessForAccessoryViewSelector :: Selector '[CDouble] ()
setReservedThicknessForAccessoryViewSelector = mkSelector "setReservedThicknessForAccessoryView:"

-- | @Selector@ for @measurementUnits@
measurementUnitsSelector :: Selector '[] (Id NSString)
measurementUnitsSelector = mkSelector "measurementUnits"

-- | @Selector@ for @setMeasurementUnits:@
setMeasurementUnitsSelector :: Selector '[Id NSString] ()
setMeasurementUnitsSelector = mkSelector "setMeasurementUnits:"

-- | @Selector@ for @originOffset@
originOffsetSelector :: Selector '[] CDouble
originOffsetSelector = mkSelector "originOffset"

-- | @Selector@ for @setOriginOffset:@
setOriginOffsetSelector :: Selector '[CDouble] ()
setOriginOffsetSelector = mkSelector "setOriginOffset:"

-- | @Selector@ for @clientView@
clientViewSelector :: Selector '[] (Id NSView)
clientViewSelector = mkSelector "clientView"

-- | @Selector@ for @setClientView:@
setClientViewSelector :: Selector '[Id NSView] ()
setClientViewSelector = mkSelector "setClientView:"

-- | @Selector@ for @markers@
markersSelector :: Selector '[] (Id NSArray)
markersSelector = mkSelector "markers"

-- | @Selector@ for @setMarkers:@
setMarkersSelector :: Selector '[Id NSArray] ()
setMarkersSelector = mkSelector "setMarkers:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector '[] (Id NSView)
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector '[Id NSView] ()
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @flipped@
flippedSelector :: Selector '[] Bool
flippedSelector = mkSelector "flipped"

