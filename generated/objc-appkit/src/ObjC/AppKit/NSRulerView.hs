{-# LANGUAGE PatternSynonyms #-}
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
  , registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector
  , initWithCoderSelector
  , initWithScrollView_orientationSelector
  , addMarkerSelector
  , removeMarkerSelector
  , trackMarker_withMouseEventSelector
  , moveRulerlineFromLocation_toLocationSelector
  , invalidateHashMarksSelector
  , drawHashMarksAndLabelsInRectSelector
  , drawMarkersInRectSelector
  , scrollViewSelector
  , setScrollViewSelector
  , orientationSelector
  , setOrientationSelector
  , baselineLocationSelector
  , requiredThicknessSelector
  , ruleThicknessSelector
  , setRuleThicknessSelector
  , reservedThicknessForMarkersSelector
  , setReservedThicknessForMarkersSelector
  , reservedThicknessForAccessoryViewSelector
  , setReservedThicknessForAccessoryViewSelector
  , measurementUnitsSelector
  , setMeasurementUnitsSelector
  , originOffsetSelector
  , setOriginOffsetSelector
  , clientViewSelector
  , setClientViewSelector
  , markersSelector
  , setMarkersSelector
  , accessoryViewSelector
  , setAccessoryViewSelector
  , flippedSelector

  -- * Enum types
  , NSRulerOrientation(NSRulerOrientation)
  , pattern NSHorizontalRuler
  , pattern NSVerticalRuler

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
import ObjC.Foundation.Internal.Classes

-- | *********************** Registering new units ************************
--
-- ObjC selector: @+ registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:@
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle :: (IsNSString unitName, IsNSString abbreviation, IsNSArray stepUpCycle, IsNSArray stepDownCycle) => unitName -> abbreviation -> CDouble -> stepUpCycle -> stepDownCycle -> IO ()
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycle unitName abbreviation conversionFactor stepUpCycle stepDownCycle =
  do
    cls' <- getRequiredClass "NSRulerView"
    withObjCPtr unitName $ \raw_unitName ->
      withObjCPtr abbreviation $ \raw_abbreviation ->
        withObjCPtr stepUpCycle $ \raw_stepUpCycle ->
          withObjCPtr stepDownCycle $ \raw_stepDownCycle ->
            sendClassMsg cls' (mkSelector "registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:") retVoid [argPtr (castPtr raw_unitName :: Ptr ()), argPtr (castPtr raw_abbreviation :: Ptr ()), argCDouble (fromIntegral conversionFactor), argPtr (castPtr raw_stepUpCycle :: Ptr ()), argPtr (castPtr raw_stepDownCycle :: Ptr ())]

-- | ************************** Initialization ***************************
--
-- ObjC selector: @- initWithCoder:@
initWithCoder :: (IsNSRulerView nsRulerView, IsNSCoder coder) => nsRulerView -> coder -> IO (Id NSRulerView)
initWithCoder nsRulerView  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsRulerView (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithScrollView:orientation:@
initWithScrollView_orientation :: (IsNSRulerView nsRulerView, IsNSScrollView scrollView) => nsRulerView -> scrollView -> NSRulerOrientation -> IO (Id NSRulerView)
initWithScrollView_orientation nsRulerView  scrollView orientation =
withObjCPtr scrollView $ \raw_scrollView ->
    sendMsg nsRulerView (mkSelector "initWithScrollView:orientation:") (retPtr retVoid) [argPtr (castPtr raw_scrollView :: Ptr ()), argCULong (coerce orientation)] >>= ownedObject . castPtr

-- | @- addMarker:@
addMarker :: (IsNSRulerView nsRulerView, IsNSRulerMarker marker) => nsRulerView -> marker -> IO ()
addMarker nsRulerView  marker =
withObjCPtr marker $ \raw_marker ->
    sendMsg nsRulerView (mkSelector "addMarker:") retVoid [argPtr (castPtr raw_marker :: Ptr ())]

-- | @- removeMarker:@
removeMarker :: (IsNSRulerView nsRulerView, IsNSRulerMarker marker) => nsRulerView -> marker -> IO ()
removeMarker nsRulerView  marker =
withObjCPtr marker $ \raw_marker ->
    sendMsg nsRulerView (mkSelector "removeMarker:") retVoid [argPtr (castPtr raw_marker :: Ptr ())]

-- | @- trackMarker:withMouseEvent:@
trackMarker_withMouseEvent :: (IsNSRulerView nsRulerView, IsNSRulerMarker marker, IsNSEvent event) => nsRulerView -> marker -> event -> IO Bool
trackMarker_withMouseEvent nsRulerView  marker event =
withObjCPtr marker $ \raw_marker ->
  withObjCPtr event $ \raw_event ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRulerView (mkSelector "trackMarker:withMouseEvent:") retCULong [argPtr (castPtr raw_marker :: Ptr ()), argPtr (castPtr raw_event :: Ptr ())]

-- | @- moveRulerlineFromLocation:toLocation:@
moveRulerlineFromLocation_toLocation :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> CDouble -> IO ()
moveRulerlineFromLocation_toLocation nsRulerView  oldLocation newLocation =
  sendMsg nsRulerView (mkSelector "moveRulerlineFromLocation:toLocation:") retVoid [argCDouble (fromIntegral oldLocation), argCDouble (fromIntegral newLocation)]

-- | ********************* Drawing and hash invalidation **********************
--
-- ObjC selector: @- invalidateHashMarks@
invalidateHashMarks :: IsNSRulerView nsRulerView => nsRulerView -> IO ()
invalidateHashMarks nsRulerView  =
  sendMsg nsRulerView (mkSelector "invalidateHashMarks") retVoid []

-- | @- drawHashMarksAndLabelsInRect:@
drawHashMarksAndLabelsInRect :: IsNSRulerView nsRulerView => nsRulerView -> NSRect -> IO ()
drawHashMarksAndLabelsInRect nsRulerView  rect =
  sendMsg nsRulerView (mkSelector "drawHashMarksAndLabelsInRect:") retVoid [argNSRect rect]

-- | @- drawMarkersInRect:@
drawMarkersInRect :: IsNSRulerView nsRulerView => nsRulerView -> NSRect -> IO ()
drawMarkersInRect nsRulerView  rect =
  sendMsg nsRulerView (mkSelector "drawMarkersInRect:") retVoid [argNSRect rect]

-- | ************************** Basic setup ***************************
--
-- ObjC selector: @- scrollView@
scrollView :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSScrollView)
scrollView nsRulerView  =
  sendMsg nsRulerView (mkSelector "scrollView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Basic setup ***************************
--
-- ObjC selector: @- setScrollView:@
setScrollView :: (IsNSRulerView nsRulerView, IsNSScrollView value) => nsRulerView -> value -> IO ()
setScrollView nsRulerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRulerView (mkSelector "setScrollView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- orientation@
orientation :: IsNSRulerView nsRulerView => nsRulerView -> IO NSRulerOrientation
orientation nsRulerView  =
  fmap (coerce :: CULong -> NSRulerOrientation) $ sendMsg nsRulerView (mkSelector "orientation") retCULong []

-- | @- setOrientation:@
setOrientation :: IsNSRulerView nsRulerView => nsRulerView -> NSRulerOrientation -> IO ()
setOrientation nsRulerView  value =
  sendMsg nsRulerView (mkSelector "setOrientation:") retVoid [argCULong (coerce value)]

-- | ************************** Ruler geometry ***************************
--
-- ObjC selector: @- baselineLocation@
baselineLocation :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
baselineLocation nsRulerView  =
  sendMsg nsRulerView (mkSelector "baselineLocation") retCDouble []

-- | @- requiredThickness@
requiredThickness :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
requiredThickness nsRulerView  =
  sendMsg nsRulerView (mkSelector "requiredThickness") retCDouble []

-- | @- ruleThickness@
ruleThickness :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
ruleThickness nsRulerView  =
  sendMsg nsRulerView (mkSelector "ruleThickness") retCDouble []

-- | @- setRuleThickness:@
setRuleThickness :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setRuleThickness nsRulerView  value =
  sendMsg nsRulerView (mkSelector "setRuleThickness:") retVoid [argCDouble (fromIntegral value)]

-- | @- reservedThicknessForMarkers@
reservedThicknessForMarkers :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
reservedThicknessForMarkers nsRulerView  =
  sendMsg nsRulerView (mkSelector "reservedThicknessForMarkers") retCDouble []

-- | @- setReservedThicknessForMarkers:@
setReservedThicknessForMarkers :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setReservedThicknessForMarkers nsRulerView  value =
  sendMsg nsRulerView (mkSelector "setReservedThicknessForMarkers:") retVoid [argCDouble (fromIntegral value)]

-- | @- reservedThicknessForAccessoryView@
reservedThicknessForAccessoryView :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
reservedThicknessForAccessoryView nsRulerView  =
  sendMsg nsRulerView (mkSelector "reservedThicknessForAccessoryView") retCDouble []

-- | @- setReservedThicknessForAccessoryView:@
setReservedThicknessForAccessoryView :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setReservedThicknessForAccessoryView nsRulerView  value =
  sendMsg nsRulerView (mkSelector "setReservedThicknessForAccessoryView:") retVoid [argCDouble (fromIntegral value)]

-- | ************************** Rule configuration ***************************
--
-- ObjC selector: @- measurementUnits@
measurementUnits :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSString)
measurementUnits nsRulerView  =
  sendMsg nsRulerView (mkSelector "measurementUnits") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Rule configuration ***************************
--
-- ObjC selector: @- setMeasurementUnits:@
setMeasurementUnits :: (IsNSRulerView nsRulerView, IsNSString value) => nsRulerView -> value -> IO ()
setMeasurementUnits nsRulerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRulerView (mkSelector "setMeasurementUnits:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- originOffset@
originOffset :: IsNSRulerView nsRulerView => nsRulerView -> IO CDouble
originOffset nsRulerView  =
  sendMsg nsRulerView (mkSelector "originOffset") retCDouble []

-- | @- setOriginOffset:@
setOriginOffset :: IsNSRulerView nsRulerView => nsRulerView -> CDouble -> IO ()
setOriginOffset nsRulerView  value =
  sendMsg nsRulerView (mkSelector "setOriginOffset:") retVoid [argCDouble (fromIntegral value)]

-- | ************************** Client view setup ***************************
--
-- ObjC selector: @- clientView@
clientView :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSView)
clientView nsRulerView  =
  sendMsg nsRulerView (mkSelector "clientView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | ************************** Client view setup ***************************
--
-- ObjC selector: @- setClientView:@
setClientView :: (IsNSRulerView nsRulerView, IsNSView value) => nsRulerView -> value -> IO ()
setClientView nsRulerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRulerView (mkSelector "setClientView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- markers@
markers :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSArray)
markers nsRulerView  =
  sendMsg nsRulerView (mkSelector "markers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMarkers:@
setMarkers :: (IsNSRulerView nsRulerView, IsNSArray value) => nsRulerView -> value -> IO ()
setMarkers nsRulerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRulerView (mkSelector "setMarkers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- accessoryView@
accessoryView :: IsNSRulerView nsRulerView => nsRulerView -> IO (Id NSView)
accessoryView nsRulerView  =
  sendMsg nsRulerView (mkSelector "accessoryView") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAccessoryView:@
setAccessoryView :: (IsNSRulerView nsRulerView, IsNSView value) => nsRulerView -> value -> IO ()
setAccessoryView nsRulerView  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsRulerView (mkSelector "setAccessoryView:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | ************************** Key overrides ***************************
--
-- ObjC selector: @- flipped@
flipped :: IsNSRulerView nsRulerView => nsRulerView -> IO Bool
flipped nsRulerView  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsRulerView (mkSelector "flipped") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:@
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector :: Selector
registerUnitWithName_abbreviation_unitToPointsConversionFactor_stepUpCycle_stepDownCycleSelector = mkSelector "registerUnitWithName:abbreviation:unitToPointsConversionFactor:stepUpCycle:stepDownCycle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithScrollView:orientation:@
initWithScrollView_orientationSelector :: Selector
initWithScrollView_orientationSelector = mkSelector "initWithScrollView:orientation:"

-- | @Selector@ for @addMarker:@
addMarkerSelector :: Selector
addMarkerSelector = mkSelector "addMarker:"

-- | @Selector@ for @removeMarker:@
removeMarkerSelector :: Selector
removeMarkerSelector = mkSelector "removeMarker:"

-- | @Selector@ for @trackMarker:withMouseEvent:@
trackMarker_withMouseEventSelector :: Selector
trackMarker_withMouseEventSelector = mkSelector "trackMarker:withMouseEvent:"

-- | @Selector@ for @moveRulerlineFromLocation:toLocation:@
moveRulerlineFromLocation_toLocationSelector :: Selector
moveRulerlineFromLocation_toLocationSelector = mkSelector "moveRulerlineFromLocation:toLocation:"

-- | @Selector@ for @invalidateHashMarks@
invalidateHashMarksSelector :: Selector
invalidateHashMarksSelector = mkSelector "invalidateHashMarks"

-- | @Selector@ for @drawHashMarksAndLabelsInRect:@
drawHashMarksAndLabelsInRectSelector :: Selector
drawHashMarksAndLabelsInRectSelector = mkSelector "drawHashMarksAndLabelsInRect:"

-- | @Selector@ for @drawMarkersInRect:@
drawMarkersInRectSelector :: Selector
drawMarkersInRectSelector = mkSelector "drawMarkersInRect:"

-- | @Selector@ for @scrollView@
scrollViewSelector :: Selector
scrollViewSelector = mkSelector "scrollView"

-- | @Selector@ for @setScrollView:@
setScrollViewSelector :: Selector
setScrollViewSelector = mkSelector "setScrollView:"

-- | @Selector@ for @orientation@
orientationSelector :: Selector
orientationSelector = mkSelector "orientation"

-- | @Selector@ for @setOrientation:@
setOrientationSelector :: Selector
setOrientationSelector = mkSelector "setOrientation:"

-- | @Selector@ for @baselineLocation@
baselineLocationSelector :: Selector
baselineLocationSelector = mkSelector "baselineLocation"

-- | @Selector@ for @requiredThickness@
requiredThicknessSelector :: Selector
requiredThicknessSelector = mkSelector "requiredThickness"

-- | @Selector@ for @ruleThickness@
ruleThicknessSelector :: Selector
ruleThicknessSelector = mkSelector "ruleThickness"

-- | @Selector@ for @setRuleThickness:@
setRuleThicknessSelector :: Selector
setRuleThicknessSelector = mkSelector "setRuleThickness:"

-- | @Selector@ for @reservedThicknessForMarkers@
reservedThicknessForMarkersSelector :: Selector
reservedThicknessForMarkersSelector = mkSelector "reservedThicknessForMarkers"

-- | @Selector@ for @setReservedThicknessForMarkers:@
setReservedThicknessForMarkersSelector :: Selector
setReservedThicknessForMarkersSelector = mkSelector "setReservedThicknessForMarkers:"

-- | @Selector@ for @reservedThicknessForAccessoryView@
reservedThicknessForAccessoryViewSelector :: Selector
reservedThicknessForAccessoryViewSelector = mkSelector "reservedThicknessForAccessoryView"

-- | @Selector@ for @setReservedThicknessForAccessoryView:@
setReservedThicknessForAccessoryViewSelector :: Selector
setReservedThicknessForAccessoryViewSelector = mkSelector "setReservedThicknessForAccessoryView:"

-- | @Selector@ for @measurementUnits@
measurementUnitsSelector :: Selector
measurementUnitsSelector = mkSelector "measurementUnits"

-- | @Selector@ for @setMeasurementUnits:@
setMeasurementUnitsSelector :: Selector
setMeasurementUnitsSelector = mkSelector "setMeasurementUnits:"

-- | @Selector@ for @originOffset@
originOffsetSelector :: Selector
originOffsetSelector = mkSelector "originOffset"

-- | @Selector@ for @setOriginOffset:@
setOriginOffsetSelector :: Selector
setOriginOffsetSelector = mkSelector "setOriginOffset:"

-- | @Selector@ for @clientView@
clientViewSelector :: Selector
clientViewSelector = mkSelector "clientView"

-- | @Selector@ for @setClientView:@
setClientViewSelector :: Selector
setClientViewSelector = mkSelector "setClientView:"

-- | @Selector@ for @markers@
markersSelector :: Selector
markersSelector = mkSelector "markers"

-- | @Selector@ for @setMarkers:@
setMarkersSelector :: Selector
setMarkersSelector = mkSelector "setMarkers:"

-- | @Selector@ for @accessoryView@
accessoryViewSelector :: Selector
accessoryViewSelector = mkSelector "accessoryView"

-- | @Selector@ for @setAccessoryView:@
setAccessoryViewSelector :: Selector
setAccessoryViewSelector = mkSelector "setAccessoryView:"

-- | @Selector@ for @flipped@
flippedSelector :: Selector
flippedSelector = mkSelector "flipped"

