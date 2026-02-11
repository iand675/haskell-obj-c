{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSEvent@.
module ObjC.AppKit.NSEvent
  ( NSEvent
  , IsNSEvent(..)
  , charactersByApplyingModifiers
  , eventWithEventRef
  , eventWithCGEvent
  , touchesMatchingPhase_inView
  , allTouches
  , touchesForView
  , coalescedTouchesForTouch
  , trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandler
  , startPeriodicEventsAfterDelay_withPeriod
  , stopPeriodicEvents
  , mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure
  , keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode
  , enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData
  , otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2
  , addGlobalMonitorForEventsMatchingMask_handler
  , addLocalMonitorForEventsMatchingMask_handler
  , removeMonitor
  , type_
  , modifierFlags
  , timestamp
  , window
  , windowNumber
  , context
  , clickCount
  , buttonNumber
  , eventNumber
  , pressure
  , locationInWindow
  , deltaX
  , deltaY
  , deltaZ
  , hasPreciseScrollingDeltas
  , scrollingDeltaX
  , scrollingDeltaY
  , momentumPhase
  , directionInvertedFromDevice
  , characters
  , charactersIgnoringModifiers
  , aRepeat
  , keyCode
  , trackingNumber
  , userData
  , trackingArea
  , subtype
  , data1
  , data2
  , eventRef
  , cgEvent
  , mouseCoalescingEnabled
  , setMouseCoalescingEnabled
  , magnification
  , deviceID
  , rotation
  , absoluteX
  , absoluteY
  , absoluteZ
  , buttonMask
  , tilt
  , tangentialPressure
  , vendorDefined
  , vendorID
  , tabletID
  , pointingDeviceID
  , systemTabletID
  , vendorPointingDeviceType
  , pointingDeviceSerialNumber
  , uniqueID
  , capabilityMask
  , pointingDeviceType
  , enteringProximity
  , phase
  , stage
  , stageTransition
  , associatedEventsMask
  , pressureBehavior
  , swipeTrackingFromScrollEventsEnabled
  , mouseLocation
  , nsEventModifierFlags
  , pressedMouseButtons
  , doubleClickInterval
  , keyRepeatDelay
  , keyRepeatInterval
  , charactersByApplyingModifiersSelector
  , eventWithEventRefSelector
  , eventWithCGEventSelector
  , touchesMatchingPhase_inViewSelector
  , allTouchesSelector
  , touchesForViewSelector
  , coalescedTouchesForTouchSelector
  , trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector
  , startPeriodicEventsAfterDelay_withPeriodSelector
  , stopPeriodicEventsSelector
  , mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector
  , keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector
  , enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector
  , otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector
  , addGlobalMonitorForEventsMatchingMask_handlerSelector
  , addLocalMonitorForEventsMatchingMask_handlerSelector
  , removeMonitorSelector
  , typeSelector
  , modifierFlagsSelector
  , timestampSelector
  , windowSelector
  , windowNumberSelector
  , contextSelector
  , clickCountSelector
  , buttonNumberSelector
  , eventNumberSelector
  , pressureSelector
  , locationInWindowSelector
  , deltaXSelector
  , deltaYSelector
  , deltaZSelector
  , hasPreciseScrollingDeltasSelector
  , scrollingDeltaXSelector
  , scrollingDeltaYSelector
  , momentumPhaseSelector
  , directionInvertedFromDeviceSelector
  , charactersSelector
  , charactersIgnoringModifiersSelector
  , aRepeatSelector
  , keyCodeSelector
  , trackingNumberSelector
  , userDataSelector
  , trackingAreaSelector
  , subtypeSelector
  , data1Selector
  , data2Selector
  , eventRefSelector
  , cgEventSelector
  , mouseCoalescingEnabledSelector
  , setMouseCoalescingEnabledSelector
  , magnificationSelector
  , deviceIDSelector
  , rotationSelector
  , absoluteXSelector
  , absoluteYSelector
  , absoluteZSelector
  , buttonMaskSelector
  , tiltSelector
  , tangentialPressureSelector
  , vendorDefinedSelector
  , vendorIDSelector
  , tabletIDSelector
  , pointingDeviceIDSelector
  , systemTabletIDSelector
  , vendorPointingDeviceTypeSelector
  , pointingDeviceSerialNumberSelector
  , uniqueIDSelector
  , capabilityMaskSelector
  , pointingDeviceTypeSelector
  , enteringProximitySelector
  , phaseSelector
  , stageSelector
  , stageTransitionSelector
  , associatedEventsMaskSelector
  , pressureBehaviorSelector
  , swipeTrackingFromScrollEventsEnabledSelector
  , mouseLocationSelector
  , pressedMouseButtonsSelector
  , doubleClickIntervalSelector
  , keyRepeatDelaySelector
  , keyRepeatIntervalSelector

  -- * Enum types
  , NSEventButtonMask(NSEventButtonMask)
  , pattern NSEventButtonMaskPenTip
  , pattern NSEventButtonMaskPenLowerSide
  , pattern NSEventButtonMaskPenUpperSide
  , NSEventMask(NSEventMask)
  , pattern NSEventMaskLeftMouseDown
  , pattern NSEventMaskLeftMouseUp
  , pattern NSEventMaskRightMouseDown
  , pattern NSEventMaskRightMouseUp
  , pattern NSEventMaskMouseMoved
  , pattern NSEventMaskLeftMouseDragged
  , pattern NSEventMaskRightMouseDragged
  , pattern NSEventMaskMouseEntered
  , pattern NSEventMaskMouseExited
  , pattern NSEventMaskKeyDown
  , pattern NSEventMaskKeyUp
  , pattern NSEventMaskFlagsChanged
  , pattern NSEventMaskAppKitDefined
  , pattern NSEventMaskSystemDefined
  , pattern NSEventMaskApplicationDefined
  , pattern NSEventMaskPeriodic
  , pattern NSEventMaskCursorUpdate
  , pattern NSEventMaskScrollWheel
  , pattern NSEventMaskTabletPoint
  , pattern NSEventMaskTabletProximity
  , pattern NSEventMaskOtherMouseDown
  , pattern NSEventMaskOtherMouseUp
  , pattern NSEventMaskOtherMouseDragged
  , pattern NSEventMaskGesture
  , pattern NSEventMaskMagnify
  , pattern NSEventMaskSwipe
  , pattern NSEventMaskRotate
  , pattern NSEventMaskBeginGesture
  , pattern NSEventMaskEndGesture
  , pattern NSEventMaskSmartMagnify
  , pattern NSEventMaskPressure
  , pattern NSEventMaskDirectTouch
  , pattern NSEventMaskChangeMode
  , pattern NSEventMaskMouseCancelled
  , pattern NSEventMaskAny
  , NSEventModifierFlags(NSEventModifierFlags)
  , pattern NSEventModifierFlagCapsLock
  , pattern NSEventModifierFlagShift
  , pattern NSEventModifierFlagControl
  , pattern NSEventModifierFlagOption
  , pattern NSEventModifierFlagCommand
  , pattern NSEventModifierFlagNumericPad
  , pattern NSEventModifierFlagHelp
  , pattern NSEventModifierFlagFunction
  , pattern NSEventModifierFlagDeviceIndependentFlagsMask
  , NSEventPhase(NSEventPhase)
  , pattern NSEventPhaseNone
  , pattern NSEventPhaseBegan
  , pattern NSEventPhaseStationary
  , pattern NSEventPhaseChanged
  , pattern NSEventPhaseEnded
  , pattern NSEventPhaseCancelled
  , pattern NSEventPhaseMayBegin
  , NSEventSubtype(NSEventSubtype)
  , pattern NSEventSubtypeWindowExposed
  , pattern NSEventSubtypeApplicationActivated
  , pattern NSEventSubtypeApplicationDeactivated
  , pattern NSEventSubtypeWindowMoved
  , pattern NSEventSubtypeScreenChanged
  , pattern NSEventSubtypePowerOff
  , pattern NSEventSubtypeMouseEvent
  , pattern NSEventSubtypeTabletPoint
  , pattern NSEventSubtypeTabletProximity
  , pattern NSEventSubtypeTouch
  , NSEventSwipeTrackingOptions(NSEventSwipeTrackingOptions)
  , pattern NSEventSwipeTrackingLockDirection
  , pattern NSEventSwipeTrackingClampGestureAmount
  , NSEventType(NSEventType)
  , pattern NSEventTypeLeftMouseDown
  , pattern NSEventTypeLeftMouseUp
  , pattern NSEventTypeRightMouseDown
  , pattern NSEventTypeRightMouseUp
  , pattern NSEventTypeMouseMoved
  , pattern NSEventTypeLeftMouseDragged
  , pattern NSEventTypeRightMouseDragged
  , pattern NSEventTypeMouseEntered
  , pattern NSEventTypeMouseExited
  , pattern NSEventTypeKeyDown
  , pattern NSEventTypeKeyUp
  , pattern NSEventTypeFlagsChanged
  , pattern NSEventTypeAppKitDefined
  , pattern NSEventTypeSystemDefined
  , pattern NSEventTypeApplicationDefined
  , pattern NSEventTypePeriodic
  , pattern NSEventTypeCursorUpdate
  , pattern NSEventTypeScrollWheel
  , pattern NSEventTypeTabletPoint
  , pattern NSEventTypeTabletProximity
  , pattern NSEventTypeOtherMouseDown
  , pattern NSEventTypeOtherMouseUp
  , pattern NSEventTypeOtherMouseDragged
  , pattern NSEventTypeGesture
  , pattern NSEventTypeMagnify
  , pattern NSEventTypeSwipe
  , pattern NSEventTypeRotate
  , pattern NSEventTypeBeginGesture
  , pattern NSEventTypeEndGesture
  , pattern NSEventTypeSmartMagnify
  , pattern NSEventTypeQuickLook
  , pattern NSEventTypePressure
  , pattern NSEventTypeDirectTouch
  , pattern NSEventTypeChangeMode
  , pattern NSEventTypeMouseCancelled
  , NSPointingDeviceType(NSPointingDeviceType)
  , pattern NSPointingDeviceTypeUnknown
  , pattern NSPointingDeviceTypePen
  , pattern NSPointingDeviceTypeCursor
  , pattern NSPointingDeviceTypeEraser
  , NSPressureBehavior(NSPressureBehavior)
  , pattern NSPressureBehaviorUnknown
  , pattern NSPressureBehaviorPrimaryDefault
  , pattern NSPressureBehaviorPrimaryClick
  , pattern NSPressureBehaviorPrimaryGeneric
  , pattern NSPressureBehaviorPrimaryAccelerator
  , pattern NSPressureBehaviorPrimaryDeepClick
  , pattern NSPressureBehaviorPrimaryDeepDrag
  , NSTouchPhase(NSTouchPhase)
  , pattern NSTouchPhaseBegan
  , pattern NSTouchPhaseMoved
  , pattern NSTouchPhaseStationary
  , pattern NSTouchPhaseEnded
  , pattern NSTouchPhaseCancelled
  , pattern NSTouchPhaseTouching
  , pattern NSTouchPhaseAny

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

-- | @- charactersByApplyingModifiers:@
charactersByApplyingModifiers :: IsNSEvent nsEvent => nsEvent -> NSEventModifierFlags -> IO (Id NSString)
charactersByApplyingModifiers nsEvent  modifiers =
    sendMsg nsEvent (mkSelector "charactersByApplyingModifiers:") (retPtr retVoid) [argCULong (coerce modifiers)] >>= retainedObject . castPtr

-- | @+ eventWithEventRef:@
eventWithEventRef :: Const (Ptr ()) -> IO (Id NSEvent)
eventWithEventRef eventRef =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "eventWithEventRef:") (retPtr retVoid) [argPtr (unConst eventRef)] >>= retainedObject . castPtr

-- | @+ eventWithCGEvent:@
eventWithCGEvent :: Ptr () -> IO (Id NSEvent)
eventWithCGEvent cgEvent =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "eventWithCGEvent:") (retPtr retVoid) [argPtr cgEvent] >>= retainedObject . castPtr

-- | @- touchesMatchingPhase:inView:@
touchesMatchingPhase_inView :: (IsNSEvent nsEvent, IsNSView view) => nsEvent -> NSTouchPhase -> view -> IO (Id NSSet)
touchesMatchingPhase_inView nsEvent  phase view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsEvent (mkSelector "touchesMatchingPhase:inView:") (retPtr retVoid) [argCULong (coerce phase), argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- allTouches@
allTouches :: IsNSEvent nsEvent => nsEvent -> IO (Id NSSet)
allTouches nsEvent  =
    sendMsg nsEvent (mkSelector "allTouches") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- touchesForView:@
touchesForView :: (IsNSEvent nsEvent, IsNSView view) => nsEvent -> view -> IO (Id NSSet)
touchesForView nsEvent  view =
  withObjCPtr view $ \raw_view ->
      sendMsg nsEvent (mkSelector "touchesForView:") (retPtr retVoid) [argPtr (castPtr raw_view :: Ptr ())] >>= retainedObject . castPtr

-- | @- coalescedTouchesForTouch:@
coalescedTouchesForTouch :: (IsNSEvent nsEvent, IsNSTouch touch) => nsEvent -> touch -> IO (Id NSArray)
coalescedTouchesForTouch nsEvent  touch =
  withObjCPtr touch $ \raw_touch ->
      sendMsg nsEvent (mkSelector "coalescedTouchesForTouch:") (retPtr retVoid) [argPtr (castPtr raw_touch :: Ptr ())] >>= retainedObject . castPtr

-- | @- trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:@
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandler :: IsNSEvent nsEvent => nsEvent -> NSEventSwipeTrackingOptions -> CDouble -> CDouble -> Ptr () -> IO ()
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandler nsEvent  options minDampenThreshold maxDampenThreshold trackingHandler =
    sendMsg nsEvent (mkSelector "trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:") retVoid [argCULong (coerce options), argCDouble minDampenThreshold, argCDouble maxDampenThreshold, argPtr (castPtr trackingHandler :: Ptr ())]

-- | @+ startPeriodicEventsAfterDelay:withPeriod:@
startPeriodicEventsAfterDelay_withPeriod :: CDouble -> CDouble -> IO ()
startPeriodicEventsAfterDelay_withPeriod delay period =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "startPeriodicEventsAfterDelay:withPeriod:") retVoid [argCDouble delay, argCDouble period]

-- | @+ stopPeriodicEvents@
stopPeriodicEvents :: IO ()
stopPeriodicEvents  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "stopPeriodicEvents") retVoid []

-- | @+ mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:@
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure :: IsNSGraphicsContext unusedPassNil => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> CLong -> CLong -> CFloat -> IO (Id NSEvent)
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure type_ location flags time wNum unusedPassNil eNum cNum pressure =
  do
    cls' <- getRequiredClass "NSEvent"
    withObjCPtr unusedPassNil $ \raw_unusedPassNil ->
      sendClassMsg cls' (mkSelector "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:") (retPtr retVoid) [argCULong (coerce type_), argNSPoint location, argCULong (coerce flags), argCDouble time, argCLong wNum, argPtr (castPtr raw_unusedPassNil :: Ptr ()), argCLong eNum, argCLong cNum, argCFloat pressure] >>= retainedObject . castPtr

-- | @+ keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:@
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode :: (IsNSGraphicsContext unusedPassNil, IsNSString keys, IsNSString ukeys) => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> keys -> ukeys -> Bool -> CUShort -> IO (Id NSEvent)
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode type_ location flags time wNum unusedPassNil keys ukeys flag code =
  do
    cls' <- getRequiredClass "NSEvent"
    withObjCPtr unusedPassNil $ \raw_unusedPassNil ->
      withObjCPtr keys $ \raw_keys ->
        withObjCPtr ukeys $ \raw_ukeys ->
          sendClassMsg cls' (mkSelector "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:") (retPtr retVoid) [argCULong (coerce type_), argNSPoint location, argCULong (coerce flags), argCDouble time, argCLong wNum, argPtr (castPtr raw_unusedPassNil :: Ptr ()), argPtr (castPtr raw_keys :: Ptr ()), argPtr (castPtr raw_ukeys :: Ptr ()), argCULong (if flag then 1 else 0), argCUInt (fromIntegral code)] >>= retainedObject . castPtr

-- | @+ enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:@
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData :: IsNSGraphicsContext unusedPassNil => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> CLong -> CLong -> Ptr () -> IO (Id NSEvent)
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData type_ location flags time wNum unusedPassNil eNum tNum data_ =
  do
    cls' <- getRequiredClass "NSEvent"
    withObjCPtr unusedPassNil $ \raw_unusedPassNil ->
      sendClassMsg cls' (mkSelector "enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:") (retPtr retVoid) [argCULong (coerce type_), argNSPoint location, argCULong (coerce flags), argCDouble time, argCLong wNum, argPtr (castPtr raw_unusedPassNil :: Ptr ()), argCLong eNum, argCLong tNum, argPtr data_] >>= retainedObject . castPtr

-- | @+ otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:@
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2 :: IsNSGraphicsContext unusedPassNil => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> CShort -> CLong -> CLong -> IO (Id NSEvent)
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2 type_ location flags time wNum unusedPassNil subtype d1 d2 =
  do
    cls' <- getRequiredClass "NSEvent"
    withObjCPtr unusedPassNil $ \raw_unusedPassNil ->
      sendClassMsg cls' (mkSelector "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:") (retPtr retVoid) [argCULong (coerce type_), argNSPoint location, argCULong (coerce flags), argCDouble time, argCLong wNum, argPtr (castPtr raw_unusedPassNil :: Ptr ()), argCInt (fromIntegral subtype), argCLong d1, argCLong d2] >>= retainedObject . castPtr

-- | @+ addGlobalMonitorForEventsMatchingMask:handler:@
addGlobalMonitorForEventsMatchingMask_handler :: NSEventMask -> Ptr () -> IO RawId
addGlobalMonitorForEventsMatchingMask_handler mask block =
  do
    cls' <- getRequiredClass "NSEvent"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "addGlobalMonitorForEventsMatchingMask:handler:") (retPtr retVoid) [argCULong (coerce mask), argPtr (castPtr block :: Ptr ())]

-- | @+ addLocalMonitorForEventsMatchingMask:handler:@
addLocalMonitorForEventsMatchingMask_handler :: NSEventMask -> Ptr () -> IO RawId
addLocalMonitorForEventsMatchingMask_handler mask block =
  do
    cls' <- getRequiredClass "NSEvent"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "addLocalMonitorForEventsMatchingMask:handler:") (retPtr retVoid) [argCULong (coerce mask), argPtr (castPtr block :: Ptr ())]

-- | @+ removeMonitor:@
removeMonitor :: RawId -> IO ()
removeMonitor eventMonitor =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "removeMonitor:") retVoid [argPtr (castPtr (unRawId eventMonitor) :: Ptr ())]

-- | @- type@
type_ :: IsNSEvent nsEvent => nsEvent -> IO NSEventType
type_ nsEvent  =
    fmap (coerce :: CULong -> NSEventType) $ sendMsg nsEvent (mkSelector "type") retCULong []

-- | @- modifierFlags@
modifierFlags :: IsNSEvent nsEvent => nsEvent -> IO NSEventModifierFlags
modifierFlags nsEvent  =
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg nsEvent (mkSelector "modifierFlags") retCULong []

-- | @- timestamp@
timestamp :: IsNSEvent nsEvent => nsEvent -> IO CDouble
timestamp nsEvent  =
    sendMsg nsEvent (mkSelector "timestamp") retCDouble []

-- | @- window@
window :: IsNSEvent nsEvent => nsEvent -> IO (Id NSWindow)
window nsEvent  =
    sendMsg nsEvent (mkSelector "window") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- windowNumber@
windowNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
windowNumber nsEvent  =
    sendMsg nsEvent (mkSelector "windowNumber") retCLong []

-- | @- context@
context :: IsNSEvent nsEvent => nsEvent -> IO (Id NSGraphicsContext)
context nsEvent  =
    sendMsg nsEvent (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- clickCount@
clickCount :: IsNSEvent nsEvent => nsEvent -> IO CLong
clickCount nsEvent  =
    sendMsg nsEvent (mkSelector "clickCount") retCLong []

-- | @- buttonNumber@
buttonNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
buttonNumber nsEvent  =
    sendMsg nsEvent (mkSelector "buttonNumber") retCLong []

-- | @- eventNumber@
eventNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
eventNumber nsEvent  =
    sendMsg nsEvent (mkSelector "eventNumber") retCLong []

-- | @- pressure@
pressure :: IsNSEvent nsEvent => nsEvent -> IO CFloat
pressure nsEvent  =
    sendMsg nsEvent (mkSelector "pressure") retCFloat []

-- | @- locationInWindow@
locationInWindow :: IsNSEvent nsEvent => nsEvent -> IO NSPoint
locationInWindow nsEvent  =
    sendMsgStret nsEvent (mkSelector "locationInWindow") retNSPoint []

-- | @- deltaX@
deltaX :: IsNSEvent nsEvent => nsEvent -> IO CDouble
deltaX nsEvent  =
    sendMsg nsEvent (mkSelector "deltaX") retCDouble []

-- | @- deltaY@
deltaY :: IsNSEvent nsEvent => nsEvent -> IO CDouble
deltaY nsEvent  =
    sendMsg nsEvent (mkSelector "deltaY") retCDouble []

-- | @- deltaZ@
deltaZ :: IsNSEvent nsEvent => nsEvent -> IO CDouble
deltaZ nsEvent  =
    sendMsg nsEvent (mkSelector "deltaZ") retCDouble []

-- | @- hasPreciseScrollingDeltas@
hasPreciseScrollingDeltas :: IsNSEvent nsEvent => nsEvent -> IO Bool
hasPreciseScrollingDeltas nsEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEvent (mkSelector "hasPreciseScrollingDeltas") retCULong []

-- | @- scrollingDeltaX@
scrollingDeltaX :: IsNSEvent nsEvent => nsEvent -> IO CDouble
scrollingDeltaX nsEvent  =
    sendMsg nsEvent (mkSelector "scrollingDeltaX") retCDouble []

-- | @- scrollingDeltaY@
scrollingDeltaY :: IsNSEvent nsEvent => nsEvent -> IO CDouble
scrollingDeltaY nsEvent  =
    sendMsg nsEvent (mkSelector "scrollingDeltaY") retCDouble []

-- | @- momentumPhase@
momentumPhase :: IsNSEvent nsEvent => nsEvent -> IO NSEventPhase
momentumPhase nsEvent  =
    fmap (coerce :: CULong -> NSEventPhase) $ sendMsg nsEvent (mkSelector "momentumPhase") retCULong []

-- | @- directionInvertedFromDevice@
directionInvertedFromDevice :: IsNSEvent nsEvent => nsEvent -> IO Bool
directionInvertedFromDevice nsEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEvent (mkSelector "directionInvertedFromDevice") retCULong []

-- | @- characters@
characters :: IsNSEvent nsEvent => nsEvent -> IO (Id NSString)
characters nsEvent  =
    sendMsg nsEvent (mkSelector "characters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- charactersIgnoringModifiers@
charactersIgnoringModifiers :: IsNSEvent nsEvent => nsEvent -> IO (Id NSString)
charactersIgnoringModifiers nsEvent  =
    sendMsg nsEvent (mkSelector "charactersIgnoringModifiers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- ARepeat@
aRepeat :: IsNSEvent nsEvent => nsEvent -> IO Bool
aRepeat nsEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEvent (mkSelector "ARepeat") retCULong []

-- | @- keyCode@
keyCode :: IsNSEvent nsEvent => nsEvent -> IO CUShort
keyCode nsEvent  =
    fmap fromIntegral $ sendMsg nsEvent (mkSelector "keyCode") retCUInt []

-- | @- trackingNumber@
trackingNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
trackingNumber nsEvent  =
    sendMsg nsEvent (mkSelector "trackingNumber") retCLong []

-- | @- userData@
userData :: IsNSEvent nsEvent => nsEvent -> IO (Ptr ())
userData nsEvent  =
    fmap castPtr $ sendMsg nsEvent (mkSelector "userData") (retPtr retVoid) []

-- | @- trackingArea@
trackingArea :: IsNSEvent nsEvent => nsEvent -> IO (Id NSTrackingArea)
trackingArea nsEvent  =
    sendMsg nsEvent (mkSelector "trackingArea") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subtype@
subtype :: IsNSEvent nsEvent => nsEvent -> IO NSEventSubtype
subtype nsEvent  =
    fmap (NSEventSubtype . fromIntegral :: CInt -> NSEventSubtype) $ sendMsg nsEvent (mkSelector "subtype") retCInt []

-- | @- data1@
data1 :: IsNSEvent nsEvent => nsEvent -> IO CLong
data1 nsEvent  =
    sendMsg nsEvent (mkSelector "data1") retCLong []

-- | @- data2@
data2 :: IsNSEvent nsEvent => nsEvent -> IO CLong
data2 nsEvent  =
    sendMsg nsEvent (mkSelector "data2") retCLong []

-- | @- eventRef@
eventRef :: IsNSEvent nsEvent => nsEvent -> IO RawId
eventRef nsEvent  =
    fmap (RawId . castPtr) $ sendMsg nsEvent (mkSelector "eventRef") (retPtr retVoid) []

-- | @- CGEvent@
cgEvent :: IsNSEvent nsEvent => nsEvent -> IO (Ptr ())
cgEvent nsEvent  =
    fmap castPtr $ sendMsg nsEvent (mkSelector "CGEvent") (retPtr retVoid) []

-- | @+ mouseCoalescingEnabled@
mouseCoalescingEnabled :: IO Bool
mouseCoalescingEnabled  =
  do
    cls' <- getRequiredClass "NSEvent"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "mouseCoalescingEnabled") retCULong []

-- | @+ setMouseCoalescingEnabled:@
setMouseCoalescingEnabled :: Bool -> IO ()
setMouseCoalescingEnabled value =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "setMouseCoalescingEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- magnification@
magnification :: IsNSEvent nsEvent => nsEvent -> IO CDouble
magnification nsEvent  =
    sendMsg nsEvent (mkSelector "magnification") retCDouble []

-- | @- deviceID@
deviceID :: IsNSEvent nsEvent => nsEvent -> IO CULong
deviceID nsEvent  =
    sendMsg nsEvent (mkSelector "deviceID") retCULong []

-- | @- rotation@
rotation :: IsNSEvent nsEvent => nsEvent -> IO CFloat
rotation nsEvent  =
    sendMsg nsEvent (mkSelector "rotation") retCFloat []

-- | @- absoluteX@
absoluteX :: IsNSEvent nsEvent => nsEvent -> IO CLong
absoluteX nsEvent  =
    sendMsg nsEvent (mkSelector "absoluteX") retCLong []

-- | @- absoluteY@
absoluteY :: IsNSEvent nsEvent => nsEvent -> IO CLong
absoluteY nsEvent  =
    sendMsg nsEvent (mkSelector "absoluteY") retCLong []

-- | @- absoluteZ@
absoluteZ :: IsNSEvent nsEvent => nsEvent -> IO CLong
absoluteZ nsEvent  =
    sendMsg nsEvent (mkSelector "absoluteZ") retCLong []

-- | @- buttonMask@
buttonMask :: IsNSEvent nsEvent => nsEvent -> IO NSEventButtonMask
buttonMask nsEvent  =
    fmap (coerce :: CULong -> NSEventButtonMask) $ sendMsg nsEvent (mkSelector "buttonMask") retCULong []

-- | @- tilt@
tilt :: IsNSEvent nsEvent => nsEvent -> IO NSPoint
tilt nsEvent  =
    sendMsgStret nsEvent (mkSelector "tilt") retNSPoint []

-- | @- tangentialPressure@
tangentialPressure :: IsNSEvent nsEvent => nsEvent -> IO CFloat
tangentialPressure nsEvent  =
    sendMsg nsEvent (mkSelector "tangentialPressure") retCFloat []

-- | @- vendorDefined@
vendorDefined :: IsNSEvent nsEvent => nsEvent -> IO RawId
vendorDefined nsEvent  =
    fmap (RawId . castPtr) $ sendMsg nsEvent (mkSelector "vendorDefined") (retPtr retVoid) []

-- | @- vendorID@
vendorID :: IsNSEvent nsEvent => nsEvent -> IO CULong
vendorID nsEvent  =
    sendMsg nsEvent (mkSelector "vendorID") retCULong []

-- | @- tabletID@
tabletID :: IsNSEvent nsEvent => nsEvent -> IO CULong
tabletID nsEvent  =
    sendMsg nsEvent (mkSelector "tabletID") retCULong []

-- | @- pointingDeviceID@
pointingDeviceID :: IsNSEvent nsEvent => nsEvent -> IO CULong
pointingDeviceID nsEvent  =
    sendMsg nsEvent (mkSelector "pointingDeviceID") retCULong []

-- | @- systemTabletID@
systemTabletID :: IsNSEvent nsEvent => nsEvent -> IO CULong
systemTabletID nsEvent  =
    sendMsg nsEvent (mkSelector "systemTabletID") retCULong []

-- | @- vendorPointingDeviceType@
vendorPointingDeviceType :: IsNSEvent nsEvent => nsEvent -> IO CULong
vendorPointingDeviceType nsEvent  =
    sendMsg nsEvent (mkSelector "vendorPointingDeviceType") retCULong []

-- | @- pointingDeviceSerialNumber@
pointingDeviceSerialNumber :: IsNSEvent nsEvent => nsEvent -> IO CULong
pointingDeviceSerialNumber nsEvent  =
    sendMsg nsEvent (mkSelector "pointingDeviceSerialNumber") retCULong []

-- | @- uniqueID@
uniqueID :: IsNSEvent nsEvent => nsEvent -> IO CULong
uniqueID nsEvent  =
    sendMsg nsEvent (mkSelector "uniqueID") retCULong []

-- | @- capabilityMask@
capabilityMask :: IsNSEvent nsEvent => nsEvent -> IO CULong
capabilityMask nsEvent  =
    sendMsg nsEvent (mkSelector "capabilityMask") retCULong []

-- | @- pointingDeviceType@
pointingDeviceType :: IsNSEvent nsEvent => nsEvent -> IO NSPointingDeviceType
pointingDeviceType nsEvent  =
    fmap (coerce :: CULong -> NSPointingDeviceType) $ sendMsg nsEvent (mkSelector "pointingDeviceType") retCULong []

-- | @- enteringProximity@
enteringProximity :: IsNSEvent nsEvent => nsEvent -> IO Bool
enteringProximity nsEvent  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsEvent (mkSelector "enteringProximity") retCULong []

-- | @- phase@
phase :: IsNSEvent nsEvent => nsEvent -> IO NSEventPhase
phase nsEvent  =
    fmap (coerce :: CULong -> NSEventPhase) $ sendMsg nsEvent (mkSelector "phase") retCULong []

-- | @- stage@
stage :: IsNSEvent nsEvent => nsEvent -> IO CLong
stage nsEvent  =
    sendMsg nsEvent (mkSelector "stage") retCLong []

-- | @- stageTransition@
stageTransition :: IsNSEvent nsEvent => nsEvent -> IO CDouble
stageTransition nsEvent  =
    sendMsg nsEvent (mkSelector "stageTransition") retCDouble []

-- | @- associatedEventsMask@
associatedEventsMask :: IsNSEvent nsEvent => nsEvent -> IO NSEventMask
associatedEventsMask nsEvent  =
    fmap (coerce :: CULong -> NSEventMask) $ sendMsg nsEvent (mkSelector "associatedEventsMask") retCULong []

-- | @- pressureBehavior@
pressureBehavior :: IsNSEvent nsEvent => nsEvent -> IO NSPressureBehavior
pressureBehavior nsEvent  =
    fmap (coerce :: CLong -> NSPressureBehavior) $ sendMsg nsEvent (mkSelector "pressureBehavior") retCLong []

-- | @+ swipeTrackingFromScrollEventsEnabled@
swipeTrackingFromScrollEventsEnabled :: IO Bool
swipeTrackingFromScrollEventsEnabled  =
  do
    cls' <- getRequiredClass "NSEvent"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "swipeTrackingFromScrollEventsEnabled") retCULong []

-- | @+ mouseLocation@
mouseLocation :: IO NSPoint
mouseLocation  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsgStret cls' (mkSelector "mouseLocation") retNSPoint []

-- | @+ modifierFlags@
nsEventModifierFlags :: IO NSEventModifierFlags
nsEventModifierFlags  =
  do
    cls' <- getRequiredClass "NSEvent"
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendClassMsg cls' (mkSelector "modifierFlags") retCULong []

-- | @+ pressedMouseButtons@
pressedMouseButtons :: IO CULong
pressedMouseButtons  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "pressedMouseButtons") retCULong []

-- | @+ doubleClickInterval@
doubleClickInterval :: IO CDouble
doubleClickInterval  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "doubleClickInterval") retCDouble []

-- | @+ keyRepeatDelay@
keyRepeatDelay :: IO CDouble
keyRepeatDelay  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "keyRepeatDelay") retCDouble []

-- | @+ keyRepeatInterval@
keyRepeatInterval :: IO CDouble
keyRepeatInterval  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMsg cls' (mkSelector "keyRepeatInterval") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @charactersByApplyingModifiers:@
charactersByApplyingModifiersSelector :: Selector
charactersByApplyingModifiersSelector = mkSelector "charactersByApplyingModifiers:"

-- | @Selector@ for @eventWithEventRef:@
eventWithEventRefSelector :: Selector
eventWithEventRefSelector = mkSelector "eventWithEventRef:"

-- | @Selector@ for @eventWithCGEvent:@
eventWithCGEventSelector :: Selector
eventWithCGEventSelector = mkSelector "eventWithCGEvent:"

-- | @Selector@ for @touchesMatchingPhase:inView:@
touchesMatchingPhase_inViewSelector :: Selector
touchesMatchingPhase_inViewSelector = mkSelector "touchesMatchingPhase:inView:"

-- | @Selector@ for @allTouches@
allTouchesSelector :: Selector
allTouchesSelector = mkSelector "allTouches"

-- | @Selector@ for @touchesForView:@
touchesForViewSelector :: Selector
touchesForViewSelector = mkSelector "touchesForView:"

-- | @Selector@ for @coalescedTouchesForTouch:@
coalescedTouchesForTouchSelector :: Selector
coalescedTouchesForTouchSelector = mkSelector "coalescedTouchesForTouch:"

-- | @Selector@ for @trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:@
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector :: Selector
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector = mkSelector "trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:"

-- | @Selector@ for @startPeriodicEventsAfterDelay:withPeriod:@
startPeriodicEventsAfterDelay_withPeriodSelector :: Selector
startPeriodicEventsAfterDelay_withPeriodSelector = mkSelector "startPeriodicEventsAfterDelay:withPeriod:"

-- | @Selector@ for @stopPeriodicEvents@
stopPeriodicEventsSelector :: Selector
stopPeriodicEventsSelector = mkSelector "stopPeriodicEvents"

-- | @Selector@ for @mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:@
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector :: Selector
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector = mkSelector "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"

-- | @Selector@ for @keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:@
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector :: Selector
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector = mkSelector "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"

-- | @Selector@ for @enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:@
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector :: Selector
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector = mkSelector "enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:"

-- | @Selector@ for @otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:@
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector :: Selector
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector = mkSelector "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"

-- | @Selector@ for @addGlobalMonitorForEventsMatchingMask:handler:@
addGlobalMonitorForEventsMatchingMask_handlerSelector :: Selector
addGlobalMonitorForEventsMatchingMask_handlerSelector = mkSelector "addGlobalMonitorForEventsMatchingMask:handler:"

-- | @Selector@ for @addLocalMonitorForEventsMatchingMask:handler:@
addLocalMonitorForEventsMatchingMask_handlerSelector :: Selector
addLocalMonitorForEventsMatchingMask_handlerSelector = mkSelector "addLocalMonitorForEventsMatchingMask:handler:"

-- | @Selector@ for @removeMonitor:@
removeMonitorSelector :: Selector
removeMonitorSelector = mkSelector "removeMonitor:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @window@
windowSelector :: Selector
windowSelector = mkSelector "window"

-- | @Selector@ for @windowNumber@
windowNumberSelector :: Selector
windowNumberSelector = mkSelector "windowNumber"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @clickCount@
clickCountSelector :: Selector
clickCountSelector = mkSelector "clickCount"

-- | @Selector@ for @buttonNumber@
buttonNumberSelector :: Selector
buttonNumberSelector = mkSelector "buttonNumber"

-- | @Selector@ for @eventNumber@
eventNumberSelector :: Selector
eventNumberSelector = mkSelector "eventNumber"

-- | @Selector@ for @pressure@
pressureSelector :: Selector
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @locationInWindow@
locationInWindowSelector :: Selector
locationInWindowSelector = mkSelector "locationInWindow"

-- | @Selector@ for @deltaX@
deltaXSelector :: Selector
deltaXSelector = mkSelector "deltaX"

-- | @Selector@ for @deltaY@
deltaYSelector :: Selector
deltaYSelector = mkSelector "deltaY"

-- | @Selector@ for @deltaZ@
deltaZSelector :: Selector
deltaZSelector = mkSelector "deltaZ"

-- | @Selector@ for @hasPreciseScrollingDeltas@
hasPreciseScrollingDeltasSelector :: Selector
hasPreciseScrollingDeltasSelector = mkSelector "hasPreciseScrollingDeltas"

-- | @Selector@ for @scrollingDeltaX@
scrollingDeltaXSelector :: Selector
scrollingDeltaXSelector = mkSelector "scrollingDeltaX"

-- | @Selector@ for @scrollingDeltaY@
scrollingDeltaYSelector :: Selector
scrollingDeltaYSelector = mkSelector "scrollingDeltaY"

-- | @Selector@ for @momentumPhase@
momentumPhaseSelector :: Selector
momentumPhaseSelector = mkSelector "momentumPhase"

-- | @Selector@ for @directionInvertedFromDevice@
directionInvertedFromDeviceSelector :: Selector
directionInvertedFromDeviceSelector = mkSelector "directionInvertedFromDevice"

-- | @Selector@ for @characters@
charactersSelector :: Selector
charactersSelector = mkSelector "characters"

-- | @Selector@ for @charactersIgnoringModifiers@
charactersIgnoringModifiersSelector :: Selector
charactersIgnoringModifiersSelector = mkSelector "charactersIgnoringModifiers"

-- | @Selector@ for @ARepeat@
aRepeatSelector :: Selector
aRepeatSelector = mkSelector "ARepeat"

-- | @Selector@ for @keyCode@
keyCodeSelector :: Selector
keyCodeSelector = mkSelector "keyCode"

-- | @Selector@ for @trackingNumber@
trackingNumberSelector :: Selector
trackingNumberSelector = mkSelector "trackingNumber"

-- | @Selector@ for @userData@
userDataSelector :: Selector
userDataSelector = mkSelector "userData"

-- | @Selector@ for @trackingArea@
trackingAreaSelector :: Selector
trackingAreaSelector = mkSelector "trackingArea"

-- | @Selector@ for @subtype@
subtypeSelector :: Selector
subtypeSelector = mkSelector "subtype"

-- | @Selector@ for @data1@
data1Selector :: Selector
data1Selector = mkSelector "data1"

-- | @Selector@ for @data2@
data2Selector :: Selector
data2Selector = mkSelector "data2"

-- | @Selector@ for @eventRef@
eventRefSelector :: Selector
eventRefSelector = mkSelector "eventRef"

-- | @Selector@ for @CGEvent@
cgEventSelector :: Selector
cgEventSelector = mkSelector "CGEvent"

-- | @Selector@ for @mouseCoalescingEnabled@
mouseCoalescingEnabledSelector :: Selector
mouseCoalescingEnabledSelector = mkSelector "mouseCoalescingEnabled"

-- | @Selector@ for @setMouseCoalescingEnabled:@
setMouseCoalescingEnabledSelector :: Selector
setMouseCoalescingEnabledSelector = mkSelector "setMouseCoalescingEnabled:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @deviceID@
deviceIDSelector :: Selector
deviceIDSelector = mkSelector "deviceID"

-- | @Selector@ for @rotation@
rotationSelector :: Selector
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @absoluteX@
absoluteXSelector :: Selector
absoluteXSelector = mkSelector "absoluteX"

-- | @Selector@ for @absoluteY@
absoluteYSelector :: Selector
absoluteYSelector = mkSelector "absoluteY"

-- | @Selector@ for @absoluteZ@
absoluteZSelector :: Selector
absoluteZSelector = mkSelector "absoluteZ"

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @tilt@
tiltSelector :: Selector
tiltSelector = mkSelector "tilt"

-- | @Selector@ for @tangentialPressure@
tangentialPressureSelector :: Selector
tangentialPressureSelector = mkSelector "tangentialPressure"

-- | @Selector@ for @vendorDefined@
vendorDefinedSelector :: Selector
vendorDefinedSelector = mkSelector "vendorDefined"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @tabletID@
tabletIDSelector :: Selector
tabletIDSelector = mkSelector "tabletID"

-- | @Selector@ for @pointingDeviceID@
pointingDeviceIDSelector :: Selector
pointingDeviceIDSelector = mkSelector "pointingDeviceID"

-- | @Selector@ for @systemTabletID@
systemTabletIDSelector :: Selector
systemTabletIDSelector = mkSelector "systemTabletID"

-- | @Selector@ for @vendorPointingDeviceType@
vendorPointingDeviceTypeSelector :: Selector
vendorPointingDeviceTypeSelector = mkSelector "vendorPointingDeviceType"

-- | @Selector@ for @pointingDeviceSerialNumber@
pointingDeviceSerialNumberSelector :: Selector
pointingDeviceSerialNumberSelector = mkSelector "pointingDeviceSerialNumber"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @capabilityMask@
capabilityMaskSelector :: Selector
capabilityMaskSelector = mkSelector "capabilityMask"

-- | @Selector@ for @pointingDeviceType@
pointingDeviceTypeSelector :: Selector
pointingDeviceTypeSelector = mkSelector "pointingDeviceType"

-- | @Selector@ for @enteringProximity@
enteringProximitySelector :: Selector
enteringProximitySelector = mkSelector "enteringProximity"

-- | @Selector@ for @phase@
phaseSelector :: Selector
phaseSelector = mkSelector "phase"

-- | @Selector@ for @stage@
stageSelector :: Selector
stageSelector = mkSelector "stage"

-- | @Selector@ for @stageTransition@
stageTransitionSelector :: Selector
stageTransitionSelector = mkSelector "stageTransition"

-- | @Selector@ for @associatedEventsMask@
associatedEventsMaskSelector :: Selector
associatedEventsMaskSelector = mkSelector "associatedEventsMask"

-- | @Selector@ for @pressureBehavior@
pressureBehaviorSelector :: Selector
pressureBehaviorSelector = mkSelector "pressureBehavior"

-- | @Selector@ for @swipeTrackingFromScrollEventsEnabled@
swipeTrackingFromScrollEventsEnabledSelector :: Selector
swipeTrackingFromScrollEventsEnabledSelector = mkSelector "swipeTrackingFromScrollEventsEnabled"

-- | @Selector@ for @mouseLocation@
mouseLocationSelector :: Selector
mouseLocationSelector = mkSelector "mouseLocation"

-- | @Selector@ for @pressedMouseButtons@
pressedMouseButtonsSelector :: Selector
pressedMouseButtonsSelector = mkSelector "pressedMouseButtons"

-- | @Selector@ for @doubleClickInterval@
doubleClickIntervalSelector :: Selector
doubleClickIntervalSelector = mkSelector "doubleClickInterval"

-- | @Selector@ for @keyRepeatDelay@
keyRepeatDelaySelector :: Selector
keyRepeatDelaySelector = mkSelector "keyRepeatDelay"

-- | @Selector@ for @keyRepeatInterval@
keyRepeatIntervalSelector :: Selector
keyRepeatIntervalSelector = mkSelector "keyRepeatInterval"

