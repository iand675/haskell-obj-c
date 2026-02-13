{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , aRepeatSelector
  , absoluteXSelector
  , absoluteYSelector
  , absoluteZSelector
  , addGlobalMonitorForEventsMatchingMask_handlerSelector
  , addLocalMonitorForEventsMatchingMask_handlerSelector
  , allTouchesSelector
  , associatedEventsMaskSelector
  , buttonMaskSelector
  , buttonNumberSelector
  , capabilityMaskSelector
  , cgEventSelector
  , charactersByApplyingModifiersSelector
  , charactersIgnoringModifiersSelector
  , charactersSelector
  , clickCountSelector
  , coalescedTouchesForTouchSelector
  , contextSelector
  , data1Selector
  , data2Selector
  , deltaXSelector
  , deltaYSelector
  , deltaZSelector
  , deviceIDSelector
  , directionInvertedFromDeviceSelector
  , doubleClickIntervalSelector
  , enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector
  , enteringProximitySelector
  , eventNumberSelector
  , eventRefSelector
  , eventWithCGEventSelector
  , eventWithEventRefSelector
  , hasPreciseScrollingDeltasSelector
  , keyCodeSelector
  , keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector
  , keyRepeatDelaySelector
  , keyRepeatIntervalSelector
  , locationInWindowSelector
  , magnificationSelector
  , modifierFlagsSelector
  , momentumPhaseSelector
  , mouseCoalescingEnabledSelector
  , mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector
  , mouseLocationSelector
  , nsEventModifierFlagsSelector
  , otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector
  , phaseSelector
  , pointingDeviceIDSelector
  , pointingDeviceSerialNumberSelector
  , pointingDeviceTypeSelector
  , pressedMouseButtonsSelector
  , pressureBehaviorSelector
  , pressureSelector
  , removeMonitorSelector
  , rotationSelector
  , scrollingDeltaXSelector
  , scrollingDeltaYSelector
  , setMouseCoalescingEnabledSelector
  , stageSelector
  , stageTransitionSelector
  , startPeriodicEventsAfterDelay_withPeriodSelector
  , stopPeriodicEventsSelector
  , subtypeSelector
  , swipeTrackingFromScrollEventsEnabledSelector
  , systemTabletIDSelector
  , tabletIDSelector
  , tangentialPressureSelector
  , tiltSelector
  , timestampSelector
  , touchesForViewSelector
  , touchesMatchingPhase_inViewSelector
  , trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector
  , trackingAreaSelector
  , trackingNumberSelector
  , typeSelector
  , uniqueIDSelector
  , userDataSelector
  , vendorDefinedSelector
  , vendorIDSelector
  , vendorPointingDeviceTypeSelector
  , windowNumberSelector
  , windowSelector

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

-- | @- charactersByApplyingModifiers:@
charactersByApplyingModifiers :: IsNSEvent nsEvent => nsEvent -> NSEventModifierFlags -> IO (Id NSString)
charactersByApplyingModifiers nsEvent modifiers =
  sendMessage nsEvent charactersByApplyingModifiersSelector modifiers

-- | @+ eventWithEventRef:@
eventWithEventRef :: Const (Ptr ()) -> IO (Id NSEvent)
eventWithEventRef eventRef =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' eventWithEventRefSelector eventRef

-- | @+ eventWithCGEvent:@
eventWithCGEvent :: Ptr () -> IO (Id NSEvent)
eventWithCGEvent cgEvent =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' eventWithCGEventSelector cgEvent

-- | @- touchesMatchingPhase:inView:@
touchesMatchingPhase_inView :: (IsNSEvent nsEvent, IsNSView view) => nsEvent -> NSTouchPhase -> view -> IO (Id NSSet)
touchesMatchingPhase_inView nsEvent phase view =
  sendMessage nsEvent touchesMatchingPhase_inViewSelector phase (toNSView view)

-- | @- allTouches@
allTouches :: IsNSEvent nsEvent => nsEvent -> IO (Id NSSet)
allTouches nsEvent =
  sendMessage nsEvent allTouchesSelector

-- | @- touchesForView:@
touchesForView :: (IsNSEvent nsEvent, IsNSView view) => nsEvent -> view -> IO (Id NSSet)
touchesForView nsEvent view =
  sendMessage nsEvent touchesForViewSelector (toNSView view)

-- | @- coalescedTouchesForTouch:@
coalescedTouchesForTouch :: (IsNSEvent nsEvent, IsNSTouch touch) => nsEvent -> touch -> IO (Id NSArray)
coalescedTouchesForTouch nsEvent touch =
  sendMessage nsEvent coalescedTouchesForTouchSelector (toNSTouch touch)

-- | @- trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:@
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandler :: IsNSEvent nsEvent => nsEvent -> NSEventSwipeTrackingOptions -> CDouble -> CDouble -> Ptr () -> IO ()
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandler nsEvent options minDampenThreshold maxDampenThreshold trackingHandler =
  sendMessage nsEvent trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector options minDampenThreshold maxDampenThreshold trackingHandler

-- | @+ startPeriodicEventsAfterDelay:withPeriod:@
startPeriodicEventsAfterDelay_withPeriod :: CDouble -> CDouble -> IO ()
startPeriodicEventsAfterDelay_withPeriod delay period =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' startPeriodicEventsAfterDelay_withPeriodSelector delay period

-- | @+ stopPeriodicEvents@
stopPeriodicEvents :: IO ()
stopPeriodicEvents  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' stopPeriodicEventsSelector

-- | @+ mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:@
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure :: IsNSGraphicsContext unusedPassNil => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> CLong -> CLong -> CFloat -> IO (Id NSEvent)
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressure type_ location flags time wNum unusedPassNil eNum cNum pressure =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector type_ location flags time wNum (toNSGraphicsContext unusedPassNil) eNum cNum pressure

-- | @+ keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:@
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode :: (IsNSGraphicsContext unusedPassNil, IsNSString keys, IsNSString ukeys) => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> keys -> ukeys -> Bool -> CUShort -> IO (Id NSEvent)
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCode type_ location flags time wNum unusedPassNil keys ukeys flag code =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector type_ location flags time wNum (toNSGraphicsContext unusedPassNil) (toNSString keys) (toNSString ukeys) flag code

-- | @+ enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:@
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData :: IsNSGraphicsContext unusedPassNil => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> CLong -> CLong -> Ptr () -> IO (Id NSEvent)
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userData type_ location flags time wNum unusedPassNil eNum tNum data_ =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector type_ location flags time wNum (toNSGraphicsContext unusedPassNil) eNum tNum data_

-- | @+ otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:@
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2 :: IsNSGraphicsContext unusedPassNil => NSEventType -> NSPoint -> NSEventModifierFlags -> CDouble -> CLong -> unusedPassNil -> CShort -> CLong -> CLong -> IO (Id NSEvent)
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2 type_ location flags time wNum unusedPassNil subtype d1 d2 =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector type_ location flags time wNum (toNSGraphicsContext unusedPassNil) subtype d1 d2

-- | @+ addGlobalMonitorForEventsMatchingMask:handler:@
addGlobalMonitorForEventsMatchingMask_handler :: NSEventMask -> Ptr () -> IO RawId
addGlobalMonitorForEventsMatchingMask_handler mask block =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' addGlobalMonitorForEventsMatchingMask_handlerSelector mask block

-- | @+ addLocalMonitorForEventsMatchingMask:handler:@
addLocalMonitorForEventsMatchingMask_handler :: NSEventMask -> Ptr () -> IO RawId
addLocalMonitorForEventsMatchingMask_handler mask block =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' addLocalMonitorForEventsMatchingMask_handlerSelector mask block

-- | @+ removeMonitor:@
removeMonitor :: RawId -> IO ()
removeMonitor eventMonitor =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' removeMonitorSelector eventMonitor

-- | @- type@
type_ :: IsNSEvent nsEvent => nsEvent -> IO NSEventType
type_ nsEvent =
  sendMessage nsEvent typeSelector

-- | @- modifierFlags@
modifierFlags :: IsNSEvent nsEvent => nsEvent -> IO NSEventModifierFlags
modifierFlags nsEvent =
  sendMessage nsEvent modifierFlagsSelector

-- | @- timestamp@
timestamp :: IsNSEvent nsEvent => nsEvent -> IO CDouble
timestamp nsEvent =
  sendMessage nsEvent timestampSelector

-- | @- window@
window :: IsNSEvent nsEvent => nsEvent -> IO (Id NSWindow)
window nsEvent =
  sendMessage nsEvent windowSelector

-- | @- windowNumber@
windowNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
windowNumber nsEvent =
  sendMessage nsEvent windowNumberSelector

-- | @- context@
context :: IsNSEvent nsEvent => nsEvent -> IO (Id NSGraphicsContext)
context nsEvent =
  sendMessage nsEvent contextSelector

-- | @- clickCount@
clickCount :: IsNSEvent nsEvent => nsEvent -> IO CLong
clickCount nsEvent =
  sendMessage nsEvent clickCountSelector

-- | @- buttonNumber@
buttonNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
buttonNumber nsEvent =
  sendMessage nsEvent buttonNumberSelector

-- | @- eventNumber@
eventNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
eventNumber nsEvent =
  sendMessage nsEvent eventNumberSelector

-- | @- pressure@
pressure :: IsNSEvent nsEvent => nsEvent -> IO CFloat
pressure nsEvent =
  sendMessage nsEvent pressureSelector

-- | @- locationInWindow@
locationInWindow :: IsNSEvent nsEvent => nsEvent -> IO NSPoint
locationInWindow nsEvent =
  sendMessage nsEvent locationInWindowSelector

-- | @- deltaX@
deltaX :: IsNSEvent nsEvent => nsEvent -> IO CDouble
deltaX nsEvent =
  sendMessage nsEvent deltaXSelector

-- | @- deltaY@
deltaY :: IsNSEvent nsEvent => nsEvent -> IO CDouble
deltaY nsEvent =
  sendMessage nsEvent deltaYSelector

-- | @- deltaZ@
deltaZ :: IsNSEvent nsEvent => nsEvent -> IO CDouble
deltaZ nsEvent =
  sendMessage nsEvent deltaZSelector

-- | @- hasPreciseScrollingDeltas@
hasPreciseScrollingDeltas :: IsNSEvent nsEvent => nsEvent -> IO Bool
hasPreciseScrollingDeltas nsEvent =
  sendMessage nsEvent hasPreciseScrollingDeltasSelector

-- | @- scrollingDeltaX@
scrollingDeltaX :: IsNSEvent nsEvent => nsEvent -> IO CDouble
scrollingDeltaX nsEvent =
  sendMessage nsEvent scrollingDeltaXSelector

-- | @- scrollingDeltaY@
scrollingDeltaY :: IsNSEvent nsEvent => nsEvent -> IO CDouble
scrollingDeltaY nsEvent =
  sendMessage nsEvent scrollingDeltaYSelector

-- | @- momentumPhase@
momentumPhase :: IsNSEvent nsEvent => nsEvent -> IO NSEventPhase
momentumPhase nsEvent =
  sendMessage nsEvent momentumPhaseSelector

-- | @- directionInvertedFromDevice@
directionInvertedFromDevice :: IsNSEvent nsEvent => nsEvent -> IO Bool
directionInvertedFromDevice nsEvent =
  sendMessage nsEvent directionInvertedFromDeviceSelector

-- | @- characters@
characters :: IsNSEvent nsEvent => nsEvent -> IO (Id NSString)
characters nsEvent =
  sendMessage nsEvent charactersSelector

-- | @- charactersIgnoringModifiers@
charactersIgnoringModifiers :: IsNSEvent nsEvent => nsEvent -> IO (Id NSString)
charactersIgnoringModifiers nsEvent =
  sendMessage nsEvent charactersIgnoringModifiersSelector

-- | @- ARepeat@
aRepeat :: IsNSEvent nsEvent => nsEvent -> IO Bool
aRepeat nsEvent =
  sendMessage nsEvent aRepeatSelector

-- | @- keyCode@
keyCode :: IsNSEvent nsEvent => nsEvent -> IO CUShort
keyCode nsEvent =
  sendMessage nsEvent keyCodeSelector

-- | @- trackingNumber@
trackingNumber :: IsNSEvent nsEvent => nsEvent -> IO CLong
trackingNumber nsEvent =
  sendMessage nsEvent trackingNumberSelector

-- | @- userData@
userData :: IsNSEvent nsEvent => nsEvent -> IO (Ptr ())
userData nsEvent =
  sendMessage nsEvent userDataSelector

-- | @- trackingArea@
trackingArea :: IsNSEvent nsEvent => nsEvent -> IO (Id NSTrackingArea)
trackingArea nsEvent =
  sendMessage nsEvent trackingAreaSelector

-- | @- subtype@
subtype :: IsNSEvent nsEvent => nsEvent -> IO NSEventSubtype
subtype nsEvent =
  sendMessage nsEvent subtypeSelector

-- | @- data1@
data1 :: IsNSEvent nsEvent => nsEvent -> IO CLong
data1 nsEvent =
  sendMessage nsEvent data1Selector

-- | @- data2@
data2 :: IsNSEvent nsEvent => nsEvent -> IO CLong
data2 nsEvent =
  sendMessage nsEvent data2Selector

-- | @- eventRef@
eventRef :: IsNSEvent nsEvent => nsEvent -> IO RawId
eventRef nsEvent =
  sendMessage nsEvent eventRefSelector

-- | @- CGEvent@
cgEvent :: IsNSEvent nsEvent => nsEvent -> IO (Ptr ())
cgEvent nsEvent =
  sendMessage nsEvent cgEventSelector

-- | @+ mouseCoalescingEnabled@
mouseCoalescingEnabled :: IO Bool
mouseCoalescingEnabled  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' mouseCoalescingEnabledSelector

-- | @+ setMouseCoalescingEnabled:@
setMouseCoalescingEnabled :: Bool -> IO ()
setMouseCoalescingEnabled value =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' setMouseCoalescingEnabledSelector value

-- | @- magnification@
magnification :: IsNSEvent nsEvent => nsEvent -> IO CDouble
magnification nsEvent =
  sendMessage nsEvent magnificationSelector

-- | @- deviceID@
deviceID :: IsNSEvent nsEvent => nsEvent -> IO CULong
deviceID nsEvent =
  sendMessage nsEvent deviceIDSelector

-- | @- rotation@
rotation :: IsNSEvent nsEvent => nsEvent -> IO CFloat
rotation nsEvent =
  sendMessage nsEvent rotationSelector

-- | @- absoluteX@
absoluteX :: IsNSEvent nsEvent => nsEvent -> IO CLong
absoluteX nsEvent =
  sendMessage nsEvent absoluteXSelector

-- | @- absoluteY@
absoluteY :: IsNSEvent nsEvent => nsEvent -> IO CLong
absoluteY nsEvent =
  sendMessage nsEvent absoluteYSelector

-- | @- absoluteZ@
absoluteZ :: IsNSEvent nsEvent => nsEvent -> IO CLong
absoluteZ nsEvent =
  sendMessage nsEvent absoluteZSelector

-- | @- buttonMask@
buttonMask :: IsNSEvent nsEvent => nsEvent -> IO NSEventButtonMask
buttonMask nsEvent =
  sendMessage nsEvent buttonMaskSelector

-- | @- tilt@
tilt :: IsNSEvent nsEvent => nsEvent -> IO NSPoint
tilt nsEvent =
  sendMessage nsEvent tiltSelector

-- | @- tangentialPressure@
tangentialPressure :: IsNSEvent nsEvent => nsEvent -> IO CFloat
tangentialPressure nsEvent =
  sendMessage nsEvent tangentialPressureSelector

-- | @- vendorDefined@
vendorDefined :: IsNSEvent nsEvent => nsEvent -> IO RawId
vendorDefined nsEvent =
  sendMessage nsEvent vendorDefinedSelector

-- | @- vendorID@
vendorID :: IsNSEvent nsEvent => nsEvent -> IO CULong
vendorID nsEvent =
  sendMessage nsEvent vendorIDSelector

-- | @- tabletID@
tabletID :: IsNSEvent nsEvent => nsEvent -> IO CULong
tabletID nsEvent =
  sendMessage nsEvent tabletIDSelector

-- | @- pointingDeviceID@
pointingDeviceID :: IsNSEvent nsEvent => nsEvent -> IO CULong
pointingDeviceID nsEvent =
  sendMessage nsEvent pointingDeviceIDSelector

-- | @- systemTabletID@
systemTabletID :: IsNSEvent nsEvent => nsEvent -> IO CULong
systemTabletID nsEvent =
  sendMessage nsEvent systemTabletIDSelector

-- | @- vendorPointingDeviceType@
vendorPointingDeviceType :: IsNSEvent nsEvent => nsEvent -> IO CULong
vendorPointingDeviceType nsEvent =
  sendMessage nsEvent vendorPointingDeviceTypeSelector

-- | @- pointingDeviceSerialNumber@
pointingDeviceSerialNumber :: IsNSEvent nsEvent => nsEvent -> IO CULong
pointingDeviceSerialNumber nsEvent =
  sendMessage nsEvent pointingDeviceSerialNumberSelector

-- | @- uniqueID@
uniqueID :: IsNSEvent nsEvent => nsEvent -> IO CULong
uniqueID nsEvent =
  sendMessage nsEvent uniqueIDSelector

-- | @- capabilityMask@
capabilityMask :: IsNSEvent nsEvent => nsEvent -> IO CULong
capabilityMask nsEvent =
  sendMessage nsEvent capabilityMaskSelector

-- | @- pointingDeviceType@
pointingDeviceType :: IsNSEvent nsEvent => nsEvent -> IO NSPointingDeviceType
pointingDeviceType nsEvent =
  sendMessage nsEvent pointingDeviceTypeSelector

-- | @- enteringProximity@
enteringProximity :: IsNSEvent nsEvent => nsEvent -> IO Bool
enteringProximity nsEvent =
  sendMessage nsEvent enteringProximitySelector

-- | @- phase@
phase :: IsNSEvent nsEvent => nsEvent -> IO NSEventPhase
phase nsEvent =
  sendMessage nsEvent phaseSelector

-- | @- stage@
stage :: IsNSEvent nsEvent => nsEvent -> IO CLong
stage nsEvent =
  sendMessage nsEvent stageSelector

-- | @- stageTransition@
stageTransition :: IsNSEvent nsEvent => nsEvent -> IO CDouble
stageTransition nsEvent =
  sendMessage nsEvent stageTransitionSelector

-- | @- associatedEventsMask@
associatedEventsMask :: IsNSEvent nsEvent => nsEvent -> IO NSEventMask
associatedEventsMask nsEvent =
  sendMessage nsEvent associatedEventsMaskSelector

-- | @- pressureBehavior@
pressureBehavior :: IsNSEvent nsEvent => nsEvent -> IO NSPressureBehavior
pressureBehavior nsEvent =
  sendMessage nsEvent pressureBehaviorSelector

-- | @+ swipeTrackingFromScrollEventsEnabled@
swipeTrackingFromScrollEventsEnabled :: IO Bool
swipeTrackingFromScrollEventsEnabled  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' swipeTrackingFromScrollEventsEnabledSelector

-- | @+ mouseLocation@
mouseLocation :: IO NSPoint
mouseLocation  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' mouseLocationSelector

-- | @+ modifierFlags@
nsEventModifierFlags :: IO NSEventModifierFlags
nsEventModifierFlags  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' nsEventModifierFlagsSelector

-- | @+ pressedMouseButtons@
pressedMouseButtons :: IO CULong
pressedMouseButtons  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' pressedMouseButtonsSelector

-- | @+ doubleClickInterval@
doubleClickInterval :: IO CDouble
doubleClickInterval  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' doubleClickIntervalSelector

-- | @+ keyRepeatDelay@
keyRepeatDelay :: IO CDouble
keyRepeatDelay  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' keyRepeatDelaySelector

-- | @+ keyRepeatInterval@
keyRepeatInterval :: IO CDouble
keyRepeatInterval  =
  do
    cls' <- getRequiredClass "NSEvent"
    sendClassMessage cls' keyRepeatIntervalSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @charactersByApplyingModifiers:@
charactersByApplyingModifiersSelector :: Selector '[NSEventModifierFlags] (Id NSString)
charactersByApplyingModifiersSelector = mkSelector "charactersByApplyingModifiers:"

-- | @Selector@ for @eventWithEventRef:@
eventWithEventRefSelector :: Selector '[Const (Ptr ())] (Id NSEvent)
eventWithEventRefSelector = mkSelector "eventWithEventRef:"

-- | @Selector@ for @eventWithCGEvent:@
eventWithCGEventSelector :: Selector '[Ptr ()] (Id NSEvent)
eventWithCGEventSelector = mkSelector "eventWithCGEvent:"

-- | @Selector@ for @touchesMatchingPhase:inView:@
touchesMatchingPhase_inViewSelector :: Selector '[NSTouchPhase, Id NSView] (Id NSSet)
touchesMatchingPhase_inViewSelector = mkSelector "touchesMatchingPhase:inView:"

-- | @Selector@ for @allTouches@
allTouchesSelector :: Selector '[] (Id NSSet)
allTouchesSelector = mkSelector "allTouches"

-- | @Selector@ for @touchesForView:@
touchesForViewSelector :: Selector '[Id NSView] (Id NSSet)
touchesForViewSelector = mkSelector "touchesForView:"

-- | @Selector@ for @coalescedTouchesForTouch:@
coalescedTouchesForTouchSelector :: Selector '[Id NSTouch] (Id NSArray)
coalescedTouchesForTouchSelector = mkSelector "coalescedTouchesForTouch:"

-- | @Selector@ for @trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:@
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector :: Selector '[NSEventSwipeTrackingOptions, CDouble, CDouble, Ptr ()] ()
trackSwipeEventWithOptions_dampenAmountThresholdMin_max_usingHandlerSelector = mkSelector "trackSwipeEventWithOptions:dampenAmountThresholdMin:max:usingHandler:"

-- | @Selector@ for @startPeriodicEventsAfterDelay:withPeriod:@
startPeriodicEventsAfterDelay_withPeriodSelector :: Selector '[CDouble, CDouble] ()
startPeriodicEventsAfterDelay_withPeriodSelector = mkSelector "startPeriodicEventsAfterDelay:withPeriod:"

-- | @Selector@ for @stopPeriodicEvents@
stopPeriodicEventsSelector :: Selector '[] ()
stopPeriodicEventsSelector = mkSelector "stopPeriodicEvents"

-- | @Selector@ for @mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:@
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector :: Selector '[NSEventType, NSPoint, NSEventModifierFlags, CDouble, CLong, Id NSGraphicsContext, CLong, CLong, CFloat] (Id NSEvent)
mouseEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_clickCount_pressureSelector = mkSelector "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"

-- | @Selector@ for @keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:@
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector :: Selector '[NSEventType, NSPoint, NSEventModifierFlags, CDouble, CLong, Id NSGraphicsContext, Id NSString, Id NSString, Bool, CUShort] (Id NSEvent)
keyEventWithType_location_modifierFlags_timestamp_windowNumber_context_characters_charactersIgnoringModifiers_isARepeat_keyCodeSelector = mkSelector "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"

-- | @Selector@ for @enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:@
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector :: Selector '[NSEventType, NSPoint, NSEventModifierFlags, CDouble, CLong, Id NSGraphicsContext, CLong, CLong, Ptr ()] (Id NSEvent)
enterExitEventWithType_location_modifierFlags_timestamp_windowNumber_context_eventNumber_trackingNumber_userDataSelector = mkSelector "enterExitEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:trackingNumber:userData:"

-- | @Selector@ for @otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:@
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector :: Selector '[NSEventType, NSPoint, NSEventModifierFlags, CDouble, CLong, Id NSGraphicsContext, CShort, CLong, CLong] (Id NSEvent)
otherEventWithType_location_modifierFlags_timestamp_windowNumber_context_subtype_data1_data2Selector = mkSelector "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"

-- | @Selector@ for @addGlobalMonitorForEventsMatchingMask:handler:@
addGlobalMonitorForEventsMatchingMask_handlerSelector :: Selector '[NSEventMask, Ptr ()] RawId
addGlobalMonitorForEventsMatchingMask_handlerSelector = mkSelector "addGlobalMonitorForEventsMatchingMask:handler:"

-- | @Selector@ for @addLocalMonitorForEventsMatchingMask:handler:@
addLocalMonitorForEventsMatchingMask_handlerSelector :: Selector '[NSEventMask, Ptr ()] RawId
addLocalMonitorForEventsMatchingMask_handlerSelector = mkSelector "addLocalMonitorForEventsMatchingMask:handler:"

-- | @Selector@ for @removeMonitor:@
removeMonitorSelector :: Selector '[RawId] ()
removeMonitorSelector = mkSelector "removeMonitor:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] NSEventType
typeSelector = mkSelector "type"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector '[] NSEventModifierFlags
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @timestamp@
timestampSelector :: Selector '[] CDouble
timestampSelector = mkSelector "timestamp"

-- | @Selector@ for @window@
windowSelector :: Selector '[] (Id NSWindow)
windowSelector = mkSelector "window"

-- | @Selector@ for @windowNumber@
windowNumberSelector :: Selector '[] CLong
windowNumberSelector = mkSelector "windowNumber"

-- | @Selector@ for @context@
contextSelector :: Selector '[] (Id NSGraphicsContext)
contextSelector = mkSelector "context"

-- | @Selector@ for @clickCount@
clickCountSelector :: Selector '[] CLong
clickCountSelector = mkSelector "clickCount"

-- | @Selector@ for @buttonNumber@
buttonNumberSelector :: Selector '[] CLong
buttonNumberSelector = mkSelector "buttonNumber"

-- | @Selector@ for @eventNumber@
eventNumberSelector :: Selector '[] CLong
eventNumberSelector = mkSelector "eventNumber"

-- | @Selector@ for @pressure@
pressureSelector :: Selector '[] CFloat
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @locationInWindow@
locationInWindowSelector :: Selector '[] NSPoint
locationInWindowSelector = mkSelector "locationInWindow"

-- | @Selector@ for @deltaX@
deltaXSelector :: Selector '[] CDouble
deltaXSelector = mkSelector "deltaX"

-- | @Selector@ for @deltaY@
deltaYSelector :: Selector '[] CDouble
deltaYSelector = mkSelector "deltaY"

-- | @Selector@ for @deltaZ@
deltaZSelector :: Selector '[] CDouble
deltaZSelector = mkSelector "deltaZ"

-- | @Selector@ for @hasPreciseScrollingDeltas@
hasPreciseScrollingDeltasSelector :: Selector '[] Bool
hasPreciseScrollingDeltasSelector = mkSelector "hasPreciseScrollingDeltas"

-- | @Selector@ for @scrollingDeltaX@
scrollingDeltaXSelector :: Selector '[] CDouble
scrollingDeltaXSelector = mkSelector "scrollingDeltaX"

-- | @Selector@ for @scrollingDeltaY@
scrollingDeltaYSelector :: Selector '[] CDouble
scrollingDeltaYSelector = mkSelector "scrollingDeltaY"

-- | @Selector@ for @momentumPhase@
momentumPhaseSelector :: Selector '[] NSEventPhase
momentumPhaseSelector = mkSelector "momentumPhase"

-- | @Selector@ for @directionInvertedFromDevice@
directionInvertedFromDeviceSelector :: Selector '[] Bool
directionInvertedFromDeviceSelector = mkSelector "directionInvertedFromDevice"

-- | @Selector@ for @characters@
charactersSelector :: Selector '[] (Id NSString)
charactersSelector = mkSelector "characters"

-- | @Selector@ for @charactersIgnoringModifiers@
charactersIgnoringModifiersSelector :: Selector '[] (Id NSString)
charactersIgnoringModifiersSelector = mkSelector "charactersIgnoringModifiers"

-- | @Selector@ for @ARepeat@
aRepeatSelector :: Selector '[] Bool
aRepeatSelector = mkSelector "ARepeat"

-- | @Selector@ for @keyCode@
keyCodeSelector :: Selector '[] CUShort
keyCodeSelector = mkSelector "keyCode"

-- | @Selector@ for @trackingNumber@
trackingNumberSelector :: Selector '[] CLong
trackingNumberSelector = mkSelector "trackingNumber"

-- | @Selector@ for @userData@
userDataSelector :: Selector '[] (Ptr ())
userDataSelector = mkSelector "userData"

-- | @Selector@ for @trackingArea@
trackingAreaSelector :: Selector '[] (Id NSTrackingArea)
trackingAreaSelector = mkSelector "trackingArea"

-- | @Selector@ for @subtype@
subtypeSelector :: Selector '[] NSEventSubtype
subtypeSelector = mkSelector "subtype"

-- | @Selector@ for @data1@
data1Selector :: Selector '[] CLong
data1Selector = mkSelector "data1"

-- | @Selector@ for @data2@
data2Selector :: Selector '[] CLong
data2Selector = mkSelector "data2"

-- | @Selector@ for @eventRef@
eventRefSelector :: Selector '[] RawId
eventRefSelector = mkSelector "eventRef"

-- | @Selector@ for @CGEvent@
cgEventSelector :: Selector '[] (Ptr ())
cgEventSelector = mkSelector "CGEvent"

-- | @Selector@ for @mouseCoalescingEnabled@
mouseCoalescingEnabledSelector :: Selector '[] Bool
mouseCoalescingEnabledSelector = mkSelector "mouseCoalescingEnabled"

-- | @Selector@ for @setMouseCoalescingEnabled:@
setMouseCoalescingEnabledSelector :: Selector '[Bool] ()
setMouseCoalescingEnabledSelector = mkSelector "setMouseCoalescingEnabled:"

-- | @Selector@ for @magnification@
magnificationSelector :: Selector '[] CDouble
magnificationSelector = mkSelector "magnification"

-- | @Selector@ for @deviceID@
deviceIDSelector :: Selector '[] CULong
deviceIDSelector = mkSelector "deviceID"

-- | @Selector@ for @rotation@
rotationSelector :: Selector '[] CFloat
rotationSelector = mkSelector "rotation"

-- | @Selector@ for @absoluteX@
absoluteXSelector :: Selector '[] CLong
absoluteXSelector = mkSelector "absoluteX"

-- | @Selector@ for @absoluteY@
absoluteYSelector :: Selector '[] CLong
absoluteYSelector = mkSelector "absoluteY"

-- | @Selector@ for @absoluteZ@
absoluteZSelector :: Selector '[] CLong
absoluteZSelector = mkSelector "absoluteZ"

-- | @Selector@ for @buttonMask@
buttonMaskSelector :: Selector '[] NSEventButtonMask
buttonMaskSelector = mkSelector "buttonMask"

-- | @Selector@ for @tilt@
tiltSelector :: Selector '[] NSPoint
tiltSelector = mkSelector "tilt"

-- | @Selector@ for @tangentialPressure@
tangentialPressureSelector :: Selector '[] CFloat
tangentialPressureSelector = mkSelector "tangentialPressure"

-- | @Selector@ for @vendorDefined@
vendorDefinedSelector :: Selector '[] RawId
vendorDefinedSelector = mkSelector "vendorDefined"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector '[] CULong
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @tabletID@
tabletIDSelector :: Selector '[] CULong
tabletIDSelector = mkSelector "tabletID"

-- | @Selector@ for @pointingDeviceID@
pointingDeviceIDSelector :: Selector '[] CULong
pointingDeviceIDSelector = mkSelector "pointingDeviceID"

-- | @Selector@ for @systemTabletID@
systemTabletIDSelector :: Selector '[] CULong
systemTabletIDSelector = mkSelector "systemTabletID"

-- | @Selector@ for @vendorPointingDeviceType@
vendorPointingDeviceTypeSelector :: Selector '[] CULong
vendorPointingDeviceTypeSelector = mkSelector "vendorPointingDeviceType"

-- | @Selector@ for @pointingDeviceSerialNumber@
pointingDeviceSerialNumberSelector :: Selector '[] CULong
pointingDeviceSerialNumberSelector = mkSelector "pointingDeviceSerialNumber"

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] CULong
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @capabilityMask@
capabilityMaskSelector :: Selector '[] CULong
capabilityMaskSelector = mkSelector "capabilityMask"

-- | @Selector@ for @pointingDeviceType@
pointingDeviceTypeSelector :: Selector '[] NSPointingDeviceType
pointingDeviceTypeSelector = mkSelector "pointingDeviceType"

-- | @Selector@ for @enteringProximity@
enteringProximitySelector :: Selector '[] Bool
enteringProximitySelector = mkSelector "enteringProximity"

-- | @Selector@ for @phase@
phaseSelector :: Selector '[] NSEventPhase
phaseSelector = mkSelector "phase"

-- | @Selector@ for @stage@
stageSelector :: Selector '[] CLong
stageSelector = mkSelector "stage"

-- | @Selector@ for @stageTransition@
stageTransitionSelector :: Selector '[] CDouble
stageTransitionSelector = mkSelector "stageTransition"

-- | @Selector@ for @associatedEventsMask@
associatedEventsMaskSelector :: Selector '[] NSEventMask
associatedEventsMaskSelector = mkSelector "associatedEventsMask"

-- | @Selector@ for @pressureBehavior@
pressureBehaviorSelector :: Selector '[] NSPressureBehavior
pressureBehaviorSelector = mkSelector "pressureBehavior"

-- | @Selector@ for @swipeTrackingFromScrollEventsEnabled@
swipeTrackingFromScrollEventsEnabledSelector :: Selector '[] Bool
swipeTrackingFromScrollEventsEnabledSelector = mkSelector "swipeTrackingFromScrollEventsEnabled"

-- | @Selector@ for @mouseLocation@
mouseLocationSelector :: Selector '[] NSPoint
mouseLocationSelector = mkSelector "mouseLocation"

-- | @Selector@ for @modifierFlags@
nsEventModifierFlagsSelector :: Selector '[] NSEventModifierFlags
nsEventModifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @pressedMouseButtons@
pressedMouseButtonsSelector :: Selector '[] CULong
pressedMouseButtonsSelector = mkSelector "pressedMouseButtons"

-- | @Selector@ for @doubleClickInterval@
doubleClickIntervalSelector :: Selector '[] CDouble
doubleClickIntervalSelector = mkSelector "doubleClickInterval"

-- | @Selector@ for @keyRepeatDelay@
keyRepeatDelaySelector :: Selector '[] CDouble
keyRepeatDelaySelector = mkSelector "keyRepeatDelay"

-- | @Selector@ for @keyRepeatInterval@
keyRepeatIntervalSelector :: Selector '[] CDouble
keyRepeatIntervalSelector = mkSelector "keyRepeatInterval"

