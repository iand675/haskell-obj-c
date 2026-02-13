{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSGestureRecognizer@.
module ObjC.AppKit.NSGestureRecognizer
  ( NSGestureRecognizer
  , IsNSGestureRecognizer(..)
  , initWithTarget_action
  , initWithCoder
  , locationInView
  , reset
  , canPreventGestureRecognizer
  , canBePreventedByGestureRecognizer
  , shouldRequireFailureOfGestureRecognizer
  , shouldBeRequiredToFailByGestureRecognizer
  , mouseDown
  , rightMouseDown
  , otherMouseDown
  , mouseUp
  , rightMouseUp
  , otherMouseUp
  , mouseDragged
  , rightMouseDragged
  , otherMouseDragged
  , mouseCancelled
  , keyDown
  , keyUp
  , flagsChanged
  , tabletPoint
  , magnifyWithEvent
  , rotateWithEvent
  , pressureChangeWithEvent
  , touchesBeganWithEvent
  , touchesMovedWithEvent
  , touchesEndedWithEvent
  , touchesCancelledWithEvent
  , target
  , setTarget
  , action
  , setAction
  , state
  , delegate
  , setDelegate
  , enabled
  , setEnabled
  , view
  , pressureConfiguration
  , setPressureConfiguration
  , delaysPrimaryMouseButtonEvents
  , setDelaysPrimaryMouseButtonEvents
  , delaysSecondaryMouseButtonEvents
  , setDelaysSecondaryMouseButtonEvents
  , delaysOtherMouseButtonEvents
  , setDelaysOtherMouseButtonEvents
  , delaysKeyEvents
  , setDelaysKeyEvents
  , delaysMagnificationEvents
  , setDelaysMagnificationEvents
  , delaysRotationEvents
  , setDelaysRotationEvents
  , name
  , setName
  , modifierFlags
  , setState
  , allowedTouchTypes
  , setAllowedTouchTypes
  , actionSelector
  , allowedTouchTypesSelector
  , canBePreventedByGestureRecognizerSelector
  , canPreventGestureRecognizerSelector
  , delaysKeyEventsSelector
  , delaysMagnificationEventsSelector
  , delaysOtherMouseButtonEventsSelector
  , delaysPrimaryMouseButtonEventsSelector
  , delaysRotationEventsSelector
  , delaysSecondaryMouseButtonEventsSelector
  , delegateSelector
  , enabledSelector
  , flagsChangedSelector
  , initWithCoderSelector
  , initWithTarget_actionSelector
  , keyDownSelector
  , keyUpSelector
  , locationInViewSelector
  , magnifyWithEventSelector
  , modifierFlagsSelector
  , mouseCancelledSelector
  , mouseDownSelector
  , mouseDraggedSelector
  , mouseUpSelector
  , nameSelector
  , otherMouseDownSelector
  , otherMouseDraggedSelector
  , otherMouseUpSelector
  , pressureChangeWithEventSelector
  , pressureConfigurationSelector
  , resetSelector
  , rightMouseDownSelector
  , rightMouseDraggedSelector
  , rightMouseUpSelector
  , rotateWithEventSelector
  , setActionSelector
  , setAllowedTouchTypesSelector
  , setDelaysKeyEventsSelector
  , setDelaysMagnificationEventsSelector
  , setDelaysOtherMouseButtonEventsSelector
  , setDelaysPrimaryMouseButtonEventsSelector
  , setDelaysRotationEventsSelector
  , setDelaysSecondaryMouseButtonEventsSelector
  , setDelegateSelector
  , setEnabledSelector
  , setNameSelector
  , setPressureConfigurationSelector
  , setStateSelector
  , setTargetSelector
  , shouldBeRequiredToFailByGestureRecognizerSelector
  , shouldRequireFailureOfGestureRecognizerSelector
  , stateSelector
  , tabletPointSelector
  , targetSelector
  , touchesBeganWithEventSelector
  , touchesCancelledWithEventSelector
  , touchesEndedWithEventSelector
  , touchesMovedWithEventSelector
  , viewSelector

  -- * Enum types
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
  , NSGestureRecognizerState(NSGestureRecognizerState)
  , pattern NSGestureRecognizerStatePossible
  , pattern NSGestureRecognizerStateBegan
  , pattern NSGestureRecognizerStateChanged
  , pattern NSGestureRecognizerStateEnded
  , pattern NSGestureRecognizerStateCancelled
  , pattern NSGestureRecognizerStateFailed
  , pattern NSGestureRecognizerStateRecognized
  , NSTouchTypeMask(NSTouchTypeMask)
  , pattern NSTouchTypeMaskDirect
  , pattern NSTouchTypeMaskIndirect

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

-- | @- initWithTarget:action:@
initWithTarget_action :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> RawId -> Sel -> IO (Id NSGestureRecognizer)
initWithTarget_action nsGestureRecognizer target action =
  sendOwnedMessage nsGestureRecognizer initWithTarget_actionSelector target action

-- | @- initWithCoder:@
initWithCoder :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSCoder coder) => nsGestureRecognizer -> coder -> IO (Id NSGestureRecognizer)
initWithCoder nsGestureRecognizer coder =
  sendOwnedMessage nsGestureRecognizer initWithCoderSelector (toNSCoder coder)

-- | @- locationInView:@
locationInView :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSView view) => nsGestureRecognizer -> view -> IO NSPoint
locationInView nsGestureRecognizer view =
  sendMessage nsGestureRecognizer locationInViewSelector (toNSView view)

-- | @- reset@
reset :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO ()
reset nsGestureRecognizer =
  sendMessage nsGestureRecognizer resetSelector

-- | @- canPreventGestureRecognizer:@
canPreventGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer preventedGestureRecognizer) => nsGestureRecognizer -> preventedGestureRecognizer -> IO Bool
canPreventGestureRecognizer nsGestureRecognizer preventedGestureRecognizer =
  sendMessage nsGestureRecognizer canPreventGestureRecognizerSelector (toNSGestureRecognizer preventedGestureRecognizer)

-- | @- canBePreventedByGestureRecognizer:@
canBePreventedByGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer preventingGestureRecognizer) => nsGestureRecognizer -> preventingGestureRecognizer -> IO Bool
canBePreventedByGestureRecognizer nsGestureRecognizer preventingGestureRecognizer =
  sendMessage nsGestureRecognizer canBePreventedByGestureRecognizerSelector (toNSGestureRecognizer preventingGestureRecognizer)

-- | @- shouldRequireFailureOfGestureRecognizer:@
shouldRequireFailureOfGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer otherGestureRecognizer) => nsGestureRecognizer -> otherGestureRecognizer -> IO Bool
shouldRequireFailureOfGestureRecognizer nsGestureRecognizer otherGestureRecognizer =
  sendMessage nsGestureRecognizer shouldRequireFailureOfGestureRecognizerSelector (toNSGestureRecognizer otherGestureRecognizer)

-- | @- shouldBeRequiredToFailByGestureRecognizer:@
shouldBeRequiredToFailByGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer otherGestureRecognizer) => nsGestureRecognizer -> otherGestureRecognizer -> IO Bool
shouldBeRequiredToFailByGestureRecognizer nsGestureRecognizer otherGestureRecognizer =
  sendMessage nsGestureRecognizer shouldBeRequiredToFailByGestureRecognizerSelector (toNSGestureRecognizer otherGestureRecognizer)

-- | @- mouseDown:@
mouseDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseDown nsGestureRecognizer event =
  sendMessage nsGestureRecognizer mouseDownSelector (toNSEvent event)

-- | @- rightMouseDown:@
rightMouseDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rightMouseDown nsGestureRecognizer event =
  sendMessage nsGestureRecognizer rightMouseDownSelector (toNSEvent event)

-- | @- otherMouseDown:@
otherMouseDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
otherMouseDown nsGestureRecognizer event =
  sendMessage nsGestureRecognizer otherMouseDownSelector (toNSEvent event)

-- | @- mouseUp:@
mouseUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseUp nsGestureRecognizer event =
  sendMessage nsGestureRecognizer mouseUpSelector (toNSEvent event)

-- | @- rightMouseUp:@
rightMouseUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rightMouseUp nsGestureRecognizer event =
  sendMessage nsGestureRecognizer rightMouseUpSelector (toNSEvent event)

-- | @- otherMouseUp:@
otherMouseUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
otherMouseUp nsGestureRecognizer event =
  sendMessage nsGestureRecognizer otherMouseUpSelector (toNSEvent event)

-- | @- mouseDragged:@
mouseDragged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseDragged nsGestureRecognizer event =
  sendMessage nsGestureRecognizer mouseDraggedSelector (toNSEvent event)

-- | @- rightMouseDragged:@
rightMouseDragged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rightMouseDragged nsGestureRecognizer event =
  sendMessage nsGestureRecognizer rightMouseDraggedSelector (toNSEvent event)

-- | @- otherMouseDragged:@
otherMouseDragged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
otherMouseDragged nsGestureRecognizer event =
  sendMessage nsGestureRecognizer otherMouseDraggedSelector (toNSEvent event)

-- | @- mouseCancelled:@
mouseCancelled :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseCancelled nsGestureRecognizer event =
  sendMessage nsGestureRecognizer mouseCancelledSelector (toNSEvent event)

-- | @- keyDown:@
keyDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
keyDown nsGestureRecognizer event =
  sendMessage nsGestureRecognizer keyDownSelector (toNSEvent event)

-- | @- keyUp:@
keyUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
keyUp nsGestureRecognizer event =
  sendMessage nsGestureRecognizer keyUpSelector (toNSEvent event)

-- | @- flagsChanged:@
flagsChanged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
flagsChanged nsGestureRecognizer event =
  sendMessage nsGestureRecognizer flagsChangedSelector (toNSEvent event)

-- | @- tabletPoint:@
tabletPoint :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
tabletPoint nsGestureRecognizer event =
  sendMessage nsGestureRecognizer tabletPointSelector (toNSEvent event)

-- | @- magnifyWithEvent:@
magnifyWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
magnifyWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer magnifyWithEventSelector (toNSEvent event)

-- | @- rotateWithEvent:@
rotateWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rotateWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer rotateWithEventSelector (toNSEvent event)

-- | @- pressureChangeWithEvent:@
pressureChangeWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
pressureChangeWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer pressureChangeWithEventSelector (toNSEvent event)

-- | @- touchesBeganWithEvent:@
touchesBeganWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesBeganWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer touchesBeganWithEventSelector (toNSEvent event)

-- | @- touchesMovedWithEvent:@
touchesMovedWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesMovedWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer touchesMovedWithEventSelector (toNSEvent event)

-- | @- touchesEndedWithEvent:@
touchesEndedWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesEndedWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer touchesEndedWithEventSelector (toNSEvent event)

-- | @- touchesCancelledWithEvent:@
touchesCancelledWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesCancelledWithEvent nsGestureRecognizer event =
  sendMessage nsGestureRecognizer touchesCancelledWithEventSelector (toNSEvent event)

-- | @- target@
target :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO RawId
target nsGestureRecognizer =
  sendMessage nsGestureRecognizer targetSelector

-- | @- setTarget:@
setTarget :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> RawId -> IO ()
setTarget nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setTargetSelector value

-- | @- action@
action :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Sel
action nsGestureRecognizer =
  sendMessage nsGestureRecognizer actionSelector

-- | @- setAction:@
setAction :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Sel -> IO ()
setAction nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setActionSelector value

-- | @- state@
state :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO NSGestureRecognizerState
state nsGestureRecognizer =
  sendMessage nsGestureRecognizer stateSelector

-- | @- delegate@
delegate :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO RawId
delegate nsGestureRecognizer =
  sendMessage nsGestureRecognizer delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> RawId -> IO ()
setDelegate nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelegateSelector value

-- | @- enabled@
enabled :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
enabled nsGestureRecognizer =
  sendMessage nsGestureRecognizer enabledSelector

-- | @- setEnabled:@
setEnabled :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setEnabled nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setEnabledSelector value

-- | @- view@
view :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO (Id NSView)
view nsGestureRecognizer =
  sendMessage nsGestureRecognizer viewSelector

-- | @- pressureConfiguration@
pressureConfiguration :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO (Id NSPressureConfiguration)
pressureConfiguration nsGestureRecognizer =
  sendMessage nsGestureRecognizer pressureConfigurationSelector

-- | @- setPressureConfiguration:@
setPressureConfiguration :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSPressureConfiguration value) => nsGestureRecognizer -> value -> IO ()
setPressureConfiguration nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setPressureConfigurationSelector (toNSPressureConfiguration value)

-- | @- delaysPrimaryMouseButtonEvents@
delaysPrimaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysPrimaryMouseButtonEvents nsGestureRecognizer =
  sendMessage nsGestureRecognizer delaysPrimaryMouseButtonEventsSelector

-- | @- setDelaysPrimaryMouseButtonEvents:@
setDelaysPrimaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysPrimaryMouseButtonEvents nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelaysPrimaryMouseButtonEventsSelector value

-- | @- delaysSecondaryMouseButtonEvents@
delaysSecondaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysSecondaryMouseButtonEvents nsGestureRecognizer =
  sendMessage nsGestureRecognizer delaysSecondaryMouseButtonEventsSelector

-- | @- setDelaysSecondaryMouseButtonEvents:@
setDelaysSecondaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysSecondaryMouseButtonEvents nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelaysSecondaryMouseButtonEventsSelector value

-- | @- delaysOtherMouseButtonEvents@
delaysOtherMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysOtherMouseButtonEvents nsGestureRecognizer =
  sendMessage nsGestureRecognizer delaysOtherMouseButtonEventsSelector

-- | @- setDelaysOtherMouseButtonEvents:@
setDelaysOtherMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysOtherMouseButtonEvents nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelaysOtherMouseButtonEventsSelector value

-- | @- delaysKeyEvents@
delaysKeyEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysKeyEvents nsGestureRecognizer =
  sendMessage nsGestureRecognizer delaysKeyEventsSelector

-- | @- setDelaysKeyEvents:@
setDelaysKeyEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysKeyEvents nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelaysKeyEventsSelector value

-- | @- delaysMagnificationEvents@
delaysMagnificationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysMagnificationEvents nsGestureRecognizer =
  sendMessage nsGestureRecognizer delaysMagnificationEventsSelector

-- | @- setDelaysMagnificationEvents:@
setDelaysMagnificationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysMagnificationEvents nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelaysMagnificationEventsSelector value

-- | @- delaysRotationEvents@
delaysRotationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysRotationEvents nsGestureRecognizer =
  sendMessage nsGestureRecognizer delaysRotationEventsSelector

-- | @- setDelaysRotationEvents:@
setDelaysRotationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysRotationEvents nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setDelaysRotationEventsSelector value

-- | @- name@
name :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO (Id NSString)
name nsGestureRecognizer =
  sendMessage nsGestureRecognizer nameSelector

-- | @- setName:@
setName :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSString value) => nsGestureRecognizer -> value -> IO ()
setName nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setNameSelector (toNSString value)

-- | @- modifierFlags@
modifierFlags :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO NSEventModifierFlags
modifierFlags nsGestureRecognizer =
  sendMessage nsGestureRecognizer modifierFlagsSelector

-- | @- setState:@
setState :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> NSGestureRecognizerState -> IO ()
setState nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setStateSelector value

-- | @- allowedTouchTypes@
allowedTouchTypes :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO NSTouchTypeMask
allowedTouchTypes nsGestureRecognizer =
  sendMessage nsGestureRecognizer allowedTouchTypesSelector

-- | @- setAllowedTouchTypes:@
setAllowedTouchTypes :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> NSTouchTypeMask -> IO ()
setAllowedTouchTypes nsGestureRecognizer value =
  sendMessage nsGestureRecognizer setAllowedTouchTypesSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTarget:action:@
initWithTarget_actionSelector :: Selector '[RawId, Sel] (Id NSGestureRecognizer)
initWithTarget_actionSelector = mkSelector "initWithTarget:action:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSGestureRecognizer)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @locationInView:@
locationInViewSelector :: Selector '[Id NSView] NSPoint
locationInViewSelector = mkSelector "locationInView:"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @canPreventGestureRecognizer:@
canPreventGestureRecognizerSelector :: Selector '[Id NSGestureRecognizer] Bool
canPreventGestureRecognizerSelector = mkSelector "canPreventGestureRecognizer:"

-- | @Selector@ for @canBePreventedByGestureRecognizer:@
canBePreventedByGestureRecognizerSelector :: Selector '[Id NSGestureRecognizer] Bool
canBePreventedByGestureRecognizerSelector = mkSelector "canBePreventedByGestureRecognizer:"

-- | @Selector@ for @shouldRequireFailureOfGestureRecognizer:@
shouldRequireFailureOfGestureRecognizerSelector :: Selector '[Id NSGestureRecognizer] Bool
shouldRequireFailureOfGestureRecognizerSelector = mkSelector "shouldRequireFailureOfGestureRecognizer:"

-- | @Selector@ for @shouldBeRequiredToFailByGestureRecognizer:@
shouldBeRequiredToFailByGestureRecognizerSelector :: Selector '[Id NSGestureRecognizer] Bool
shouldBeRequiredToFailByGestureRecognizerSelector = mkSelector "shouldBeRequiredToFailByGestureRecognizer:"

-- | @Selector@ for @mouseDown:@
mouseDownSelector :: Selector '[Id NSEvent] ()
mouseDownSelector = mkSelector "mouseDown:"

-- | @Selector@ for @rightMouseDown:@
rightMouseDownSelector :: Selector '[Id NSEvent] ()
rightMouseDownSelector = mkSelector "rightMouseDown:"

-- | @Selector@ for @otherMouseDown:@
otherMouseDownSelector :: Selector '[Id NSEvent] ()
otherMouseDownSelector = mkSelector "otherMouseDown:"

-- | @Selector@ for @mouseUp:@
mouseUpSelector :: Selector '[Id NSEvent] ()
mouseUpSelector = mkSelector "mouseUp:"

-- | @Selector@ for @rightMouseUp:@
rightMouseUpSelector :: Selector '[Id NSEvent] ()
rightMouseUpSelector = mkSelector "rightMouseUp:"

-- | @Selector@ for @otherMouseUp:@
otherMouseUpSelector :: Selector '[Id NSEvent] ()
otherMouseUpSelector = mkSelector "otherMouseUp:"

-- | @Selector@ for @mouseDragged:@
mouseDraggedSelector :: Selector '[Id NSEvent] ()
mouseDraggedSelector = mkSelector "mouseDragged:"

-- | @Selector@ for @rightMouseDragged:@
rightMouseDraggedSelector :: Selector '[Id NSEvent] ()
rightMouseDraggedSelector = mkSelector "rightMouseDragged:"

-- | @Selector@ for @otherMouseDragged:@
otherMouseDraggedSelector :: Selector '[Id NSEvent] ()
otherMouseDraggedSelector = mkSelector "otherMouseDragged:"

-- | @Selector@ for @mouseCancelled:@
mouseCancelledSelector :: Selector '[Id NSEvent] ()
mouseCancelledSelector = mkSelector "mouseCancelled:"

-- | @Selector@ for @keyDown:@
keyDownSelector :: Selector '[Id NSEvent] ()
keyDownSelector = mkSelector "keyDown:"

-- | @Selector@ for @keyUp:@
keyUpSelector :: Selector '[Id NSEvent] ()
keyUpSelector = mkSelector "keyUp:"

-- | @Selector@ for @flagsChanged:@
flagsChangedSelector :: Selector '[Id NSEvent] ()
flagsChangedSelector = mkSelector "flagsChanged:"

-- | @Selector@ for @tabletPoint:@
tabletPointSelector :: Selector '[Id NSEvent] ()
tabletPointSelector = mkSelector "tabletPoint:"

-- | @Selector@ for @magnifyWithEvent:@
magnifyWithEventSelector :: Selector '[Id NSEvent] ()
magnifyWithEventSelector = mkSelector "magnifyWithEvent:"

-- | @Selector@ for @rotateWithEvent:@
rotateWithEventSelector :: Selector '[Id NSEvent] ()
rotateWithEventSelector = mkSelector "rotateWithEvent:"

-- | @Selector@ for @pressureChangeWithEvent:@
pressureChangeWithEventSelector :: Selector '[Id NSEvent] ()
pressureChangeWithEventSelector = mkSelector "pressureChangeWithEvent:"

-- | @Selector@ for @touchesBeganWithEvent:@
touchesBeganWithEventSelector :: Selector '[Id NSEvent] ()
touchesBeganWithEventSelector = mkSelector "touchesBeganWithEvent:"

-- | @Selector@ for @touchesMovedWithEvent:@
touchesMovedWithEventSelector :: Selector '[Id NSEvent] ()
touchesMovedWithEventSelector = mkSelector "touchesMovedWithEvent:"

-- | @Selector@ for @touchesEndedWithEvent:@
touchesEndedWithEventSelector :: Selector '[Id NSEvent] ()
touchesEndedWithEventSelector = mkSelector "touchesEndedWithEvent:"

-- | @Selector@ for @touchesCancelledWithEvent:@
touchesCancelledWithEventSelector :: Selector '[Id NSEvent] ()
touchesCancelledWithEventSelector = mkSelector "touchesCancelledWithEvent:"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector '[RawId] ()
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] Sel
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector '[Sel] ()
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] NSGestureRecognizerState
stateSelector = mkSelector "state"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @view@
viewSelector :: Selector '[] (Id NSView)
viewSelector = mkSelector "view"

-- | @Selector@ for @pressureConfiguration@
pressureConfigurationSelector :: Selector '[] (Id NSPressureConfiguration)
pressureConfigurationSelector = mkSelector "pressureConfiguration"

-- | @Selector@ for @setPressureConfiguration:@
setPressureConfigurationSelector :: Selector '[Id NSPressureConfiguration] ()
setPressureConfigurationSelector = mkSelector "setPressureConfiguration:"

-- | @Selector@ for @delaysPrimaryMouseButtonEvents@
delaysPrimaryMouseButtonEventsSelector :: Selector '[] Bool
delaysPrimaryMouseButtonEventsSelector = mkSelector "delaysPrimaryMouseButtonEvents"

-- | @Selector@ for @setDelaysPrimaryMouseButtonEvents:@
setDelaysPrimaryMouseButtonEventsSelector :: Selector '[Bool] ()
setDelaysPrimaryMouseButtonEventsSelector = mkSelector "setDelaysPrimaryMouseButtonEvents:"

-- | @Selector@ for @delaysSecondaryMouseButtonEvents@
delaysSecondaryMouseButtonEventsSelector :: Selector '[] Bool
delaysSecondaryMouseButtonEventsSelector = mkSelector "delaysSecondaryMouseButtonEvents"

-- | @Selector@ for @setDelaysSecondaryMouseButtonEvents:@
setDelaysSecondaryMouseButtonEventsSelector :: Selector '[Bool] ()
setDelaysSecondaryMouseButtonEventsSelector = mkSelector "setDelaysSecondaryMouseButtonEvents:"

-- | @Selector@ for @delaysOtherMouseButtonEvents@
delaysOtherMouseButtonEventsSelector :: Selector '[] Bool
delaysOtherMouseButtonEventsSelector = mkSelector "delaysOtherMouseButtonEvents"

-- | @Selector@ for @setDelaysOtherMouseButtonEvents:@
setDelaysOtherMouseButtonEventsSelector :: Selector '[Bool] ()
setDelaysOtherMouseButtonEventsSelector = mkSelector "setDelaysOtherMouseButtonEvents:"

-- | @Selector@ for @delaysKeyEvents@
delaysKeyEventsSelector :: Selector '[] Bool
delaysKeyEventsSelector = mkSelector "delaysKeyEvents"

-- | @Selector@ for @setDelaysKeyEvents:@
setDelaysKeyEventsSelector :: Selector '[Bool] ()
setDelaysKeyEventsSelector = mkSelector "setDelaysKeyEvents:"

-- | @Selector@ for @delaysMagnificationEvents@
delaysMagnificationEventsSelector :: Selector '[] Bool
delaysMagnificationEventsSelector = mkSelector "delaysMagnificationEvents"

-- | @Selector@ for @setDelaysMagnificationEvents:@
setDelaysMagnificationEventsSelector :: Selector '[Bool] ()
setDelaysMagnificationEventsSelector = mkSelector "setDelaysMagnificationEvents:"

-- | @Selector@ for @delaysRotationEvents@
delaysRotationEventsSelector :: Selector '[] Bool
delaysRotationEventsSelector = mkSelector "delaysRotationEvents"

-- | @Selector@ for @setDelaysRotationEvents:@
setDelaysRotationEventsSelector :: Selector '[Bool] ()
setDelaysRotationEventsSelector = mkSelector "setDelaysRotationEvents:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector '[] NSEventModifierFlags
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[NSGestureRecognizerState] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @allowedTouchTypes@
allowedTouchTypesSelector :: Selector '[] NSTouchTypeMask
allowedTouchTypesSelector = mkSelector "allowedTouchTypes"

-- | @Selector@ for @setAllowedTouchTypes:@
setAllowedTouchTypesSelector :: Selector '[NSTouchTypeMask] ()
setAllowedTouchTypesSelector = mkSelector "setAllowedTouchTypes:"

