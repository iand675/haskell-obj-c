{-# LANGUAGE PatternSynonyms #-}
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
  , initWithTarget_actionSelector
  , initWithCoderSelector
  , locationInViewSelector
  , resetSelector
  , canPreventGestureRecognizerSelector
  , canBePreventedByGestureRecognizerSelector
  , shouldRequireFailureOfGestureRecognizerSelector
  , shouldBeRequiredToFailByGestureRecognizerSelector
  , mouseDownSelector
  , rightMouseDownSelector
  , otherMouseDownSelector
  , mouseUpSelector
  , rightMouseUpSelector
  , otherMouseUpSelector
  , mouseDraggedSelector
  , rightMouseDraggedSelector
  , otherMouseDraggedSelector
  , mouseCancelledSelector
  , keyDownSelector
  , keyUpSelector
  , flagsChangedSelector
  , tabletPointSelector
  , magnifyWithEventSelector
  , rotateWithEventSelector
  , pressureChangeWithEventSelector
  , touchesBeganWithEventSelector
  , touchesMovedWithEventSelector
  , touchesEndedWithEventSelector
  , touchesCancelledWithEventSelector
  , targetSelector
  , setTargetSelector
  , actionSelector
  , setActionSelector
  , stateSelector
  , delegateSelector
  , setDelegateSelector
  , enabledSelector
  , setEnabledSelector
  , viewSelector
  , pressureConfigurationSelector
  , setPressureConfigurationSelector
  , delaysPrimaryMouseButtonEventsSelector
  , setDelaysPrimaryMouseButtonEventsSelector
  , delaysSecondaryMouseButtonEventsSelector
  , setDelaysSecondaryMouseButtonEventsSelector
  , delaysOtherMouseButtonEventsSelector
  , setDelaysOtherMouseButtonEventsSelector
  , delaysKeyEventsSelector
  , setDelaysKeyEventsSelector
  , delaysMagnificationEventsSelector
  , setDelaysMagnificationEventsSelector
  , delaysRotationEventsSelector
  , setDelaysRotationEventsSelector
  , nameSelector
  , setNameSelector
  , modifierFlagsSelector
  , setStateSelector
  , allowedTouchTypesSelector
  , setAllowedTouchTypesSelector

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

-- | @- initWithTarget:action:@
initWithTarget_action :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> RawId -> Selector -> IO (Id NSGestureRecognizer)
initWithTarget_action nsGestureRecognizer  target action =
    sendMsg nsGestureRecognizer (mkSelector "initWithTarget:action:") (retPtr retVoid) [argPtr (castPtr (unRawId target) :: Ptr ()), argPtr (unSelector action)] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSCoder coder) => nsGestureRecognizer -> coder -> IO (Id NSGestureRecognizer)
initWithCoder nsGestureRecognizer  coder =
  withObjCPtr coder $ \raw_coder ->
      sendMsg nsGestureRecognizer (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- locationInView:@
locationInView :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSView view) => nsGestureRecognizer -> view -> IO NSPoint
locationInView nsGestureRecognizer  view =
  withObjCPtr view $ \raw_view ->
      sendMsgStret nsGestureRecognizer (mkSelector "locationInView:") retNSPoint [argPtr (castPtr raw_view :: Ptr ())]

-- | @- reset@
reset :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO ()
reset nsGestureRecognizer  =
    sendMsg nsGestureRecognizer (mkSelector "reset") retVoid []

-- | @- canPreventGestureRecognizer:@
canPreventGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer preventedGestureRecognizer) => nsGestureRecognizer -> preventedGestureRecognizer -> IO Bool
canPreventGestureRecognizer nsGestureRecognizer  preventedGestureRecognizer =
  withObjCPtr preventedGestureRecognizer $ \raw_preventedGestureRecognizer ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "canPreventGestureRecognizer:") retCULong [argPtr (castPtr raw_preventedGestureRecognizer :: Ptr ())]

-- | @- canBePreventedByGestureRecognizer:@
canBePreventedByGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer preventingGestureRecognizer) => nsGestureRecognizer -> preventingGestureRecognizer -> IO Bool
canBePreventedByGestureRecognizer nsGestureRecognizer  preventingGestureRecognizer =
  withObjCPtr preventingGestureRecognizer $ \raw_preventingGestureRecognizer ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "canBePreventedByGestureRecognizer:") retCULong [argPtr (castPtr raw_preventingGestureRecognizer :: Ptr ())]

-- | @- shouldRequireFailureOfGestureRecognizer:@
shouldRequireFailureOfGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer otherGestureRecognizer) => nsGestureRecognizer -> otherGestureRecognizer -> IO Bool
shouldRequireFailureOfGestureRecognizer nsGestureRecognizer  otherGestureRecognizer =
  withObjCPtr otherGestureRecognizer $ \raw_otherGestureRecognizer ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "shouldRequireFailureOfGestureRecognizer:") retCULong [argPtr (castPtr raw_otherGestureRecognizer :: Ptr ())]

-- | @- shouldBeRequiredToFailByGestureRecognizer:@
shouldBeRequiredToFailByGestureRecognizer :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSGestureRecognizer otherGestureRecognizer) => nsGestureRecognizer -> otherGestureRecognizer -> IO Bool
shouldBeRequiredToFailByGestureRecognizer nsGestureRecognizer  otherGestureRecognizer =
  withObjCPtr otherGestureRecognizer $ \raw_otherGestureRecognizer ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "shouldBeRequiredToFailByGestureRecognizer:") retCULong [argPtr (castPtr raw_otherGestureRecognizer :: Ptr ())]

-- | @- mouseDown:@
mouseDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseDown nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "mouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rightMouseDown:@
rightMouseDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rightMouseDown nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "rightMouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- otherMouseDown:@
otherMouseDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
otherMouseDown nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "otherMouseDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseUp:@
mouseUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseUp nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "mouseUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rightMouseUp:@
rightMouseUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rightMouseUp nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "rightMouseUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- otherMouseUp:@
otherMouseUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
otherMouseUp nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "otherMouseUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseDragged:@
mouseDragged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseDragged nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "mouseDragged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rightMouseDragged:@
rightMouseDragged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rightMouseDragged nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "rightMouseDragged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- otherMouseDragged:@
otherMouseDragged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
otherMouseDragged nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "otherMouseDragged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- mouseCancelled:@
mouseCancelled :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
mouseCancelled nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "mouseCancelled:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- keyDown:@
keyDown :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
keyDown nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "keyDown:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- keyUp:@
keyUp :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
keyUp nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "keyUp:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- flagsChanged:@
flagsChanged :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
flagsChanged nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "flagsChanged:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- tabletPoint:@
tabletPoint :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
tabletPoint nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "tabletPoint:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- magnifyWithEvent:@
magnifyWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
magnifyWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "magnifyWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- rotateWithEvent:@
rotateWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
rotateWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "rotateWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- pressureChangeWithEvent:@
pressureChangeWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
pressureChangeWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "pressureChangeWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesBeganWithEvent:@
touchesBeganWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesBeganWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "touchesBeganWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesMovedWithEvent:@
touchesMovedWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesMovedWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "touchesMovedWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesEndedWithEvent:@
touchesEndedWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesEndedWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "touchesEndedWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- touchesCancelledWithEvent:@
touchesCancelledWithEvent :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSEvent event) => nsGestureRecognizer -> event -> IO ()
touchesCancelledWithEvent nsGestureRecognizer  event =
  withObjCPtr event $ \raw_event ->
      sendMsg nsGestureRecognizer (mkSelector "touchesCancelledWithEvent:") retVoid [argPtr (castPtr raw_event :: Ptr ())]

-- | @- target@
target :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO RawId
target nsGestureRecognizer  =
    fmap (RawId . castPtr) $ sendMsg nsGestureRecognizer (mkSelector "target") (retPtr retVoid) []

-- | @- setTarget:@
setTarget :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> RawId -> IO ()
setTarget nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setTarget:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- action@
action :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Selector
action nsGestureRecognizer  =
    fmap (Selector . castPtr) $ sendMsg nsGestureRecognizer (mkSelector "action") (retPtr retVoid) []

-- | @- setAction:@
setAction :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Selector -> IO ()
setAction nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setAction:") retVoid [argPtr (unSelector value)]

-- | @- state@
state :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO NSGestureRecognizerState
state nsGestureRecognizer  =
    fmap (coerce :: CLong -> NSGestureRecognizerState) $ sendMsg nsGestureRecognizer (mkSelector "state") retCLong []

-- | @- delegate@
delegate :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO RawId
delegate nsGestureRecognizer  =
    fmap (RawId . castPtr) $ sendMsg nsGestureRecognizer (mkSelector "delegate") (retPtr retVoid) []

-- | @- setDelegate:@
setDelegate :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> RawId -> IO ()
setDelegate nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelegate:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- enabled@
enabled :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
enabled nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "enabled") retCULong []

-- | @- setEnabled:@
setEnabled :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setEnabled nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | @- view@
view :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO (Id NSView)
view nsGestureRecognizer  =
    sendMsg nsGestureRecognizer (mkSelector "view") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- pressureConfiguration@
pressureConfiguration :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO (Id NSPressureConfiguration)
pressureConfiguration nsGestureRecognizer  =
    sendMsg nsGestureRecognizer (mkSelector "pressureConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPressureConfiguration:@
setPressureConfiguration :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSPressureConfiguration value) => nsGestureRecognizer -> value -> IO ()
setPressureConfiguration nsGestureRecognizer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsGestureRecognizer (mkSelector "setPressureConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- delaysPrimaryMouseButtonEvents@
delaysPrimaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysPrimaryMouseButtonEvents nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "delaysPrimaryMouseButtonEvents") retCULong []

-- | @- setDelaysPrimaryMouseButtonEvents:@
setDelaysPrimaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysPrimaryMouseButtonEvents nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelaysPrimaryMouseButtonEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delaysSecondaryMouseButtonEvents@
delaysSecondaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysSecondaryMouseButtonEvents nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "delaysSecondaryMouseButtonEvents") retCULong []

-- | @- setDelaysSecondaryMouseButtonEvents:@
setDelaysSecondaryMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysSecondaryMouseButtonEvents nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelaysSecondaryMouseButtonEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delaysOtherMouseButtonEvents@
delaysOtherMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysOtherMouseButtonEvents nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "delaysOtherMouseButtonEvents") retCULong []

-- | @- setDelaysOtherMouseButtonEvents:@
setDelaysOtherMouseButtonEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysOtherMouseButtonEvents nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelaysOtherMouseButtonEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delaysKeyEvents@
delaysKeyEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysKeyEvents nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "delaysKeyEvents") retCULong []

-- | @- setDelaysKeyEvents:@
setDelaysKeyEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysKeyEvents nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelaysKeyEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delaysMagnificationEvents@
delaysMagnificationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysMagnificationEvents nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "delaysMagnificationEvents") retCULong []

-- | @- setDelaysMagnificationEvents:@
setDelaysMagnificationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysMagnificationEvents nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelaysMagnificationEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- delaysRotationEvents@
delaysRotationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO Bool
delaysRotationEvents nsGestureRecognizer  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsGestureRecognizer (mkSelector "delaysRotationEvents") retCULong []

-- | @- setDelaysRotationEvents:@
setDelaysRotationEvents :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> Bool -> IO ()
setDelaysRotationEvents nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setDelaysRotationEvents:") retVoid [argCULong (if value then 1 else 0)]

-- | @- name@
name :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO (Id NSString)
name nsGestureRecognizer  =
    sendMsg nsGestureRecognizer (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSGestureRecognizer nsGestureRecognizer, IsNSString value) => nsGestureRecognizer -> value -> IO ()
setName nsGestureRecognizer  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nsGestureRecognizer (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- modifierFlags@
modifierFlags :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO NSEventModifierFlags
modifierFlags nsGestureRecognizer  =
    fmap (coerce :: CULong -> NSEventModifierFlags) $ sendMsg nsGestureRecognizer (mkSelector "modifierFlags") retCULong []

-- | @- setState:@
setState :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> NSGestureRecognizerState -> IO ()
setState nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setState:") retVoid [argCLong (coerce value)]

-- | @- allowedTouchTypes@
allowedTouchTypes :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> IO NSTouchTypeMask
allowedTouchTypes nsGestureRecognizer  =
    fmap (coerce :: CULong -> NSTouchTypeMask) $ sendMsg nsGestureRecognizer (mkSelector "allowedTouchTypes") retCULong []

-- | @- setAllowedTouchTypes:@
setAllowedTouchTypes :: IsNSGestureRecognizer nsGestureRecognizer => nsGestureRecognizer -> NSTouchTypeMask -> IO ()
setAllowedTouchTypes nsGestureRecognizer  value =
    sendMsg nsGestureRecognizer (mkSelector "setAllowedTouchTypes:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithTarget:action:@
initWithTarget_actionSelector :: Selector
initWithTarget_actionSelector = mkSelector "initWithTarget:action:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @locationInView:@
locationInViewSelector :: Selector
locationInViewSelector = mkSelector "locationInView:"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @canPreventGestureRecognizer:@
canPreventGestureRecognizerSelector :: Selector
canPreventGestureRecognizerSelector = mkSelector "canPreventGestureRecognizer:"

-- | @Selector@ for @canBePreventedByGestureRecognizer:@
canBePreventedByGestureRecognizerSelector :: Selector
canBePreventedByGestureRecognizerSelector = mkSelector "canBePreventedByGestureRecognizer:"

-- | @Selector@ for @shouldRequireFailureOfGestureRecognizer:@
shouldRequireFailureOfGestureRecognizerSelector :: Selector
shouldRequireFailureOfGestureRecognizerSelector = mkSelector "shouldRequireFailureOfGestureRecognizer:"

-- | @Selector@ for @shouldBeRequiredToFailByGestureRecognizer:@
shouldBeRequiredToFailByGestureRecognizerSelector :: Selector
shouldBeRequiredToFailByGestureRecognizerSelector = mkSelector "shouldBeRequiredToFailByGestureRecognizer:"

-- | @Selector@ for @mouseDown:@
mouseDownSelector :: Selector
mouseDownSelector = mkSelector "mouseDown:"

-- | @Selector@ for @rightMouseDown:@
rightMouseDownSelector :: Selector
rightMouseDownSelector = mkSelector "rightMouseDown:"

-- | @Selector@ for @otherMouseDown:@
otherMouseDownSelector :: Selector
otherMouseDownSelector = mkSelector "otherMouseDown:"

-- | @Selector@ for @mouseUp:@
mouseUpSelector :: Selector
mouseUpSelector = mkSelector "mouseUp:"

-- | @Selector@ for @rightMouseUp:@
rightMouseUpSelector :: Selector
rightMouseUpSelector = mkSelector "rightMouseUp:"

-- | @Selector@ for @otherMouseUp:@
otherMouseUpSelector :: Selector
otherMouseUpSelector = mkSelector "otherMouseUp:"

-- | @Selector@ for @mouseDragged:@
mouseDraggedSelector :: Selector
mouseDraggedSelector = mkSelector "mouseDragged:"

-- | @Selector@ for @rightMouseDragged:@
rightMouseDraggedSelector :: Selector
rightMouseDraggedSelector = mkSelector "rightMouseDragged:"

-- | @Selector@ for @otherMouseDragged:@
otherMouseDraggedSelector :: Selector
otherMouseDraggedSelector = mkSelector "otherMouseDragged:"

-- | @Selector@ for @mouseCancelled:@
mouseCancelledSelector :: Selector
mouseCancelledSelector = mkSelector "mouseCancelled:"

-- | @Selector@ for @keyDown:@
keyDownSelector :: Selector
keyDownSelector = mkSelector "keyDown:"

-- | @Selector@ for @keyUp:@
keyUpSelector :: Selector
keyUpSelector = mkSelector "keyUp:"

-- | @Selector@ for @flagsChanged:@
flagsChangedSelector :: Selector
flagsChangedSelector = mkSelector "flagsChanged:"

-- | @Selector@ for @tabletPoint:@
tabletPointSelector :: Selector
tabletPointSelector = mkSelector "tabletPoint:"

-- | @Selector@ for @magnifyWithEvent:@
magnifyWithEventSelector :: Selector
magnifyWithEventSelector = mkSelector "magnifyWithEvent:"

-- | @Selector@ for @rotateWithEvent:@
rotateWithEventSelector :: Selector
rotateWithEventSelector = mkSelector "rotateWithEvent:"

-- | @Selector@ for @pressureChangeWithEvent:@
pressureChangeWithEventSelector :: Selector
pressureChangeWithEventSelector = mkSelector "pressureChangeWithEvent:"

-- | @Selector@ for @touchesBeganWithEvent:@
touchesBeganWithEventSelector :: Selector
touchesBeganWithEventSelector = mkSelector "touchesBeganWithEvent:"

-- | @Selector@ for @touchesMovedWithEvent:@
touchesMovedWithEventSelector :: Selector
touchesMovedWithEventSelector = mkSelector "touchesMovedWithEvent:"

-- | @Selector@ for @touchesEndedWithEvent:@
touchesEndedWithEventSelector :: Selector
touchesEndedWithEventSelector = mkSelector "touchesEndedWithEvent:"

-- | @Selector@ for @touchesCancelledWithEvent:@
touchesCancelledWithEventSelector :: Selector
touchesCancelledWithEventSelector = mkSelector "touchesCancelledWithEvent:"

-- | @Selector@ for @target@
targetSelector :: Selector
targetSelector = mkSelector "target"

-- | @Selector@ for @setTarget:@
setTargetSelector :: Selector
setTargetSelector = mkSelector "setTarget:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @setAction:@
setActionSelector :: Selector
setActionSelector = mkSelector "setAction:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @delegate@
delegateSelector :: Selector
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @view@
viewSelector :: Selector
viewSelector = mkSelector "view"

-- | @Selector@ for @pressureConfiguration@
pressureConfigurationSelector :: Selector
pressureConfigurationSelector = mkSelector "pressureConfiguration"

-- | @Selector@ for @setPressureConfiguration:@
setPressureConfigurationSelector :: Selector
setPressureConfigurationSelector = mkSelector "setPressureConfiguration:"

-- | @Selector@ for @delaysPrimaryMouseButtonEvents@
delaysPrimaryMouseButtonEventsSelector :: Selector
delaysPrimaryMouseButtonEventsSelector = mkSelector "delaysPrimaryMouseButtonEvents"

-- | @Selector@ for @setDelaysPrimaryMouseButtonEvents:@
setDelaysPrimaryMouseButtonEventsSelector :: Selector
setDelaysPrimaryMouseButtonEventsSelector = mkSelector "setDelaysPrimaryMouseButtonEvents:"

-- | @Selector@ for @delaysSecondaryMouseButtonEvents@
delaysSecondaryMouseButtonEventsSelector :: Selector
delaysSecondaryMouseButtonEventsSelector = mkSelector "delaysSecondaryMouseButtonEvents"

-- | @Selector@ for @setDelaysSecondaryMouseButtonEvents:@
setDelaysSecondaryMouseButtonEventsSelector :: Selector
setDelaysSecondaryMouseButtonEventsSelector = mkSelector "setDelaysSecondaryMouseButtonEvents:"

-- | @Selector@ for @delaysOtherMouseButtonEvents@
delaysOtherMouseButtonEventsSelector :: Selector
delaysOtherMouseButtonEventsSelector = mkSelector "delaysOtherMouseButtonEvents"

-- | @Selector@ for @setDelaysOtherMouseButtonEvents:@
setDelaysOtherMouseButtonEventsSelector :: Selector
setDelaysOtherMouseButtonEventsSelector = mkSelector "setDelaysOtherMouseButtonEvents:"

-- | @Selector@ for @delaysKeyEvents@
delaysKeyEventsSelector :: Selector
delaysKeyEventsSelector = mkSelector "delaysKeyEvents"

-- | @Selector@ for @setDelaysKeyEvents:@
setDelaysKeyEventsSelector :: Selector
setDelaysKeyEventsSelector = mkSelector "setDelaysKeyEvents:"

-- | @Selector@ for @delaysMagnificationEvents@
delaysMagnificationEventsSelector :: Selector
delaysMagnificationEventsSelector = mkSelector "delaysMagnificationEvents"

-- | @Selector@ for @setDelaysMagnificationEvents:@
setDelaysMagnificationEventsSelector :: Selector
setDelaysMagnificationEventsSelector = mkSelector "setDelaysMagnificationEvents:"

-- | @Selector@ for @delaysRotationEvents@
delaysRotationEventsSelector :: Selector
delaysRotationEventsSelector = mkSelector "delaysRotationEvents"

-- | @Selector@ for @setDelaysRotationEvents:@
setDelaysRotationEventsSelector :: Selector
setDelaysRotationEventsSelector = mkSelector "setDelaysRotationEvents:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @modifierFlags@
modifierFlagsSelector :: Selector
modifierFlagsSelector = mkSelector "modifierFlags"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @allowedTouchTypes@
allowedTouchTypesSelector :: Selector
allowedTouchTypesSelector = mkSelector "allowedTouchTypes"

-- | @Selector@ for @setAllowedTouchTypes:@
setAllowedTouchTypesSelector :: Selector
setAllowedTouchTypesSelector = mkSelector "setAllowedTouchTypes:"

