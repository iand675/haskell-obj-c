{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMEvent@.
module ObjC.WebKit.DOMEvent
  ( DOMEvent
  , IsDOMEvent(..)
  , stopPropagation
  , preventDefault
  , initEvent_canBubbleArg_cancelableArg
  , initEvent
  , type_
  , target
  , currentTarget
  , eventPhase
  , bubbles
  , cancelable
  , timeStamp
  , srcElement
  , returnValue
  , setReturnValue
  , cancelBubble
  , setCancelBubble
  , bubblesSelector
  , cancelBubbleSelector
  , cancelableSelector
  , currentTargetSelector
  , eventPhaseSelector
  , initEventSelector
  , initEvent_canBubbleArg_cancelableArgSelector
  , preventDefaultSelector
  , returnValueSelector
  , setCancelBubbleSelector
  , setReturnValueSelector
  , srcElementSelector
  , stopPropagationSelector
  , targetSelector
  , timeStampSelector
  , typeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stopPropagation@
stopPropagation :: IsDOMEvent domEvent => domEvent -> IO ()
stopPropagation domEvent =
  sendMessage domEvent stopPropagationSelector

-- | @- preventDefault@
preventDefault :: IsDOMEvent domEvent => domEvent -> IO ()
preventDefault domEvent =
  sendMessage domEvent preventDefaultSelector

-- | @- initEvent:canBubbleArg:cancelableArg:@
initEvent_canBubbleArg_cancelableArg :: (IsDOMEvent domEvent, IsNSString eventTypeArg) => domEvent -> eventTypeArg -> Bool -> Bool -> IO ()
initEvent_canBubbleArg_cancelableArg domEvent eventTypeArg canBubbleArg cancelableArg =
  sendOwnedMessage domEvent initEvent_canBubbleArg_cancelableArgSelector (toNSString eventTypeArg) canBubbleArg cancelableArg

-- | @- initEvent:::@
initEvent :: (IsDOMEvent domEvent, IsNSString eventTypeArg) => domEvent -> eventTypeArg -> Bool -> Bool -> IO ()
initEvent domEvent eventTypeArg canBubbleArg cancelableArg =
  sendOwnedMessage domEvent initEventSelector (toNSString eventTypeArg) canBubbleArg cancelableArg

-- | @- type@
type_ :: IsDOMEvent domEvent => domEvent -> IO (Id NSString)
type_ domEvent =
  sendMessage domEvent typeSelector

-- | @- target@
target :: IsDOMEvent domEvent => domEvent -> IO RawId
target domEvent =
  sendMessage domEvent targetSelector

-- | @- currentTarget@
currentTarget :: IsDOMEvent domEvent => domEvent -> IO RawId
currentTarget domEvent =
  sendMessage domEvent currentTargetSelector

-- | @- eventPhase@
eventPhase :: IsDOMEvent domEvent => domEvent -> IO CUShort
eventPhase domEvent =
  sendMessage domEvent eventPhaseSelector

-- | @- bubbles@
bubbles :: IsDOMEvent domEvent => domEvent -> IO Bool
bubbles domEvent =
  sendMessage domEvent bubblesSelector

-- | @- cancelable@
cancelable :: IsDOMEvent domEvent => domEvent -> IO Bool
cancelable domEvent =
  sendMessage domEvent cancelableSelector

-- | @- timeStamp@
timeStamp :: IsDOMEvent domEvent => domEvent -> IO CULong
timeStamp domEvent =
  sendMessage domEvent timeStampSelector

-- | @- srcElement@
srcElement :: IsDOMEvent domEvent => domEvent -> IO RawId
srcElement domEvent =
  sendMessage domEvent srcElementSelector

-- | @- returnValue@
returnValue :: IsDOMEvent domEvent => domEvent -> IO Bool
returnValue domEvent =
  sendMessage domEvent returnValueSelector

-- | @- setReturnValue:@
setReturnValue :: IsDOMEvent domEvent => domEvent -> Bool -> IO ()
setReturnValue domEvent value =
  sendMessage domEvent setReturnValueSelector value

-- | @- cancelBubble@
cancelBubble :: IsDOMEvent domEvent => domEvent -> IO Bool
cancelBubble domEvent =
  sendMessage domEvent cancelBubbleSelector

-- | @- setCancelBubble:@
setCancelBubble :: IsDOMEvent domEvent => domEvent -> Bool -> IO ()
setCancelBubble domEvent value =
  sendMessage domEvent setCancelBubbleSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopPropagation@
stopPropagationSelector :: Selector '[] ()
stopPropagationSelector = mkSelector "stopPropagation"

-- | @Selector@ for @preventDefault@
preventDefaultSelector :: Selector '[] ()
preventDefaultSelector = mkSelector "preventDefault"

-- | @Selector@ for @initEvent:canBubbleArg:cancelableArg:@
initEvent_canBubbleArg_cancelableArgSelector :: Selector '[Id NSString, Bool, Bool] ()
initEvent_canBubbleArg_cancelableArgSelector = mkSelector "initEvent:canBubbleArg:cancelableArg:"

-- | @Selector@ for @initEvent:::@
initEventSelector :: Selector '[Id NSString, Bool, Bool] ()
initEventSelector = mkSelector "initEvent:::"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @target@
targetSelector :: Selector '[] RawId
targetSelector = mkSelector "target"

-- | @Selector@ for @currentTarget@
currentTargetSelector :: Selector '[] RawId
currentTargetSelector = mkSelector "currentTarget"

-- | @Selector@ for @eventPhase@
eventPhaseSelector :: Selector '[] CUShort
eventPhaseSelector = mkSelector "eventPhase"

-- | @Selector@ for @bubbles@
bubblesSelector :: Selector '[] Bool
bubblesSelector = mkSelector "bubbles"

-- | @Selector@ for @cancelable@
cancelableSelector :: Selector '[] Bool
cancelableSelector = mkSelector "cancelable"

-- | @Selector@ for @timeStamp@
timeStampSelector :: Selector '[] CULong
timeStampSelector = mkSelector "timeStamp"

-- | @Selector@ for @srcElement@
srcElementSelector :: Selector '[] RawId
srcElementSelector = mkSelector "srcElement"

-- | @Selector@ for @returnValue@
returnValueSelector :: Selector '[] Bool
returnValueSelector = mkSelector "returnValue"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector '[Bool] ()
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @cancelBubble@
cancelBubbleSelector :: Selector '[] Bool
cancelBubbleSelector = mkSelector "cancelBubble"

-- | @Selector@ for @setCancelBubble:@
setCancelBubbleSelector :: Selector '[Bool] ()
setCancelBubbleSelector = mkSelector "setCancelBubble:"

