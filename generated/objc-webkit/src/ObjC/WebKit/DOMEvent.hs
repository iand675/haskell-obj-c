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
  , eventPhase
  , bubbles
  , cancelable
  , timeStamp
  , returnValue
  , setReturnValue
  , cancelBubble
  , setCancelBubble
  , stopPropagationSelector
  , preventDefaultSelector
  , initEvent_canBubbleArg_cancelableArgSelector
  , initEventSelector
  , typeSelector
  , eventPhaseSelector
  , bubblesSelector
  , cancelableSelector
  , timeStampSelector
  , returnValueSelector
  , setReturnValueSelector
  , cancelBubbleSelector
  , setCancelBubbleSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- stopPropagation@
stopPropagation :: IsDOMEvent domEvent => domEvent -> IO ()
stopPropagation domEvent  =
  sendMsg domEvent (mkSelector "stopPropagation") retVoid []

-- | @- preventDefault@
preventDefault :: IsDOMEvent domEvent => domEvent -> IO ()
preventDefault domEvent  =
  sendMsg domEvent (mkSelector "preventDefault") retVoid []

-- | @- initEvent:canBubbleArg:cancelableArg:@
initEvent_canBubbleArg_cancelableArg :: (IsDOMEvent domEvent, IsNSString eventTypeArg) => domEvent -> eventTypeArg -> Bool -> Bool -> IO ()
initEvent_canBubbleArg_cancelableArg domEvent  eventTypeArg canBubbleArg cancelableArg =
withObjCPtr eventTypeArg $ \raw_eventTypeArg ->
    sendMsg domEvent (mkSelector "initEvent:canBubbleArg:cancelableArg:") retVoid [argPtr (castPtr raw_eventTypeArg :: Ptr ()), argCULong (if canBubbleArg then 1 else 0), argCULong (if cancelableArg then 1 else 0)]

-- | @- initEvent:::@
initEvent :: (IsDOMEvent domEvent, IsNSString eventTypeArg) => domEvent -> eventTypeArg -> Bool -> Bool -> IO ()
initEvent domEvent  eventTypeArg canBubbleArg cancelableArg =
withObjCPtr eventTypeArg $ \raw_eventTypeArg ->
    sendMsg domEvent (mkSelector "initEvent:::") retVoid [argPtr (castPtr raw_eventTypeArg :: Ptr ()), argCULong (if canBubbleArg then 1 else 0), argCULong (if cancelableArg then 1 else 0)]

-- | @- type@
type_ :: IsDOMEvent domEvent => domEvent -> IO (Id NSString)
type_ domEvent  =
  sendMsg domEvent (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- eventPhase@
eventPhase :: IsDOMEvent domEvent => domEvent -> IO CUShort
eventPhase domEvent  =
  fmap fromIntegral $ sendMsg domEvent (mkSelector "eventPhase") retCUInt []

-- | @- bubbles@
bubbles :: IsDOMEvent domEvent => domEvent -> IO Bool
bubbles domEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domEvent (mkSelector "bubbles") retCULong []

-- | @- cancelable@
cancelable :: IsDOMEvent domEvent => domEvent -> IO Bool
cancelable domEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domEvent (mkSelector "cancelable") retCULong []

-- | @- timeStamp@
timeStamp :: IsDOMEvent domEvent => domEvent -> IO CULong
timeStamp domEvent  =
  sendMsg domEvent (mkSelector "timeStamp") retCULong []

-- | @- returnValue@
returnValue :: IsDOMEvent domEvent => domEvent -> IO Bool
returnValue domEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domEvent (mkSelector "returnValue") retCULong []

-- | @- setReturnValue:@
setReturnValue :: IsDOMEvent domEvent => domEvent -> Bool -> IO ()
setReturnValue domEvent  value =
  sendMsg domEvent (mkSelector "setReturnValue:") retVoid [argCULong (if value then 1 else 0)]

-- | @- cancelBubble@
cancelBubble :: IsDOMEvent domEvent => domEvent -> IO Bool
cancelBubble domEvent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg domEvent (mkSelector "cancelBubble") retCULong []

-- | @- setCancelBubble:@
setCancelBubble :: IsDOMEvent domEvent => domEvent -> Bool -> IO ()
setCancelBubble domEvent  value =
  sendMsg domEvent (mkSelector "setCancelBubble:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @stopPropagation@
stopPropagationSelector :: Selector
stopPropagationSelector = mkSelector "stopPropagation"

-- | @Selector@ for @preventDefault@
preventDefaultSelector :: Selector
preventDefaultSelector = mkSelector "preventDefault"

-- | @Selector@ for @initEvent:canBubbleArg:cancelableArg:@
initEvent_canBubbleArg_cancelableArgSelector :: Selector
initEvent_canBubbleArg_cancelableArgSelector = mkSelector "initEvent:canBubbleArg:cancelableArg:"

-- | @Selector@ for @initEvent:::@
initEventSelector :: Selector
initEventSelector = mkSelector "initEvent:::"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @eventPhase@
eventPhaseSelector :: Selector
eventPhaseSelector = mkSelector "eventPhase"

-- | @Selector@ for @bubbles@
bubblesSelector :: Selector
bubblesSelector = mkSelector "bubbles"

-- | @Selector@ for @cancelable@
cancelableSelector :: Selector
cancelableSelector = mkSelector "cancelable"

-- | @Selector@ for @timeStamp@
timeStampSelector :: Selector
timeStampSelector = mkSelector "timeStamp"

-- | @Selector@ for @returnValue@
returnValueSelector :: Selector
returnValueSelector = mkSelector "returnValue"

-- | @Selector@ for @setReturnValue:@
setReturnValueSelector :: Selector
setReturnValueSelector = mkSelector "setReturnValue:"

-- | @Selector@ for @cancelBubble@
cancelBubbleSelector :: Selector
cancelBubbleSelector = mkSelector "cancelBubble"

-- | @Selector@ for @setCancelBubble:@
setCancelBubbleSelector :: Selector
setCancelBubbleSelector = mkSelector "setCancelBubble:"

