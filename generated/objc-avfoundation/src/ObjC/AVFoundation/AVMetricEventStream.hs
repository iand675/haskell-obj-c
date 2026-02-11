{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetricEventStream allows clients to add publishers and then subscribe to specific metric event classes from those publishers. Publishers are AVFoundation instances implementing AVMetricEventStreamPublisher. The interface allows clients to receive metric events via a subscriber delegate which implements AVMetricEventStreamSubscriber.
--
-- Generated bindings for @AVMetricEventStream@.
module ObjC.AVFoundation.AVMetricEventStream
  ( AVMetricEventStream
  , IsAVMetricEventStream(..)
  , init_
  , new
  , eventStream
  , addPublisher
  , setSubscriber_queue
  , subscribeToMetricEvent
  , subscribeToMetricEvents
  , subscribeToAllMetricEvents
  , initSelector
  , newSelector
  , eventStreamSelector
  , addPublisherSelector
  , setSubscriber_queueSelector
  , subscribeToMetricEventSelector
  , subscribeToMetricEventsSelector
  , subscribeToAllMetricEventsSelector


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

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> IO (Id AVMetricEventStream)
init_ avMetricEventStream  =
  sendMsg avMetricEventStream (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMetricEventStream)
new  =
  do
    cls' <- getRequiredClass "AVMetricEventStream"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns an autoreleased instance.
--
-- ObjC selector: @+ eventStream@
eventStream :: IO (Id AVMetricEventStream)
eventStream  =
  do
    cls' <- getRequiredClass "AVMetricEventStream"
    sendClassMsg cls' (mkSelector "eventStream") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The publisher should be an AVFoundation instance conforming to AVMetricEventStreamPublisher.
--
-- ObjC selector: @- addPublisher:@
addPublisher :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> RawId -> IO Bool
addPublisher avMetricEventStream  publisher =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricEventStream (mkSelector "addPublisher:") retCULong [argPtr (castPtr (unRawId publisher) :: Ptr ())]

-- | Set a subscriber delegate.
--
-- - Parameter subscriber: A subscriber delegate object conforming to AVMetricEventStreamSubscriber. - Parameter queue: Dispatch queue for the delegate callbacks.
--
-- ObjC selector: @- setSubscriber:queue:@
setSubscriber_queue :: (IsAVMetricEventStream avMetricEventStream, IsNSObject queue) => avMetricEventStream -> RawId -> queue -> IO Bool
setSubscriber_queue avMetricEventStream  subscriber queue =
withObjCPtr queue $ \raw_queue ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetricEventStream (mkSelector "setSubscriber:queue:") retCULong [argPtr (castPtr (unRawId subscriber) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())]

-- | Subscribe to a specific metric event class.
--
-- - Parameter metricEventClass: Type of metric event class to subscribe to.
--
-- ObjC selector: @- subscribeToMetricEvent:@
subscribeToMetricEvent :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> Class -> IO ()
subscribeToMetricEvent avMetricEventStream  metricEventClass =
  sendMsg avMetricEventStream (mkSelector "subscribeToMetricEvent:") retVoid [argPtr (unClass metricEventClass)]

-- | Subscribe to set of metric event classes.
--
-- - Parameter metricEventClasses: Set of metric event classes to subscribe to.
--
-- ObjC selector: @- subscribeToMetricEvents:@
subscribeToMetricEvents :: (IsAVMetricEventStream avMetricEventStream, IsNSArray metricEventClasses) => avMetricEventStream -> metricEventClasses -> IO ()
subscribeToMetricEvents avMetricEventStream  metricEventClasses =
withObjCPtr metricEventClasses $ \raw_metricEventClasses ->
    sendMsg avMetricEventStream (mkSelector "subscribeToMetricEvents:") retVoid [argPtr (castPtr raw_metricEventClasses :: Ptr ())]

-- | Subscribe to all metric event classes.
--
-- ObjC selector: @- subscribeToAllMetricEvents@
subscribeToAllMetricEvents :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> IO ()
subscribeToAllMetricEvents avMetricEventStream  =
  sendMsg avMetricEventStream (mkSelector "subscribeToAllMetricEvents") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @eventStream@
eventStreamSelector :: Selector
eventStreamSelector = mkSelector "eventStream"

-- | @Selector@ for @addPublisher:@
addPublisherSelector :: Selector
addPublisherSelector = mkSelector "addPublisher:"

-- | @Selector@ for @setSubscriber:queue:@
setSubscriber_queueSelector :: Selector
setSubscriber_queueSelector = mkSelector "setSubscriber:queue:"

-- | @Selector@ for @subscribeToMetricEvent:@
subscribeToMetricEventSelector :: Selector
subscribeToMetricEventSelector = mkSelector "subscribeToMetricEvent:"

-- | @Selector@ for @subscribeToMetricEvents:@
subscribeToMetricEventsSelector :: Selector
subscribeToMetricEventsSelector = mkSelector "subscribeToMetricEvents:"

-- | @Selector@ for @subscribeToAllMetricEvents@
subscribeToAllMetricEventsSelector :: Selector
subscribeToAllMetricEventsSelector = mkSelector "subscribeToAllMetricEvents"

