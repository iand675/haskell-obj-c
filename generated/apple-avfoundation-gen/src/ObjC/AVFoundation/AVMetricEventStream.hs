{-# LANGUAGE DataKinds #-}
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
  , addPublisherSelector
  , eventStreamSelector
  , initSelector
  , newSelector
  , setSubscriber_queueSelector
  , subscribeToAllMetricEventsSelector
  , subscribeToMetricEventSelector
  , subscribeToMetricEventsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> IO (Id AVMetricEventStream)
init_ avMetricEventStream =
  sendOwnedMessage avMetricEventStream initSelector

-- | @+ new@
new :: IO (Id AVMetricEventStream)
new  =
  do
    cls' <- getRequiredClass "AVMetricEventStream"
    sendOwnedClassMessage cls' newSelector

-- | Returns an autoreleased instance.
--
-- ObjC selector: @+ eventStream@
eventStream :: IO (Id AVMetricEventStream)
eventStream  =
  do
    cls' <- getRequiredClass "AVMetricEventStream"
    sendClassMessage cls' eventStreamSelector

-- | The publisher should be an AVFoundation instance conforming to AVMetricEventStreamPublisher.
--
-- ObjC selector: @- addPublisher:@
addPublisher :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> RawId -> IO Bool
addPublisher avMetricEventStream publisher =
  sendMessage avMetricEventStream addPublisherSelector publisher

-- | Set a subscriber delegate.
--
-- - Parameter subscriber: A subscriber delegate object conforming to AVMetricEventStreamSubscriber. - Parameter queue: Dispatch queue for the delegate callbacks.
--
-- ObjC selector: @- setSubscriber:queue:@
setSubscriber_queue :: (IsAVMetricEventStream avMetricEventStream, IsNSObject queue) => avMetricEventStream -> RawId -> queue -> IO Bool
setSubscriber_queue avMetricEventStream subscriber queue =
  sendMessage avMetricEventStream setSubscriber_queueSelector subscriber (toNSObject queue)

-- | Subscribe to a specific metric event class.
--
-- - Parameter metricEventClass: Type of metric event class to subscribe to.
--
-- ObjC selector: @- subscribeToMetricEvent:@
subscribeToMetricEvent :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> Class -> IO ()
subscribeToMetricEvent avMetricEventStream metricEventClass =
  sendMessage avMetricEventStream subscribeToMetricEventSelector metricEventClass

-- | Subscribe to set of metric event classes.
--
-- - Parameter metricEventClasses: Set of metric event classes to subscribe to.
--
-- ObjC selector: @- subscribeToMetricEvents:@
subscribeToMetricEvents :: (IsAVMetricEventStream avMetricEventStream, IsNSArray metricEventClasses) => avMetricEventStream -> metricEventClasses -> IO ()
subscribeToMetricEvents avMetricEventStream metricEventClasses =
  sendMessage avMetricEventStream subscribeToMetricEventsSelector (toNSArray metricEventClasses)

-- | Subscribe to all metric event classes.
--
-- ObjC selector: @- subscribeToAllMetricEvents@
subscribeToAllMetricEvents :: IsAVMetricEventStream avMetricEventStream => avMetricEventStream -> IO ()
subscribeToAllMetricEvents avMetricEventStream =
  sendMessage avMetricEventStream subscribeToAllMetricEventsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetricEventStream)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetricEventStream)
newSelector = mkSelector "new"

-- | @Selector@ for @eventStream@
eventStreamSelector :: Selector '[] (Id AVMetricEventStream)
eventStreamSelector = mkSelector "eventStream"

-- | @Selector@ for @addPublisher:@
addPublisherSelector :: Selector '[RawId] Bool
addPublisherSelector = mkSelector "addPublisher:"

-- | @Selector@ for @setSubscriber:queue:@
setSubscriber_queueSelector :: Selector '[RawId, Id NSObject] Bool
setSubscriber_queueSelector = mkSelector "setSubscriber:queue:"

-- | @Selector@ for @subscribeToMetricEvent:@
subscribeToMetricEventSelector :: Selector '[Class] ()
subscribeToMetricEventSelector = mkSelector "subscribeToMetricEvent:"

-- | @Selector@ for @subscribeToMetricEvents:@
subscribeToMetricEventsSelector :: Selector '[Id NSArray] ()
subscribeToMetricEventsSelector = mkSelector "subscribeToMetricEvents:"

-- | @Selector@ for @subscribeToAllMetricEvents@
subscribeToAllMetricEventsSelector :: Selector '[] ()
subscribeToAllMetricEventsSelector = mkSelector "subscribeToAllMetricEvents"

