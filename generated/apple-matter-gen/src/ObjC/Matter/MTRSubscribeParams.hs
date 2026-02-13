{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTRSubscribeParams    This is used to control the behavior of attribute/event subscribes.  If not    provided (i.e. nil passed for the MTRSubscribeParams argument), will be    treated as if a default-initialized object was passed in.
--
-- Generated bindings for @MTRSubscribeParams@.
module ObjC.Matter.MTRSubscribeParams
  ( MTRSubscribeParams
  , IsMTRSubscribeParams(..)
  , initWithMinInterval_maxInterval
  , init_
  , new
  , replaceExistingSubscriptions
  , setReplaceExistingSubscriptions
  , resubscribeAutomatically
  , setResubscribeAutomatically
  , minInterval
  , setMinInterval
  , maxInterval
  , setMaxInterval
  , reportEventsUrgently
  , setReportEventsUrgently
  , keepPreviousSubscriptions
  , setKeepPreviousSubscriptions
  , autoResubscribe
  , setAutoResubscribe
  , autoResubscribeSelector
  , initSelector
  , initWithMinInterval_maxIntervalSelector
  , keepPreviousSubscriptionsSelector
  , maxIntervalSelector
  , minIntervalSelector
  , newSelector
  , replaceExistingSubscriptionsSelector
  , reportEventsUrgentlySelector
  , resubscribeAutomaticallySelector
  , setAutoResubscribeSelector
  , setKeepPreviousSubscriptionsSelector
  , setMaxIntervalSelector
  , setMinIntervalSelector
  , setReplaceExistingSubscriptionsSelector
  , setReportEventsUrgentlySelector
  , setResubscribeAutomaticallySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRSubscribeParams.  Must provide a minInterval and maxInterval; there are no default values for those.
--
-- ObjC selector: @- initWithMinInterval:maxInterval:@
initWithMinInterval_maxInterval :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber minInterval, IsNSNumber maxInterval) => mtrSubscribeParams -> minInterval -> maxInterval -> IO (Id MTRSubscribeParams)
initWithMinInterval_maxInterval mtrSubscribeParams minInterval maxInterval =
  sendOwnedMessage mtrSubscribeParams initWithMinInterval_maxIntervalSelector (toNSNumber minInterval) (toNSNumber maxInterval)

-- | init and new exist for now, for backwards compatibility, and initialize with minInterval set to 1 and maxInterval set to 0, which will not work on its own.  Uses of MTRSubscribeParams that rely on init must all be using (deprecated) APIs that pass in a separate minInterval and maxInterval.
--
-- ObjC selector: @- init@
init_ :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id MTRSubscribeParams)
init_ mtrSubscribeParams =
  sendOwnedMessage mtrSubscribeParams initSelector

-- | @+ new@
new :: IO (Id MTRSubscribeParams)
new  =
  do
    cls' <- getRequiredClass "MTRSubscribeParams"
    sendOwnedClassMessage cls' newSelector

-- | Whether the subscribe should replace already-existing subscriptions.  The default value is YES.
--
-- If YES, the subscribe will cancel any existing subscriptions to the target node when it sets up the new one.
--
-- If NO, the subscribe will allow any previous subscriptions to remain.
--
-- ObjC selector: @- replaceExistingSubscriptions@
replaceExistingSubscriptions :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO Bool
replaceExistingSubscriptions mtrSubscribeParams =
  sendMessage mtrSubscribeParams replaceExistingSubscriptionsSelector

-- | Whether the subscribe should replace already-existing subscriptions.  The default value is YES.
--
-- If YES, the subscribe will cancel any existing subscriptions to the target node when it sets up the new one.
--
-- If NO, the subscribe will allow any previous subscriptions to remain.
--
-- ObjC selector: @- setReplaceExistingSubscriptions:@
setReplaceExistingSubscriptions :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> Bool -> IO ()
setReplaceExistingSubscriptions mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setReplaceExistingSubscriptionsSelector value

-- | Whether the subscription should automatically try to re-establish if it drops.  The default value is YES.
--
-- If NO, loss of subscription will simply lead to an error report.  Some subscription APIs do not support this value.
--
-- If YES, loss of subscription will lead to an automatic resubscription attempt.  If this succeeds, the subscriptionEstablished callback will be called again.
--
-- ObjC selector: @- resubscribeAutomatically@
resubscribeAutomatically :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO Bool
resubscribeAutomatically mtrSubscribeParams =
  sendMessage mtrSubscribeParams resubscribeAutomaticallySelector

-- | Whether the subscription should automatically try to re-establish if it drops.  The default value is YES.
--
-- If NO, loss of subscription will simply lead to an error report.  Some subscription APIs do not support this value.
--
-- If YES, loss of subscription will lead to an automatic resubscription attempt.  If this succeeds, the subscriptionEstablished callback will be called again.
--
-- ObjC selector: @- setResubscribeAutomatically:@
setResubscribeAutomatically :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> Bool -> IO ()
setResubscribeAutomatically mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setResubscribeAutomaticallySelector value

-- | The minimum time, in seconds, between consecutive reports a server will send for this subscription.  This can be used to rate-limit the subscription traffic.  Any non-negative value is allowed, including 0.
--
-- ObjC selector: @- minInterval@
minInterval :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
minInterval mtrSubscribeParams =
  sendMessage mtrSubscribeParams minIntervalSelector

-- | The minimum time, in seconds, between consecutive reports a server will send for this subscription.  This can be used to rate-limit the subscription traffic.  Any non-negative value is allowed, including 0.
--
-- ObjC selector: @- setMinInterval:@
setMinInterval :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setMinInterval mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setMinIntervalSelector (toNSNumber value)

-- | The suggested maximum time, in seconds, during which the server is allowed to send no reports at all for this subscription.  Must be at least as large as minInterval.  The server is allowed to use a larger time than this as the maxInterval it selects if it needs to (e.g. to meet its power budget).
--
-- ObjC selector: @- maxInterval@
maxInterval :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
maxInterval mtrSubscribeParams =
  sendMessage mtrSubscribeParams maxIntervalSelector

-- | The suggested maximum time, in seconds, during which the server is allowed to send no reports at all for this subscription.  Must be at least as large as minInterval.  The server is allowed to use a larger time than this as the maxInterval it selects if it needs to (e.g. to meet its power budget).
--
-- ObjC selector: @- setMaxInterval:@
setMaxInterval :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setMaxInterval mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setMaxIntervalSelector (toNSNumber value)

-- | Controls whether events will be reported urgently. The default value is YES.
--
-- If YES, the events will be reported as soon as the minInterval does not prevent it.
--
-- If NO, the events will be reported at the maximum interval.
--
-- ObjC selector: @- reportEventsUrgently@
reportEventsUrgently :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO Bool
reportEventsUrgently mtrSubscribeParams =
  sendMessage mtrSubscribeParams reportEventsUrgentlySelector

-- | Controls whether events will be reported urgently. The default value is YES.
--
-- If YES, the events will be reported as soon as the minInterval does not prevent it.
--
-- If NO, the events will be reported at the maximum interval.
--
-- ObjC selector: @- setReportEventsUrgently:@
setReportEventsUrgently :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> Bool -> IO ()
setReportEventsUrgently mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setReportEventsUrgentlySelector value

-- | @- keepPreviousSubscriptions@
keepPreviousSubscriptions :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
keepPreviousSubscriptions mtrSubscribeParams =
  sendMessage mtrSubscribeParams keepPreviousSubscriptionsSelector

-- | @- setKeepPreviousSubscriptions:@
setKeepPreviousSubscriptions :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setKeepPreviousSubscriptions mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setKeepPreviousSubscriptionsSelector (toNSNumber value)

-- | @- autoResubscribe@
autoResubscribe :: IsMTRSubscribeParams mtrSubscribeParams => mtrSubscribeParams -> IO (Id NSNumber)
autoResubscribe mtrSubscribeParams =
  sendMessage mtrSubscribeParams autoResubscribeSelector

-- | @- setAutoResubscribe:@
setAutoResubscribe :: (IsMTRSubscribeParams mtrSubscribeParams, IsNSNumber value) => mtrSubscribeParams -> value -> IO ()
setAutoResubscribe mtrSubscribeParams value =
  sendMessage mtrSubscribeParams setAutoResubscribeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMinInterval:maxInterval:@
initWithMinInterval_maxIntervalSelector :: Selector '[Id NSNumber, Id NSNumber] (Id MTRSubscribeParams)
initWithMinInterval_maxIntervalSelector = mkSelector "initWithMinInterval:maxInterval:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MTRSubscribeParams)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MTRSubscribeParams)
newSelector = mkSelector "new"

-- | @Selector@ for @replaceExistingSubscriptions@
replaceExistingSubscriptionsSelector :: Selector '[] Bool
replaceExistingSubscriptionsSelector = mkSelector "replaceExistingSubscriptions"

-- | @Selector@ for @setReplaceExistingSubscriptions:@
setReplaceExistingSubscriptionsSelector :: Selector '[Bool] ()
setReplaceExistingSubscriptionsSelector = mkSelector "setReplaceExistingSubscriptions:"

-- | @Selector@ for @resubscribeAutomatically@
resubscribeAutomaticallySelector :: Selector '[] Bool
resubscribeAutomaticallySelector = mkSelector "resubscribeAutomatically"

-- | @Selector@ for @setResubscribeAutomatically:@
setResubscribeAutomaticallySelector :: Selector '[Bool] ()
setResubscribeAutomaticallySelector = mkSelector "setResubscribeAutomatically:"

-- | @Selector@ for @minInterval@
minIntervalSelector :: Selector '[] (Id NSNumber)
minIntervalSelector = mkSelector "minInterval"

-- | @Selector@ for @setMinInterval:@
setMinIntervalSelector :: Selector '[Id NSNumber] ()
setMinIntervalSelector = mkSelector "setMinInterval:"

-- | @Selector@ for @maxInterval@
maxIntervalSelector :: Selector '[] (Id NSNumber)
maxIntervalSelector = mkSelector "maxInterval"

-- | @Selector@ for @setMaxInterval:@
setMaxIntervalSelector :: Selector '[Id NSNumber] ()
setMaxIntervalSelector = mkSelector "setMaxInterval:"

-- | @Selector@ for @reportEventsUrgently@
reportEventsUrgentlySelector :: Selector '[] Bool
reportEventsUrgentlySelector = mkSelector "reportEventsUrgently"

-- | @Selector@ for @setReportEventsUrgently:@
setReportEventsUrgentlySelector :: Selector '[Bool] ()
setReportEventsUrgentlySelector = mkSelector "setReportEventsUrgently:"

-- | @Selector@ for @keepPreviousSubscriptions@
keepPreviousSubscriptionsSelector :: Selector '[] (Id NSNumber)
keepPreviousSubscriptionsSelector = mkSelector "keepPreviousSubscriptions"

-- | @Selector@ for @setKeepPreviousSubscriptions:@
setKeepPreviousSubscriptionsSelector :: Selector '[Id NSNumber] ()
setKeepPreviousSubscriptionsSelector = mkSelector "setKeepPreviousSubscriptions:"

-- | @Selector@ for @autoResubscribe@
autoResubscribeSelector :: Selector '[] (Id NSNumber)
autoResubscribeSelector = mkSelector "autoResubscribe"

-- | @Selector@ for @setAutoResubscribe:@
setAutoResubscribeSelector :: Selector '[Id NSNumber] ()
setAutoResubscribeSelector = mkSelector "setAutoResubscribe:"

