{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXMetricManager
--
-- An instance of this class can be used to retrieve periodic, aggregated power and performance metrics.
--
-- To receive metrics, clients must acquire a reference to the shared instance of the metric manager and add an eligible MXMetricManagerSubscriber.
--
-- Metrics are not guaranteed to be delivered, but can be expected atleast once per day when conditions permit.
--
-- Subscribers to the metric manager can remove themselves using removeSubscriber:subscriber if they no longer wish to receive metrics.
--
-- Generated bindings for @MXMetricManager@.
module ObjC.MetricKit.MXMetricManager
  ( MXMetricManager
  , IsMXMetricManager(..)
  , makeLogHandleWithCategory
  , addSubscriber
  , removeSubscriber
  , pastPayloads
  , pastDiagnosticPayloads
  , sharedManager
  , addSubscriberSelector
  , makeLogHandleWithCategorySelector
  , pastDiagnosticPayloadsSelector
  , pastPayloadsSelector
  , removeSubscriberSelector
  , sharedManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | makeLogHandleWithCategory:category
--
-- Retrieve a log handle for flagging critical sections with os_signpost().
--
-- @category@ — A string used to define a log category
--
-- The log handle configures persistence for any signposts emit while using the log handle.
--
-- Returns: A log handle that can be used with the logging framework.
--
-- ObjC selector: @+ makeLogHandleWithCategory:@
makeLogHandleWithCategory :: IsNSString category => category -> IO (Id NSObject)
makeLogHandleWithCategory category =
  do
    cls' <- getRequiredClass "MXMetricManager"
    sendClassMessage cls' makeLogHandleWithCategorySelector (toNSString category)

-- | addSubscriber:subscriber
--
-- Adds a subscriber to the metric manager.
--
-- @subscriber@ — An object that conforms to the MXMetricManagerSubscriber protocol.
--
-- Subscribers can receive metric payloads by conforming to the MXMetricManagerSubscriber protocol.
--
-- ObjC selector: @- addSubscriber:@
addSubscriber :: IsMXMetricManager mxMetricManager => mxMetricManager -> RawId -> IO ()
addSubscriber mxMetricManager subscriber =
  sendMessage mxMetricManager addSubscriberSelector subscriber

-- | removeSubscriber:subscriber
--
-- Removes a subscriber from the metric manager.
--
-- @subscriber@ — An object that conforms to the MXMetricManagerSubscriber protocol.
--
-- The subscriber indicated, if previously registered, will no longer receive metric payloads.
--
-- ObjC selector: @- removeSubscriber:@
removeSubscriber :: IsMXMetricManager mxMetricManager => mxMetricManager -> RawId -> IO ()
removeSubscriber mxMetricManager subscriber =
  sendMessage mxMetricManager removeSubscriberSelector subscriber

-- | pastPayloads
--
-- A list of past metric payloads received.
--
-- ObjC selector: @- pastPayloads@
pastPayloads :: IsMXMetricManager mxMetricManager => mxMetricManager -> IO (Id NSArray)
pastPayloads mxMetricManager =
  sendMessage mxMetricManager pastPayloadsSelector

-- | pastDiagnosticPayloads
--
-- A list of past diagnostic payloads received.
--
-- ObjC selector: @- pastDiagnosticPayloads@
pastDiagnosticPayloads :: IsMXMetricManager mxMetricManager => mxMetricManager -> IO (Id NSArray)
pastDiagnosticPayloads mxMetricManager =
  sendMessage mxMetricManager pastDiagnosticPayloadsSelector

-- | sharedManager
--
-- Singleton instance of MXMetricManager.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id MXMetricManager)
sharedManager  =
  do
    cls' <- getRequiredClass "MXMetricManager"
    sendClassMessage cls' sharedManagerSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @makeLogHandleWithCategory:@
makeLogHandleWithCategorySelector :: Selector '[Id NSString] (Id NSObject)
makeLogHandleWithCategorySelector = mkSelector "makeLogHandleWithCategory:"

-- | @Selector@ for @addSubscriber:@
addSubscriberSelector :: Selector '[RawId] ()
addSubscriberSelector = mkSelector "addSubscriber:"

-- | @Selector@ for @removeSubscriber:@
removeSubscriberSelector :: Selector '[RawId] ()
removeSubscriberSelector = mkSelector "removeSubscriber:"

-- | @Selector@ for @pastPayloads@
pastPayloadsSelector :: Selector '[] (Id NSArray)
pastPayloadsSelector = mkSelector "pastPayloads"

-- | @Selector@ for @pastDiagnosticPayloads@
pastDiagnosticPayloadsSelector :: Selector '[] (Id NSArray)
pastDiagnosticPayloadsSelector = mkSelector "pastDiagnosticPayloads"

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id MXMetricManager)
sharedManagerSelector = mkSelector "sharedManager"

