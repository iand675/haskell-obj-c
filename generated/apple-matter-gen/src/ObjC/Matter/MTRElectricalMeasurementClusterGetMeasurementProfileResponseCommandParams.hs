{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams@.
module ObjC.Matter.MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams
  ( MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams
  , IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams(..)
  , initWithResponseValue_error
  , startTime
  , setStartTime
  , status
  , setStatus
  , profileIntervalPeriod
  , setProfileIntervalPeriod
  , numberOfIntervalsDelivered
  , setNumberOfIntervalsDelivered
  , attributeId
  , setAttributeId
  , intervals
  , setIntervals
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , attributeIdSelector
  , initWithResponseValue_errorSelector
  , intervalsSelector
  , numberOfIntervalsDeliveredSelector
  , profileIntervalPeriodSelector
  , setAttributeIdSelector
  , setIntervalsSelector
  , setNumberOfIntervalsDeliveredSelector
  , setProfileIntervalPeriodSelector
  , setStartTimeSelector
  , setStatusSelector
  , setTimedInvokeTimeoutMsSelector
  , startTimeSelector
  , statusSelector
  , timedInvokeTimeoutMsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize an MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSDictionary responseValue, IsNSError error_) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> responseValue -> error_ -> IO (Id MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams)
initWithResponseValue_error mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams responseValue error_ =
  sendOwnedMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- startTime@
startTime :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
startTime mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setStartTime mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setStartTimeSelector (toNSNumber value)

-- | @- status@
status :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
status mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams statusSelector

-- | @- setStatus:@
setStatus :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setStatus mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setStatusSelector (toNSNumber value)

-- | @- profileIntervalPeriod@
profileIntervalPeriod :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
profileIntervalPeriod mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams profileIntervalPeriodSelector

-- | @- setProfileIntervalPeriod:@
setProfileIntervalPeriod :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setProfileIntervalPeriod mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setProfileIntervalPeriodSelector (toNSNumber value)

-- | @- numberOfIntervalsDelivered@
numberOfIntervalsDelivered :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
numberOfIntervalsDelivered mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams numberOfIntervalsDeliveredSelector

-- | @- setNumberOfIntervalsDelivered:@
setNumberOfIntervalsDelivered :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setNumberOfIntervalsDelivered mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setNumberOfIntervalsDeliveredSelector (toNSNumber value)

-- | @- attributeId@
attributeId :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
attributeId mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams attributeIdSelector

-- | @- setAttributeId:@
setAttributeId :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setAttributeId mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setAttributeIdSelector (toNSNumber value)

-- | @- intervals@
intervals :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSArray)
intervals mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams intervalsSelector

-- | @- setIntervals:@
setIntervals :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSArray value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setIntervals mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setIntervalsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRElectricalMeasurementClusterGetMeasurementProfileResponseCommandParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @status@
statusSelector :: Selector '[] (Id NSNumber)
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector '[Id NSNumber] ()
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @profileIntervalPeriod@
profileIntervalPeriodSelector :: Selector '[] (Id NSNumber)
profileIntervalPeriodSelector = mkSelector "profileIntervalPeriod"

-- | @Selector@ for @setProfileIntervalPeriod:@
setProfileIntervalPeriodSelector :: Selector '[Id NSNumber] ()
setProfileIntervalPeriodSelector = mkSelector "setProfileIntervalPeriod:"

-- | @Selector@ for @numberOfIntervalsDelivered@
numberOfIntervalsDeliveredSelector :: Selector '[] (Id NSNumber)
numberOfIntervalsDeliveredSelector = mkSelector "numberOfIntervalsDelivered"

-- | @Selector@ for @setNumberOfIntervalsDelivered:@
setNumberOfIntervalsDeliveredSelector :: Selector '[Id NSNumber] ()
setNumberOfIntervalsDeliveredSelector = mkSelector "setNumberOfIntervalsDelivered:"

-- | @Selector@ for @attributeId@
attributeIdSelector :: Selector '[] (Id NSNumber)
attributeIdSelector = mkSelector "attributeId"

-- | @Selector@ for @setAttributeId:@
setAttributeIdSelector :: Selector '[Id NSNumber] ()
setAttributeIdSelector = mkSelector "setAttributeId:"

-- | @Selector@ for @intervals@
intervalsSelector :: Selector '[] (Id NSArray)
intervalsSelector = mkSelector "intervals"

-- | @Selector@ for @setIntervals:@
setIntervalsSelector :: Selector '[Id NSArray] ()
setIntervalsSelector = mkSelector "setIntervals:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

