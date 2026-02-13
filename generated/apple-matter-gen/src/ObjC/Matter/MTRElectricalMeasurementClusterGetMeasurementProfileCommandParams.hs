{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams@.
module ObjC.Matter.MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams
  ( MTRElectricalMeasurementClusterGetMeasurementProfileCommandParams
  , IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams(..)
  , attributeId
  , setAttributeId
  , startTime
  , setStartTime
  , numberOfIntervals
  , setNumberOfIntervals
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , attributeIdSelector
  , numberOfIntervalsSelector
  , serverSideProcessingTimeoutSelector
  , setAttributeIdSelector
  , setNumberOfIntervalsSelector
  , setServerSideProcessingTimeoutSelector
  , setStartTimeSelector
  , setTimedInvokeTimeoutMsSelector
  , startTimeSelector
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

-- | @- attributeId@
attributeId :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
attributeId mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams attributeIdSelector

-- | @- setAttributeId:@
setAttributeId :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setAttributeId mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams setAttributeIdSelector (toNSNumber value)

-- | @- startTime@
startTime :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
startTime mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams startTimeSelector

-- | @- setStartTime:@
setStartTime :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setStartTime mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams setStartTimeSelector (toNSNumber value)

-- | @- numberOfIntervals@
numberOfIntervals :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
numberOfIntervals mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams numberOfIntervalsSelector

-- | @- setNumberOfIntervals:@
setNumberOfIntervals :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setNumberOfIntervals mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams setNumberOfIntervalsSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRElectricalMeasurementClusterGetMeasurementProfileCommandParams mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams, IsNSNumber value) => mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams -> value -> IO ()
setServerSideProcessingTimeout mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams value =
  sendMessage mtrElectricalMeasurementClusterGetMeasurementProfileCommandParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @attributeId@
attributeIdSelector :: Selector '[] (Id NSNumber)
attributeIdSelector = mkSelector "attributeId"

-- | @Selector@ for @setAttributeId:@
setAttributeIdSelector :: Selector '[Id NSNumber] ()
setAttributeIdSelector = mkSelector "setAttributeId:"

-- | @Selector@ for @startTime@
startTimeSelector :: Selector '[] (Id NSNumber)
startTimeSelector = mkSelector "startTime"

-- | @Selector@ for @setStartTime:@
setStartTimeSelector :: Selector '[Id NSNumber] ()
setStartTimeSelector = mkSelector "setStartTime:"

-- | @Selector@ for @numberOfIntervals@
numberOfIntervalsSelector :: Selector '[] (Id NSNumber)
numberOfIntervalsSelector = mkSelector "numberOfIntervals"

-- | @Selector@ for @setNumberOfIntervals:@
setNumberOfIntervalsSelector :: Selector '[Id NSNumber] ()
setNumberOfIntervalsSelector = mkSelector "setNumberOfIntervals:"

-- | @Selector@ for @timedInvokeTimeoutMs@
timedInvokeTimeoutMsSelector :: Selector '[] (Id NSNumber)
timedInvokeTimeoutMsSelector = mkSelector "timedInvokeTimeoutMs"

-- | @Selector@ for @setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMsSelector :: Selector '[Id NSNumber] ()
setTimedInvokeTimeoutMsSelector = mkSelector "setTimedInvokeTimeoutMs:"

-- | @Selector@ for @serverSideProcessingTimeout@
serverSideProcessingTimeoutSelector :: Selector '[] (Id NSNumber)
serverSideProcessingTimeoutSelector = mkSelector "serverSideProcessingTimeout"

-- | @Selector@ for @setServerSideProcessingTimeout:@
setServerSideProcessingTimeoutSelector :: Selector '[Id NSNumber] ()
setServerSideProcessingTimeoutSelector = mkSelector "setServerSideProcessingTimeout:"

