{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSetActiveScheduleRequestParams@.
module ObjC.Matter.MTRThermostatClusterSetActiveScheduleRequestParams
  ( MTRThermostatClusterSetActiveScheduleRequestParams
  , IsMTRThermostatClusterSetActiveScheduleRequestParams(..)
  , scheduleHandle
  , setScheduleHandle
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , scheduleHandleSelector
  , serverSideProcessingTimeoutSelector
  , setScheduleHandleSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
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

-- | @- scheduleHandle@
scheduleHandle :: IsMTRThermostatClusterSetActiveScheduleRequestParams mtrThermostatClusterSetActiveScheduleRequestParams => mtrThermostatClusterSetActiveScheduleRequestParams -> IO (Id NSData)
scheduleHandle mtrThermostatClusterSetActiveScheduleRequestParams =
  sendMessage mtrThermostatClusterSetActiveScheduleRequestParams scheduleHandleSelector

-- | @- setScheduleHandle:@
setScheduleHandle :: (IsMTRThermostatClusterSetActiveScheduleRequestParams mtrThermostatClusterSetActiveScheduleRequestParams, IsNSData value) => mtrThermostatClusterSetActiveScheduleRequestParams -> value -> IO ()
setScheduleHandle mtrThermostatClusterSetActiveScheduleRequestParams value =
  sendMessage mtrThermostatClusterSetActiveScheduleRequestParams setScheduleHandleSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterSetActiveScheduleRequestParams mtrThermostatClusterSetActiveScheduleRequestParams => mtrThermostatClusterSetActiveScheduleRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterSetActiveScheduleRequestParams =
  sendMessage mtrThermostatClusterSetActiveScheduleRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterSetActiveScheduleRequestParams mtrThermostatClusterSetActiveScheduleRequestParams, IsNSNumber value) => mtrThermostatClusterSetActiveScheduleRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterSetActiveScheduleRequestParams value =
  sendMessage mtrThermostatClusterSetActiveScheduleRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterSetActiveScheduleRequestParams mtrThermostatClusterSetActiveScheduleRequestParams => mtrThermostatClusterSetActiveScheduleRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterSetActiveScheduleRequestParams =
  sendMessage mtrThermostatClusterSetActiveScheduleRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterSetActiveScheduleRequestParams mtrThermostatClusterSetActiveScheduleRequestParams, IsNSNumber value) => mtrThermostatClusterSetActiveScheduleRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterSetActiveScheduleRequestParams value =
  sendMessage mtrThermostatClusterSetActiveScheduleRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @scheduleHandle@
scheduleHandleSelector :: Selector '[] (Id NSData)
scheduleHandleSelector = mkSelector "scheduleHandle"

-- | @Selector@ for @setScheduleHandle:@
setScheduleHandleSelector :: Selector '[Id NSData] ()
setScheduleHandleSelector = mkSelector "setScheduleHandle:"

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

