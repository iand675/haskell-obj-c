{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPauseRequestParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPauseRequestParams
  ( MTRDeviceEnergyManagementClusterPauseRequestParams
  , IsMTRDeviceEnergyManagementClusterPauseRequestParams(..)
  , duration
  , setDuration
  , cause
  , setCause
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , causeSelector
  , durationSelector
  , serverSideProcessingTimeoutSelector
  , setCauseSelector
  , setDurationSelector
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

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterPauseRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterPauseRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams setDurationSelector (toNSNumber value)

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPauseRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPauseRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams setCauseSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPauseRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPauseRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams => mtrDeviceEnergyManagementClusterPauseRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterPauseRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterPauseRequestParams mtrDeviceEnergyManagementClusterPauseRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPauseRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterPauseRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPauseRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @duration@
durationSelector :: Selector '[] (Id NSNumber)
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[Id NSNumber] ()
setDurationSelector = mkSelector "setDuration:"

-- | @Selector@ for @cause@
causeSelector :: Selector '[] (Id NSNumber)
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector '[Id NSNumber] ()
setCauseSelector = mkSelector "setCause:"

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

