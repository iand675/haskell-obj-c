{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterPowerAdjustRequestParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterPowerAdjustRequestParams
  ( MTRDeviceEnergyManagementClusterPowerAdjustRequestParams
  , IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams(..)
  , power
  , setPower
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
  , powerSelector
  , serverSideProcessingTimeoutSelector
  , setCauseSelector
  , setDurationSelector
  , setPowerSelector
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

-- | @- power@
power :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
power mtrDeviceEnergyManagementClusterPowerAdjustRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams powerSelector

-- | @- setPower:@
setPower :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setPower mtrDeviceEnergyManagementClusterPowerAdjustRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams setPowerSelector (toNSNumber value)

-- | @- duration@
duration :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
duration mtrDeviceEnergyManagementClusterPowerAdjustRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams durationSelector

-- | @- setDuration:@
setDuration :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setDuration mtrDeviceEnergyManagementClusterPowerAdjustRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams setDurationSelector (toNSNumber value)

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterPowerAdjustRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterPowerAdjustRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams setCauseSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPowerAdjustRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterPowerAdjustRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterPowerAdjustRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterPowerAdjustRequestParams mtrDeviceEnergyManagementClusterPowerAdjustRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterPowerAdjustRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterPowerAdjustRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterPowerAdjustRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @power@
powerSelector :: Selector '[] (Id NSNumber)
powerSelector = mkSelector "power"

-- | @Selector@ for @setPower:@
setPowerSelector :: Selector '[Id NSNumber] ()
setPowerSelector = mkSelector "setPower:"

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

