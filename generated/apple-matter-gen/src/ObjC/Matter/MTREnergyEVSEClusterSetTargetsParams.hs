{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterSetTargetsParams@.
module ObjC.Matter.MTREnergyEVSEClusterSetTargetsParams
  ( MTREnergyEVSEClusterSetTargetsParams
  , IsMTREnergyEVSEClusterSetTargetsParams(..)
  , chargingTargetSchedules
  , setChargingTargetSchedules
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , chargingTargetSchedulesSelector
  , serverSideProcessingTimeoutSelector
  , setChargingTargetSchedulesSelector
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

-- | @- chargingTargetSchedules@
chargingTargetSchedules :: IsMTREnergyEVSEClusterSetTargetsParams mtrEnergyEVSEClusterSetTargetsParams => mtrEnergyEVSEClusterSetTargetsParams -> IO (Id NSArray)
chargingTargetSchedules mtrEnergyEVSEClusterSetTargetsParams =
  sendMessage mtrEnergyEVSEClusterSetTargetsParams chargingTargetSchedulesSelector

-- | @- setChargingTargetSchedules:@
setChargingTargetSchedules :: (IsMTREnergyEVSEClusterSetTargetsParams mtrEnergyEVSEClusterSetTargetsParams, IsNSArray value) => mtrEnergyEVSEClusterSetTargetsParams -> value -> IO ()
setChargingTargetSchedules mtrEnergyEVSEClusterSetTargetsParams value =
  sendMessage mtrEnergyEVSEClusterSetTargetsParams setChargingTargetSchedulesSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTREnergyEVSEClusterSetTargetsParams mtrEnergyEVSEClusterSetTargetsParams => mtrEnergyEVSEClusterSetTargetsParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrEnergyEVSEClusterSetTargetsParams =
  sendMessage mtrEnergyEVSEClusterSetTargetsParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTREnergyEVSEClusterSetTargetsParams mtrEnergyEVSEClusterSetTargetsParams, IsNSNumber value) => mtrEnergyEVSEClusterSetTargetsParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrEnergyEVSEClusterSetTargetsParams value =
  sendMessage mtrEnergyEVSEClusterSetTargetsParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTREnergyEVSEClusterSetTargetsParams mtrEnergyEVSEClusterSetTargetsParams => mtrEnergyEVSEClusterSetTargetsParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrEnergyEVSEClusterSetTargetsParams =
  sendMessage mtrEnergyEVSEClusterSetTargetsParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTREnergyEVSEClusterSetTargetsParams mtrEnergyEVSEClusterSetTargetsParams, IsNSNumber value) => mtrEnergyEVSEClusterSetTargetsParams -> value -> IO ()
setServerSideProcessingTimeout mtrEnergyEVSEClusterSetTargetsParams value =
  sendMessage mtrEnergyEVSEClusterSetTargetsParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @chargingTargetSchedules@
chargingTargetSchedulesSelector :: Selector '[] (Id NSArray)
chargingTargetSchedulesSelector = mkSelector "chargingTargetSchedules"

-- | @Selector@ for @setChargingTargetSchedules:@
setChargingTargetSchedulesSelector :: Selector '[Id NSArray] ()
setChargingTargetSchedulesSelector = mkSelector "setChargingTargetSchedules:"

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

