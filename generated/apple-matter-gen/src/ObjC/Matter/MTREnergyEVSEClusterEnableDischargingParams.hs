{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnableDischargingParams@.
module ObjC.Matter.MTREnergyEVSEClusterEnableDischargingParams
  ( MTREnergyEVSEClusterEnableDischargingParams
  , IsMTREnergyEVSEClusterEnableDischargingParams(..)
  , dischargingEnabledUntil
  , setDischargingEnabledUntil
  , maximumDischargeCurrent
  , setMaximumDischargeCurrent
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dischargingEnabledUntilSelector
  , maximumDischargeCurrentSelector
  , serverSideProcessingTimeoutSelector
  , setDischargingEnabledUntilSelector
  , setMaximumDischargeCurrentSelector
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

-- | @- dischargingEnabledUntil@
dischargingEnabledUntil :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
dischargingEnabledUntil mtrEnergyEVSEClusterEnableDischargingParams =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams dischargingEnabledUntilSelector

-- | @- setDischargingEnabledUntil:@
setDischargingEnabledUntil :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setDischargingEnabledUntil mtrEnergyEVSEClusterEnableDischargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams setDischargingEnabledUntilSelector (toNSNumber value)

-- | @- maximumDischargeCurrent@
maximumDischargeCurrent :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
maximumDischargeCurrent mtrEnergyEVSEClusterEnableDischargingParams =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams maximumDischargeCurrentSelector

-- | @- setMaximumDischargeCurrent:@
setMaximumDischargeCurrent :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setMaximumDischargeCurrent mtrEnergyEVSEClusterEnableDischargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams setMaximumDischargeCurrentSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrEnergyEVSEClusterEnableDischargingParams =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrEnergyEVSEClusterEnableDischargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams => mtrEnergyEVSEClusterEnableDischargingParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrEnergyEVSEClusterEnableDischargingParams =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTREnergyEVSEClusterEnableDischargingParams mtrEnergyEVSEClusterEnableDischargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableDischargingParams -> value -> IO ()
setServerSideProcessingTimeout mtrEnergyEVSEClusterEnableDischargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableDischargingParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @dischargingEnabledUntil@
dischargingEnabledUntilSelector :: Selector '[] (Id NSNumber)
dischargingEnabledUntilSelector = mkSelector "dischargingEnabledUntil"

-- | @Selector@ for @setDischargingEnabledUntil:@
setDischargingEnabledUntilSelector :: Selector '[Id NSNumber] ()
setDischargingEnabledUntilSelector = mkSelector "setDischargingEnabledUntil:"

-- | @Selector@ for @maximumDischargeCurrent@
maximumDischargeCurrentSelector :: Selector '[] (Id NSNumber)
maximumDischargeCurrentSelector = mkSelector "maximumDischargeCurrent"

-- | @Selector@ for @setMaximumDischargeCurrent:@
setMaximumDischargeCurrentSelector :: Selector '[Id NSNumber] ()
setMaximumDischargeCurrentSelector = mkSelector "setMaximumDischargeCurrent:"

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

