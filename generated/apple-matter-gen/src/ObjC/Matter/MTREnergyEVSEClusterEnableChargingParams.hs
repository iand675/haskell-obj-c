{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyEVSEClusterEnableChargingParams@.
module ObjC.Matter.MTREnergyEVSEClusterEnableChargingParams
  ( MTREnergyEVSEClusterEnableChargingParams
  , IsMTREnergyEVSEClusterEnableChargingParams(..)
  , chargingEnabledUntil
  , setChargingEnabledUntil
  , minimumChargeCurrent
  , setMinimumChargeCurrent
  , maximumChargeCurrent
  , setMaximumChargeCurrent
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , chargingEnabledUntilSelector
  , maximumChargeCurrentSelector
  , minimumChargeCurrentSelector
  , serverSideProcessingTimeoutSelector
  , setChargingEnabledUntilSelector
  , setMaximumChargeCurrentSelector
  , setMinimumChargeCurrentSelector
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

-- | @- chargingEnabledUntil@
chargingEnabledUntil :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
chargingEnabledUntil mtrEnergyEVSEClusterEnableChargingParams =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams chargingEnabledUntilSelector

-- | @- setChargingEnabledUntil:@
setChargingEnabledUntil :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setChargingEnabledUntil mtrEnergyEVSEClusterEnableChargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams setChargingEnabledUntilSelector (toNSNumber value)

-- | @- minimumChargeCurrent@
minimumChargeCurrent :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
minimumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams minimumChargeCurrentSelector

-- | @- setMinimumChargeCurrent:@
setMinimumChargeCurrent :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setMinimumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams setMinimumChargeCurrentSelector (toNSNumber value)

-- | @- maximumChargeCurrent@
maximumChargeCurrent :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
maximumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams maximumChargeCurrentSelector

-- | @- setMaximumChargeCurrent:@
setMaximumChargeCurrent :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setMaximumChargeCurrent mtrEnergyEVSEClusterEnableChargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams setMaximumChargeCurrentSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrEnergyEVSEClusterEnableChargingParams =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrEnergyEVSEClusterEnableChargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams => mtrEnergyEVSEClusterEnableChargingParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrEnergyEVSEClusterEnableChargingParams =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTREnergyEVSEClusterEnableChargingParams mtrEnergyEVSEClusterEnableChargingParams, IsNSNumber value) => mtrEnergyEVSEClusterEnableChargingParams -> value -> IO ()
setServerSideProcessingTimeout mtrEnergyEVSEClusterEnableChargingParams value =
  sendMessage mtrEnergyEVSEClusterEnableChargingParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @chargingEnabledUntil@
chargingEnabledUntilSelector :: Selector '[] (Id NSNumber)
chargingEnabledUntilSelector = mkSelector "chargingEnabledUntil"

-- | @Selector@ for @setChargingEnabledUntil:@
setChargingEnabledUntilSelector :: Selector '[Id NSNumber] ()
setChargingEnabledUntilSelector = mkSelector "setChargingEnabledUntil:"

-- | @Selector@ for @minimumChargeCurrent@
minimumChargeCurrentSelector :: Selector '[] (Id NSNumber)
minimumChargeCurrentSelector = mkSelector "minimumChargeCurrent"

-- | @Selector@ for @setMinimumChargeCurrent:@
setMinimumChargeCurrentSelector :: Selector '[Id NSNumber] ()
setMinimumChargeCurrentSelector = mkSelector "setMinimumChargeCurrent:"

-- | @Selector@ for @maximumChargeCurrent@
maximumChargeCurrentSelector :: Selector '[] (Id NSNumber)
maximumChargeCurrentSelector = mkSelector "maximumChargeCurrent"

-- | @Selector@ for @setMaximumChargeCurrent:@
setMaximumChargeCurrentSelector :: Selector '[Id NSNumber] ()
setMaximumChargeCurrentSelector = mkSelector "setMaximumChargeCurrent:"

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

