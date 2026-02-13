{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSetpointRaiseLowerParams@.
module ObjC.Matter.MTRThermostatClusterSetpointRaiseLowerParams
  ( MTRThermostatClusterSetpointRaiseLowerParams
  , IsMTRThermostatClusterSetpointRaiseLowerParams(..)
  , mode
  , setMode
  , amount
  , setAmount
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , amountSelector
  , modeSelector
  , serverSideProcessingTimeoutSelector
  , setAmountSelector
  , setModeSelector
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

-- | @- mode@
mode :: IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams => mtrThermostatClusterSetpointRaiseLowerParams -> IO (Id NSNumber)
mode mtrThermostatClusterSetpointRaiseLowerParams =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams modeSelector

-- | @- setMode:@
setMode :: (IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams, IsNSNumber value) => mtrThermostatClusterSetpointRaiseLowerParams -> value -> IO ()
setMode mtrThermostatClusterSetpointRaiseLowerParams value =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams setModeSelector (toNSNumber value)

-- | @- amount@
amount :: IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams => mtrThermostatClusterSetpointRaiseLowerParams -> IO (Id NSNumber)
amount mtrThermostatClusterSetpointRaiseLowerParams =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams amountSelector

-- | @- setAmount:@
setAmount :: (IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams, IsNSNumber value) => mtrThermostatClusterSetpointRaiseLowerParams -> value -> IO ()
setAmount mtrThermostatClusterSetpointRaiseLowerParams value =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams setAmountSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams => mtrThermostatClusterSetpointRaiseLowerParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterSetpointRaiseLowerParams =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams, IsNSNumber value) => mtrThermostatClusterSetpointRaiseLowerParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterSetpointRaiseLowerParams value =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams => mtrThermostatClusterSetpointRaiseLowerParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterSetpointRaiseLowerParams =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterSetpointRaiseLowerParams mtrThermostatClusterSetpointRaiseLowerParams, IsNSNumber value) => mtrThermostatClusterSetpointRaiseLowerParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterSetpointRaiseLowerParams value =
  sendMessage mtrThermostatClusterSetpointRaiseLowerParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @mode@
modeSelector :: Selector '[] (Id NSNumber)
modeSelector = mkSelector "mode"

-- | @Selector@ for @setMode:@
setModeSelector :: Selector '[Id NSNumber] ()
setModeSelector = mkSelector "setMode:"

-- | @Selector@ for @amount@
amountSelector :: Selector '[] (Id NSNumber)
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector '[Id NSNumber] ()
setAmountSelector = mkSelector "setAmount:"

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

