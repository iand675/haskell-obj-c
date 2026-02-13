{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterSetWeeklyScheduleParams@.
module ObjC.Matter.MTRThermostatClusterSetWeeklyScheduleParams
  ( MTRThermostatClusterSetWeeklyScheduleParams
  , IsMTRThermostatClusterSetWeeklyScheduleParams(..)
  , numberOfTransitionsForSequence
  , setNumberOfTransitionsForSequence
  , dayOfWeekForSequence
  , setDayOfWeekForSequence
  , modeForSequence
  , setModeForSequence
  , transitions
  , setTransitions
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , dayOfWeekForSequenceSelector
  , modeForSequenceSelector
  , numberOfTransitionsForSequenceSelector
  , serverSideProcessingTimeoutSelector
  , setDayOfWeekForSequenceSelector
  , setModeForSequenceSelector
  , setNumberOfTransitionsForSequenceSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransitionsSelector
  , timedInvokeTimeoutMsSelector
  , transitionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- numberOfTransitionsForSequence@
numberOfTransitionsForSequence :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
numberOfTransitionsForSequence mtrThermostatClusterSetWeeklyScheduleParams =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams numberOfTransitionsForSequenceSelector

-- | @- setNumberOfTransitionsForSequence:@
setNumberOfTransitionsForSequence :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setNumberOfTransitionsForSequence mtrThermostatClusterSetWeeklyScheduleParams value =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams setNumberOfTransitionsForSequenceSelector (toNSNumber value)

-- | @- dayOfWeekForSequence@
dayOfWeekForSequence :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
dayOfWeekForSequence mtrThermostatClusterSetWeeklyScheduleParams =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams dayOfWeekForSequenceSelector

-- | @- setDayOfWeekForSequence:@
setDayOfWeekForSequence :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setDayOfWeekForSequence mtrThermostatClusterSetWeeklyScheduleParams value =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams setDayOfWeekForSequenceSelector (toNSNumber value)

-- | @- modeForSequence@
modeForSequence :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
modeForSequence mtrThermostatClusterSetWeeklyScheduleParams =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams modeForSequenceSelector

-- | @- setModeForSequence:@
setModeForSequence :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setModeForSequence mtrThermostatClusterSetWeeklyScheduleParams value =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams setModeForSequenceSelector (toNSNumber value)

-- | @- transitions@
transitions :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSArray)
transitions mtrThermostatClusterSetWeeklyScheduleParams =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams transitionsSelector

-- | @- setTransitions:@
setTransitions :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSArray value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setTransitions mtrThermostatClusterSetWeeklyScheduleParams value =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams setTransitionsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterSetWeeklyScheduleParams =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterSetWeeklyScheduleParams value =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams => mtrThermostatClusterSetWeeklyScheduleParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterSetWeeklyScheduleParams =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterSetWeeklyScheduleParams mtrThermostatClusterSetWeeklyScheduleParams, IsNSNumber value) => mtrThermostatClusterSetWeeklyScheduleParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterSetWeeklyScheduleParams value =
  sendMessage mtrThermostatClusterSetWeeklyScheduleParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @numberOfTransitionsForSequence@
numberOfTransitionsForSequenceSelector :: Selector '[] (Id NSNumber)
numberOfTransitionsForSequenceSelector = mkSelector "numberOfTransitionsForSequence"

-- | @Selector@ for @setNumberOfTransitionsForSequence:@
setNumberOfTransitionsForSequenceSelector :: Selector '[Id NSNumber] ()
setNumberOfTransitionsForSequenceSelector = mkSelector "setNumberOfTransitionsForSequence:"

-- | @Selector@ for @dayOfWeekForSequence@
dayOfWeekForSequenceSelector :: Selector '[] (Id NSNumber)
dayOfWeekForSequenceSelector = mkSelector "dayOfWeekForSequence"

-- | @Selector@ for @setDayOfWeekForSequence:@
setDayOfWeekForSequenceSelector :: Selector '[Id NSNumber] ()
setDayOfWeekForSequenceSelector = mkSelector "setDayOfWeekForSequence:"

-- | @Selector@ for @modeForSequence@
modeForSequenceSelector :: Selector '[] (Id NSNumber)
modeForSequenceSelector = mkSelector "modeForSequence"

-- | @Selector@ for @setModeForSequence:@
setModeForSequenceSelector :: Selector '[Id NSNumber] ()
setModeForSequenceSelector = mkSelector "setModeForSequence:"

-- | @Selector@ for @transitions@
transitionsSelector :: Selector '[] (Id NSArray)
transitionsSelector = mkSelector "transitions"

-- | @Selector@ for @setTransitions:@
setTransitionsSelector :: Selector '[Id NSArray] ()
setTransitionsSelector = mkSelector "setTransitions:"

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

