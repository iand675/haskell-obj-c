{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterGetWeeklyScheduleResponseParams@.
module ObjC.Matter.MTRThermostatClusterGetWeeklyScheduleResponseParams
  ( MTRThermostatClusterGetWeeklyScheduleResponseParams
  , IsMTRThermostatClusterGetWeeklyScheduleResponseParams(..)
  , initWithResponseValue_error
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
  , dayOfWeekForSequenceSelector
  , initWithResponseValue_errorSelector
  , modeForSequenceSelector
  , numberOfTransitionsForSequenceSelector
  , setDayOfWeekForSequenceSelector
  , setModeForSequenceSelector
  , setNumberOfTransitionsForSequenceSelector
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

-- | Initialize an MTRThermostatClusterGetWeeklyScheduleResponseParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSDictionary responseValue, IsNSError error_) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> responseValue -> error_ -> IO (Id MTRThermostatClusterGetWeeklyScheduleResponseParams)
initWithResponseValue_error mtrThermostatClusterGetWeeklyScheduleResponseParams responseValue error_ =
  sendOwnedMessage mtrThermostatClusterGetWeeklyScheduleResponseParams initWithResponseValue_errorSelector (toNSDictionary responseValue) (toNSError error_)

-- | @- numberOfTransitionsForSequence@
numberOfTransitionsForSequence :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
numberOfTransitionsForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams numberOfTransitionsForSequenceSelector

-- | @- setNumberOfTransitionsForSequence:@
setNumberOfTransitionsForSequence :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setNumberOfTransitionsForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams value =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams setNumberOfTransitionsForSequenceSelector (toNSNumber value)

-- | @- dayOfWeekForSequence@
dayOfWeekForSequence :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
dayOfWeekForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams dayOfWeekForSequenceSelector

-- | @- setDayOfWeekForSequence:@
setDayOfWeekForSequence :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setDayOfWeekForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams value =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams setDayOfWeekForSequenceSelector (toNSNumber value)

-- | @- modeForSequence@
modeForSequence :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
modeForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams modeForSequenceSelector

-- | @- setModeForSequence:@
setModeForSequence :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setModeForSequence mtrThermostatClusterGetWeeklyScheduleResponseParams value =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams setModeForSequenceSelector (toNSNumber value)

-- | @- transitions@
transitions :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSArray)
transitions mtrThermostatClusterGetWeeklyScheduleResponseParams =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams transitionsSelector

-- | @- setTransitions:@
setTransitions :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSArray value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setTransitions mtrThermostatClusterGetWeeklyScheduleResponseParams value =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams setTransitionsSelector (toNSArray value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams => mtrThermostatClusterGetWeeklyScheduleResponseParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterGetWeeklyScheduleResponseParams =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterGetWeeklyScheduleResponseParams mtrThermostatClusterGetWeeklyScheduleResponseParams, IsNSNumber value) => mtrThermostatClusterGetWeeklyScheduleResponseParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterGetWeeklyScheduleResponseParams value =
  sendMessage mtrThermostatClusterGetWeeklyScheduleResponseParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector '[Id NSDictionary, Id NSError] (Id MTRThermostatClusterGetWeeklyScheduleResponseParams)
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

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

