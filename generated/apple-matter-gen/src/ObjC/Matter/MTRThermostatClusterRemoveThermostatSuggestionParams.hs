{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterRemoveThermostatSuggestionParams@.
module ObjC.Matter.MTRThermostatClusterRemoveThermostatSuggestionParams
  ( MTRThermostatClusterRemoveThermostatSuggestionParams
  , IsMTRThermostatClusterRemoveThermostatSuggestionParams(..)
  , uniqueID
  , setUniqueID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setUniqueIDSelector
  , timedInvokeTimeoutMsSelector
  , uniqueIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- uniqueID@
uniqueID :: IsMTRThermostatClusterRemoveThermostatSuggestionParams mtrThermostatClusterRemoveThermostatSuggestionParams => mtrThermostatClusterRemoveThermostatSuggestionParams -> IO (Id NSNumber)
uniqueID mtrThermostatClusterRemoveThermostatSuggestionParams =
  sendMessage mtrThermostatClusterRemoveThermostatSuggestionParams uniqueIDSelector

-- | @- setUniqueID:@
setUniqueID :: (IsMTRThermostatClusterRemoveThermostatSuggestionParams mtrThermostatClusterRemoveThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterRemoveThermostatSuggestionParams -> value -> IO ()
setUniqueID mtrThermostatClusterRemoveThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterRemoveThermostatSuggestionParams setUniqueIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterRemoveThermostatSuggestionParams mtrThermostatClusterRemoveThermostatSuggestionParams => mtrThermostatClusterRemoveThermostatSuggestionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterRemoveThermostatSuggestionParams =
  sendMessage mtrThermostatClusterRemoveThermostatSuggestionParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterRemoveThermostatSuggestionParams mtrThermostatClusterRemoveThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterRemoveThermostatSuggestionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterRemoveThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterRemoveThermostatSuggestionParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterRemoveThermostatSuggestionParams mtrThermostatClusterRemoveThermostatSuggestionParams => mtrThermostatClusterRemoveThermostatSuggestionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterRemoveThermostatSuggestionParams =
  sendMessage mtrThermostatClusterRemoveThermostatSuggestionParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterRemoveThermostatSuggestionParams mtrThermostatClusterRemoveThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterRemoveThermostatSuggestionParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterRemoveThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterRemoveThermostatSuggestionParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueID@
uniqueIDSelector :: Selector '[] (Id NSNumber)
uniqueIDSelector = mkSelector "uniqueID"

-- | @Selector@ for @setUniqueID:@
setUniqueIDSelector :: Selector '[Id NSNumber] ()
setUniqueIDSelector = mkSelector "setUniqueID:"

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

