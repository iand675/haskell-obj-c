{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAddThermostatSuggestionParams@.
module ObjC.Matter.MTRThermostatClusterAddThermostatSuggestionParams
  ( MTRThermostatClusterAddThermostatSuggestionParams
  , IsMTRThermostatClusterAddThermostatSuggestionParams(..)
  , presetHandle
  , setPresetHandle
  , effectiveTime
  , setEffectiveTime
  , expirationInMinutes
  , setExpirationInMinutes
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , effectiveTimeSelector
  , expirationInMinutesSelector
  , presetHandleSelector
  , serverSideProcessingTimeoutSelector
  , setEffectiveTimeSelector
  , setExpirationInMinutesSelector
  , setPresetHandleSelector
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

-- | @- presetHandle@
presetHandle :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSData)
presetHandle mtrThermostatClusterAddThermostatSuggestionParams =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams presetHandleSelector

-- | @- setPresetHandle:@
setPresetHandle :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSData value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setPresetHandle mtrThermostatClusterAddThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams setPresetHandleSelector (toNSData value)

-- | @- effectiveTime@
effectiveTime :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
effectiveTime mtrThermostatClusterAddThermostatSuggestionParams =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams effectiveTimeSelector

-- | @- setEffectiveTime:@
setEffectiveTime :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setEffectiveTime mtrThermostatClusterAddThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams setEffectiveTimeSelector (toNSNumber value)

-- | @- expirationInMinutes@
expirationInMinutes :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
expirationInMinutes mtrThermostatClusterAddThermostatSuggestionParams =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams expirationInMinutesSelector

-- | @- setExpirationInMinutes:@
setExpirationInMinutes :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setExpirationInMinutes mtrThermostatClusterAddThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams setExpirationInMinutesSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterAddThermostatSuggestionParams =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterAddThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams => mtrThermostatClusterAddThermostatSuggestionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterAddThermostatSuggestionParams =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterAddThermostatSuggestionParams mtrThermostatClusterAddThermostatSuggestionParams, IsNSNumber value) => mtrThermostatClusterAddThermostatSuggestionParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterAddThermostatSuggestionParams value =
  sendMessage mtrThermostatClusterAddThermostatSuggestionParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @presetHandle@
presetHandleSelector :: Selector '[] (Id NSData)
presetHandleSelector = mkSelector "presetHandle"

-- | @Selector@ for @setPresetHandle:@
setPresetHandleSelector :: Selector '[Id NSData] ()
setPresetHandleSelector = mkSelector "setPresetHandle:"

-- | @Selector@ for @effectiveTime@
effectiveTimeSelector :: Selector '[] (Id NSNumber)
effectiveTimeSelector = mkSelector "effectiveTime"

-- | @Selector@ for @setEffectiveTime:@
setEffectiveTimeSelector :: Selector '[Id NSNumber] ()
setEffectiveTimeSelector = mkSelector "setEffectiveTime:"

-- | @Selector@ for @expirationInMinutes@
expirationInMinutesSelector :: Selector '[] (Id NSNumber)
expirationInMinutesSelector = mkSelector "expirationInMinutes"

-- | @Selector@ for @setExpirationInMinutes:@
setExpirationInMinutesSelector :: Selector '[Id NSNumber] ()
setExpirationInMinutesSelector = mkSelector "setExpirationInMinutes:"

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

