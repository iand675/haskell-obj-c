{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRZoneManagementClusterCreateOrUpdateTriggerParams@.
module ObjC.Matter.MTRZoneManagementClusterCreateOrUpdateTriggerParams
  ( MTRZoneManagementClusterCreateOrUpdateTriggerParams
  , IsMTRZoneManagementClusterCreateOrUpdateTriggerParams(..)
  , trigger
  , setTrigger
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTriggerSelector
  , timedInvokeTimeoutMsSelector
  , triggerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- trigger@
trigger :: IsMTRZoneManagementClusterCreateOrUpdateTriggerParams mtrZoneManagementClusterCreateOrUpdateTriggerParams => mtrZoneManagementClusterCreateOrUpdateTriggerParams -> IO (Id MTRZoneManagementClusterZoneTriggerControlStruct)
trigger mtrZoneManagementClusterCreateOrUpdateTriggerParams =
  sendMessage mtrZoneManagementClusterCreateOrUpdateTriggerParams triggerSelector

-- | @- setTrigger:@
setTrigger :: (IsMTRZoneManagementClusterCreateOrUpdateTriggerParams mtrZoneManagementClusterCreateOrUpdateTriggerParams, IsMTRZoneManagementClusterZoneTriggerControlStruct value) => mtrZoneManagementClusterCreateOrUpdateTriggerParams -> value -> IO ()
setTrigger mtrZoneManagementClusterCreateOrUpdateTriggerParams value =
  sendMessage mtrZoneManagementClusterCreateOrUpdateTriggerParams setTriggerSelector (toMTRZoneManagementClusterZoneTriggerControlStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRZoneManagementClusterCreateOrUpdateTriggerParams mtrZoneManagementClusterCreateOrUpdateTriggerParams => mtrZoneManagementClusterCreateOrUpdateTriggerParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrZoneManagementClusterCreateOrUpdateTriggerParams =
  sendMessage mtrZoneManagementClusterCreateOrUpdateTriggerParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRZoneManagementClusterCreateOrUpdateTriggerParams mtrZoneManagementClusterCreateOrUpdateTriggerParams, IsNSNumber value) => mtrZoneManagementClusterCreateOrUpdateTriggerParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrZoneManagementClusterCreateOrUpdateTriggerParams value =
  sendMessage mtrZoneManagementClusterCreateOrUpdateTriggerParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRZoneManagementClusterCreateOrUpdateTriggerParams mtrZoneManagementClusterCreateOrUpdateTriggerParams => mtrZoneManagementClusterCreateOrUpdateTriggerParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrZoneManagementClusterCreateOrUpdateTriggerParams =
  sendMessage mtrZoneManagementClusterCreateOrUpdateTriggerParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRZoneManagementClusterCreateOrUpdateTriggerParams mtrZoneManagementClusterCreateOrUpdateTriggerParams, IsNSNumber value) => mtrZoneManagementClusterCreateOrUpdateTriggerParams -> value -> IO ()
setServerSideProcessingTimeout mtrZoneManagementClusterCreateOrUpdateTriggerParams value =
  sendMessage mtrZoneManagementClusterCreateOrUpdateTriggerParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @trigger@
triggerSelector :: Selector '[] (Id MTRZoneManagementClusterZoneTriggerControlStruct)
triggerSelector = mkSelector "trigger"

-- | @Selector@ for @setTrigger:@
setTriggerSelector :: Selector '[Id MTRZoneManagementClusterZoneTriggerControlStruct] ()
setTriggerSelector = mkSelector "setTrigger:"

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

