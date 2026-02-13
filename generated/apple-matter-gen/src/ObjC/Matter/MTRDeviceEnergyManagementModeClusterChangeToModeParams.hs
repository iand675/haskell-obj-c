{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementModeClusterChangeToModeParams@.
module ObjC.Matter.MTRDeviceEnergyManagementModeClusterChangeToModeParams
  ( MTRDeviceEnergyManagementModeClusterChangeToModeParams
  , IsMTRDeviceEnergyManagementModeClusterChangeToModeParams(..)
  , newMode
  , setNewMode
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , newModeSelector
  , serverSideProcessingTimeoutSelector
  , setNewModeSelector
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

-- | @- newMode@
newMode :: IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> IO (Id NSNumber)
newMode mtrDeviceEnergyManagementModeClusterChangeToModeParams =
  sendOwnedMessage mtrDeviceEnergyManagementModeClusterChangeToModeParams newModeSelector

-- | @- setNewMode:@
setNewMode :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> value -> IO ()
setNewMode mtrDeviceEnergyManagementModeClusterChangeToModeParams value =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeParams setNewModeSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementModeClusterChangeToModeParams =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementModeClusterChangeToModeParams value =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementModeClusterChangeToModeParams =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementModeClusterChangeToModeParams mtrDeviceEnergyManagementModeClusterChangeToModeParams, IsNSNumber value) => mtrDeviceEnergyManagementModeClusterChangeToModeParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementModeClusterChangeToModeParams value =
  sendMessage mtrDeviceEnergyManagementModeClusterChangeToModeParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @newMode@
newModeSelector :: Selector '[] (Id NSNumber)
newModeSelector = mkSelector "newMode"

-- | @Selector@ for @setNewMode:@
setNewModeSelector :: Selector '[Id NSNumber] ()
setNewModeSelector = mkSelector "setNewMode:"

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

