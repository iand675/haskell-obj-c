{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterModifyForecastRequestParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterModifyForecastRequestParams
  ( MTRDeviceEnergyManagementClusterModifyForecastRequestParams
  , IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams(..)
  , forecastID
  , setForecastID
  , slotAdjustments
  , setSlotAdjustments
  , cause
  , setCause
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , causeSelector
  , forecastIDSelector
  , serverSideProcessingTimeoutSelector
  , setCauseSelector
  , setForecastIDSelector
  , setServerSideProcessingTimeoutSelector
  , setSlotAdjustmentsSelector
  , setTimedInvokeTimeoutMsSelector
  , slotAdjustmentsSelector
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

-- | @- forecastID@
forecastID :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
forecastID mtrDeviceEnergyManagementClusterModifyForecastRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams forecastIDSelector

-- | @- setForecastID:@
setForecastID :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setForecastID mtrDeviceEnergyManagementClusterModifyForecastRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams setForecastIDSelector (toNSNumber value)

-- | @- slotAdjustments@
slotAdjustments :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSArray)
slotAdjustments mtrDeviceEnergyManagementClusterModifyForecastRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams slotAdjustmentsSelector

-- | @- setSlotAdjustments:@
setSlotAdjustments :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSArray value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setSlotAdjustments mtrDeviceEnergyManagementClusterModifyForecastRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams setSlotAdjustmentsSelector (toNSArray value)

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterModifyForecastRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterModifyForecastRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams setCauseSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterModifyForecastRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterModifyForecastRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterModifyForecastRequestParams =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterModifyForecastRequestParams mtrDeviceEnergyManagementClusterModifyForecastRequestParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterModifyForecastRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterModifyForecastRequestParams value =
  sendMessage mtrDeviceEnergyManagementClusterModifyForecastRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forecastID@
forecastIDSelector :: Selector '[] (Id NSNumber)
forecastIDSelector = mkSelector "forecastID"

-- | @Selector@ for @setForecastID:@
setForecastIDSelector :: Selector '[Id NSNumber] ()
setForecastIDSelector = mkSelector "setForecastID:"

-- | @Selector@ for @slotAdjustments@
slotAdjustmentsSelector :: Selector '[] (Id NSArray)
slotAdjustmentsSelector = mkSelector "slotAdjustments"

-- | @Selector@ for @setSlotAdjustments:@
setSlotAdjustmentsSelector :: Selector '[Id NSArray] ()
setSlotAdjustmentsSelector = mkSelector "setSlotAdjustments:"

-- | @Selector@ for @cause@
causeSelector :: Selector '[] (Id NSNumber)
causeSelector = mkSelector "cause"

-- | @Selector@ for @setCause:@
setCauseSelector :: Selector '[Id NSNumber] ()
setCauseSelector = mkSelector "setCause:"

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

