{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams@.
module ObjC.Matter.MTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams
  ( MTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams
  , IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams(..)
  , constraints
  , setConstraints
  , cause
  , setCause
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , causeSelector
  , constraintsSelector
  , serverSideProcessingTimeoutSelector
  , setCauseSelector
  , setConstraintsSelector
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

-- | @- constraints@
constraints :: IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> IO (Id NSArray)
constraints mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams constraintsSelector

-- | @- setConstraints:@
setConstraints :: (IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams, IsNSArray value) => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> value -> IO ()
setConstraints mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams value =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams setConstraintsSelector (toNSArray value)

-- | @- cause@
cause :: IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> IO (Id NSNumber)
cause mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams causeSelector

-- | @- setCause:@
setCause :: (IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> value -> IO ()
setCause mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams value =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams setCauseSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams value =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRDeviceEnergyManagementClusterRequestConstraintBasedForecastParams mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams, IsNSNumber value) => mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams -> value -> IO ()
setServerSideProcessingTimeout mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams value =
  sendMessage mtrDeviceEnergyManagementClusterRequestConstraintBasedForecastParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @constraints@
constraintsSelector :: Selector '[] (Id NSArray)
constraintsSelector = mkSelector "constraints"

-- | @Selector@ for @setConstraints:@
setConstraintsSelector :: Selector '[Id NSArray] ()
setConstraintsSelector = mkSelector "setConstraints:"

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

