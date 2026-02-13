{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams@.
module ObjC.Matter.MTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams
  ( MTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams
  , IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams(..)
  , pendingDataset
  , setPendingDataset
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , pendingDatasetSelector
  , serverSideProcessingTimeoutSelector
  , setPendingDatasetSelector
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

-- | @- pendingDataset@
pendingDataset :: IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams -> IO (Id NSData)
pendingDataset mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams pendingDatasetSelector

-- | @- setPendingDataset:@
setPendingDataset :: (IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams, IsNSData value) => mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams -> value -> IO ()
setPendingDataset mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams setPendingDatasetSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThreadBorderRouterManagementClusterSetPendingDatasetRequestParams mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetPendingDatasetRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pendingDataset@
pendingDatasetSelector :: Selector '[] (Id NSData)
pendingDatasetSelector = mkSelector "pendingDataset"

-- | @Selector@ for @setPendingDataset:@
setPendingDatasetSelector :: Selector '[Id NSData] ()
setPendingDatasetSelector = mkSelector "setPendingDataset:"

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

