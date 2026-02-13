{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams@.
module ObjC.Matter.MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams
  ( MTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams
  , IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams(..)
  , activeDataset
  , setActiveDataset
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , activeDatasetSelector
  , breadcrumbSelector
  , serverSideProcessingTimeoutSelector
  , setActiveDatasetSelector
  , setBreadcrumbSelector
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

-- | @- activeDataset@
activeDataset :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSData)
activeDataset mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams activeDatasetSelector

-- | @- setActiveDataset:@
setActiveDataset :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSData value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setActiveDataset mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams setActiveDatasetSelector (toNSData value)

-- | @- breadcrumb@
breadcrumb :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSNumber)
breadcrumb mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setBreadcrumb mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams setBreadcrumbSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThreadBorderRouterManagementClusterSetActiveDatasetRequestParams mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams, IsNSNumber value) => mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams value =
  sendMessage mtrThreadBorderRouterManagementClusterSetActiveDatasetRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @activeDataset@
activeDatasetSelector :: Selector '[] (Id NSData)
activeDatasetSelector = mkSelector "activeDataset"

-- | @Selector@ for @setActiveDataset:@
setActiveDatasetSelector :: Selector '[Id NSData] ()
setActiveDatasetSelector = mkSelector "setActiveDataset:"

-- | @Selector@ for @breadcrumb@
breadcrumbSelector :: Selector '[] (Id NSNumber)
breadcrumbSelector = mkSelector "breadcrumb"

-- | @Selector@ for @setBreadcrumb:@
setBreadcrumbSelector :: Selector '[Id NSNumber] ()
setBreadcrumbSelector = mkSelector "setBreadcrumb:"

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

