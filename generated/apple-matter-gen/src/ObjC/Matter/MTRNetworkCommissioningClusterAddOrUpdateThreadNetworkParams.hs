{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams
  ( MTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams
  , IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams(..)
  , operationalDataset
  , setOperationalDataset
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , breadcrumbSelector
  , operationalDatasetSelector
  , serverSideProcessingTimeoutSelector
  , setBreadcrumbSelector
  , setOperationalDatasetSelector
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

-- | @- operationalDataset@
operationalDataset :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSData)
operationalDataset mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams operationalDatasetSelector

-- | @- setOperationalDataset:@
setOperationalDataset :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setOperationalDataset mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams setOperationalDatasetSelector (toNSData value)

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams setBreadcrumbSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterAddOrUpdateThreadNetworkParams mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterAddOrUpdateThreadNetworkParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationalDataset@
operationalDatasetSelector :: Selector '[] (Id NSData)
operationalDatasetSelector = mkSelector "operationalDataset"

-- | @Selector@ for @setOperationalDataset:@
setOperationalDatasetSelector :: Selector '[Id NSData] ()
setOperationalDatasetSelector = mkSelector "setOperationalDataset:"

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

