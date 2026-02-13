{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterConnectNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterConnectNetworkParams
  ( MTRNetworkCommissioningClusterConnectNetworkParams
  , IsMTRNetworkCommissioningClusterConnectNetworkParams(..)
  , networkID
  , setNetworkID
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , breadcrumbSelector
  , networkIDSelector
  , serverSideProcessingTimeoutSelector
  , setBreadcrumbSelector
  , setNetworkIDSelector
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

-- | @- networkID@
networkID :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterConnectNetworkParams =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams networkIDSelector

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterConnectNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams setNetworkIDSelector (toNSData value)

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterConnectNetworkParams =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterConnectNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams setBreadcrumbSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterConnectNetworkParams =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterConnectNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams => mtrNetworkCommissioningClusterConnectNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterConnectNetworkParams =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterConnectNetworkParams mtrNetworkCommissioningClusterConnectNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterConnectNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterConnectNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterConnectNetworkParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @networkID@
networkIDSelector :: Selector '[] (Id NSData)
networkIDSelector = mkSelector "networkID"

-- | @Selector@ for @setNetworkID:@
setNetworkIDSelector :: Selector '[Id NSData] ()
setNetworkIDSelector = mkSelector "setNetworkID:"

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

