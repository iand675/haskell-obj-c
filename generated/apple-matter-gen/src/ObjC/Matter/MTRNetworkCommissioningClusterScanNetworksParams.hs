{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterScanNetworksParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterScanNetworksParams
  ( MTRNetworkCommissioningClusterScanNetworksParams
  , IsMTRNetworkCommissioningClusterScanNetworksParams(..)
  , ssid
  , setSsid
  , breadcrumb
  , setBreadcrumb
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , breadcrumbSelector
  , serverSideProcessingTimeoutSelector
  , setBreadcrumbSelector
  , setServerSideProcessingTimeoutSelector
  , setSsidSelector
  , setTimedInvokeTimeoutMsSelector
  , ssidSelector
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

-- | @- ssid@
ssid :: IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams => mtrNetworkCommissioningClusterScanNetworksParams -> IO (Id NSData)
ssid mtrNetworkCommissioningClusterScanNetworksParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams ssidSelector

-- | @- setSsid:@
setSsid :: (IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams, IsNSData value) => mtrNetworkCommissioningClusterScanNetworksParams -> value -> IO ()
setSsid mtrNetworkCommissioningClusterScanNetworksParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams setSsidSelector (toNSData value)

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams => mtrNetworkCommissioningClusterScanNetworksParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterScanNetworksParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterScanNetworksParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams setBreadcrumbSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams => mtrNetworkCommissioningClusterScanNetworksParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterScanNetworksParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterScanNetworksParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams => mtrNetworkCommissioningClusterScanNetworksParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterScanNetworksParams =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterScanNetworksParams mtrNetworkCommissioningClusterScanNetworksParams, IsNSNumber value) => mtrNetworkCommissioningClusterScanNetworksParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterScanNetworksParams value =
  sendMessage mtrNetworkCommissioningClusterScanNetworksParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @ssid@
ssidSelector :: Selector '[] (Id NSData)
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @setSsid:@
setSsidSelector :: Selector '[Id NSData] ()
setSsidSelector = mkSelector "setSsid:"

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

