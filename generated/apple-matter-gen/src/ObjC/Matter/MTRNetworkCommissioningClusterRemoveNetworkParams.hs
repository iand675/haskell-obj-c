{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRNetworkCommissioningClusterRemoveNetworkParams@.
module ObjC.Matter.MTRNetworkCommissioningClusterRemoveNetworkParams
  ( MTRNetworkCommissioningClusterRemoveNetworkParams
  , IsMTRNetworkCommissioningClusterRemoveNetworkParams(..)
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
networkID :: IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams => mtrNetworkCommissioningClusterRemoveNetworkParams -> IO (Id NSData)
networkID mtrNetworkCommissioningClusterRemoveNetworkParams =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams networkIDSelector

-- | @- setNetworkID:@
setNetworkID :: (IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams, IsNSData value) => mtrNetworkCommissioningClusterRemoveNetworkParams -> value -> IO ()
setNetworkID mtrNetworkCommissioningClusterRemoveNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams setNetworkIDSelector (toNSData value)

-- | @- breadcrumb@
breadcrumb :: IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams => mtrNetworkCommissioningClusterRemoveNetworkParams -> IO (Id NSNumber)
breadcrumb mtrNetworkCommissioningClusterRemoveNetworkParams =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams breadcrumbSelector

-- | @- setBreadcrumb:@
setBreadcrumb :: (IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterRemoveNetworkParams -> value -> IO ()
setBreadcrumb mtrNetworkCommissioningClusterRemoveNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams setBreadcrumbSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams => mtrNetworkCommissioningClusterRemoveNetworkParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrNetworkCommissioningClusterRemoveNetworkParams =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterRemoveNetworkParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrNetworkCommissioningClusterRemoveNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams => mtrNetworkCommissioningClusterRemoveNetworkParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrNetworkCommissioningClusterRemoveNetworkParams =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRNetworkCommissioningClusterRemoveNetworkParams mtrNetworkCommissioningClusterRemoveNetworkParams, IsNSNumber value) => mtrNetworkCommissioningClusterRemoveNetworkParams -> value -> IO ()
setServerSideProcessingTimeout mtrNetworkCommissioningClusterRemoveNetworkParams value =
  sendMessage mtrNetworkCommissioningClusterRemoveNetworkParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

