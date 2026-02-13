{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterProvisionEndpointParams@.
module ObjC.Matter.MTRTLSClientManagementClusterProvisionEndpointParams
  ( MTRTLSClientManagementClusterProvisionEndpointParams
  , IsMTRTLSClientManagementClusterProvisionEndpointParams(..)
  , hostname
  , setHostname
  , port
  , setPort
  , caid
  , setCaid
  , ccdid
  , setCcdid
  , endpointID
  , setEndpointID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , caidSelector
  , ccdidSelector
  , endpointIDSelector
  , hostnameSelector
  , portSelector
  , serverSideProcessingTimeoutSelector
  , setCaidSelector
  , setCcdidSelector
  , setEndpointIDSelector
  , setHostnameSelector
  , setPortSelector
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

-- | @- hostname@
hostname :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSData)
hostname mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams hostnameSelector

-- | @- setHostname:@
setHostname :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSData value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setHostname mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setHostnameSelector (toNSData value)

-- | @- port@
port :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
port mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams portSelector

-- | @- setPort:@
setPort :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setPort mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setPortSelector (toNSNumber value)

-- | @- caid@
caid :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
caid mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams caidSelector

-- | @- setCaid:@
setCaid :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setCaid mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setCaidSelector (toNSNumber value)

-- | @- ccdid@
ccdid :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
ccdid mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams ccdidSelector

-- | @- setCcdid:@
setCcdid :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setCcdid mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setCcdidSelector (toNSNumber value)

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setEndpointIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams => mtrtlsClientManagementClusterProvisionEndpointParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsClientManagementClusterProvisionEndpointParams =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSClientManagementClusterProvisionEndpointParams mtrtlsClientManagementClusterProvisionEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterProvisionEndpointParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsClientManagementClusterProvisionEndpointParams value =
  sendMessage mtrtlsClientManagementClusterProvisionEndpointParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hostname@
hostnameSelector :: Selector '[] (Id NSData)
hostnameSelector = mkSelector "hostname"

-- | @Selector@ for @setHostname:@
setHostnameSelector :: Selector '[Id NSData] ()
setHostnameSelector = mkSelector "setHostname:"

-- | @Selector@ for @port@
portSelector :: Selector '[] (Id NSNumber)
portSelector = mkSelector "port"

-- | @Selector@ for @setPort:@
setPortSelector :: Selector '[Id NSNumber] ()
setPortSelector = mkSelector "setPort:"

-- | @Selector@ for @caid@
caidSelector :: Selector '[] (Id NSNumber)
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector '[Id NSNumber] ()
setCaidSelector = mkSelector "setCaid:"

-- | @Selector@ for @ccdid@
ccdidSelector :: Selector '[] (Id NSNumber)
ccdidSelector = mkSelector "ccdid"

-- | @Selector@ for @setCcdid:@
setCcdidSelector :: Selector '[Id NSNumber] ()
setCcdidSelector = mkSelector "setCcdid:"

-- | @Selector@ for @endpointID@
endpointIDSelector :: Selector '[] (Id NSNumber)
endpointIDSelector = mkSelector "endpointID"

-- | @Selector@ for @setEndpointID:@
setEndpointIDSelector :: Selector '[Id NSNumber] ()
setEndpointIDSelector = mkSelector "setEndpointID:"

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

