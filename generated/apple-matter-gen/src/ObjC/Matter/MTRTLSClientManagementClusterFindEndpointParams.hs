{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTLSClientManagementClusterFindEndpointParams@.
module ObjC.Matter.MTRTLSClientManagementClusterFindEndpointParams
  ( MTRTLSClientManagementClusterFindEndpointParams
  , IsMTRTLSClientManagementClusterFindEndpointParams(..)
  , endpointID
  , setEndpointID
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , endpointIDSelector
  , serverSideProcessingTimeoutSelector
  , setEndpointIDSelector
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

-- | @- endpointID@
endpointID :: IsMTRTLSClientManagementClusterFindEndpointParams mtrtlsClientManagementClusterFindEndpointParams => mtrtlsClientManagementClusterFindEndpointParams -> IO (Id NSNumber)
endpointID mtrtlsClientManagementClusterFindEndpointParams =
  sendMessage mtrtlsClientManagementClusterFindEndpointParams endpointIDSelector

-- | @- setEndpointID:@
setEndpointID :: (IsMTRTLSClientManagementClusterFindEndpointParams mtrtlsClientManagementClusterFindEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterFindEndpointParams -> value -> IO ()
setEndpointID mtrtlsClientManagementClusterFindEndpointParams value =
  sendMessage mtrtlsClientManagementClusterFindEndpointParams setEndpointIDSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRTLSClientManagementClusterFindEndpointParams mtrtlsClientManagementClusterFindEndpointParams => mtrtlsClientManagementClusterFindEndpointParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrtlsClientManagementClusterFindEndpointParams =
  sendMessage mtrtlsClientManagementClusterFindEndpointParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRTLSClientManagementClusterFindEndpointParams mtrtlsClientManagementClusterFindEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterFindEndpointParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrtlsClientManagementClusterFindEndpointParams value =
  sendMessage mtrtlsClientManagementClusterFindEndpointParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRTLSClientManagementClusterFindEndpointParams mtrtlsClientManagementClusterFindEndpointParams => mtrtlsClientManagementClusterFindEndpointParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrtlsClientManagementClusterFindEndpointParams =
  sendMessage mtrtlsClientManagementClusterFindEndpointParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRTLSClientManagementClusterFindEndpointParams mtrtlsClientManagementClusterFindEndpointParams, IsNSNumber value) => mtrtlsClientManagementClusterFindEndpointParams -> value -> IO ()
setServerSideProcessingTimeout mtrtlsClientManagementClusterFindEndpointParams value =
  sendMessage mtrtlsClientManagementClusterFindEndpointParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

