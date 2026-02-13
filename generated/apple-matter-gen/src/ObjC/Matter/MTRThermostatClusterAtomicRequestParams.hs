{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRThermostatClusterAtomicRequestParams@.
module ObjC.Matter.MTRThermostatClusterAtomicRequestParams
  ( MTRThermostatClusterAtomicRequestParams
  , IsMTRThermostatClusterAtomicRequestParams(..)
  , requestType
  , setRequestType
  , attributeRequests
  , setAttributeRequests
  , timeout
  , setTimeout
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , attributeRequestsSelector
  , requestTypeSelector
  , serverSideProcessingTimeoutSelector
  , setAttributeRequestsSelector
  , setRequestTypeSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTimeoutSelector
  , timedInvokeTimeoutMsSelector
  , timeoutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- requestType@
requestType :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
requestType mtrThermostatClusterAtomicRequestParams =
  sendMessage mtrThermostatClusterAtomicRequestParams requestTypeSelector

-- | @- setRequestType:@
setRequestType :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setRequestType mtrThermostatClusterAtomicRequestParams value =
  sendMessage mtrThermostatClusterAtomicRequestParams setRequestTypeSelector (toNSNumber value)

-- | @- attributeRequests@
attributeRequests :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSArray)
attributeRequests mtrThermostatClusterAtomicRequestParams =
  sendMessage mtrThermostatClusterAtomicRequestParams attributeRequestsSelector

-- | @- setAttributeRequests:@
setAttributeRequests :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSArray value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setAttributeRequests mtrThermostatClusterAtomicRequestParams value =
  sendMessage mtrThermostatClusterAtomicRequestParams setAttributeRequestsSelector (toNSArray value)

-- | @- timeout@
timeout :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
timeout mtrThermostatClusterAtomicRequestParams =
  sendMessage mtrThermostatClusterAtomicRequestParams timeoutSelector

-- | @- setTimeout:@
setTimeout :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setTimeout mtrThermostatClusterAtomicRequestParams value =
  sendMessage mtrThermostatClusterAtomicRequestParams setTimeoutSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrThermostatClusterAtomicRequestParams =
  sendMessage mtrThermostatClusterAtomicRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrThermostatClusterAtomicRequestParams value =
  sendMessage mtrThermostatClusterAtomicRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams => mtrThermostatClusterAtomicRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrThermostatClusterAtomicRequestParams =
  sendMessage mtrThermostatClusterAtomicRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRThermostatClusterAtomicRequestParams mtrThermostatClusterAtomicRequestParams, IsNSNumber value) => mtrThermostatClusterAtomicRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrThermostatClusterAtomicRequestParams value =
  sendMessage mtrThermostatClusterAtomicRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @requestType@
requestTypeSelector :: Selector '[] (Id NSNumber)
requestTypeSelector = mkSelector "requestType"

-- | @Selector@ for @setRequestType:@
setRequestTypeSelector :: Selector '[Id NSNumber] ()
setRequestTypeSelector = mkSelector "setRequestType:"

-- | @Selector@ for @attributeRequests@
attributeRequestsSelector :: Selector '[] (Id NSArray)
attributeRequestsSelector = mkSelector "attributeRequests"

-- | @Selector@ for @setAttributeRequests:@
setAttributeRequestsSelector :: Selector '[Id NSArray] ()
setAttributeRequestsSelector = mkSelector "setAttributeRequests:"

-- | @Selector@ for @timeout@
timeoutSelector :: Selector '[] (Id NSNumber)
timeoutSelector = mkSelector "timeout"

-- | @Selector@ for @setTimeout:@
setTimeoutSelector :: Selector '[Id NSNumber] ()
setTimeoutSelector = mkSelector "setTimeout:"

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

