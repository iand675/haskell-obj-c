{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRUnitTestingClusterStringEchoRequestParams@.
module ObjC.Matter.MTRUnitTestingClusterStringEchoRequestParams
  ( MTRUnitTestingClusterStringEchoRequestParams
  , IsMTRUnitTestingClusterStringEchoRequestParams(..)
  , payload
  , setPayload
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , payloadSelector
  , serverSideProcessingTimeoutSelector
  , setPayloadSelector
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

-- | @- payload@
payload :: IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams => mtrUnitTestingClusterStringEchoRequestParams -> IO (Id NSData)
payload mtrUnitTestingClusterStringEchoRequestParams =
  sendMessage mtrUnitTestingClusterStringEchoRequestParams payloadSelector

-- | @- setPayload:@
setPayload :: (IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams, IsNSData value) => mtrUnitTestingClusterStringEchoRequestParams -> value -> IO ()
setPayload mtrUnitTestingClusterStringEchoRequestParams value =
  sendMessage mtrUnitTestingClusterStringEchoRequestParams setPayloadSelector (toNSData value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams => mtrUnitTestingClusterStringEchoRequestParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrUnitTestingClusterStringEchoRequestParams =
  sendMessage mtrUnitTestingClusterStringEchoRequestParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterStringEchoRequestParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrUnitTestingClusterStringEchoRequestParams value =
  sendMessage mtrUnitTestingClusterStringEchoRequestParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams => mtrUnitTestingClusterStringEchoRequestParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrUnitTestingClusterStringEchoRequestParams =
  sendMessage mtrUnitTestingClusterStringEchoRequestParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRUnitTestingClusterStringEchoRequestParams mtrUnitTestingClusterStringEchoRequestParams, IsNSNumber value) => mtrUnitTestingClusterStringEchoRequestParams -> value -> IO ()
setServerSideProcessingTimeout mtrUnitTestingClusterStringEchoRequestParams value =
  sendMessage mtrUnitTestingClusterStringEchoRequestParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @payload@
payloadSelector :: Selector '[] (Id NSData)
payloadSelector = mkSelector "payload"

-- | @Selector@ for @setPayload:@
setPayloadSelector :: Selector '[Id NSData] ()
setPayloadSelector = mkSelector "setPayload:"

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

