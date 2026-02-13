{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterAllocatePushTransportParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterAllocatePushTransportParams
  ( MTRPushAVStreamTransportClusterAllocatePushTransportParams
  , IsMTRPushAVStreamTransportClusterAllocatePushTransportParams(..)
  , transportOptions
  , setTransportOptions
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , serverSideProcessingTimeoutSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransportOptionsSelector
  , timedInvokeTimeoutMsSelector
  , transportOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- transportOptions@
transportOptions :: IsMTRPushAVStreamTransportClusterAllocatePushTransportParams mtrPushAVStreamTransportClusterAllocatePushTransportParams => mtrPushAVStreamTransportClusterAllocatePushTransportParams -> IO (Id MTRPushAVStreamTransportClusterTransportOptionsStruct)
transportOptions mtrPushAVStreamTransportClusterAllocatePushTransportParams =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportParams transportOptionsSelector

-- | @- setTransportOptions:@
setTransportOptions :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportParams mtrPushAVStreamTransportClusterAllocatePushTransportParams, IsMTRPushAVStreamTransportClusterTransportOptionsStruct value) => mtrPushAVStreamTransportClusterAllocatePushTransportParams -> value -> IO ()
setTransportOptions mtrPushAVStreamTransportClusterAllocatePushTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportParams setTransportOptionsSelector (toMTRPushAVStreamTransportClusterTransportOptionsStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRPushAVStreamTransportClusterAllocatePushTransportParams mtrPushAVStreamTransportClusterAllocatePushTransportParams => mtrPushAVStreamTransportClusterAllocatePushTransportParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrPushAVStreamTransportClusterAllocatePushTransportParams =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportParams mtrPushAVStreamTransportClusterAllocatePushTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterAllocatePushTransportParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrPushAVStreamTransportClusterAllocatePushTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRPushAVStreamTransportClusterAllocatePushTransportParams mtrPushAVStreamTransportClusterAllocatePushTransportParams => mtrPushAVStreamTransportClusterAllocatePushTransportParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrPushAVStreamTransportClusterAllocatePushTransportParams =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRPushAVStreamTransportClusterAllocatePushTransportParams mtrPushAVStreamTransportClusterAllocatePushTransportParams, IsNSNumber value) => mtrPushAVStreamTransportClusterAllocatePushTransportParams -> value -> IO ()
setServerSideProcessingTimeout mtrPushAVStreamTransportClusterAllocatePushTransportParams value =
  sendMessage mtrPushAVStreamTransportClusterAllocatePushTransportParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @transportOptions@
transportOptionsSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterTransportOptionsStruct)
transportOptionsSelector = mkSelector "transportOptions"

-- | @Selector@ for @setTransportOptions:@
setTransportOptionsSelector :: Selector '[Id MTRPushAVStreamTransportClusterTransportOptionsStruct] ()
setTransportOptionsSelector = mkSelector "setTransportOptions:"

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

