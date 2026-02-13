{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterSetTransportStatusParams@.
module ObjC.Matter.MTRPushAVStreamTransportClusterSetTransportStatusParams
  ( MTRPushAVStreamTransportClusterSetTransportStatusParams
  , IsMTRPushAVStreamTransportClusterSetTransportStatusParams(..)
  , connectionID
  , setConnectionID
  , transportStatus
  , setTransportStatus
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , connectionIDSelector
  , serverSideProcessingTimeoutSelector
  , setConnectionIDSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setTransportStatusSelector
  , timedInvokeTimeoutMsSelector
  , transportStatusSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- connectionID@
connectionID :: IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams => mtrPushAVStreamTransportClusterSetTransportStatusParams -> IO (Id NSNumber)
connectionID mtrPushAVStreamTransportClusterSetTransportStatusParams =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams connectionIDSelector

-- | @- setConnectionID:@
setConnectionID :: (IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams, IsNSNumber value) => mtrPushAVStreamTransportClusterSetTransportStatusParams -> value -> IO ()
setConnectionID mtrPushAVStreamTransportClusterSetTransportStatusParams value =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams setConnectionIDSelector (toNSNumber value)

-- | @- transportStatus@
transportStatus :: IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams => mtrPushAVStreamTransportClusterSetTransportStatusParams -> IO (Id NSNumber)
transportStatus mtrPushAVStreamTransportClusterSetTransportStatusParams =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams transportStatusSelector

-- | @- setTransportStatus:@
setTransportStatus :: (IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams, IsNSNumber value) => mtrPushAVStreamTransportClusterSetTransportStatusParams -> value -> IO ()
setTransportStatus mtrPushAVStreamTransportClusterSetTransportStatusParams value =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams setTransportStatusSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams => mtrPushAVStreamTransportClusterSetTransportStatusParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrPushAVStreamTransportClusterSetTransportStatusParams =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams, IsNSNumber value) => mtrPushAVStreamTransportClusterSetTransportStatusParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrPushAVStreamTransportClusterSetTransportStatusParams value =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams => mtrPushAVStreamTransportClusterSetTransportStatusParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrPushAVStreamTransportClusterSetTransportStatusParams =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRPushAVStreamTransportClusterSetTransportStatusParams mtrPushAVStreamTransportClusterSetTransportStatusParams, IsNSNumber value) => mtrPushAVStreamTransportClusterSetTransportStatusParams -> value -> IO ()
setServerSideProcessingTimeout mtrPushAVStreamTransportClusterSetTransportStatusParams value =
  sendMessage mtrPushAVStreamTransportClusterSetTransportStatusParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @connectionID@
connectionIDSelector :: Selector '[] (Id NSNumber)
connectionIDSelector = mkSelector "connectionID"

-- | @Selector@ for @setConnectionID:@
setConnectionIDSelector :: Selector '[Id NSNumber] ()
setConnectionIDSelector = mkSelector "setConnectionID:"

-- | @Selector@ for @transportStatus@
transportStatusSelector :: Selector '[] (Id NSNumber)
transportStatusSelector = mkSelector "transportStatus"

-- | @Selector@ for @setTransportStatus:@
setTransportStatusSelector :: Selector '[Id NSNumber] ()
setTransportStatusSelector = mkSelector "setTransportStatus:"

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

