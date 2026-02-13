{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportRequestorClusterEndParams@.
module ObjC.Matter.MTRWebRTCTransportRequestorClusterEndParams
  ( MTRWebRTCTransportRequestorClusterEndParams
  , IsMTRWebRTCTransportRequestorClusterEndParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , reason
  , setReason
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , reasonSelector
  , serverSideProcessingTimeoutSelector
  , setReasonSelector
  , setServerSideProcessingTimeoutSelector
  , setTimedInvokeTimeoutMsSelector
  , setWebRTCSessionIDSelector
  , timedInvokeTimeoutMsSelector
  , webRTCSessionIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- webRTCSessionID@
webRTCSessionID :: IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams => mtrWebRTCTransportRequestorClusterEndParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportRequestorClusterEndParams =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterEndParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportRequestorClusterEndParams value =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- reason@
reason :: IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams => mtrWebRTCTransportRequestorClusterEndParams -> IO (Id NSNumber)
reason mtrWebRTCTransportRequestorClusterEndParams =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams reasonSelector

-- | @- setReason:@
setReason :: (IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterEndParams -> value -> IO ()
setReason mtrWebRTCTransportRequestorClusterEndParams value =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams setReasonSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams => mtrWebRTCTransportRequestorClusterEndParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterEndParams =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterEndParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterEndParams value =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams => mtrWebRTCTransportRequestorClusterEndParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportRequestorClusterEndParams =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportRequestorClusterEndParams mtrWebRTCTransportRequestorClusterEndParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterEndParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportRequestorClusterEndParams value =
  sendMessage mtrWebRTCTransportRequestorClusterEndParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector '[] (Id NSNumber)
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector '[Id NSNumber] ()
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @reason@
reasonSelector :: Selector '[] (Id NSNumber)
reasonSelector = mkSelector "reason"

-- | @Selector@ for @setReason:@
setReasonSelector :: Selector '[Id NSNumber] ()
setReasonSelector = mkSelector "setReason:"

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

