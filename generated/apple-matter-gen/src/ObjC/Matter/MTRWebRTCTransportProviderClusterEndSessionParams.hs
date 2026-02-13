{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterEndSessionParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterEndSessionParams
  ( MTRWebRTCTransportProviderClusterEndSessionParams
  , IsMTRWebRTCTransportProviderClusterEndSessionParams(..)
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
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterEndSessionParams =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterEndSessionParams value =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- reason@
reason :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
reason mtrWebRTCTransportProviderClusterEndSessionParams =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams reasonSelector

-- | @- setReason:@
setReason :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setReason mtrWebRTCTransportProviderClusterEndSessionParams value =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams setReasonSelector (toNSNumber value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterEndSessionParams =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterEndSessionParams value =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams => mtrWebRTCTransportProviderClusterEndSessionParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterEndSessionParams =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterEndSessionParams mtrWebRTCTransportProviderClusterEndSessionParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterEndSessionParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterEndSessionParams value =
  sendMessage mtrWebRTCTransportProviderClusterEndSessionParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

