{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideAnswerParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideAnswerParams
  ( MTRWebRTCTransportProviderClusterProvideAnswerParams
  , IsMTRWebRTCTransportProviderClusterProvideAnswerParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , sdp
  , setSdp
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , sdpSelector
  , serverSideProcessingTimeoutSelector
  , setSdpSelector
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
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideAnswerParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideAnswerParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- sdp@
sdp :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSString)
sdp mtrWebRTCTransportProviderClusterProvideAnswerParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams sdpSelector

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSString value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setSdp mtrWebRTCTransportProviderClusterProvideAnswerParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams setSdpSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideAnswerParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideAnswerParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams => mtrWebRTCTransportProviderClusterProvideAnswerParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideAnswerParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterProvideAnswerParams mtrWebRTCTransportProviderClusterProvideAnswerParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideAnswerParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideAnswerParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideAnswerParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @webRTCSessionID@
webRTCSessionIDSelector :: Selector '[] (Id NSNumber)
webRTCSessionIDSelector = mkSelector "webRTCSessionID"

-- | @Selector@ for @setWebRTCSessionID:@
setWebRTCSessionIDSelector :: Selector '[Id NSNumber] ()
setWebRTCSessionIDSelector = mkSelector "setWebRTCSessionID:"

-- | @Selector@ for @sdp@
sdpSelector :: Selector '[] (Id NSString)
sdpSelector = mkSelector "sdp"

-- | @Selector@ for @setSdp:@
setSdpSelector :: Selector '[Id NSString] ()
setSdpSelector = mkSelector "setSdp:"

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

