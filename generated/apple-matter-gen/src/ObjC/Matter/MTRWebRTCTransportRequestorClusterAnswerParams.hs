{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportRequestorClusterAnswerParams@.
module ObjC.Matter.MTRWebRTCTransportRequestorClusterAnswerParams
  ( MTRWebRTCTransportRequestorClusterAnswerParams
  , IsMTRWebRTCTransportRequestorClusterAnswerParams(..)
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
webRTCSessionID :: IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams => mtrWebRTCTransportRequestorClusterAnswerParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportRequestorClusterAnswerParams =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterAnswerParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportRequestorClusterAnswerParams value =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- sdp@
sdp :: IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams => mtrWebRTCTransportRequestorClusterAnswerParams -> IO (Id NSString)
sdp mtrWebRTCTransportRequestorClusterAnswerParams =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams sdpSelector

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams, IsNSString value) => mtrWebRTCTransportRequestorClusterAnswerParams -> value -> IO ()
setSdp mtrWebRTCTransportRequestorClusterAnswerParams value =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams setSdpSelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams => mtrWebRTCTransportRequestorClusterAnswerParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterAnswerParams =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterAnswerParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterAnswerParams value =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams => mtrWebRTCTransportRequestorClusterAnswerParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportRequestorClusterAnswerParams =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportRequestorClusterAnswerParams mtrWebRTCTransportRequestorClusterAnswerParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterAnswerParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportRequestorClusterAnswerParams value =
  sendMessage mtrWebRTCTransportRequestorClusterAnswerParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

