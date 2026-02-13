{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportRequestorClusterOfferParams@.
module ObjC.Matter.MTRWebRTCTransportRequestorClusterOfferParams
  ( MTRWebRTCTransportRequestorClusterOfferParams
  , IsMTRWebRTCTransportRequestorClusterOfferParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , sdp
  , setSdp
  , iceServers
  , setIceServers
  , iceTransportPolicy
  , setIceTransportPolicy
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , iceServersSelector
  , iceTransportPolicySelector
  , sdpSelector
  , serverSideProcessingTimeoutSelector
  , setIceServersSelector
  , setIceTransportPolicySelector
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
webRTCSessionID :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportRequestorClusterOfferParams =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportRequestorClusterOfferParams value =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- sdp@
sdp :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSString)
sdp mtrWebRTCTransportRequestorClusterOfferParams =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams sdpSelector

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSString value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setSdp mtrWebRTCTransportRequestorClusterOfferParams value =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams setSdpSelector (toNSString value)

-- | @- iceServers@
iceServers :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSArray)
iceServers mtrWebRTCTransportRequestorClusterOfferParams =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams iceServersSelector

-- | @- setIceServers:@
setIceServers :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSArray value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setIceServers mtrWebRTCTransportRequestorClusterOfferParams value =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams setIceServersSelector (toNSArray value)

-- | @- iceTransportPolicy@
iceTransportPolicy :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSString)
iceTransportPolicy mtrWebRTCTransportRequestorClusterOfferParams =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams iceTransportPolicySelector

-- | @- setIceTransportPolicy:@
setIceTransportPolicy :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSString value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setIceTransportPolicy mtrWebRTCTransportRequestorClusterOfferParams value =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams setIceTransportPolicySelector (toNSString value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterOfferParams =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportRequestorClusterOfferParams value =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams => mtrWebRTCTransportRequestorClusterOfferParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportRequestorClusterOfferParams =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportRequestorClusterOfferParams mtrWebRTCTransportRequestorClusterOfferParams, IsNSNumber value) => mtrWebRTCTransportRequestorClusterOfferParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportRequestorClusterOfferParams value =
  sendMessage mtrWebRTCTransportRequestorClusterOfferParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @iceServers@
iceServersSelector :: Selector '[] (Id NSArray)
iceServersSelector = mkSelector "iceServers"

-- | @Selector@ for @setIceServers:@
setIceServersSelector :: Selector '[Id NSArray] ()
setIceServersSelector = mkSelector "setIceServers:"

-- | @Selector@ for @iceTransportPolicy@
iceTransportPolicySelector :: Selector '[] (Id NSString)
iceTransportPolicySelector = mkSelector "iceTransportPolicy"

-- | @Selector@ for @setIceTransportPolicy:@
setIceTransportPolicySelector :: Selector '[Id NSString] ()
setIceTransportPolicySelector = mkSelector "setIceTransportPolicy:"

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

