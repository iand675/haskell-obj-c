{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterProvideOfferParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterProvideOfferParams
  ( MTRWebRTCTransportProviderClusterProvideOfferParams
  , IsMTRWebRTCTransportProviderClusterProvideOfferParams(..)
  , webRTCSessionID
  , setWebRTCSessionID
  , sdp
  , setSdp
  , streamUsage
  , setStreamUsage
  , originatingEndpointID
  , setOriginatingEndpointID
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , iceServers
  , setIceServers
  , iceTransportPolicy
  , setIceTransportPolicy
  , metadataEnabled
  , setMetadataEnabled
  , sFrameConfig
  , setSFrameConfig
  , timedInvokeTimeoutMs
  , setTimedInvokeTimeoutMs
  , serverSideProcessingTimeout
  , setServerSideProcessingTimeout
  , audioStreamIDSelector
  , iceServersSelector
  , iceTransportPolicySelector
  , metadataEnabledSelector
  , originatingEndpointIDSelector
  , sFrameConfigSelector
  , sdpSelector
  , serverSideProcessingTimeoutSelector
  , setAudioStreamIDSelector
  , setIceServersSelector
  , setIceTransportPolicySelector
  , setMetadataEnabledSelector
  , setOriginatingEndpointIDSelector
  , setSFrameConfigSelector
  , setSdpSelector
  , setServerSideProcessingTimeoutSelector
  , setStreamUsageSelector
  , setTimedInvokeTimeoutMsSelector
  , setVideoStreamIDSelector
  , setWebRTCSessionIDSelector
  , streamUsageSelector
  , timedInvokeTimeoutMsSelector
  , videoStreamIDSelector
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
webRTCSessionID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
webRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams webRTCSessionIDSelector

-- | @- setWebRTCSessionID:@
setWebRTCSessionID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setWebRTCSessionID mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setWebRTCSessionIDSelector (toNSNumber value)

-- | @- sdp@
sdp :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSString)
sdp mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams sdpSelector

-- | @- setSdp:@
setSdp :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSString value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setSdp mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setSdpSelector (toNSString value)

-- | @- streamUsage@
streamUsage :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
streamUsage mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setStreamUsage mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setStreamUsageSelector (toNSNumber value)

-- | @- originatingEndpointID@
originatingEndpointID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
originatingEndpointID mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams originatingEndpointIDSelector

-- | @- setOriginatingEndpointID:@
setOriginatingEndpointID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setOriginatingEndpointID mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setOriginatingEndpointIDSelector (toNSNumber value)

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setVideoStreamIDSelector (toNSNumber value)

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setAudioStreamIDSelector (toNSNumber value)

-- | @- iceServers@
iceServers :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSArray)
iceServers mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams iceServersSelector

-- | @- setIceServers:@
setIceServers :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSArray value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setIceServers mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setIceServersSelector (toNSArray value)

-- | @- iceTransportPolicy@
iceTransportPolicy :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSString)
iceTransportPolicy mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams iceTransportPolicySelector

-- | @- setIceTransportPolicy:@
setIceTransportPolicy :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSString value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setIceTransportPolicy mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setIceTransportPolicySelector (toNSString value)

-- | @- metadataEnabled@
metadataEnabled :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
metadataEnabled mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams metadataEnabledSelector

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setMetadataEnabled mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setMetadataEnabledSelector (toNSNumber value)

-- | @- sFrameConfig@
sFrameConfig :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id MTRWebRTCTransportProviderClusterSFrameStruct)
sFrameConfig mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams sFrameConfigSelector

-- | @- setSFrameConfig:@
setSFrameConfig :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsMTRWebRTCTransportProviderClusterSFrameStruct value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setSFrameConfig mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setSFrameConfigSelector (toMTRWebRTCTransportProviderClusterSFrameStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams => mtrWebRTCTransportProviderClusterProvideOfferParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterProvideOfferParams mtrWebRTCTransportProviderClusterProvideOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterProvideOfferParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterProvideOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterProvideOfferParams setServerSideProcessingTimeoutSelector (toNSNumber value)

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

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector '[] (Id NSNumber)
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector '[Id NSNumber] ()
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @originatingEndpointID@
originatingEndpointIDSelector :: Selector '[] (Id NSNumber)
originatingEndpointIDSelector = mkSelector "originatingEndpointID"

-- | @Selector@ for @setOriginatingEndpointID:@
setOriginatingEndpointIDSelector :: Selector '[Id NSNumber] ()
setOriginatingEndpointIDSelector = mkSelector "setOriginatingEndpointID:"

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector '[] (Id NSNumber)
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector '[Id NSNumber] ()
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

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

-- | @Selector@ for @metadataEnabled@
metadataEnabledSelector :: Selector '[] (Id NSNumber)
metadataEnabledSelector = mkSelector "metadataEnabled"

-- | @Selector@ for @setMetadataEnabled:@
setMetadataEnabledSelector :: Selector '[Id NSNumber] ()
setMetadataEnabledSelector = mkSelector "setMetadataEnabled:"

-- | @Selector@ for @sFrameConfig@
sFrameConfigSelector :: Selector '[] (Id MTRWebRTCTransportProviderClusterSFrameStruct)
sFrameConfigSelector = mkSelector "sFrameConfig"

-- | @Selector@ for @setSFrameConfig:@
setSFrameConfigSelector :: Selector '[Id MTRWebRTCTransportProviderClusterSFrameStruct] ()
setSFrameConfigSelector = mkSelector "setSFrameConfig:"

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

