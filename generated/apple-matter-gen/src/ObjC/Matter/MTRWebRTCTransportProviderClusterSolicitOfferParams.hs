{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterSolicitOfferParams@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterSolicitOfferParams
  ( MTRWebRTCTransportProviderClusterSolicitOfferParams
  , IsMTRWebRTCTransportProviderClusterSolicitOfferParams(..)
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
  , serverSideProcessingTimeoutSelector
  , setAudioStreamIDSelector
  , setIceServersSelector
  , setIceTransportPolicySelector
  , setMetadataEnabledSelector
  , setOriginatingEndpointIDSelector
  , setSFrameConfigSelector
  , setServerSideProcessingTimeoutSelector
  , setStreamUsageSelector
  , setTimedInvokeTimeoutMsSelector
  , setVideoStreamIDSelector
  , streamUsageSelector
  , timedInvokeTimeoutMsSelector
  , videoStreamIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- streamUsage@
streamUsage :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
streamUsage mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setStreamUsage mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setStreamUsageSelector (toNSNumber value)

-- | @- originatingEndpointID@
originatingEndpointID :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
originatingEndpointID mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams originatingEndpointIDSelector

-- | @- setOriginatingEndpointID:@
setOriginatingEndpointID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setOriginatingEndpointID mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setOriginatingEndpointIDSelector (toNSNumber value)

-- | @- videoStreamID@
videoStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
videoStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setVideoStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setVideoStreamIDSelector (toNSNumber value)

-- | @- audioStreamID@
audioStreamID :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
audioStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setAudioStreamID mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setAudioStreamIDSelector (toNSNumber value)

-- | @- iceServers@
iceServers :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSArray)
iceServers mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams iceServersSelector

-- | @- setIceServers:@
setIceServers :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSArray value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setIceServers mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setIceServersSelector (toNSArray value)

-- | @- iceTransportPolicy@
iceTransportPolicy :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSString)
iceTransportPolicy mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams iceTransportPolicySelector

-- | @- setIceTransportPolicy:@
setIceTransportPolicy :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSString value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setIceTransportPolicy mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setIceTransportPolicySelector (toNSString value)

-- | @- metadataEnabled@
metadataEnabled :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
metadataEnabled mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams metadataEnabledSelector

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setMetadataEnabled mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setMetadataEnabledSelector (toNSNumber value)

-- | @- sFrameConfig@
sFrameConfig :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id MTRWebRTCTransportProviderClusterSFrameStruct)
sFrameConfig mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams sFrameConfigSelector

-- | @- setSFrameConfig:@
setSFrameConfig :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsMTRWebRTCTransportProviderClusterSFrameStruct value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setSFrameConfig mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setSFrameConfigSelector (toMTRWebRTCTransportProviderClusterSFrameStruct value)

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- timedInvokeTimeoutMs@
timedInvokeTimeoutMs :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
timedInvokeTimeoutMs mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams timedInvokeTimeoutMsSelector

-- | Controls whether the command is a timed command (using Timed Invoke).
--
-- If nil (the default value), a regular invoke is done for commands that do not require a timed invoke and a timed invoke with some default timed request timeout is done for commands that require a timed invoke.
--
-- If not nil, a timed invoke is done, with the provided value used as the timed request timeout.  The value should be chosen small enough to provide the desired security properties but large enough that it will allow a round-trip from the sever to the client (for the status response and actual invoke request) within the timeout window.
--
-- ObjC selector: @- setTimedInvokeTimeoutMs:@
setTimedInvokeTimeoutMs :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setTimedInvokeTimeoutMs mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setTimedInvokeTimeoutMsSelector (toNSNumber value)

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- serverSideProcessingTimeout@
serverSideProcessingTimeout :: IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams => mtrWebRTCTransportProviderClusterSolicitOfferParams -> IO (Id NSNumber)
serverSideProcessingTimeout mtrWebRTCTransportProviderClusterSolicitOfferParams =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams serverSideProcessingTimeoutSelector

-- | Controls how much time, in seconds, we will allow for the server to process the command.
--
-- The command will then time out if that much time, plus an allowance for retransmits due to network failures, passes.
--
-- If nil, the framework will try to select an appropriate timeout value itself.
--
-- ObjC selector: @- setServerSideProcessingTimeout:@
setServerSideProcessingTimeout :: (IsMTRWebRTCTransportProviderClusterSolicitOfferParams mtrWebRTCTransportProviderClusterSolicitOfferParams, IsNSNumber value) => mtrWebRTCTransportProviderClusterSolicitOfferParams -> value -> IO ()
setServerSideProcessingTimeout mtrWebRTCTransportProviderClusterSolicitOfferParams value =
  sendMessage mtrWebRTCTransportProviderClusterSolicitOfferParams setServerSideProcessingTimeoutSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

