{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterTransportOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterTransportOptionsStruct
  ( MTRPushAVStreamTransportClusterTransportOptionsStruct
  , IsMTRPushAVStreamTransportClusterTransportOptionsStruct(..)
  , streamUsage
  , setStreamUsage
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , tlsEndpointID
  , setTlsEndpointID
  , url
  , setUrl
  , triggerOptions
  , setTriggerOptions
  , ingestMethod
  , setIngestMethod
  , containerOptions
  , setContainerOptions
  , expiryTime
  , setExpiryTime
  , audioStreamIDSelector
  , containerOptionsSelector
  , expiryTimeSelector
  , ingestMethodSelector
  , setAudioStreamIDSelector
  , setContainerOptionsSelector
  , setExpiryTimeSelector
  , setIngestMethodSelector
  , setStreamUsageSelector
  , setTlsEndpointIDSelector
  , setTriggerOptionsSelector
  , setUrlSelector
  , setVideoStreamIDSelector
  , streamUsageSelector
  , tlsEndpointIDSelector
  , triggerOptionsSelector
  , urlSelector
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
streamUsage :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
streamUsage mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setStreamUsage mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setStreamUsageSelector (toNSNumber value)

-- | @- videoStreamID@
videoStreamID :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
videoStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setVideoStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setVideoStreamIDSelector (toNSNumber value)

-- | @- audioStreamID@
audioStreamID :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
audioStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setAudioStreamID mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setAudioStreamIDSelector (toNSNumber value)

-- | @- tlsEndpointID@
tlsEndpointID :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
tlsEndpointID mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct tlsEndpointIDSelector

-- | @- setTlsEndpointID:@
setTlsEndpointID :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setTlsEndpointID mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setTlsEndpointIDSelector (toNSNumber value)

-- | @- url@
url :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSString)
url mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct urlSelector

-- | @- setUrl:@
setUrl :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSString value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setUrl mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setUrlSelector (toNSString value)

-- | @- triggerOptions@
triggerOptions :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct)
triggerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct triggerOptionsSelector

-- | @- setTriggerOptions:@
setTriggerOptions :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setTriggerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setTriggerOptionsSelector (toMTRPushAVStreamTransportClusterTransportTriggerOptionsStruct value)

-- | @- ingestMethod@
ingestMethod :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
ingestMethod mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct ingestMethodSelector

-- | @- setIngestMethod:@
setIngestMethod :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setIngestMethod mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setIngestMethodSelector (toNSNumber value)

-- | @- containerOptions@
containerOptions :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterContainerOptionsStruct)
containerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct containerOptionsSelector

-- | @- setContainerOptions:@
setContainerOptions :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsMTRPushAVStreamTransportClusterContainerOptionsStruct value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setContainerOptions mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setContainerOptionsSelector (toMTRPushAVStreamTransportClusterContainerOptionsStruct value)

-- | @- expiryTime@
expiryTime :: IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct => mtrPushAVStreamTransportClusterTransportOptionsStruct -> IO (Id NSNumber)
expiryTime mtrPushAVStreamTransportClusterTransportOptionsStruct =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct expiryTimeSelector

-- | @- setExpiryTime:@
setExpiryTime :: (IsMTRPushAVStreamTransportClusterTransportOptionsStruct mtrPushAVStreamTransportClusterTransportOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterTransportOptionsStruct -> value -> IO ()
setExpiryTime mtrPushAVStreamTransportClusterTransportOptionsStruct value =
  sendMessage mtrPushAVStreamTransportClusterTransportOptionsStruct setExpiryTimeSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector '[] (Id NSNumber)
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector '[Id NSNumber] ()
setStreamUsageSelector = mkSelector "setStreamUsage:"

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

-- | @Selector@ for @tlsEndpointID@
tlsEndpointIDSelector :: Selector '[] (Id NSNumber)
tlsEndpointIDSelector = mkSelector "tlsEndpointID"

-- | @Selector@ for @setTlsEndpointID:@
setTlsEndpointIDSelector :: Selector '[Id NSNumber] ()
setTlsEndpointIDSelector = mkSelector "setTlsEndpointID:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSString)
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[Id NSString] ()
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @triggerOptions@
triggerOptionsSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct)
triggerOptionsSelector = mkSelector "triggerOptions"

-- | @Selector@ for @setTriggerOptions:@
setTriggerOptionsSelector :: Selector '[Id MTRPushAVStreamTransportClusterTransportTriggerOptionsStruct] ()
setTriggerOptionsSelector = mkSelector "setTriggerOptions:"

-- | @Selector@ for @ingestMethod@
ingestMethodSelector :: Selector '[] (Id NSNumber)
ingestMethodSelector = mkSelector "ingestMethod"

-- | @Selector@ for @setIngestMethod:@
setIngestMethodSelector :: Selector '[Id NSNumber] ()
setIngestMethodSelector = mkSelector "setIngestMethod:"

-- | @Selector@ for @containerOptions@
containerOptionsSelector :: Selector '[] (Id MTRPushAVStreamTransportClusterContainerOptionsStruct)
containerOptionsSelector = mkSelector "containerOptions"

-- | @Selector@ for @setContainerOptions:@
setContainerOptionsSelector :: Selector '[Id MTRPushAVStreamTransportClusterContainerOptionsStruct] ()
setContainerOptionsSelector = mkSelector "setContainerOptions:"

-- | @Selector@ for @expiryTime@
expiryTimeSelector :: Selector '[] (Id NSNumber)
expiryTimeSelector = mkSelector "expiryTime"

-- | @Selector@ for @setExpiryTime:@
setExpiryTimeSelector :: Selector '[Id NSNumber] ()
setExpiryTimeSelector = mkSelector "setExpiryTime:"

