{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeWebRTCSessionStruct@.
module ObjC.Matter.MTRDataTypeWebRTCSessionStruct
  ( MTRDataTypeWebRTCSessionStruct
  , IsMTRDataTypeWebRTCSessionStruct(..)
  , id_
  , setId
  , peerNodeID
  , setPeerNodeID
  , peerEndpointID
  , setPeerEndpointID
  , streamUsage
  , setStreamUsage
  , videoStreamID
  , setVideoStreamID
  , audioStreamID
  , setAudioStreamID
  , metadataEnabled
  , setMetadataEnabled
  , fabricIndex
  , setFabricIndex
  , audioStreamIDSelector
  , fabricIndexSelector
  , idSelector
  , metadataEnabledSelector
  , peerEndpointIDSelector
  , peerNodeIDSelector
  , setAudioStreamIDSelector
  , setFabricIndexSelector
  , setIdSelector
  , setMetadataEnabledSelector
  , setPeerEndpointIDSelector
  , setPeerNodeIDSelector
  , setStreamUsageSelector
  , setVideoStreamIDSelector
  , streamUsageSelector
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

-- | @- id@
id_ :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
id_ mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct idSelector

-- | @- setId:@
setId :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setId mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setIdSelector (toNSNumber value)

-- | @- peerNodeID@
peerNodeID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
peerNodeID mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct peerNodeIDSelector

-- | @- setPeerNodeID:@
setPeerNodeID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setPeerNodeID mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setPeerNodeIDSelector (toNSNumber value)

-- | @- peerEndpointID@
peerEndpointID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
peerEndpointID mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct peerEndpointIDSelector

-- | @- setPeerEndpointID:@
setPeerEndpointID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setPeerEndpointID mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setPeerEndpointIDSelector (toNSNumber value)

-- | @- streamUsage@
streamUsage :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
streamUsage mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setStreamUsage mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setStreamUsageSelector (toNSNumber value)

-- | @- videoStreamID@
videoStreamID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
videoStreamID mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setVideoStreamID mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setVideoStreamIDSelector (toNSNumber value)

-- | @- audioStreamID@
audioStreamID :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
audioStreamID mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setAudioStreamID mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setAudioStreamIDSelector (toNSNumber value)

-- | @- metadataEnabled@
metadataEnabled :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
metadataEnabled mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct metadataEnabledSelector

-- | @- setMetadataEnabled:@
setMetadataEnabled :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setMetadataEnabled mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setMetadataEnabledSelector (toNSNumber value)

-- | @- fabricIndex@
fabricIndex :: IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct => mtrDataTypeWebRTCSessionStruct -> IO (Id NSNumber)
fabricIndex mtrDataTypeWebRTCSessionStruct =
  sendMessage mtrDataTypeWebRTCSessionStruct fabricIndexSelector

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTRDataTypeWebRTCSessionStruct mtrDataTypeWebRTCSessionStruct, IsNSNumber value) => mtrDataTypeWebRTCSessionStruct -> value -> IO ()
setFabricIndex mtrDataTypeWebRTCSessionStruct value =
  sendMessage mtrDataTypeWebRTCSessionStruct setFabricIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @id@
idSelector :: Selector '[] (Id NSNumber)
idSelector = mkSelector "id"

-- | @Selector@ for @setId:@
setIdSelector :: Selector '[Id NSNumber] ()
setIdSelector = mkSelector "setId:"

-- | @Selector@ for @peerNodeID@
peerNodeIDSelector :: Selector '[] (Id NSNumber)
peerNodeIDSelector = mkSelector "peerNodeID"

-- | @Selector@ for @setPeerNodeID:@
setPeerNodeIDSelector :: Selector '[Id NSNumber] ()
setPeerNodeIDSelector = mkSelector "setPeerNodeID:"

-- | @Selector@ for @peerEndpointID@
peerEndpointIDSelector :: Selector '[] (Id NSNumber)
peerEndpointIDSelector = mkSelector "peerEndpointID"

-- | @Selector@ for @setPeerEndpointID:@
setPeerEndpointIDSelector :: Selector '[Id NSNumber] ()
setPeerEndpointIDSelector = mkSelector "setPeerEndpointID:"

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

-- | @Selector@ for @metadataEnabled@
metadataEnabledSelector :: Selector '[] (Id NSNumber)
metadataEnabledSelector = mkSelector "metadataEnabled"

-- | @Selector@ for @setMetadataEnabled:@
setMetadataEnabledSelector :: Selector '[Id NSNumber] ()
setMetadataEnabledSelector = mkSelector "setMetadataEnabled:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector '[] (Id NSNumber)
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector '[Id NSNumber] ()
setFabricIndexSelector = mkSelector "setFabricIndex:"

