{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterAudioStreamStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterAudioStreamStruct
  ( MTRCameraAVStreamManagementClusterAudioStreamStruct
  , IsMTRCameraAVStreamManagementClusterAudioStreamStruct(..)
  , audioStreamID
  , setAudioStreamID
  , streamUsage
  , setStreamUsage
  , audioCodec
  , setAudioCodec
  , channelCount
  , setChannelCount
  , sampleRate
  , setSampleRate
  , bitRate
  , setBitRate
  , bitDepth
  , setBitDepth
  , referenceCount
  , setReferenceCount
  , audioCodecSelector
  , audioStreamIDSelector
  , bitDepthSelector
  , bitRateSelector
  , channelCountSelector
  , referenceCountSelector
  , sampleRateSelector
  , setAudioCodecSelector
  , setAudioStreamIDSelector
  , setBitDepthSelector
  , setBitRateSelector
  , setChannelCountSelector
  , setReferenceCountSelector
  , setSampleRateSelector
  , setStreamUsageSelector
  , streamUsageSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- audioStreamID@
audioStreamID :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
audioStreamID mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct audioStreamIDSelector

-- | @- setAudioStreamID:@
setAudioStreamID :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setAudioStreamID mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setAudioStreamIDSelector (toNSNumber value)

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setStreamUsageSelector (toNSNumber value)

-- | @- audioCodec@
audioCodec :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
audioCodec mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct audioCodecSelector

-- | @- setAudioCodec:@
setAudioCodec :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setAudioCodec mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setAudioCodecSelector (toNSNumber value)

-- | @- channelCount@
channelCount :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
channelCount mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct channelCountSelector

-- | @- setChannelCount:@
setChannelCount :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setChannelCount mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setChannelCountSelector (toNSNumber value)

-- | @- sampleRate@
sampleRate :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
sampleRate mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct sampleRateSelector

-- | @- setSampleRate:@
setSampleRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setSampleRate mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setSampleRateSelector (toNSNumber value)

-- | @- bitRate@
bitRate :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
bitRate mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct bitRateSelector

-- | @- setBitRate:@
setBitRate :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setBitRate mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setBitRateSelector (toNSNumber value)

-- | @- bitDepth@
bitDepth :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
bitDepth mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct bitDepthSelector

-- | @- setBitDepth:@
setBitDepth :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setBitDepth mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setBitDepthSelector (toNSNumber value)

-- | @- referenceCount@
referenceCount :: IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct => mtrCameraAVStreamManagementClusterAudioStreamStruct -> IO (Id NSNumber)
referenceCount mtrCameraAVStreamManagementClusterAudioStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct referenceCountSelector

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRCameraAVStreamManagementClusterAudioStreamStruct mtrCameraAVStreamManagementClusterAudioStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterAudioStreamStruct -> value -> IO ()
setReferenceCount mtrCameraAVStreamManagementClusterAudioStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterAudioStreamStruct setReferenceCountSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @audioStreamID@
audioStreamIDSelector :: Selector '[] (Id NSNumber)
audioStreamIDSelector = mkSelector "audioStreamID"

-- | @Selector@ for @setAudioStreamID:@
setAudioStreamIDSelector :: Selector '[Id NSNumber] ()
setAudioStreamIDSelector = mkSelector "setAudioStreamID:"

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector '[] (Id NSNumber)
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector '[Id NSNumber] ()
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @audioCodec@
audioCodecSelector :: Selector '[] (Id NSNumber)
audioCodecSelector = mkSelector "audioCodec"

-- | @Selector@ for @setAudioCodec:@
setAudioCodecSelector :: Selector '[Id NSNumber] ()
setAudioCodecSelector = mkSelector "setAudioCodec:"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] (Id NSNumber)
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @setChannelCount:@
setChannelCountSelector :: Selector '[Id NSNumber] ()
setChannelCountSelector = mkSelector "setChannelCount:"

-- | @Selector@ for @sampleRate@
sampleRateSelector :: Selector '[] (Id NSNumber)
sampleRateSelector = mkSelector "sampleRate"

-- | @Selector@ for @setSampleRate:@
setSampleRateSelector :: Selector '[Id NSNumber] ()
setSampleRateSelector = mkSelector "setSampleRate:"

-- | @Selector@ for @bitRate@
bitRateSelector :: Selector '[] (Id NSNumber)
bitRateSelector = mkSelector "bitRate"

-- | @Selector@ for @setBitRate:@
setBitRateSelector :: Selector '[Id NSNumber] ()
setBitRateSelector = mkSelector "setBitRate:"

-- | @Selector@ for @bitDepth@
bitDepthSelector :: Selector '[] (Id NSNumber)
bitDepthSelector = mkSelector "bitDepth"

-- | @Selector@ for @setBitDepth:@
setBitDepthSelector :: Selector '[Id NSNumber] ()
setBitDepthSelector = mkSelector "setBitDepth:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector '[] (Id NSNumber)
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector '[Id NSNumber] ()
setReferenceCountSelector = mkSelector "setReferenceCount:"

