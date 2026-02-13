{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoStreamStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoStreamStruct
  ( MTRCameraAVStreamManagementClusterVideoStreamStruct
  , IsMTRCameraAVStreamManagementClusterVideoStreamStruct(..)
  , videoStreamID
  , setVideoStreamID
  , streamUsage
  , setStreamUsage
  , videoCodec
  , setVideoCodec
  , minFrameRate
  , setMinFrameRate
  , maxFrameRate
  , setMaxFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , minBitRate
  , setMinBitRate
  , maxBitRate
  , setMaxBitRate
  , keyFrameInterval
  , setKeyFrameInterval
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , referenceCount
  , setReferenceCount
  , keyFrameIntervalSelector
  , maxBitRateSelector
  , maxFrameRateSelector
  , maxResolutionSelector
  , minBitRateSelector
  , minFrameRateSelector
  , minResolutionSelector
  , osdEnabledSelector
  , referenceCountSelector
  , setKeyFrameIntervalSelector
  , setMaxBitRateSelector
  , setMaxFrameRateSelector
  , setMaxResolutionSelector
  , setMinBitRateSelector
  , setMinFrameRateSelector
  , setMinResolutionSelector
  , setOsdEnabledSelector
  , setReferenceCountSelector
  , setStreamUsageSelector
  , setVideoCodecSelector
  , setVideoStreamIDSelector
  , setWatermarkEnabledSelector
  , streamUsageSelector
  , videoCodecSelector
  , videoStreamIDSelector
  , watermarkEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- videoStreamID@
videoStreamID :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
videoStreamID mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setVideoStreamID mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setVideoStreamIDSelector (toNSNumber value)

-- | @- streamUsage@
streamUsage :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
streamUsage mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct streamUsageSelector

-- | @- setStreamUsage:@
setStreamUsage :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setStreamUsage mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setStreamUsageSelector (toNSNumber value)

-- | @- videoCodec@
videoCodec :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
videoCodec mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct videoCodecSelector

-- | @- setVideoCodec:@
setVideoCodec :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setVideoCodec mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setVideoCodecSelector (toNSNumber value)

-- | @- minFrameRate@
minFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
minFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct minFrameRateSelector

-- | @- setMinFrameRate:@
setMinFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMinFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setMinFrameRateSelector (toNSNumber value)

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct maxFrameRateSelector

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setMaxFrameRateSelector (toNSNumber value)

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct minResolutionSelector

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setMinResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct maxResolutionSelector

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setMaxResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- minBitRate@
minBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
minBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct minBitRateSelector

-- | @- setMinBitRate:@
setMinBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMinBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setMinBitRateSelector (toNSNumber value)

-- | @- maxBitRate@
maxBitRate :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
maxBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct maxBitRateSelector

-- | @- setMaxBitRate:@
setMaxBitRate :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setMaxBitRate mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setMaxBitRateSelector (toNSNumber value)

-- | @- keyFrameInterval@
keyFrameInterval :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
keyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct keyFrameIntervalSelector

-- | @- setKeyFrameInterval:@
setKeyFrameInterval :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setKeyFrameInterval mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setKeyFrameIntervalSelector (toNSNumber value)

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct watermarkEnabledSelector

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setWatermarkEnabledSelector (toNSNumber value)

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct osdEnabledSelector

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setOsdEnabledSelector (toNSNumber value)

-- | @- referenceCount@
referenceCount :: IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct => mtrCameraAVStreamManagementClusterVideoStreamStruct -> IO (Id NSNumber)
referenceCount mtrCameraAVStreamManagementClusterVideoStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct referenceCountSelector

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRCameraAVStreamManagementClusterVideoStreamStruct mtrCameraAVStreamManagementClusterVideoStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoStreamStruct -> value -> IO ()
setReferenceCount mtrCameraAVStreamManagementClusterVideoStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoStreamStruct setReferenceCountSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @streamUsage@
streamUsageSelector :: Selector '[] (Id NSNumber)
streamUsageSelector = mkSelector "streamUsage"

-- | @Selector@ for @setStreamUsage:@
setStreamUsageSelector :: Selector '[Id NSNumber] ()
setStreamUsageSelector = mkSelector "setStreamUsage:"

-- | @Selector@ for @videoCodec@
videoCodecSelector :: Selector '[] (Id NSNumber)
videoCodecSelector = mkSelector "videoCodec"

-- | @Selector@ for @setVideoCodec:@
setVideoCodecSelector :: Selector '[Id NSNumber] ()
setVideoCodecSelector = mkSelector "setVideoCodec:"

-- | @Selector@ for @minFrameRate@
minFrameRateSelector :: Selector '[] (Id NSNumber)
minFrameRateSelector = mkSelector "minFrameRate"

-- | @Selector@ for @setMinFrameRate:@
setMinFrameRateSelector :: Selector '[Id NSNumber] ()
setMinFrameRateSelector = mkSelector "setMinFrameRate:"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector '[] (Id NSNumber)
maxFrameRateSelector = mkSelector "maxFrameRate"

-- | @Selector@ for @setMaxFrameRate:@
setMaxFrameRateSelector :: Selector '[Id NSNumber] ()
setMaxFrameRateSelector = mkSelector "setMaxFrameRate:"

-- | @Selector@ for @minResolution@
minResolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolutionSelector = mkSelector "minResolution"

-- | @Selector@ for @setMinResolution:@
setMinResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setMinResolutionSelector = mkSelector "setMinResolution:"

-- | @Selector@ for @maxResolution@
maxResolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolutionSelector = mkSelector "maxResolution"

-- | @Selector@ for @setMaxResolution:@
setMaxResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setMaxResolutionSelector = mkSelector "setMaxResolution:"

-- | @Selector@ for @minBitRate@
minBitRateSelector :: Selector '[] (Id NSNumber)
minBitRateSelector = mkSelector "minBitRate"

-- | @Selector@ for @setMinBitRate:@
setMinBitRateSelector :: Selector '[Id NSNumber] ()
setMinBitRateSelector = mkSelector "setMinBitRate:"

-- | @Selector@ for @maxBitRate@
maxBitRateSelector :: Selector '[] (Id NSNumber)
maxBitRateSelector = mkSelector "maxBitRate"

-- | @Selector@ for @setMaxBitRate:@
setMaxBitRateSelector :: Selector '[Id NSNumber] ()
setMaxBitRateSelector = mkSelector "setMaxBitRate:"

-- | @Selector@ for @keyFrameInterval@
keyFrameIntervalSelector :: Selector '[] (Id NSNumber)
keyFrameIntervalSelector = mkSelector "keyFrameInterval"

-- | @Selector@ for @setKeyFrameInterval:@
setKeyFrameIntervalSelector :: Selector '[Id NSNumber] ()
setKeyFrameIntervalSelector = mkSelector "setKeyFrameInterval:"

-- | @Selector@ for @watermarkEnabled@
watermarkEnabledSelector :: Selector '[] (Id NSNumber)
watermarkEnabledSelector = mkSelector "watermarkEnabled"

-- | @Selector@ for @setWatermarkEnabled:@
setWatermarkEnabledSelector :: Selector '[Id NSNumber] ()
setWatermarkEnabledSelector = mkSelector "setWatermarkEnabled:"

-- | @Selector@ for @osdEnabled@
osdEnabledSelector :: Selector '[] (Id NSNumber)
osdEnabledSelector = mkSelector "osdEnabled"

-- | @Selector@ for @setOsdEnabled:@
setOsdEnabledSelector :: Selector '[Id NSNumber] ()
setOsdEnabledSelector = mkSelector "setOsdEnabled:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector '[] (Id NSNumber)
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector '[Id NSNumber] ()
setReferenceCountSelector = mkSelector "setReferenceCount:"

