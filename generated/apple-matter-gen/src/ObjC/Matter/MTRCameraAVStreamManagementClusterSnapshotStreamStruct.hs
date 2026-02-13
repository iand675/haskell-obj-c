{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotStreamStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotStreamStruct
  ( MTRCameraAVStreamManagementClusterSnapshotStreamStruct
  , IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct(..)
  , snapshotStreamID
  , setSnapshotStreamID
  , imageCodec
  , setImageCodec
  , frameRate
  , setFrameRate
  , minResolution
  , setMinResolution
  , maxResolution
  , setMaxResolution
  , quality
  , setQuality
  , referenceCount
  , setReferenceCount
  , encodedPixels
  , setEncodedPixels
  , hardwareEncoder
  , setHardwareEncoder
  , watermarkEnabled
  , setWatermarkEnabled
  , osdEnabled
  , setOsdEnabled
  , encodedPixelsSelector
  , frameRateSelector
  , hardwareEncoderSelector
  , imageCodecSelector
  , maxResolutionSelector
  , minResolutionSelector
  , osdEnabledSelector
  , qualitySelector
  , referenceCountSelector
  , setEncodedPixelsSelector
  , setFrameRateSelector
  , setHardwareEncoderSelector
  , setImageCodecSelector
  , setMaxResolutionSelector
  , setMinResolutionSelector
  , setOsdEnabledSelector
  , setQualitySelector
  , setReferenceCountSelector
  , setSnapshotStreamIDSelector
  , setWatermarkEnabledSelector
  , snapshotStreamIDSelector
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

-- | @- snapshotStreamID@
snapshotStreamID :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
snapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct snapshotStreamIDSelector

-- | @- setSnapshotStreamID:@
setSnapshotStreamID :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setSnapshotStreamID mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setSnapshotStreamIDSelector (toNSNumber value)

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct imageCodecSelector

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setImageCodecSelector (toNSNumber value)

-- | @- frameRate@
frameRate :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
frameRate mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct frameRateSelector

-- | @- setFrameRate:@
setFrameRate :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setFrameRate mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setFrameRateSelector (toNSNumber value)

-- | @- minResolution@
minResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
minResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct minResolutionSelector

-- | @- setMinResolution:@
setMinResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setMinResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setMinResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- maxResolution@
maxResolution :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
maxResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct maxResolutionSelector

-- | @- setMaxResolution:@
setMaxResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setMaxResolution mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setMaxResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- quality@
quality :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
quality mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct qualitySelector

-- | @- setQuality:@
setQuality :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setQuality mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setQualitySelector (toNSNumber value)

-- | @- referenceCount@
referenceCount :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
referenceCount mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct referenceCountSelector

-- | @- setReferenceCount:@
setReferenceCount :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setReferenceCount mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setReferenceCountSelector (toNSNumber value)

-- | @- encodedPixels@
encodedPixels :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
encodedPixels mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct encodedPixelsSelector

-- | @- setEncodedPixels:@
setEncodedPixels :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setEncodedPixels mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setEncodedPixelsSelector (toNSNumber value)

-- | @- hardwareEncoder@
hardwareEncoder :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
hardwareEncoder mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct hardwareEncoderSelector

-- | @- setHardwareEncoder:@
setHardwareEncoder :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setHardwareEncoder mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setHardwareEncoderSelector (toNSNumber value)

-- | @- watermarkEnabled@
watermarkEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
watermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct watermarkEnabledSelector

-- | @- setWatermarkEnabled:@
setWatermarkEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setWatermarkEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setWatermarkEnabledSelector (toNSNumber value)

-- | @- osdEnabled@
osdEnabled :: IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> IO (Id NSNumber)
osdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct osdEnabledSelector

-- | @- setOsdEnabled:@
setOsdEnabled :: (IsMTRCameraAVStreamManagementClusterSnapshotStreamStruct mtrCameraAVStreamManagementClusterSnapshotStreamStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotStreamStruct -> value -> IO ()
setOsdEnabled mtrCameraAVStreamManagementClusterSnapshotStreamStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotStreamStruct setOsdEnabledSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @snapshotStreamID@
snapshotStreamIDSelector :: Selector '[] (Id NSNumber)
snapshotStreamIDSelector = mkSelector "snapshotStreamID"

-- | @Selector@ for @setSnapshotStreamID:@
setSnapshotStreamIDSelector :: Selector '[Id NSNumber] ()
setSnapshotStreamIDSelector = mkSelector "setSnapshotStreamID:"

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector '[] (Id NSNumber)
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector '[Id NSNumber] ()
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @frameRate@
frameRateSelector :: Selector '[] (Id NSNumber)
frameRateSelector = mkSelector "frameRate"

-- | @Selector@ for @setFrameRate:@
setFrameRateSelector :: Selector '[Id NSNumber] ()
setFrameRateSelector = mkSelector "setFrameRate:"

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

-- | @Selector@ for @quality@
qualitySelector :: Selector '[] (Id NSNumber)
qualitySelector = mkSelector "quality"

-- | @Selector@ for @setQuality:@
setQualitySelector :: Selector '[Id NSNumber] ()
setQualitySelector = mkSelector "setQuality:"

-- | @Selector@ for @referenceCount@
referenceCountSelector :: Selector '[] (Id NSNumber)
referenceCountSelector = mkSelector "referenceCount"

-- | @Selector@ for @setReferenceCount:@
setReferenceCountSelector :: Selector '[Id NSNumber] ()
setReferenceCountSelector = mkSelector "setReferenceCount:"

-- | @Selector@ for @encodedPixels@
encodedPixelsSelector :: Selector '[] (Id NSNumber)
encodedPixelsSelector = mkSelector "encodedPixels"

-- | @Selector@ for @setEncodedPixels:@
setEncodedPixelsSelector :: Selector '[Id NSNumber] ()
setEncodedPixelsSelector = mkSelector "setEncodedPixels:"

-- | @Selector@ for @hardwareEncoder@
hardwareEncoderSelector :: Selector '[] (Id NSNumber)
hardwareEncoderSelector = mkSelector "hardwareEncoder"

-- | @Selector@ for @setHardwareEncoder:@
setHardwareEncoderSelector :: Selector '[Id NSNumber] ()
setHardwareEncoderSelector = mkSelector "setHardwareEncoder:"

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

