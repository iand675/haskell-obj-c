{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct
  ( MTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct
  , IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct(..)
  , resolution
  , setResolution
  , maxFrameRate
  , setMaxFrameRate
  , imageCodec
  , setImageCodec
  , requiresEncodedPixels
  , setRequiresEncodedPixels
  , requiresHardwareEncoder
  , setRequiresHardwareEncoder
  , imageCodecSelector
  , maxFrameRateSelector
  , requiresEncodedPixelsSelector
  , requiresHardwareEncoderSelector
  , resolutionSelector
  , setImageCodecSelector
  , setMaxFrameRateSelector
  , setRequiresEncodedPixelsSelector
  , setRequiresHardwareEncoderSelector
  , setResolutionSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- resolution@
resolution :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolution mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct resolutionSelector

-- | @- setResolution:@
setResolution :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setResolution mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct setResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- maxFrameRate@
maxFrameRate :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
maxFrameRate mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct maxFrameRateSelector

-- | @- setMaxFrameRate:@
setMaxFrameRate :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setMaxFrameRate mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct setMaxFrameRateSelector (toNSNumber value)

-- | @- imageCodec@
imageCodec :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
imageCodec mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct imageCodecSelector

-- | @- setImageCodec:@
setImageCodec :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setImageCodec mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct setImageCodecSelector (toNSNumber value)

-- | @- requiresEncodedPixels@
requiresEncodedPixels :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
requiresEncodedPixels mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct requiresEncodedPixelsSelector

-- | @- setRequiresEncodedPixels:@
setRequiresEncodedPixels :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setRequiresEncodedPixels mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct setRequiresEncodedPixelsSelector (toNSNumber value)

-- | @- requiresHardwareEncoder@
requiresHardwareEncoder :: IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> IO (Id NSNumber)
requiresHardwareEncoder mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct requiresHardwareEncoderSelector

-- | @- setRequiresHardwareEncoder:@
setRequiresHardwareEncoder :: (IsMTRCameraAVStreamManagementClusterSnapshotCapabilitiesStruct mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct -> value -> IO ()
setRequiresHardwareEncoder mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct value =
  sendMessage mtrCameraAVStreamManagementClusterSnapshotCapabilitiesStruct setRequiresHardwareEncoderSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @resolution@
resolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setResolutionSelector = mkSelector "setResolution:"

-- | @Selector@ for @maxFrameRate@
maxFrameRateSelector :: Selector '[] (Id NSNumber)
maxFrameRateSelector = mkSelector "maxFrameRate"

-- | @Selector@ for @setMaxFrameRate:@
setMaxFrameRateSelector :: Selector '[Id NSNumber] ()
setMaxFrameRateSelector = mkSelector "setMaxFrameRate:"

-- | @Selector@ for @imageCodec@
imageCodecSelector :: Selector '[] (Id NSNumber)
imageCodecSelector = mkSelector "imageCodec"

-- | @Selector@ for @setImageCodec:@
setImageCodecSelector :: Selector '[Id NSNumber] ()
setImageCodecSelector = mkSelector "setImageCodec:"

-- | @Selector@ for @requiresEncodedPixels@
requiresEncodedPixelsSelector :: Selector '[] (Id NSNumber)
requiresEncodedPixelsSelector = mkSelector "requiresEncodedPixels"

-- | @Selector@ for @setRequiresEncodedPixels:@
setRequiresEncodedPixelsSelector :: Selector '[Id NSNumber] ()
setRequiresEncodedPixelsSelector = mkSelector "setRequiresEncodedPixels:"

-- | @Selector@ for @requiresHardwareEncoder@
requiresHardwareEncoderSelector :: Selector '[] (Id NSNumber)
requiresHardwareEncoderSelector = mkSelector "requiresHardwareEncoder"

-- | @Selector@ for @setRequiresHardwareEncoder:@
setRequiresHardwareEncoderSelector :: Selector '[Id NSNumber] ()
setRequiresHardwareEncoderSelector = mkSelector "setRequiresHardwareEncoder:"

