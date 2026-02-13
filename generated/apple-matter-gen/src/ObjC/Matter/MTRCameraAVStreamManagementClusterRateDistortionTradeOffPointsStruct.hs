{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct
  ( MTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct
  , IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct(..)
  , codec
  , setCodec
  , resolution
  , setResolution
  , minBitRate
  , setMinBitRate
  , codecSelector
  , minBitRateSelector
  , resolutionSelector
  , setCodecSelector
  , setMinBitRateSelector
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

-- | @- codec@
codec :: IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> IO (Id NSNumber)
codec mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct =
  sendMessage mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct codecSelector

-- | @- setCodec:@
setCodec :: (IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> value -> IO ()
setCodec mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct setCodecSelector (toNSNumber value)

-- | @- resolution@
resolution :: IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> IO (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolution mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct =
  sendMessage mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct resolutionSelector

-- | @- setResolution:@
setResolution :: (IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct, IsMTRCameraAVStreamManagementClusterVideoResolutionStruct value) => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> value -> IO ()
setResolution mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct setResolutionSelector (toMTRCameraAVStreamManagementClusterVideoResolutionStruct value)

-- | @- minBitRate@
minBitRate :: IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> IO (Id NSNumber)
minBitRate mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct =
  sendMessage mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct minBitRateSelector

-- | @- setMinBitRate:@
setMinBitRate :: (IsMTRCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct -> value -> IO ()
setMinBitRate mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterRateDistortionTradeOffPointsStruct setMinBitRateSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @codec@
codecSelector :: Selector '[] (Id NSNumber)
codecSelector = mkSelector "codec"

-- | @Selector@ for @setCodec:@
setCodecSelector :: Selector '[Id NSNumber] ()
setCodecSelector = mkSelector "setCodec:"

-- | @Selector@ for @resolution@
resolutionSelector :: Selector '[] (Id MTRCameraAVStreamManagementClusterVideoResolutionStruct)
resolutionSelector = mkSelector "resolution"

-- | @Selector@ for @setResolution:@
setResolutionSelector :: Selector '[Id MTRCameraAVStreamManagementClusterVideoResolutionStruct] ()
setResolutionSelector = mkSelector "setResolution:"

-- | @Selector@ for @minBitRate@
minBitRateSelector :: Selector '[] (Id NSNumber)
minBitRateSelector = mkSelector "minBitRate"

-- | @Selector@ for @setMinBitRate:@
setMinBitRateSelector :: Selector '[Id NSNumber] ()
setMinBitRateSelector = mkSelector "setMinBitRate:"

