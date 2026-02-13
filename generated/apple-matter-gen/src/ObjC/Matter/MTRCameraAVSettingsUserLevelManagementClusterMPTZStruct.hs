{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct
  ( MTRCameraAVSettingsUserLevelManagementClusterMPTZStruct
  , IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct(..)
  , pan
  , setPan
  , tilt
  , setTilt
  , zoom
  , setZoom
  , panSelector
  , setPanSelector
  , setTiltSelector
  , setZoomSelector
  , tiltSelector
  , zoomSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- pan@
pan :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> IO (Id NSNumber)
pan mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct panSelector

-- | @- setPan:@
setPan :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> value -> IO ()
setPan mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct setPanSelector (toNSNumber value)

-- | @- tilt@
tilt :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> IO (Id NSNumber)
tilt mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct tiltSelector

-- | @- setTilt:@
setTilt :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> value -> IO ()
setTilt mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct setTiltSelector (toNSNumber value)

-- | @- zoom@
zoom :: IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> IO (Id NSNumber)
zoom mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct zoomSelector

-- | @- setZoom:@
setZoom :: (IsMTRCameraAVSettingsUserLevelManagementClusterMPTZStruct mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct -> value -> IO ()
setZoom mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterMPTZStruct setZoomSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @pan@
panSelector :: Selector '[] (Id NSNumber)
panSelector = mkSelector "pan"

-- | @Selector@ for @setPan:@
setPanSelector :: Selector '[Id NSNumber] ()
setPanSelector = mkSelector "setPan:"

-- | @Selector@ for @tilt@
tiltSelector :: Selector '[] (Id NSNumber)
tiltSelector = mkSelector "tilt"

-- | @Selector@ for @setTilt:@
setTiltSelector :: Selector '[Id NSNumber] ()
setTiltSelector = mkSelector "setTilt:"

-- | @Selector@ for @zoom@
zoomSelector :: Selector '[] (Id NSNumber)
zoomSelector = mkSelector "zoom"

-- | @Selector@ for @setZoom:@
setZoomSelector :: Selector '[Id NSNumber] ()
setZoomSelector = mkSelector "setZoom:"

