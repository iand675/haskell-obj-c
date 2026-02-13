{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVSettingsUserLevelManagementClusterDPTZStruct@.
module ObjC.Matter.MTRCameraAVSettingsUserLevelManagementClusterDPTZStruct
  ( MTRCameraAVSettingsUserLevelManagementClusterDPTZStruct
  , IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct(..)
  , videoStreamID
  , setVideoStreamID
  , viewport
  , setViewport
  , setVideoStreamIDSelector
  , setViewportSelector
  , videoStreamIDSelector
  , viewportSelector


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
videoStreamID :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> IO (Id NSNumber)
videoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct videoStreamIDSelector

-- | @- setVideoStreamID:@
setVideoStreamID :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct, IsNSNumber value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> value -> IO ()
setVideoStreamID mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct setVideoStreamIDSelector (toNSNumber value)

-- | @- viewport@
viewport :: IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> IO (Id MTRDataTypeViewportStruct)
viewport mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct viewportSelector

-- | @- setViewport:@
setViewport :: (IsMTRCameraAVSettingsUserLevelManagementClusterDPTZStruct mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct, IsMTRDataTypeViewportStruct value) => mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct -> value -> IO ()
setViewport mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct value =
  sendMessage mtrCameraAVSettingsUserLevelManagementClusterDPTZStruct setViewportSelector (toMTRDataTypeViewportStruct value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @videoStreamID@
videoStreamIDSelector :: Selector '[] (Id NSNumber)
videoStreamIDSelector = mkSelector "videoStreamID"

-- | @Selector@ for @setVideoStreamID:@
setVideoStreamIDSelector :: Selector '[Id NSNumber] ()
setVideoStreamIDSelector = mkSelector "setVideoStreamID:"

-- | @Selector@ for @viewport@
viewportSelector :: Selector '[] (Id MTRDataTypeViewportStruct)
viewportSelector = mkSelector "viewport"

-- | @Selector@ for @setViewport:@
setViewportSelector :: Selector '[Id MTRDataTypeViewportStruct] ()
setViewportSelector = mkSelector "setViewport:"

