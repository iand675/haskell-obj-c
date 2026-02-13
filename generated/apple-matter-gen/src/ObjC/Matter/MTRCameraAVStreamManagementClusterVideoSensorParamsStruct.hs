{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCameraAVStreamManagementClusterVideoSensorParamsStruct@.
module ObjC.Matter.MTRCameraAVStreamManagementClusterVideoSensorParamsStruct
  ( MTRCameraAVStreamManagementClusterVideoSensorParamsStruct
  , IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct(..)
  , sensorWidth
  , setSensorWidth
  , sensorHeight
  , setSensorHeight
  , maxFPS
  , setMaxFPS
  , maxHDRFPS
  , setMaxHDRFPS
  , maxFPSSelector
  , maxHDRFPSSelector
  , sensorHeightSelector
  , sensorWidthSelector
  , setMaxFPSSelector
  , setMaxHDRFPSSelector
  , setSensorHeightSelector
  , setSensorWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- sensorWidth@
sensorWidth :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
sensorWidth mtrCameraAVStreamManagementClusterVideoSensorParamsStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct sensorWidthSelector

-- | @- setSensorWidth:@
setSensorWidth :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setSensorWidth mtrCameraAVStreamManagementClusterVideoSensorParamsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct setSensorWidthSelector (toNSNumber value)

-- | @- sensorHeight@
sensorHeight :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
sensorHeight mtrCameraAVStreamManagementClusterVideoSensorParamsStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct sensorHeightSelector

-- | @- setSensorHeight:@
setSensorHeight :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setSensorHeight mtrCameraAVStreamManagementClusterVideoSensorParamsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct setSensorHeightSelector (toNSNumber value)

-- | @- maxFPS@
maxFPS :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
maxFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct maxFPSSelector

-- | @- setMaxFPS:@
setMaxFPS :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setMaxFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct setMaxFPSSelector (toNSNumber value)

-- | @- maxHDRFPS@
maxHDRFPS :: IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> IO (Id NSNumber)
maxHDRFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct maxHDRFPSSelector

-- | @- setMaxHDRFPS:@
setMaxHDRFPS :: (IsMTRCameraAVStreamManagementClusterVideoSensorParamsStruct mtrCameraAVStreamManagementClusterVideoSensorParamsStruct, IsNSNumber value) => mtrCameraAVStreamManagementClusterVideoSensorParamsStruct -> value -> IO ()
setMaxHDRFPS mtrCameraAVStreamManagementClusterVideoSensorParamsStruct value =
  sendMessage mtrCameraAVStreamManagementClusterVideoSensorParamsStruct setMaxHDRFPSSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sensorWidth@
sensorWidthSelector :: Selector '[] (Id NSNumber)
sensorWidthSelector = mkSelector "sensorWidth"

-- | @Selector@ for @setSensorWidth:@
setSensorWidthSelector :: Selector '[Id NSNumber] ()
setSensorWidthSelector = mkSelector "setSensorWidth:"

-- | @Selector@ for @sensorHeight@
sensorHeightSelector :: Selector '[] (Id NSNumber)
sensorHeightSelector = mkSelector "sensorHeight"

-- | @Selector@ for @setSensorHeight:@
setSensorHeightSelector :: Selector '[Id NSNumber] ()
setSensorHeightSelector = mkSelector "setSensorHeight:"

-- | @Selector@ for @maxFPS@
maxFPSSelector :: Selector '[] (Id NSNumber)
maxFPSSelector = mkSelector "maxFPS"

-- | @Selector@ for @setMaxFPS:@
setMaxFPSSelector :: Selector '[Id NSNumber] ()
setMaxFPSSelector = mkSelector "setMaxFPS:"

-- | @Selector@ for @maxHDRFPS@
maxHDRFPSSelector :: Selector '[] (Id NSNumber)
maxHDRFPSSelector = mkSelector "maxHDRFPS"

-- | @Selector@ for @setMaxHDRFPS:@
setMaxHDRFPSSelector :: Selector '[Id NSNumber] ()
setMaxHDRFPSSelector = mkSelector "setMaxHDRFPS:"

