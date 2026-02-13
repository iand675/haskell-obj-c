{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that describes the compute device.
--
-- Generated bindings for @MPSGraphDevice@.
module ObjC.MetalPerformanceShadersGraph.MPSGraphDevice
  ( MPSGraphDevice
  , IsMPSGraphDevice(..)
  , deviceWithMTLDevice
  , type_
  , metalDevice
  , deviceWithMTLDeviceSelector
  , metalDeviceSelector
  , typeSelector

  -- * Enum types
  , MPSGraphDeviceType(MPSGraphDeviceType)
  , pattern MPSGraphDeviceTypeMetal

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShadersGraph.Internal.Classes
import ObjC.MetalPerformanceShadersGraph.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates a device from a given Metal device.
--
-- - Parameters:   - metalDevice: @MTLDevice@ to create an MPSGraphDevice from. - Returns: A valid device.
--
-- ObjC selector: @+ deviceWithMTLDevice:@
deviceWithMTLDevice :: RawId -> IO (Id MPSGraphDevice)
deviceWithMTLDevice metalDevice =
  do
    cls' <- getRequiredClass "MPSGraphDevice"
    sendClassMessage cls' deviceWithMTLDeviceSelector metalDevice

-- | Device of the MPSGraphDevice.
--
-- ObjC selector: @- type@
type_ :: IsMPSGraphDevice mpsGraphDevice => mpsGraphDevice -> IO MPSGraphDeviceType
type_ mpsGraphDevice =
  sendMessage mpsGraphDevice typeSelector

-- | If device type is Metal then returns the corresponding MTLDevice else nil.
--
-- ObjC selector: @- metalDevice@
metalDevice :: IsMPSGraphDevice mpsGraphDevice => mpsGraphDevice -> IO RawId
metalDevice mpsGraphDevice =
  sendMessage mpsGraphDevice metalDeviceSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceWithMTLDevice:@
deviceWithMTLDeviceSelector :: Selector '[RawId] (Id MPSGraphDevice)
deviceWithMTLDeviceSelector = mkSelector "deviceWithMTLDevice:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] MPSGraphDeviceType
typeSelector = mkSelector "type"

-- | @Selector@ for @metalDevice@
metalDeviceSelector :: Selector '[] RawId
metalDeviceSelector = mkSelector "metalDevice"

