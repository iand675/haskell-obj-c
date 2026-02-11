{-# LANGUAGE PatternSynonyms #-}
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
  , typeSelector
  , metalDeviceSelector

  -- * Enum types
  , MPSGraphDeviceType(MPSGraphDeviceType)
  , pattern MPSGraphDeviceTypeMetal

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
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
    sendClassMsg cls' (mkSelector "deviceWithMTLDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId metalDevice) :: Ptr ())] >>= retainedObject . castPtr

-- | Device of the MPSGraphDevice.
--
-- ObjC selector: @- type@
type_ :: IsMPSGraphDevice mpsGraphDevice => mpsGraphDevice -> IO MPSGraphDeviceType
type_ mpsGraphDevice  =
    fmap (coerce :: CUInt -> MPSGraphDeviceType) $ sendMsg mpsGraphDevice (mkSelector "type") retCUInt []

-- | If device type is Metal then returns the corresponding MTLDevice else nil.
--
-- ObjC selector: @- metalDevice@
metalDevice :: IsMPSGraphDevice mpsGraphDevice => mpsGraphDevice -> IO RawId
metalDevice mpsGraphDevice  =
    fmap (RawId . castPtr) $ sendMsg mpsGraphDevice (mkSelector "metalDevice") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceWithMTLDevice:@
deviceWithMTLDeviceSelector :: Selector
deviceWithMTLDeviceSelector = mkSelector "deviceWithMTLDevice:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @metalDevice@
metalDeviceSelector :: Selector
metalDeviceSelector = mkSelector "metalDevice"

