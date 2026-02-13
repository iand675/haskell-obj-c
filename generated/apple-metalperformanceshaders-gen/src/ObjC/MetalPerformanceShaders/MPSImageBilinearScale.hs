{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageBilinearScale
--
-- Resize an image and / or change its aspect ratio
--
-- The MPSImageBilinearScale filter can be used to resample an existing image              using a bilinear filter. This is typically used to reduce the size of an image.
--
-- Generated bindings for @MPSImageBilinearScale@.
module ObjC.MetalPerformanceShaders.MPSImageBilinearScale
  ( MPSImageBilinearScale
  , IsMPSImageBilinearScale(..)
  , initWithDevice
  , initWithCoder_device
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageBilinearScale mpsImageBilinearScale => mpsImageBilinearScale -> RawId -> IO (Id MPSImageBilinearScale)
initWithDevice mpsImageBilinearScale device =
  sendOwnedMessage mpsImageBilinearScale initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSKernel object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSImageBilinearScale mpsImageBilinearScale, IsNSCoder aDecoder) => mpsImageBilinearScale -> aDecoder -> RawId -> IO (Id MPSImageBilinearScale)
initWithCoder_device mpsImageBilinearScale aDecoder device =
  sendOwnedMessage mpsImageBilinearScale initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageBilinearScale)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageBilinearScale)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

