{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageLanczosScale
--
-- Resize an image and / or change its aspect ratio
--
-- The MPSImageLanczosScale filter can be used to resample an existing image              using a different sampling frequency in each dimension. This can be              used to enlarge or reduce the size of an image, or change the aspect              ratio of an image.  The filter uses a Lanczos resampling algorithm              which typically produces better quality for photographs, but is slower              than linear sampling using the GPU texture units. Lanczos downsampling               does not require a low pass filter to be applied before it is used.               Because the resampling function has negative lobes, Lanczos can result               in ringing near sharp edges, making it less suitable for vector art.
--
-- Generated bindings for @MPSImageLanczosScale@.
module ObjC.MetalPerformanceShaders.MPSImageLanczosScale
  ( MPSImageLanczosScale
  , IsMPSImageLanczosScale(..)
  , initWithDevice
  , initWithCoder_device
  , initWithDeviceSelector
  , initWithCoder_deviceSelector


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

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageLanczosScale mpsImageLanczosScale => mpsImageLanczosScale -> RawId -> IO (Id MPSImageLanczosScale)
initWithDevice mpsImageLanczosScale  device =
  sendMsg mpsImageLanczosScale (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSImageLanczosScale mpsImageLanczosScale, IsNSCoder aDecoder) => mpsImageLanczosScale -> aDecoder -> RawId -> IO (Id MPSImageLanczosScale)
initWithCoder_device mpsImageLanczosScale  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsImageLanczosScale (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

