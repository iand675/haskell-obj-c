{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageAreaMax
--
-- The MPSImageAreaMax kernel finds the maximum pixel value in a rectangular region centered around each pixel              in the source image. If there are multiple channels in the source image, each channel is processed independently.              The edgeMode property is assumed to always be MPSImageEdgeModeClamp for this filter.
--
-- Generated bindings for @MPSImageAreaMax@.
module ObjC.MetalPerformanceShaders.MPSImageAreaMax
  ( MPSImageAreaMax
  , IsMPSImageAreaMax(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithCoder_device
  , initWithDevice
  , kernelHeight
  , kernelWidth
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_kernelWidth_kernelHeightSelector
  , kernelHeightSelector
  , kernelWidthSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Set the kernel height and width
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel. Must be an odd number.
--
-- @kernelHeight@ — The height of the kernel. Must be an odd number.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeight :: IsMPSImageAreaMax mpsImageAreaMax => mpsImageAreaMax -> RawId -> CULong -> CULong -> IO (Id MPSImageAreaMax)
initWithDevice_kernelWidth_kernelHeight mpsImageAreaMax device kernelWidth kernelHeight =
  sendOwnedMessage mpsImageAreaMax initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

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
initWithCoder_device :: (IsMPSImageAreaMax mpsImageAreaMax, IsNSCoder aDecoder) => mpsImageAreaMax -> aDecoder -> RawId -> IO (Id MPSImageAreaMax)
initWithCoder_device mpsImageAreaMax aDecoder device =
  sendOwnedMessage mpsImageAreaMax initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageAreaMax mpsImageAreaMax => mpsImageAreaMax -> RawId -> IO (Id MPSImageAreaMax)
initWithDevice mpsImageAreaMax device =
  sendOwnedMessage mpsImageAreaMax initWithDeviceSelector device

-- | kernelHeight
--
-- The height of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSImageAreaMax mpsImageAreaMax => mpsImageAreaMax -> IO CULong
kernelHeight mpsImageAreaMax =
  sendMessage mpsImageAreaMax kernelHeightSelector

-- | kernelWidth
--
-- The width of the filter window. Must be an odd number.
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSImageAreaMax mpsImageAreaMax => mpsImageAreaMax -> IO CULong
kernelWidth mpsImageAreaMax =
  sendMessage mpsImageAreaMax kernelWidthSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSImageAreaMax)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageAreaMax)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageAreaMax)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector '[] CULong
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector '[] CULong
kernelWidthSelector = mkSelector "kernelWidth"

