{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPooling
--
-- This depends on Metal.framework
--
-- Pooling is a form of non-linear sub-sampling. Pooling partitions the input image into a set of              rectangles (overlapping or non-overlapping) and, for each such sub-region, outputs a value.              The pooling operation is used in computer vision to reduce the dimensionality of intermediate representations.
--
-- Generated bindings for @MPSCNNPooling@.
module ObjC.MetalPerformanceShaders.MPSCNNPooling
  ( MPSCNNPooling
  , IsMPSCNNPooling(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , initWithDevice
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_kernelWidth_kernelHeightSelector
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize a pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- Returns: A valid MPSCNNPooling object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeight :: IsMPSCNNPooling mpscnnPooling => mpscnnPooling -> RawId -> CULong -> CULong -> IO (Id MPSCNNPooling)
initWithDevice_kernelWidth_kernelHeight mpscnnPooling device kernelWidth kernelHeight =
  sendOwnedMessage mpscnnPooling initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

-- | Initialize a pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- @strideInPixelsX@ — The output stride (downsampling factor) in the x dimension.
--
-- @strideInPixelsY@ — The output stride (downsampling factor) in the y dimension.
--
-- Returns: A valid MPSCNNPooling object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPooling mpscnnPooling => mpscnnPooling -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPooling)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPooling device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnPooling initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight strideInPixelsX strideInPixelsY

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPooling
--
-- @device@ — The MTLDevice on which to make the MPSCNNPooling
--
-- Returns: A new MPSCNNPooling object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNPooling mpscnnPooling, IsNSCoder aDecoder) => mpscnnPooling -> aDecoder -> RawId -> IO (Id MPSCNNPooling)
initWithCoder_device mpscnnPooling aDecoder device =
  sendOwnedMessage mpscnnPooling initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNPooling mpscnnPooling => mpscnnPooling -> RawId -> IO (Id MPSCNNPooling)
initWithDevice mpscnnPooling device =
  sendOwnedMessage mpscnnPooling initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNPooling)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong] (Id MPSCNNPooling)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNPooling)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNPooling)
initWithDeviceSelector = mkSelector "initWithDevice:"

