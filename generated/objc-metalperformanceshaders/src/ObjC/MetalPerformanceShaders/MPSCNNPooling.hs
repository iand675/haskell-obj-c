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
  , initWithDevice_kernelWidth_kernelHeightSelector
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


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
initWithDevice_kernelWidth_kernelHeight mpscnnPooling  device kernelWidth kernelHeight =
  sendMsg mpscnnPooling (mkSelector "initWithDevice:kernelWidth:kernelHeight:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight)] >>= ownedObject . castPtr

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
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPooling  device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendMsg mpscnnPooling (mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

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
initWithCoder_device mpscnnPooling  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnPooling (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNPooling mpscnnPooling => mpscnnPooling -> RawId -> IO (Id MPSCNNPooling)
initWithDevice mpscnnPooling  device =
  sendMsg mpscnnPooling (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

