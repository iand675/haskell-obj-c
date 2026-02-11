{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPoolingAverage
--
-- This depends on Metal.framework
--
-- Specifies the average pooling filter.  For each pixel, returns the mean value of pixels              in the kernelWidth x kernelHeight filter region.              When edgeMode is MPSImageEdgeModeClamp the filtering window is shrunk to remain              within the source image borders. What this means is that close to image borders the filtering window              will be smaller in order to fit inside the source image and less values will be used to compute the              average. In case the filtering window is entirely outside the source image border the              outputted value will be zero.
--
-- Generated bindings for @MPSCNNPoolingAverage@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingAverage
  ( MPSCNNPoolingAverage
  , IsMPSCNNPoolingAverage(..)
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , zeroPadSizeX
  , setZeroPadSizeX
  , zeroPadSizeY
  , setZeroPadSizeY
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
  , initWithCoder_deviceSelector
  , zeroPadSizeXSelector
  , setZeroPadSizeXSelector
  , zeroPadSizeYSelector
  , setZeroPadSizeYSelector


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

-- | Initialize a MPSCNNPoolingAverage pooling filter
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
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPoolingAverage mpscnnPoolingAverage => mpscnnPoolingAverage -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingAverage)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingAverage  device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendMsg mpscnnPoolingAverage (mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNPoolingAverage mpscnnPoolingAverage, IsNSCoder aDecoder) => mpscnnPoolingAverage -> aDecoder -> RawId -> IO (Id MPSCNNPoolingAverage)
initWithCoder_device mpscnnPoolingAverage  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnPoolingAverage (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | zeroPadSizeX
--
-- How much zero padding to apply to both left and right borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- zeroPadSizeX@
zeroPadSizeX :: IsMPSCNNPoolingAverage mpscnnPoolingAverage => mpscnnPoolingAverage -> IO CULong
zeroPadSizeX mpscnnPoolingAverage  =
  sendMsg mpscnnPoolingAverage (mkSelector "zeroPadSizeX") retCULong []

-- | zeroPadSizeX
--
-- How much zero padding to apply to both left and right borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- setZeroPadSizeX:@
setZeroPadSizeX :: IsMPSCNNPoolingAverage mpscnnPoolingAverage => mpscnnPoolingAverage -> CULong -> IO ()
setZeroPadSizeX mpscnnPoolingAverage  value =
  sendMsg mpscnnPoolingAverage (mkSelector "setZeroPadSizeX:") retVoid [argCULong (fromIntegral value)]

-- | zeroPadSizeY
--
-- How much zero padding to apply to both top and bottom borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- zeroPadSizeY@
zeroPadSizeY :: IsMPSCNNPoolingAverage mpscnnPoolingAverage => mpscnnPoolingAverage -> IO CULong
zeroPadSizeY mpscnnPoolingAverage  =
  sendMsg mpscnnPoolingAverage (mkSelector "zeroPadSizeY") retCULong []

-- | zeroPadSizeY
--
-- How much zero padding to apply to both top and bottom borders of the input image for average pooling,              when using
--
-- See: edgeMode MPSImageEdgeModeClamp. For
--
-- See: edgeMode MPSImageEdgeModeZero this property is              ignored and the area outside the image is interpreted to contain zeros.              The zero padding size is used to shrink the pooling window to fit inside the area bound by the source image              and its padding region, but the effect is that the normalization factor of the average computation is computed              also for the zeros in the padding region.
--
-- ObjC selector: @- setZeroPadSizeY:@
setZeroPadSizeY :: IsMPSCNNPoolingAverage mpscnnPoolingAverage => mpscnnPoolingAverage -> CULong -> IO ()
setZeroPadSizeY mpscnnPoolingAverage  value =
  sendMsg mpscnnPoolingAverage (mkSelector "setZeroPadSizeY:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @zeroPadSizeX@
zeroPadSizeXSelector :: Selector
zeroPadSizeXSelector = mkSelector "zeroPadSizeX"

-- | @Selector@ for @setZeroPadSizeX:@
setZeroPadSizeXSelector :: Selector
setZeroPadSizeXSelector = mkSelector "setZeroPadSizeX:"

-- | @Selector@ for @zeroPadSizeY@
zeroPadSizeYSelector :: Selector
zeroPadSizeYSelector = mkSelector "zeroPadSizeY"

-- | @Selector@ for @setZeroPadSizeY:@
setZeroPadSizeYSelector :: Selector
setZeroPadSizeYSelector = mkSelector "setZeroPadSizeY:"

