{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNDilatedPoolingMaxGradient
--
-- This depends on Metal.framework
--
-- Specifies the filter for computing the gradient of the dilated max pooling filter.              For details see comments on MPSCNNPoolingMaxGradient.
--
-- Generated bindings for @MPSCNNDilatedPoolingMaxGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNDilatedPoolingMaxGradient
  ( MPSCNNDilatedPoolingMaxGradient
  , IsMPSCNNDilatedPoolingMaxGradient(..)
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
  , initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector
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

-- | Initialize a MPSCNNDilatedPoolingMaxGradient pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- @dilationRateX@ — The dilation rate in the x dimension.
--
-- @dilationRateY@ — The dilation rate in the y dimension.
--
-- @strideInPixelsX@ — The output stride (downsampling factor) in the x dimension.
--
-- @strideInPixelsY@ — The output stride (downsampling factor) in the y dimension.
--
-- Returns: A valid MPSCNNDilatedPoolingMax object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY :: IsMPSCNNDilatedPoolingMaxGradient mpscnnDilatedPoolingMaxGradient => mpscnnDilatedPoolingMaxGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsY mpscnnDilatedPoolingMaxGradient  device kernelWidth kernelHeight dilationRateX dilationRateY strideInPixelsX strideInPixelsY =
  sendMsg mpscnnDilatedPoolingMaxGradient (mkSelector "initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral dilationRateX), argCULong (fromIntegral dilationRateY), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

-- | @- initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNDilatedPoolingMaxGradient mpscnnDilatedPoolingMaxGradient => mpscnnDilatedPoolingMaxGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNDilatedPoolingMaxGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnDilatedPoolingMaxGradient  device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendMsg mpscnnDilatedPoolingMaxGradient (mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPoolingMaxGradient
--
-- @device@ — The MTLDevice on which to make the MPSCNNPoolingMaxGradient
--
-- Returns: A new MPSCNNPoolingMaxGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNDilatedPoolingMaxGradient mpscnnDilatedPoolingMaxGradient, IsNSCoder aDecoder) => mpscnnDilatedPoolingMaxGradient -> aDecoder -> RawId -> IO (Id MPSCNNDilatedPoolingMaxGradient)
initWithCoder_device mpscnnDilatedPoolingMaxGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnDilatedPoolingMaxGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_dilationRateX_dilationRateY_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:dilationRateX:dilationRateY:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

