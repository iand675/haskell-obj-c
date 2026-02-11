{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPoolingL2Norm
--
-- This depends on Metal.framework
--
-- Specifies the L2-norm pooling filter.  For each pixel, returns L2-Norm of pixels              in the kernelWidth x kernelHeight filter region.                  out[c,x,y] = sqrt ( sum_{dx,dy} in[c,x+dx,y+dy] * in[c,x+dx,y+dy] ).
--
-- Generated bindings for @MPSCNNPoolingL2Norm@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingL2Norm
  ( MPSCNNPoolingL2Norm
  , IsMPSCNNPoolingL2Norm(..)
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithCoder_device
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

-- | Initialize a MPSCNNPoolingL2Norm pooling filter
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
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPoolingL2Norm mpscnnPoolingL2Norm => mpscnnPoolingL2Norm -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingL2Norm)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingL2Norm  device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendMsg mpscnnPoolingL2Norm (mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (fromIntegral kernelWidth), argCULong (fromIntegral kernelHeight), argCULong (fromIntegral strideInPixelsX), argCULong (fromIntegral strideInPixelsY)] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNPoolingL2Norm mpscnnPoolingL2Norm, IsNSCoder aDecoder) => mpscnnPoolingL2Norm -> aDecoder -> RawId -> IO (Id MPSCNNPoolingL2Norm)
initWithCoder_device mpscnnPoolingL2Norm  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnPoolingL2Norm (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

