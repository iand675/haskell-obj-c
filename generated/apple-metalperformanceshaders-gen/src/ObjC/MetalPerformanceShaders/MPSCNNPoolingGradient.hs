{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNPoolingGradient
--
-- This depends on Metal.framework
--
-- Specifies the base class for computing the gradient of the pooling filters.              The operation backpropagates a gradient vector using the chain rule.
--
-- Given the input gradient vector dL(x) = dL/d out(x), which is the derivative of the              loss-function wrt. (original) pooling filter output the output gradient at position y              (dL/d in(y)) is computed as follows:
--
-- dL/d in(y) = sum_x (dL/d out(x)) * (d out(x)/d in(y)), where
--
-- the sum runs over the input gradient pixels starting from primaryOffset              extending to primaryOffset + sourceSize. Note here that we need a separate              variable 'sourceSize' to specify which input gradients are included in the output              gradient computation as this information cannot be deduced directly from the cliprect              size due to fractional striding or simply because the user wants to examine a subset              of the contributions to the gradients. In normal operation the sourceSize is specified              as the cliprect.size of the forward pooling filter in order to compute the gradients for              all outputs the forward direction produced and the primaryOffset is set to              cliprect.origin of the original forward pooling operation for the same reason.
--
-- The cliprect property of the filter allows the user to send the gradients to a new location,              which may not match the original forward pooling filter window locations:              The index 'y' in the formula above refers to the pixel location in the secondary source,              which is the source image of the original forward pooling filter and secondaryOffset specifies              the center of the first pooling window as specified in MPSCNNPooling filter specification.              The first (top leftmost) pixel written into the cliprect computes the derivative of the first pixel              within the first pooling window that is contained within the secondary source image and              subsequent values are defined by normal striding rules from secondary source to primary source.              This means that typically the cliprect is set to fill the effective source area of the original forward              operation, clamped to edges of the original source image, which in the normal case is the same size              as the size of the gradient destination image.
--
-- If there are any values in the destination cliprect that do not contribute to the forward              pooling result in the area specified by primaryOffset and sourceSize,              due to large strides or dilation factors or simply because all forward pass induced values would be              outside the source area, then those result values are set to zero.
--
-- The actual value of d out(x) / d in(y) depends on the pooling operation and these are defined in the              subclasses of MPSCNNPoolingGradient.
--
-- Generated bindings for @MPSCNNPoolingGradient@.
module ObjC.MetalPerformanceShaders.MPSCNNPoolingGradient
  ( MPSCNNPoolingGradient
  , IsMPSCNNPoolingGradient(..)
  , initWithDevice_kernelWidth_kernelHeight
  , initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY
  , initWithDevice
  , initWithCoder_device
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

-- | Initialize a gradient pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- Returns: A valid MPSCNNPoolingGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeight :: IsMPSCNNPoolingGradient mpscnnPoolingGradient => mpscnnPoolingGradient -> RawId -> CULong -> CULong -> IO (Id MPSCNNPoolingGradient)
initWithDevice_kernelWidth_kernelHeight mpscnnPoolingGradient device kernelWidth kernelHeight =
  sendOwnedMessage mpscnnPoolingGradient initWithDevice_kernelWidth_kernelHeightSelector device kernelWidth kernelHeight

-- | Initialize a gradient pooling filter
--
-- @device@ — The device the filter will run on
--
-- @kernelWidth@ — The width of the kernel.  Can be an odd or even value.
--
-- @kernelHeight@ — The height of the kernel.  Can be an odd or even value.
--
-- @strideInPixelsX@ — The input stride (upsampling factor) in the x dimension.
--
-- @strideInPixelsY@ — The input stride (upsampling factor) in the y dimension.
--
-- Returns: A valid MPSCNNPoolingGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY :: IsMPSCNNPoolingGradient mpscnnPoolingGradient => mpscnnPoolingGradient -> RawId -> CULong -> CULong -> CULong -> CULong -> IO (Id MPSCNNPoolingGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsY mpscnnPoolingGradient device kernelWidth kernelHeight strideInPixelsX strideInPixelsY =
  sendOwnedMessage mpscnnPoolingGradient initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector device kernelWidth kernelHeight strideInPixelsX strideInPixelsY

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNPoolingGradient mpscnnPoolingGradient => mpscnnPoolingGradient -> RawId -> IO (Id MPSCNNPoolingGradient)
initWithDevice mpscnnPoolingGradient device =
  sendOwnedMessage mpscnnPoolingGradient initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSCNNPoolingGradient
--
-- @device@ — The MTLDevice on which to make the MPSCNNPoolingGradient
--
-- Returns: A new MPSCNNPooling object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNPoolingGradient mpscnnPoolingGradient, IsNSCoder aDecoder) => mpscnnPoolingGradient -> aDecoder -> RawId -> IO (Id MPSCNNPoolingGradient)
initWithCoder_device mpscnnPoolingGradient aDecoder device =
  sendOwnedMessage mpscnnPoolingGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:@
initWithDevice_kernelWidth_kernelHeightSelector :: Selector '[RawId, CULong, CULong] (Id MPSCNNPoolingGradient)
initWithDevice_kernelWidth_kernelHeightSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:"

-- | @Selector@ for @initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:@
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector :: Selector '[RawId, CULong, CULong, CULong, CULong] (Id MPSCNNPoolingGradient)
initWithDevice_kernelWidth_kernelHeight_strideInPixelsX_strideInPixelsYSelector = mkSelector "initWithDevice:kernelWidth:kernelHeight:strideInPixelsX:strideInPixelsY:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNPoolingGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNPoolingGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

