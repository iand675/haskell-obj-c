{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNFullyConnected
--
-- This depends on Metal.framework
--
-- The MPSCNNFullyConnected specifies a fully connected convolution layer a.k.a. Inner product              layer. A fully connected CNN layer is one where every input channel is connected              to every output channel. The kernel width is equal to width of source image              and the kernel height is equal to the height of source image. Width and height of the output              is 1x1. Thus, it takes a srcW x srcH x Ni MPSCNNImage, convolves it with Weights[No][SrcW][srcH][Ni]              and produces a 1 x 1 x No output. The following must be true:
--
-- kernelWidth  == source.width
-- kernelHeight == source.height
-- clipRect.size.width == 1
-- clipRect.size.height == 1
--
-- One can think of a fully connected layer as a matrix multiplication that flattens an image into a vector of length              srcW*srcH*Ni. The weights are arragned in a matrix of dimension No x (srcW*srcH*Ni) for product output vectors              of length No. The strideInPixelsX, strideInPixelsY, and group must be 1. Offset is not applicable and is ignored.              Since clipRect is clamped to the destination image bounds, if the destination is 1x1, one doesn't need to set the              clipRect.
--
-- Note that one can implement an inner product using MPSCNNConvolution by setting
--
-- offset = (kernelWidth/2,kernelHeight/2)
-- clipRect.origin = (ox,oy), clipRect.size = (1,1)
-- strideX = strideY = group = 1
--
-- However, using the MPSCNNFullyConnected for this is better for performance as it lets us choose the most              performant method which may not be possible when using a general convolution. For example,              we may internally use matrix multiplication or special reduction kernels for a specific platform.
--
-- Generated bindings for @MPSCNNFullyConnected@.
module ObjC.MetalPerformanceShaders.MPSCNNFullyConnected
  ( MPSCNNFullyConnected
  , IsMPSCNNFullyConnected(..)
  , initWithDevice_weights
  , initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags
  , initWithCoder_device
  , initWithDevice
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector
  , initWithDevice_weightsSelector

  -- * Enum types
  , MPSCNNConvolutionFlags(MPSCNNConvolutionFlags)
  , pattern MPSCNNConvolutionFlagsNone

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Initializes a fully connected kernel
--
-- @device@ — The MTLDevice on which this MPSCNNFullyConnected filter will be used
--
-- @weights@ — A pointer to a object that conforms to the MPSCNNConvolutionDataSource                                              protocol. The MPSCNNConvolutionDataSource protocol declares the methods that an                                              instance of MPSCNNFullyConnected uses to obtain the weights and bias terms                                              for the CNN fully connected filter.
--
-- Returns: A valid MPSCNNFullyConnected object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:weights:@
initWithDevice_weights :: IsMPSCNNFullyConnected mpscnnFullyConnected => mpscnnFullyConnected -> RawId -> RawId -> IO (Id MPSCNNFullyConnected)
initWithDevice_weights mpscnnFullyConnected device weights =
  sendOwnedMessage mpscnnFullyConnected initWithDevice_weightsSelector device weights

-- | Initializes a convolution kernel              WARNING:                        This API is depreated and will be removed in the future. It cannot be used                                              when training. Also serialization/unserialization wont work for MPSCNNConvolution                                              objects created with this init. Please move onto using initWithDevice:weights:.
--
-- @device@ — The MTLDevice on which this MPSCNNConvolution filter will be used
--
-- @convolutionDescriptor@ — A pointer to a MPSCNNConvolutionDescriptor.
--
-- @kernelWeights@ — A pointer to a weights array.  Each entry is a float value. The number of entries is =                                              inputFeatureChannels * outputFeatureChannels * kernelHeight * kernelWidth                                              The layout of filter weight is so that it can be reinterpreted as 4D tensor (array)                                              weight[ outputChannels ][ kernelHeight ][ kernelWidth ][ inputChannels / groups ]                                              Weights are converted to half float (fp16) internally for best performance.
--
-- @biasTerms@ — A pointer to bias terms to be applied to the convolution output.  Each entry is a float value.                                              The number of entries is = numberOfOutputFeatureMaps
--
-- @flags@ — Currently unused. Pass MPSCNNConvolutionFlagsNone
--
-- Returns: A valid MPSCNNConvolution object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:@
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags :: IsMPSCNNFullyConnected mpscnnFullyConnected => mpscnnFullyConnected -> RawId -> Const (Id MPSCNNConvolutionDescriptor) -> Const (Ptr CFloat) -> Const (Ptr CFloat) -> MPSCNNConvolutionFlags -> IO (Id MPSCNNFullyConnected)
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flags mpscnnFullyConnected device convolutionDescriptor kernelWeights biasTerms flags =
  sendOwnedMessage mpscnnFullyConnected initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector device convolutionDescriptor kernelWeights biasTerms flags

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
initWithCoder_device :: (IsMPSCNNFullyConnected mpscnnFullyConnected, IsNSCoder aDecoder) => mpscnnFullyConnected -> aDecoder -> RawId -> IO (Id MPSCNNFullyConnected)
initWithCoder_device mpscnnFullyConnected aDecoder device =
  sendOwnedMessage mpscnnFullyConnected initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNFullyConnected mpscnnFullyConnected => mpscnnFullyConnected -> RawId -> IO (Id MPSCNNFullyConnected)
initWithDevice mpscnnFullyConnected device =
  sendOwnedMessage mpscnnFullyConnected initWithDeviceSelector device

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:weights:@
initWithDevice_weightsSelector :: Selector '[RawId, RawId] (Id MPSCNNFullyConnected)
initWithDevice_weightsSelector = mkSelector "initWithDevice:weights:"

-- | @Selector@ for @initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:@
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector :: Selector '[RawId, Const (Id MPSCNNConvolutionDescriptor), Const (Ptr CFloat), Const (Ptr CFloat), MPSCNNConvolutionFlags] (Id MPSCNNFullyConnected)
initWithDevice_convolutionDescriptor_kernelWeights_biasTerms_flagsSelector = mkSelector "initWithDevice:convolutionDescriptor:kernelWeights:biasTerms:flags:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNFullyConnected)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNFullyConnected)
initWithDeviceSelector = mkSelector "initWithDevice:"

