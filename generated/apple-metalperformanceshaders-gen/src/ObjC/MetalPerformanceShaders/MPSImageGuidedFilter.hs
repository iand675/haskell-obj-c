{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageGuidedFilter
--
-- Perform Guided Filter to produce a coefficients image              The filter is broken into two stages:                  - Regression                  - Reconstruction
--
-- The regression stage learns a 4-channel "coefficient" texture (typically at a very low resolution),              and represents the per-pixel linear regression of the source texture to the guidance texture.
--
-- The reconstruction stage upsamples the coefficeints to the same size as the final output and              then at each pixel computes the inner product to produce the output.
--
-- The filter is broken into two stages to allow coefficients to be filtered (such as for example - temporally filtering for video to prevent flicker).
--
-- There is also support for an optional weight texture that can be used to discard values in the source data.
--
-- Guided Filter is described at https://arxiv.org/pdf/1505.00996.pdf.
--
-- Generated bindings for @MPSImageGuidedFilter@.
module ObjC.MetalPerformanceShaders.MPSImageGuidedFilter
  ( MPSImageGuidedFilter
  , IsMPSImageGuidedFilter(..)
  , initWithDevice_kernelDiameter
  , initWithDevice
  , initWithCoder_device
  , encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTexture
  , encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTexture
  , encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureB
  , encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTexture
  , kernelDiameter
  , epsilon
  , setEpsilon
  , reconstructScale
  , setReconstructScale
  , reconstructOffset
  , setReconstructOffset
  , encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTextureSelector
  , encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTextureSelector
  , encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureBSelector
  , encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureSelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_kernelDiameterSelector
  , kernelDiameterSelector
  , reconstructOffsetSelector
  , reconstructScaleSelector
  , setEpsilonSelector
  , setReconstructOffsetSelector
  , setReconstructScaleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies information to apply the guided filter regression.
--
-- @device@ — The device the filter will run on
--
-- @kernelDiameter@ — The local window size
--
-- Returns: A valid MPSImageGuidedFilterRegression object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:kernelDiameter:@
initWithDevice_kernelDiameter :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> RawId -> CULong -> IO (Id MPSImageGuidedFilter)
initWithDevice_kernelDiameter mpsImageGuidedFilter device kernelDiameter =
  sendOwnedMessage mpsImageGuidedFilter initWithDevice_kernelDiameterSelector device kernelDiameter

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> RawId -> IO (Id MPSImageGuidedFilter)
initWithDevice mpsImageGuidedFilter device =
  sendOwnedMessage mpsImageGuidedFilter initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSImageGuidedFilter mpsImageGuidedFilter, IsNSCoder aDecoder) => mpsImageGuidedFilter -> aDecoder -> RawId -> IO (Id MPSImageGuidedFilter)
initWithCoder_device mpsImageGuidedFilter aDecoder device =
  sendOwnedMessage mpsImageGuidedFilter initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Perform Guided Filter Regression (correlation) to produce a coefficients texture
--
-- The filter will not begin to execute until after the command buffer has been enqueued and committed.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @sourceTexture@ — Input source texture to be filtered (typically a mask).  This should be a single channel image.
--
-- @guidanceTexture@ — Input guidance texture.  This should be a color (RGB) image.
--
-- @weightsTexture@ — Optional input confidence texture.  This should also a single channel image.
--
-- @destinationCoefficientsTexture@ — Output texture with four coefficients that minimize the mean squared error between                                         the source and an affine function of guidance R, G, B. Note: The destinationCoefficientsTexture computes the linear cofficients "a" and "b".  The "a" coefficient is       stored in the RGB channels of destinationCoefficientsTexture and the "b" coefficient in the alpha chnanel.
--
-- Set the MPSKernelOptionsAllowReducedPrecision in the "options" property for this kernel to peform the       computations using half-precision arithmetic.  This can potentially improve performance and/or power usage.
--
-- ObjC selector: @- encodeRegressionToCommandBuffer:sourceTexture:guidanceTexture:weightsTexture:destinationCoefficientsTexture:@
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTexture :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTexture mpsImageGuidedFilter commandBuffer sourceTexture guidanceTexture weightsTexture destinationCoefficientsTexture =
  sendMessage mpsImageGuidedFilter encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureSelector commandBuffer sourceTexture guidanceTexture weightsTexture destinationCoefficientsTexture

-- | Perform Guided Filter Reconstruction (inference) to produce the filtered output
--
-- The filter will not begin to execute until after the command buffer has been enqueued and committed.
--
-- sourceGuidanceTexture Input guidance pixel buffer.  This should be a color (RGB) image.  coefficientsTexture   Input coefficients texture generated generated by a previous encodeRegressionToCommandBuffer
--
-- @destinationTexture@ — Output texture
--
-- Note: The coefficients are upsampled at the reconstruction of the filtered data.       Reconstruct(guidance RGB) = a.r * R + a.g * G + a.b * B + b, where a and b       are the coefficients learnt using encodeRegressionToCommandBuffer.
--
-- Final reconstructed value = value * reconstructScale + reconstructOffset
--
-- ObjC selector: @- encodeReconstructionToCommandBuffer:guidanceTexture:coefficientsTexture:destinationTexture:@
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTexture :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTexture mpsImageGuidedFilter commandBuffer guidanceTexture coefficientsTexture destinationTexture =
  sendMessage mpsImageGuidedFilter encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTextureSelector commandBuffer guidanceTexture coefficientsTexture destinationTexture

-- | Perform per-channel (non-color correlated) Guided Filter Regression (correlation) to produce a coefficients texture
--
-- The filter will not begin to execute until after the command buffer has been enqueued and committed.              This encode call differs from the one above in that the correlations are not computed across channels              and therefore this filter computes two coefficient textures: ai and bi.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @sourceTexture@ — Input source texture to be filtered.
--
-- @guidanceTexture@ — Input guidance texture.  This should be a color (RGB) image.
--
-- @weightsTexture@ — Optional input confidence texture.  This should be a single channel image.
--
-- @destinationCoefficientsTextureA@ — Output texture with four coefficients A that minimize the mean squared error between                                         the source channels and an affine function of guidance channels.
--
-- @destinationCoefficientsTextureB@ — Output texture with four coefficients B that minimize the mean squared error between                                         the source channels and an affine function of guidance channels.
--
-- Set the MPSKernelOptionsAllowReducedPrecision in the "options" property for this kernel to peform the       computations using half-precision arithmetic.  This can potentially improve performance and/or power usage.
--
-- ObjC selector: @- encodeRegressionToCommandBuffer:sourceTexture:guidanceTexture:weightsTexture:destinationCoefficientsTextureA:destinationCoefficientsTextureB:@
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureB :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureB mpsImageGuidedFilter commandBuffer sourceTexture guidanceTexture weightsTexture destinationCoefficientsTextureA destinationCoefficientsTextureB =
  sendMessage mpsImageGuidedFilter encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureBSelector commandBuffer sourceTexture guidanceTexture weightsTexture destinationCoefficientsTextureA destinationCoefficientsTextureB

-- | Perform Guided Filter Reconstruction (inference) to produce the filtered output
--
-- The filter will not begin to execute until after the command buffer has been enqueued and committed.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @guidanceTexture@ — Input guidance pixel buffer.
--
-- @coefficientsTextureA@ — Input coefficients A texture generated generated by a previous encodeRegressionToCommandBuffer.
--
-- @coefficientsTextureB@ — Input coefficients B texture generated generated by a previous encodeRegressionToCommandBuffer.
--
-- @destinationTexture@ — Output texture
--
-- ObjC selector: @- encodeReconstructionToCommandBuffer:guidanceTexture:coefficientsTextureA:coefficientsTextureB:destinationTexture:@
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTexture :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTexture mpsImageGuidedFilter commandBuffer guidanceTexture coefficientsTextureA coefficientsTextureB destinationTexture =
  sendMessage mpsImageGuidedFilter encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTextureSelector commandBuffer guidanceTexture coefficientsTextureA coefficientsTextureB destinationTexture

-- | kernelDiameter
--
-- The local window size
--
-- The local window size.
--
-- ObjC selector: @- kernelDiameter@
kernelDiameter :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> IO CULong
kernelDiameter mpsImageGuidedFilter =
  sendMessage mpsImageGuidedFilter kernelDiameterSelector

-- | epsilon
--
-- The regularization parameter
--
-- The parameter used when computing the linear coefficients a and b.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> IO CFloat
epsilon mpsImageGuidedFilter =
  sendMessage mpsImageGuidedFilter epsilonSelector

-- | epsilon
--
-- The regularization parameter
--
-- The parameter used when computing the linear coefficients a and b.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> CFloat -> IO ()
setEpsilon mpsImageGuidedFilter value =
  sendMessage mpsImageGuidedFilter setEpsilonSelector value

-- | reconstructScale
--
-- The scale parameter
--
-- The parameter used to scale the result of the reconstruction operation.              The default value is 1.0f.
--
-- ObjC selector: @- reconstructScale@
reconstructScale :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> IO CFloat
reconstructScale mpsImageGuidedFilter =
  sendMessage mpsImageGuidedFilter reconstructScaleSelector

-- | reconstructScale
--
-- The scale parameter
--
-- The parameter used to scale the result of the reconstruction operation.              The default value is 1.0f.
--
-- ObjC selector: @- setReconstructScale:@
setReconstructScale :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> CFloat -> IO ()
setReconstructScale mpsImageGuidedFilter value =
  sendMessage mpsImageGuidedFilter setReconstructScaleSelector value

-- | reconstructOffset
--
-- The offset parameter
--
-- The offset parameter added to the result of the scaled reconstructed value.              The default value is 0.0f.
--
-- ObjC selector: @- reconstructOffset@
reconstructOffset :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> IO CFloat
reconstructOffset mpsImageGuidedFilter =
  sendMessage mpsImageGuidedFilter reconstructOffsetSelector

-- | reconstructOffset
--
-- The offset parameter
--
-- The offset parameter added to the result of the scaled reconstructed value.              The default value is 0.0f.
--
-- ObjC selector: @- setReconstructOffset:@
setReconstructOffset :: IsMPSImageGuidedFilter mpsImageGuidedFilter => mpsImageGuidedFilter -> CFloat -> IO ()
setReconstructOffset mpsImageGuidedFilter value =
  sendMessage mpsImageGuidedFilter setReconstructOffsetSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:kernelDiameter:@
initWithDevice_kernelDiameterSelector :: Selector '[RawId, CULong] (Id MPSImageGuidedFilter)
initWithDevice_kernelDiameterSelector = mkSelector "initWithDevice:kernelDiameter:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageGuidedFilter)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageGuidedFilter)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeRegressionToCommandBuffer:sourceTexture:guidanceTexture:weightsTexture:destinationCoefficientsTexture:@
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId] ()
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureSelector = mkSelector "encodeRegressionToCommandBuffer:sourceTexture:guidanceTexture:weightsTexture:destinationCoefficientsTexture:"

-- | @Selector@ for @encodeReconstructionToCommandBuffer:guidanceTexture:coefficientsTexture:destinationTexture:@
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTextureSelector :: Selector '[RawId, RawId, RawId, RawId] ()
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTexture_destinationTextureSelector = mkSelector "encodeReconstructionToCommandBuffer:guidanceTexture:coefficientsTexture:destinationTexture:"

-- | @Selector@ for @encodeRegressionToCommandBuffer:sourceTexture:guidanceTexture:weightsTexture:destinationCoefficientsTextureA:destinationCoefficientsTextureB:@
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureBSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeRegressionToCommandBuffer_sourceTexture_guidanceTexture_weightsTexture_destinationCoefficientsTextureA_destinationCoefficientsTextureBSelector = mkSelector "encodeRegressionToCommandBuffer:sourceTexture:guidanceTexture:weightsTexture:destinationCoefficientsTextureA:destinationCoefficientsTextureB:"

-- | @Selector@ for @encodeReconstructionToCommandBuffer:guidanceTexture:coefficientsTextureA:coefficientsTextureB:destinationTexture:@
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId] ()
encodeReconstructionToCommandBuffer_guidanceTexture_coefficientsTextureA_coefficientsTextureB_destinationTextureSelector = mkSelector "encodeReconstructionToCommandBuffer:guidanceTexture:coefficientsTextureA:coefficientsTextureB:destinationTexture:"

-- | @Selector@ for @kernelDiameter@
kernelDiameterSelector :: Selector '[] CULong
kernelDiameterSelector = mkSelector "kernelDiameter"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector '[CFloat] ()
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @reconstructScale@
reconstructScaleSelector :: Selector '[] CFloat
reconstructScaleSelector = mkSelector "reconstructScale"

-- | @Selector@ for @setReconstructScale:@
setReconstructScaleSelector :: Selector '[CFloat] ()
setReconstructScaleSelector = mkSelector "setReconstructScale:"

-- | @Selector@ for @reconstructOffset@
reconstructOffsetSelector :: Selector '[] CFloat
reconstructOffsetSelector = mkSelector "reconstructOffset"

-- | @Selector@ for @setReconstructOffset:@
setReconstructOffsetSelector :: Selector '[CFloat] ()
setReconstructOffsetSelector = mkSelector "setReconstructOffset:"

