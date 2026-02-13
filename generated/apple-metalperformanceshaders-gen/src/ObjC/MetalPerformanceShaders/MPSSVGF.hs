{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Reduces noise in images rendered with Monte Carlo ray tracing methods
--
-- This filter uses temporal reprojection to accumulate samples over time, followed by an edge-avoiding blur to smooth out the noise. It uses depth and surface normal textures to detect edges in the image(s) to be denoised. The filter also computes an estimate of the luminance variance of the accumulated samples for each pixel to reject neighboring pixels whose luminance is too dissimilar while blurring.
--
-- This filter requires noise-free depth and normal textures, so it is not compatible with stochastic visibility effects such as depth of field, motion blur, or pixel subsampling. These effects need to be applied as a post-process instead. Furthermore, because the depth and normal textures can only represent directly visible geometry, the filter may over-blur reflections. The use of temporal reprojection may introduce artifacts such as ghosting or streaking, as well as a temporal lag for changes in luminance such as moving shadows. However, the filter is relatively fast as it is intended for realtime use. Slower but higher quality filters are available in the literature.
--
-- This filter can process up to two images simultaneously assuming they share the same depth and normal textures. This is typically faster than processing the two images independently because memory bandwidth spent fetching depth and normal values and ALU time spent computing various weighting functions can be shared by both images. This is useful if e.g. you want to denoise direct and indirect lighting terms separately to avoid mixing the two terms. The filter is also optimized for processing single-channel images for effects such as shadows and ambient occlusion. Denoising these images can be much faster than denoising a full RGB image, so it may be useful to separate out these terms and denoise them specifically.
--
-- This filter operates in three stages: temporal reprojection, variance estimation, and finally a series of edge-avoiding bilateral blurs. The temporal reprojection stage accepts the image to be denoised for the current frame and the denoised image from the previous frame, the depth and normal textures from the current and previous frame and, finally, a motion vector texture. It uses the motion vector texture to look up the accumulated samples from the previous frame. It then compares the depth and normals to determine if those samples are consistent with the current frame. If so, the previous frame is blended with the current frame. This stage also accumulates the first and second moments of the sample luminance which is used to compute the luminance variance in the next stage.
--
-- The variance estimation stage computes an estimate of the variance of the luminance of the accumulated samples for each pixel. This stage may fall back to a spatial estimate if not enough samples have been accumulated. The luminance variance is used in the final stage to reject outlying neighboring pixels while blurring to avoid blurring across luminance discontinuities such as shadow boundaries.
--
-- The final stage performs consecutive edge-avoiding bilateral blurs to smooth out noise in the image. The blurs are dilated with increasing power of two step distances starting from 1, which cheaply approximates a very large radius bilateral blur. Each iteration blurs both the input image and the variance image as variance is reduced after each iteration. It is recommended that the output of the first iteration be used as the input to the next frame's reprojection stage to further reduce noise.
--
-- Tips:
--
-- - It may be helpful to further divide out texture details such as surface albedo before   denoising to avoid blurring texture detail and to preserve any careful texture filtering that   may have been performed. The albedo can be reapplied after denoising. - High frequency geometry and normal maps may cause excessive disocclusions during reprojection   manifesting as noise. - Jittering sample positions from frame to frame for temporal antialiasing may also cause   disocclusions. However, this can be partially hidden by the temporal antialiasing algorithm   itself. - This kernel, like many convolutions, requires quite a bit of bandwidth. Use the texture pixel   formats with the smallest number of bits-per-pixel and the lowest resolution possible for the   required quality level. Lower resolution images can be combined with a bilateral upsampling   filter, especially if the image being denoised is mostly low frequency lighting or ambient   occlusion. - The increasing dilation during the bilateral blurring stage can introduce ringing artifacts   around geometric discontinuities. These can be partially hidden at the cost of potentially   increased noise by reducing the bilateral blur's sigma value slightly after each iteration. - Use lower precision pixel formats if possible to reduce memory bandwidth.
--
-- Refer to "Spatiotemporal Variance-Guided Filtering: Real-Time Reconstruction for Path-Traced Global Illumination" for more information.
--
-- Generated bindings for @MPSSVGF@.
module ObjC.MetalPerformanceShaders.MPSSVGF
  ( MPSSVGF
  , IsMPSSVGF(..)
  , initWithDevice
  , initWithCoder_device
  , copyWithZone_device
  , encodeWithCoder
  , encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture
  , encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture
  , encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTexture
  , encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTexture
  , encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTexture
  , encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTexture
  , depthWeight
  , setDepthWeight
  , normalWeight
  , setNormalWeight
  , luminanceWeight
  , setLuminanceWeight
  , temporalWeighting
  , setTemporalWeighting
  , temporalReprojectionBlendFactor
  , setTemporalReprojectionBlendFactor
  , reprojectionThreshold
  , setReprojectionThreshold
  , minimumFramesForVarianceEstimation
  , setMinimumFramesForVarianceEstimation
  , varianceEstimationRadius
  , setVarianceEstimationRadius
  , varianceEstimationSigma
  , setVarianceEstimationSigma
  , variancePrefilterSigma
  , setVariancePrefilterSigma
  , variancePrefilterRadius
  , setVariancePrefilterRadius
  , bilateralFilterSigma
  , setBilateralFilterSigma
  , bilateralFilterRadius
  , setBilateralFilterRadius
  , channelCount
  , setChannelCount
  , channelCount2
  , setChannelCount2
  , bilateralFilterRadiusSelector
  , bilateralFilterSigmaSelector
  , channelCount2Selector
  , channelCountSelector
  , copyWithZone_deviceSelector
  , depthWeightSelector
  , encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTextureSelector
  , encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTextureSelector
  , encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector
  , encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector
  , encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTextureSelector
  , encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTextureSelector
  , encodeWithCoderSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , luminanceWeightSelector
  , minimumFramesForVarianceEstimationSelector
  , normalWeightSelector
  , reprojectionThresholdSelector
  , setBilateralFilterRadiusSelector
  , setBilateralFilterSigmaSelector
  , setChannelCount2Selector
  , setChannelCountSelector
  , setDepthWeightSelector
  , setLuminanceWeightSelector
  , setMinimumFramesForVarianceEstimationSelector
  , setNormalWeightSelector
  , setReprojectionThresholdSelector
  , setTemporalReprojectionBlendFactorSelector
  , setTemporalWeightingSelector
  , setVarianceEstimationRadiusSelector
  , setVarianceEstimationSigmaSelector
  , setVariancePrefilterRadiusSelector
  , setVariancePrefilterSigmaSelector
  , temporalReprojectionBlendFactorSelector
  , temporalWeightingSelector
  , varianceEstimationRadiusSelector
  , varianceEstimationSigmaSelector
  , variancePrefilterRadiusSelector
  , variancePrefilterSigmaSelector

  -- * Enum types
  , MPSTemporalWeighting(MPSTemporalWeighting)
  , pattern MPSTemporalWeightingAverage
  , pattern MPSTemporalWeightingExponentialMovingAverage

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> IO (Id MPSSVGF)
initWithDevice mpssvgf device =
  sendOwnedMessage mpssvgf initWithDeviceSelector device

-- | @- initWithCoder:device:@
initWithCoder_device :: (IsMPSSVGF mpssvgf, IsNSCoder aDecoder) => mpssvgf -> aDecoder -> RawId -> IO (Id MPSSVGF)
initWithCoder_device mpssvgf aDecoder device =
  sendOwnedMessage mpssvgf initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | @- copyWithZone:device:@
copyWithZone_device :: IsMPSSVGF mpssvgf => mpssvgf -> Ptr () -> RawId -> IO (Id MPSSVGF)
copyWithZone_device mpssvgf zone device =
  sendOwnedMessage mpssvgf copyWithZone_deviceSelector zone device

-- | @- encodeWithCoder:@
encodeWithCoder :: (IsMPSSVGF mpssvgf, IsNSCoder coder) => mpssvgf -> coder -> IO ()
encodeWithCoder mpssvgf coder =
  sendMessage mpssvgf encodeWithCoderSelector (toNSCoder coder)

-- | Encode reprojection into a command buffer
--
-- Normal and depth values from the previous frame will be compared with normal and depth values from the current frame to determine if they are similar enough to reproject into the current frame. These values are weighted by the depthWeight and normalWeight properties. If the combined weight exceeds the reprojectionThreshold property's value, the previous frame will be blended with the current frame according to the temporalWeighting and temporalReprojectionBlendFactor properties.
--
-- The reprojection kernel can operate on two sets of source and destination textures simultaneously to share costs such as loading depth and normal values from memory, computing various weights, etc. The second set of textures may be nil. The two images are assumed to share the same depth and normal values.
--
-- The number of channels in the source image(s), previous frame's image(s), and destination image(s) are given by the channelCount and channelCount2 properties. These images must have at least as many channels as given by these properties. Channels beyond the required number are ignored when reading from source images and set to zero when writing to the destination images, except the alpha channel which will be set to one if present. The previous frame's image will be ignored on the first frame.
--
-- The source and destination luminance moments textures must be at least two-channel textures, which will be set to the accumulated first and second moments of luminance. Channels beyond the first two will be ignored when reading from the previous frame's texture and set to zero when writing to the destination texture. The previous frame's luminance moments will be ignored on the first frame.
--
-- The frame count textures track the number of accumulated frames and must be at least R32Uint textures. The remaining channels will be ignored when reading from the source texture and set to zero when writing to the destination texture, if present. The previous frame count texture must be cleared to zero on the first frame or to reset the accumulated images to the current frame's image.
--
-- The motion vector texture must be at least a two channel texture representing how many texels each texel in the source image(s) have moved since the previous frame. The remaining channels will be ignored if present. This texture may be nil, in which case the motion vector is assumed to be zero, which is suitable for static images.
--
-- The depth/normal texture must contain the depth and normal values for directly visible geometry for the current frame for each pixel. These values are packed into a four channel texture to reduce the number of texture sampling instructions required to load them. The first channel must store the depth value from zero to infinity. The normals must be stored in the last three channels as the three signed X, Y, and z components each between negative one and one.  The depth and normal values are not required if the motion vector texture is nil.
--
-- The destination texture, destination luminance moments texture, and destination frame count texture are used by subsequent stages of the denoising filter. The destination frame count texture is also used as the source frame count texture the reprojection kernel in the next frame.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @sourceTexture@ — Current frame to denoise
--
-- @previousTexture@ — Previous denoised frame to reproject into current                                            frame
--
-- @destinationTexture@ — Output blended image
--
-- @previousLuminanceMomentsTexture@ — Previous accumulated luminance moments image
--
-- @destinationLuminanceMomentsTexture@ — Output accumulated luminance moments image
--
-- @previousFrameCountTexture@ — The number of frames accumulated in the previous                                            source image
--
-- @destinationFrameCountTexture@ — The number of frames accumulated in the destination                                            texture(s) including the current frame
--
-- @motionVectorTexture@ — Motion vector texture
--
-- @depthNormalTexture@ — The depth and normal values for the current frame
--
-- @previousDepthNormalTexture@ — The depth and normal values for the previous frame
--
-- ObjC selector: @- encodeReprojectionToCommandBuffer:sourceTexture:previousTexture:destinationTexture:previousLuminanceMomentsTexture:destinationLuminanceMomentsTexture:previousFrameCountTexture:destinationFrameCountTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture mpssvgf commandBuffer sourceTexture previousTexture destinationTexture previousLuminanceMomentsTexture destinationLuminanceMomentsTexture previousFrameCountTexture destinationFrameCountTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture =
  sendMessage mpssvgf encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector commandBuffer sourceTexture previousTexture destinationTexture previousLuminanceMomentsTexture destinationLuminanceMomentsTexture previousFrameCountTexture destinationFrameCountTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture

-- | Encode reprojection into a command buffer
--
-- Normal and depth values from the previous frame will be compared with normal and depth values from the current frame to determine if they are similar enough to reproject into the current frame. These values are weighted by the depthWeight and normalWeight properties. If the combined weight exceeds the reprojectionThreshold property's value, the previous frame will be blended with the current frame according to the temporalWeighting and temporalReprojectionBlendFactor properties.
--
-- The reprojection kernel can operate on two sets of source and destination textures simultaneously to share costs such as loading depth and normal values from memory, computing various weights, etc. The second set of textures may be nil. The two images are assumed to share the same depth and normal values.
--
-- The number of channels in the source image(s), previous frame's image(s), and destination image(s) are given by the channelCount and channelCount2 properties. These images must have at least as many channels as given by these properties. Channels beyond the required number are ignored when reading from source images and set to zero when writing to the destination images, except the alpha channel which will be set to one if present. The previous frame's image will be ignored on the first frame.
--
-- The source and destination luminance moments textures must be at least two-channel textures, which will be set to the accumulated first and second moments of luminance. Channels beyond the first two will be ignored when reading from the previous frame's texture and set to zero when writing to the destination texture. The previous frame's luminance moments will be ignored on the first frame.
--
-- The frame count textures track the number of accumulated frames and must be at least R32Uint textures. The remaining channels will be ignored when reading from the source texture and set to zero when writing to the destination texture, if present. The previous frame count texture must be cleared to zero on the first frame or to reset the accumulated images to the current frame's image.
--
-- The motion vector texture must be at least a two channel texture representing how many texels each texel in the source image(s) have moved since the previous frame. The remaining channels will be ignored if present. This texture may be nil, in which case the motion vector is assumed to be zero, which is suitable for static images.
--
-- The depth/normal texture must contain the depth and normal values for directly visible geometry for the current frame for each pixel. These values are packed into a four channel texture to reduce the number of texture sampling instructions required to load them. The first channel must store the depth value from zero to infinity. The normals must be stored in the last three channels as the three signed X, Y, and z components each between negative one and one.  The depth and normal values are not required if the motion vector texture is nil.
--
-- The destination texture, destination luminance moments texture, and destination frame count texture are used by subsequent stages of the denoising filter. The destination frame count texture is also used as the source frame count texture the reprojection kernel in the next frame.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @sourceTexture@ — Current frame to denoise
--
-- @previousTexture@ — Previous denoised frame to reproject into current                                            frame
--
-- @destinationTexture@ — Output blended image
--
-- @previousLuminanceMomentsTexture@ — Previous accumulated luminance moments image
--
-- @destinationLuminanceMomentsTexture@ — Output accumulated luminance moments image
--
-- @sourceTexture2@ — Second source image
--
-- @previousTexture2@ — Second previous image
--
-- @destinationTexture2@ — Second destination image
--
-- @previousLuminanceMomentsTexture2@ — Second previous luminance moments texture
--
-- @destinationLuminanceMomentsTexture2@ — Second destination luminance moments texture
--
-- @previousFrameCountTexture@ — The number of frames accumulated in the previous                                            source image
--
-- @destinationFrameCountTexture@ — The number of frames accumulated in the destination                                            texture(s) including the current frame
--
-- @motionVectorTexture@ — Motion vector texture
--
-- @depthNormalTexture@ — The depth and normal values for the current frame
--
-- @previousDepthNormalTexture@ — The depth and normal values for the previous frame
--
-- ObjC selector: @- encodeReprojectionToCommandBuffer:sourceTexture:previousTexture:destinationTexture:previousLuminanceMomentsTexture:destinationLuminanceMomentsTexture:sourceTexture2:previousTexture2:destinationTexture2:previousLuminanceMomentsTexture2:destinationLuminanceMomentsTexture2:previousFrameCountTexture:destinationFrameCountTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTexture mpssvgf commandBuffer sourceTexture previousTexture destinationTexture previousLuminanceMomentsTexture destinationLuminanceMomentsTexture sourceTexture2 previousTexture2 destinationTexture2 previousLuminanceMomentsTexture2 destinationLuminanceMomentsTexture2 previousFrameCountTexture destinationFrameCountTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture =
  sendMessage mpssvgf encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector commandBuffer sourceTexture previousTexture destinationTexture previousLuminanceMomentsTexture destinationLuminanceMomentsTexture sourceTexture2 previousTexture2 destinationTexture2 previousLuminanceMomentsTexture2 destinationLuminanceMomentsTexture2 previousFrameCountTexture destinationFrameCountTexture motionVectorTexture depthNormalTexture previousDepthNormalTexture

-- | Encode variance estimation into a command buffer
--
-- Variance is computed from the accumulated first and second luminance moments. If the number of accumulated frames is below the minimumFramesForVarianceEstimation property, the luminance variance will be computed using a spatial estimate instead. The spatial estimate is computed using a bilateral filter with radius given by the varianceEstimationRadius property. Neighboring samples will be weighted according to a gaussian function with sigma given by the varianceEstimationSigma property. Normal and depth values from neighboring pixels will be compared with depth and normal values of the center pixel to determine if they are similar enough to include in the spatial blur. These values are weighted by the depthWeight and normalWeight properties.
--
-- The variance kernel can operate on two sets of source and destination textures simultaneously to share costs such as loading depth and normal values from memory, computing various weights, etc. The second set of textures may be nil. The two images are assumed to share the same depth and normal values.
--
-- The reprojected source texture, luminance moments texture and frame count texture are computed by the reprojection kernel.
--
-- The computed variance will be stored in the last channel of the destination image, while the source image will be copied into the previous channels, to reduce the number of texture sample instructured required by the bilateral filter in the final stage of the denoising kernel. The number of channels in the source image(s) are given by the channelCount and channelCount2 properties. Therefore, the destination image(s) must have at least channelCount + 1 and channelCount2 + 1 channels and the source image(s) must have at least channelCount and channelCount2 channels. Channels beyond the required number are ignored when reading from source textures and set to zero when writing to destination textures.
--
-- The depth/normal texture must contain the depth and normal values for directly visible geometry for the current frame for each pixel. These values are packed into a four channel texture to reduce the number of texture sampling instructions required to load them. The first channel must store the depth value from zero to infinity. The normals must be stored in the last three channels as the three signed X, Y, and z components each between negative one and one. If the minimumFramesForVarianceEstimation property is less than or equal to one, variance will be estimated directly from the accumulated luminance moments so the depth/normal texture may be nil.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @sourceTexture@ — Current reprojected frame to denoise
--
-- @luminanceMomentsTexture@ — Luminance moments texture
--
-- @destinationTexture@ — Output packed color and variance image
--
-- @frameCountTexture@ — Number of frames accumulated into the source image
--
-- @depthNormalTexture@ — The depth and normal values for the current frame
--
-- ObjC selector: @- encodeVarianceEstimationToCommandBuffer:sourceTexture:luminanceMomentsTexture:destinationTexture:frameCountTexture:depthNormalTexture:@
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTexture :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTexture mpssvgf commandBuffer sourceTexture luminanceMomentsTexture destinationTexture frameCountTexture depthNormalTexture =
  sendMessage mpssvgf encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTextureSelector commandBuffer sourceTexture luminanceMomentsTexture destinationTexture frameCountTexture depthNormalTexture

-- | Encode variance estimation into a command buffer
--
-- Variance is computed from the accumulated first and second luminance moments. If the number of accumulated frames is below the minimumFramesForVarianceEstimation property, the luminance variance will be computed using a spatial estimate instead. The spatial estimate is computed using a bilateral filter with radius given by the varianceEstimationRadius property. Neighboring samples will be weighted according to a gaussian function with sigma given by the varianceEstimationSigma property. Normal and depth values from neighboring pixels will be compared with depth and normal values of the center pixel to determine if they are similar enough to include in the spatial blur. These values are weighted by the depthWeight and normalWeight properties.
--
-- The variance kernel can operate on two sets of source and destination textures simultaneously to share costs such as loading depth and normal values from memory, computing various weights, etc. The second set of textures may be nil. The two images are assumed to share the same depth and normal values.
--
-- The reprojected source texture, luminance moments texture and frame count texture are computed by the reprojection kernel.
--
-- The computed variance will be stored in the last channel of the destination image, while the source image will be copied into the previous channels, to reduce the number of texture sample instructured required by the bilateral filter in the final stage of the denoising kernel. The number of channels in the source image(s) are given by the channelCount and channelCount2 properties. Therefore, the destination image(s) must have at least channelCount + 1 and channelCount2 + 1 channels and the source image(s) must have at least channelCount and channelCount2 channels. Channels beyond the required number are ignored when reading from source textures and set to zero when writing to destination textures.
--
-- The depth/normal texture must contain the depth and normal values for directly visible geometry for the current frame for each pixel. These values are packed into a four channel texture to reduce the number of texture sampling instructions required to load them. The first channel must store the depth value from zero to infinity. The normals must be stored in the last three channels as the three signed X, Y, and z components each between negative one and one. If the minimumFramesForVarianceEstimation property is less than or equal to one, variance will be estimated directly from the accumulated luminance moments so the depth/normal texture may be nil.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @sourceTexture@ — Current reprojected frame to denoise
--
-- @luminanceMomentsTexture@ — Luminance moments texture
--
-- @destinationTexture@ — Output packed color and variance image
--
-- @sourceTexture2@ — Second source image
--
-- @luminanceMomentsTexture2@ — Second luminance moments image
--
-- @destinationTexture2@ — Second destination image
--
-- @frameCountTexture@ — Number of frames accumulated into the source image
--
-- @depthNormalTexture@ — The depth and normal values for the current frame
--
-- ObjC selector: @- encodeVarianceEstimationToCommandBuffer:sourceTexture:luminanceMomentsTexture:destinationTexture:sourceTexture2:luminanceMomentsTexture2:destinationTexture2:frameCountTexture:depthNormalTexture:@
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTexture :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTexture mpssvgf commandBuffer sourceTexture luminanceMomentsTexture destinationTexture sourceTexture2 luminanceMomentsTexture2 destinationTexture2 frameCountTexture depthNormalTexture =
  sendMessage mpssvgf encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTextureSelector commandBuffer sourceTexture luminanceMomentsTexture destinationTexture sourceTexture2 luminanceMomentsTexture2 destinationTexture2 frameCountTexture depthNormalTexture

-- | Encode bilateral filter into a command buffer
--
-- Performs an edge avoiding blur with radius given by the bilateraFilterRadius property with sampling weighted by a Gaussian filter with sigma given by the bilteralFilterSigma property. Normal and depth values from neighboring pixels will be compared with depth and normal values of the center pixel to determine if they are similar enough to include in the blur. These values are weighted by the depthWeight, normalWeight, and luminanceWeight properties.
--
-- Before the variance values are used for luminance weighting, the variance is prefiltered with a small Gaussian blur with radius given by the variancePrefilterRadius property and sigma given by the variancePrefilterSigma property.
--
-- This kernel should be run multiple times with a step distance of pow(2, i), starting with i = 0. It is recommended that the output of the first iteration be used as the image to be reprojected in the next frame. Then several more iterations should be run to compute the denoised image for the current frame. 5 total iterations is reasonable.
--
-- The bilateral filter can operate on two sets of source and destination textures simultaneously to share costs such as loading depth and normal values from memory, computing various weights, etc. The second set of textures may be nil. The two images are assumed to share the same normal and depth values.
--
-- The number of channels to filter in the source image(s) are given by the channelCount and channelCount2 properties. Furthermore, the luminance variance is packed into the final channel of the source image(s) to reduce the number of texture sample instructions required. The filtered color and variance values are packed the same way in the destination image(s). Therefore, the source and destination images must have at least channelCount + 1 and channelCount2 + 1 channels. Channels beyond the required number are ignored when reading from source images and set to zero when writing to destination images. The source image should be produced by either the variance estimation kernel or a previous iteration of the bilateral filter.
--
-- The depth/normal texture must contain the depth and normal values for directly visible geometry for the current frame for each pixel. These values are packed into a four channel texture to reduce the number of texture sampling instructions required to load them. The first channel must store the depth value from zero to infinity. The normals must be stored in the last three channels as the three signed X, Y, and z components each between negative one and one.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @stepDistance@ — Number of pixels to skip between samples
--
-- @sourceTexture@ — Source packed color and variance texture
--
-- @destinationTexture@ — Destination packed color and variance texture
--
-- @depthNormalTexture@ — The depth and normal values for the current frame
--
-- ObjC selector: @- encodeBilateralFilterToCommandBuffer:stepDistance:sourceTexture:destinationTexture:depthNormalTexture:@
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTexture :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> CULong -> RawId -> RawId -> RawId -> IO ()
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTexture mpssvgf commandBuffer stepDistance sourceTexture destinationTexture depthNormalTexture =
  sendMessage mpssvgf encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTextureSelector commandBuffer stepDistance sourceTexture destinationTexture depthNormalTexture

-- | Encode bilateral filter into a command buffer
--
-- Performs an edge avoiding blur with radius given by the bilateraFilterRadius property with sampling weighted by a Gaussian filter with sigma given by the bilteralFilterSigma property. Normal and depth values from neighboring pixels will be compared with depth and normal values of the center pixel to determine if they are similar enough to include in the blur. These values are weighted by the depthWeight, normalWeight, and luminanceWeight properties.
--
-- Before the variance values are used for luminance weighting, the variance is prefiltered with a small Gaussian blur with radius given by the variancePrefilterRadius property and sigma given by the variancePrefilterSigma property.
--
-- This kernel should be run multiple times with a step distance of pow(2, i), starting with i = 0. It is recommended that the output of the first iteration be used as the image to be reprojected in the next frame. Then several more iterations should be run to compute the denoised image for the current frame. 5 total iterations is reasonable.
--
-- The bilateral filter can operate on two sets of source and destination textures simultaneously to share costs such as loading depth and normal values from memory, computing various weights, etc. The second set of textures may be nil. The two images are assumed to share the same normal and depth values.
--
-- The number of channels to filter in the source image(s) are given by the channelCount and channelCount2 properties. Furthermore, the luminance variance is packed into the final channel of the source image(s) to reduce the number of texture sample instructions required. The filtered color and variance values are packed the same way in the destination image(s). Therefore, the source and destination images must have at least channelCount + 1 and channelCount2 + 1 channels. Channels beyond the required number are ignored when reading from source images and set to zero when writing to destination images. The source image should be produced by either the variance estimation kernel or a previous iteration of the bilateral filter.
--
-- The depth/normal texture must contain the depth and normal values for directly visible geometry for the current frame for each pixel. These values are packed into a four channel texture to reduce the number of texture sampling instructions required to load them. The first channel must store the depth value from zero to infinity. The normals must be stored in the last three channels as the three signed X, Y, and z components each between negative one and one.
--
-- @commandBuffer@ — Command buffer to encode into
--
-- @stepDistance@ — Number of pixels to skip between samples
--
-- @sourceTexture@ — Source packed color and variance texture
--
-- @destinationTexture@ — Destination packed color and variance texture
--
-- @sourceTexture2@ — Second source image
--
-- @destinationTexture2@ — Second destination image
--
-- @depthNormalTexture@ — The depth and normal values for the current frame
--
-- ObjC selector: @- encodeBilateralFilterToCommandBuffer:stepDistance:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:depthNormalTexture:@
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTexture :: IsMPSSVGF mpssvgf => mpssvgf -> RawId -> CULong -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTexture mpssvgf commandBuffer stepDistance sourceTexture destinationTexture sourceTexture2 destinationTexture2 depthNormalTexture =
  sendMessage mpssvgf encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTextureSelector commandBuffer stepDistance sourceTexture destinationTexture sourceTexture2 destinationTexture2 depthNormalTexture

-- | Controls how samples' depths are compared during reprojection, variance estimation, and bilateral filtering. The final weight is given by exp(-abs(Z1 - Z2) / depthWeight). Must be greater than zero. Defaults to 1.0.
--
-- ObjC selector: @- depthWeight@
depthWeight :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
depthWeight mpssvgf =
  sendMessage mpssvgf depthWeightSelector

-- | Controls how samples' depths are compared during reprojection, variance estimation, and bilateral filtering. The final weight is given by exp(-abs(Z1 - Z2) / depthWeight). Must be greater than zero. Defaults to 1.0.
--
-- ObjC selector: @- setDepthWeight:@
setDepthWeight :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setDepthWeight mpssvgf value =
  sendMessage mpssvgf setDepthWeightSelector value

-- | Controls how samples' normals are compared during reprojection, variance estimation, and bilateral filtering. The final weight is given by pow(max(dot(N1, N2)), normalWeight). Must be greater than or equal to zero. Defaults to 128.
--
-- ObjC selector: @- normalWeight@
normalWeight :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
normalWeight mpssvgf =
  sendMessage mpssvgf normalWeightSelector

-- | Controls how samples' normals are compared during reprojection, variance estimation, and bilateral filtering. The final weight is given by pow(max(dot(N1, N2)), normalWeight). Must be greater than or equal to zero. Defaults to 128.
--
-- ObjC selector: @- setNormalWeight:@
setNormalWeight :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setNormalWeight mpssvgf value =
  sendMessage mpssvgf setNormalWeightSelector value

-- | Controls how samples' luminance values are compared during bilateral filtering. The final weight is given by exp(-abs(L1 - L2) / (luminanceWeight * luminanceVariance + EPSILON)). Must be greater than or equal to zero. Defaults to 4.
--
-- ObjC selector: @- luminanceWeight@
luminanceWeight :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
luminanceWeight mpssvgf =
  sendMessage mpssvgf luminanceWeightSelector

-- | Controls how samples' luminance values are compared during bilateral filtering. The final weight is given by exp(-abs(L1 - L2) / (luminanceWeight * luminanceVariance + EPSILON)). Must be greater than or equal to zero. Defaults to 4.
--
-- ObjC selector: @- setLuminanceWeight:@
setLuminanceWeight :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setLuminanceWeight mpssvgf value =
  sendMessage mpssvgf setLuminanceWeightSelector value

-- | How to weight samples during temporal reprojection. Defaults to MPSTemporalWeightingAverage.
--
-- ObjC selector: @- temporalWeighting@
temporalWeighting :: IsMPSSVGF mpssvgf => mpssvgf -> IO MPSTemporalWeighting
temporalWeighting mpssvgf =
  sendMessage mpssvgf temporalWeightingSelector

-- | How to weight samples during temporal reprojection. Defaults to MPSTemporalWeightingAverage.
--
-- ObjC selector: @- setTemporalWeighting:@
setTemporalWeighting :: IsMPSSVGF mpssvgf => mpssvgf -> MPSTemporalWeighting -> IO ()
setTemporalWeighting mpssvgf value =
  sendMessage mpssvgf setTemporalWeightingSelector value

-- | When using MPSTemporalWeightingExponentialMovingAverage, how much to blend the current frame with the previous frame during reprojection. The final value is given by current * temporalReprojectionBlendFactor + previous * (1 - temporalReprojectionBlendFactor). Must be between zero and one, inclusive. Defaults to 0.2.
--
-- ObjC selector: @- temporalReprojectionBlendFactor@
temporalReprojectionBlendFactor :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
temporalReprojectionBlendFactor mpssvgf =
  sendMessage mpssvgf temporalReprojectionBlendFactorSelector

-- | When using MPSTemporalWeightingExponentialMovingAverage, how much to blend the current frame with the previous frame during reprojection. The final value is given by current * temporalReprojectionBlendFactor + previous * (1 - temporalReprojectionBlendFactor). Must be between zero and one, inclusive. Defaults to 0.2.
--
-- ObjC selector: @- setTemporalReprojectionBlendFactor:@
setTemporalReprojectionBlendFactor :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setTemporalReprojectionBlendFactor mpssvgf value =
  sendMessage mpssvgf setTemporalReprojectionBlendFactorSelector value

-- | During reprojection, minimum combined depth and normal weight needed to consider a pixel from the previous frame consistent with a pixel from the current frame. Must be greater than or equal to zero. Defaults to 0.01.
--
-- ObjC selector: @- reprojectionThreshold@
reprojectionThreshold :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
reprojectionThreshold mpssvgf =
  sendMessage mpssvgf reprojectionThresholdSelector

-- | During reprojection, minimum combined depth and normal weight needed to consider a pixel from the previous frame consistent with a pixel from the current frame. Must be greater than or equal to zero. Defaults to 0.01.
--
-- ObjC selector: @- setReprojectionThreshold:@
setReprojectionThreshold :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setReprojectionThreshold mpssvgf value =
  sendMessage mpssvgf setReprojectionThresholdSelector value

-- | The minimum number of frames which must be accumulated before variance can be computed directly from the accumulated luminance moments. If enough frames have not been accumulated, variance will be estimated with a spatial filter instead. Defaults to 4.
--
-- ObjC selector: @- minimumFramesForVarianceEstimation@
minimumFramesForVarianceEstimation :: IsMPSSVGF mpssvgf => mpssvgf -> IO CULong
minimumFramesForVarianceEstimation mpssvgf =
  sendMessage mpssvgf minimumFramesForVarianceEstimationSelector

-- | The minimum number of frames which must be accumulated before variance can be computed directly from the accumulated luminance moments. If enough frames have not been accumulated, variance will be estimated with a spatial filter instead. Defaults to 4.
--
-- ObjC selector: @- setMinimumFramesForVarianceEstimation:@
setMinimumFramesForVarianceEstimation :: IsMPSSVGF mpssvgf => mpssvgf -> CULong -> IO ()
setMinimumFramesForVarianceEstimation mpssvgf value =
  sendMessage mpssvgf setMinimumFramesForVarianceEstimationSelector value

-- | The radius of the spatial filter used when not enough frames have been accumulated to compute variance from accumulated luminance moments. Defaults to 3 resulting in a 7x7 filter.
--
-- ObjC selector: @- varianceEstimationRadius@
varianceEstimationRadius :: IsMPSSVGF mpssvgf => mpssvgf -> IO CULong
varianceEstimationRadius mpssvgf =
  sendMessage mpssvgf varianceEstimationRadiusSelector

-- | The radius of the spatial filter used when not enough frames have been accumulated to compute variance from accumulated luminance moments. Defaults to 3 resulting in a 7x7 filter.
--
-- ObjC selector: @- setVarianceEstimationRadius:@
setVarianceEstimationRadius :: IsMPSSVGF mpssvgf => mpssvgf -> CULong -> IO ()
setVarianceEstimationRadius mpssvgf value =
  sendMessage mpssvgf setVarianceEstimationRadiusSelector value

-- | The sigma value of the Gaussian function used by the spatial filter used when not enough frames have been accumulated to compute variance from accumulated luminance moments. Must be greater than zero. Defaults to 2.0.
--
-- ObjC selector: @- varianceEstimationSigma@
varianceEstimationSigma :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
varianceEstimationSigma mpssvgf =
  sendMessage mpssvgf varianceEstimationSigmaSelector

-- | The sigma value of the Gaussian function used by the spatial filter used when not enough frames have been accumulated to compute variance from accumulated luminance moments. Must be greater than zero. Defaults to 2.0.
--
-- ObjC selector: @- setVarianceEstimationSigma:@
setVarianceEstimationSigma :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setVarianceEstimationSigma mpssvgf value =
  sendMessage mpssvgf setVarianceEstimationSigmaSelector value

-- | The sigma value of the Gaussian function used by the variance pre-filter of the bilateral filter. Must be greater than zero. Defaults to 1.33.
--
-- ObjC selector: @- variancePrefilterSigma@
variancePrefilterSigma :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
variancePrefilterSigma mpssvgf =
  sendMessage mpssvgf variancePrefilterSigmaSelector

-- | The sigma value of the Gaussian function used by the variance pre-filter of the bilateral filter. Must be greater than zero. Defaults to 1.33.
--
-- ObjC selector: @- setVariancePrefilterSigma:@
setVariancePrefilterSigma :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setVariancePrefilterSigma mpssvgf value =
  sendMessage mpssvgf setVariancePrefilterSigmaSelector value

-- | The radius of the variance pre-filter of the bilateral filter. Defaults to 1 resulting in a 3x3 filter.
--
-- ObjC selector: @- variancePrefilterRadius@
variancePrefilterRadius :: IsMPSSVGF mpssvgf => mpssvgf -> IO CULong
variancePrefilterRadius mpssvgf =
  sendMessage mpssvgf variancePrefilterRadiusSelector

-- | The radius of the variance pre-filter of the bilateral filter. Defaults to 1 resulting in a 3x3 filter.
--
-- ObjC selector: @- setVariancePrefilterRadius:@
setVariancePrefilterRadius :: IsMPSSVGF mpssvgf => mpssvgf -> CULong -> IO ()
setVariancePrefilterRadius mpssvgf value =
  sendMessage mpssvgf setVariancePrefilterRadiusSelector value

-- | The sigma value of the Gaussian function used by the bilateral filter. Must be greater than zero. Defaults to 1.2.
--
-- ObjC selector: @- bilateralFilterSigma@
bilateralFilterSigma :: IsMPSSVGF mpssvgf => mpssvgf -> IO CFloat
bilateralFilterSigma mpssvgf =
  sendMessage mpssvgf bilateralFilterSigmaSelector

-- | The sigma value of the Gaussian function used by the bilateral filter. Must be greater than zero. Defaults to 1.2.
--
-- ObjC selector: @- setBilateralFilterSigma:@
setBilateralFilterSigma :: IsMPSSVGF mpssvgf => mpssvgf -> CFloat -> IO ()
setBilateralFilterSigma mpssvgf value =
  sendMessage mpssvgf setBilateralFilterSigmaSelector value

-- | The radius of the bilateral filter. Defaults to 2 resulting in a 5x5 filter.
--
-- ObjC selector: @- bilateralFilterRadius@
bilateralFilterRadius :: IsMPSSVGF mpssvgf => mpssvgf -> IO CULong
bilateralFilterRadius mpssvgf =
  sendMessage mpssvgf bilateralFilterRadiusSelector

-- | The radius of the bilateral filter. Defaults to 2 resulting in a 5x5 filter.
--
-- ObjC selector: @- setBilateralFilterRadius:@
setBilateralFilterRadius :: IsMPSSVGF mpssvgf => mpssvgf -> CULong -> IO ()
setBilateralFilterRadius mpssvgf value =
  sendMessage mpssvgf setBilateralFilterRadiusSelector value

-- | The number of channels to filter in the source image. Must be at least one and at most three. Defaults to 3.
--
-- ObjC selector: @- channelCount@
channelCount :: IsMPSSVGF mpssvgf => mpssvgf -> IO CULong
channelCount mpssvgf =
  sendMessage mpssvgf channelCountSelector

-- | The number of channels to filter in the source image. Must be at least one and at most three. Defaults to 3.
--
-- ObjC selector: @- setChannelCount:@
setChannelCount :: IsMPSSVGF mpssvgf => mpssvgf -> CULong -> IO ()
setChannelCount mpssvgf value =
  sendMessage mpssvgf setChannelCountSelector value

-- | The number of channels to filter in the second source image. Must be at least one and at most three. Defaults to 3.
--
-- ObjC selector: @- channelCount2@
channelCount2 :: IsMPSSVGF mpssvgf => mpssvgf -> IO CULong
channelCount2 mpssvgf =
  sendMessage mpssvgf channelCount2Selector

-- | The number of channels to filter in the second source image. Must be at least one and at most three. Defaults to 3.
--
-- ObjC selector: @- setChannelCount2:@
setChannelCount2 :: IsMPSSVGF mpssvgf => mpssvgf -> CULong -> IO ()
setChannelCount2 mpssvgf value =
  sendMessage mpssvgf setChannelCount2Selector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSSVGF)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSSVGF)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSSVGF)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @encodeWithCoder:@
encodeWithCoderSelector :: Selector '[Id NSCoder] ()
encodeWithCoderSelector = mkSelector "encodeWithCoder:"

-- | @Selector@ for @encodeReprojectionToCommandBuffer:sourceTexture:previousTexture:destinationTexture:previousLuminanceMomentsTexture:destinationLuminanceMomentsTexture:previousFrameCountTexture:destinationFrameCountTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector = mkSelector "encodeReprojectionToCommandBuffer:sourceTexture:previousTexture:destinationTexture:previousLuminanceMomentsTexture:destinationLuminanceMomentsTexture:previousFrameCountTexture:destinationFrameCountTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:"

-- | @Selector@ for @encodeReprojectionToCommandBuffer:sourceTexture:previousTexture:destinationTexture:previousLuminanceMomentsTexture:destinationLuminanceMomentsTexture:sourceTexture2:previousTexture2:destinationTexture2:previousLuminanceMomentsTexture2:destinationLuminanceMomentsTexture2:previousFrameCountTexture:destinationFrameCountTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:@
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeReprojectionToCommandBuffer_sourceTexture_previousTexture_destinationTexture_previousLuminanceMomentsTexture_destinationLuminanceMomentsTexture_sourceTexture2_previousTexture2_destinationTexture2_previousLuminanceMomentsTexture2_destinationLuminanceMomentsTexture2_previousFrameCountTexture_destinationFrameCountTexture_motionVectorTexture_depthNormalTexture_previousDepthNormalTextureSelector = mkSelector "encodeReprojectionToCommandBuffer:sourceTexture:previousTexture:destinationTexture:previousLuminanceMomentsTexture:destinationLuminanceMomentsTexture:sourceTexture2:previousTexture2:destinationTexture2:previousLuminanceMomentsTexture2:destinationLuminanceMomentsTexture2:previousFrameCountTexture:destinationFrameCountTexture:motionVectorTexture:depthNormalTexture:previousDepthNormalTexture:"

-- | @Selector@ for @encodeVarianceEstimationToCommandBuffer:sourceTexture:luminanceMomentsTexture:destinationTexture:frameCountTexture:depthNormalTexture:@
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_frameCountTexture_depthNormalTextureSelector = mkSelector "encodeVarianceEstimationToCommandBuffer:sourceTexture:luminanceMomentsTexture:destinationTexture:frameCountTexture:depthNormalTexture:"

-- | @Selector@ for @encodeVarianceEstimationToCommandBuffer:sourceTexture:luminanceMomentsTexture:destinationTexture:sourceTexture2:luminanceMomentsTexture2:destinationTexture2:frameCountTexture:depthNormalTexture:@
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTextureSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeVarianceEstimationToCommandBuffer_sourceTexture_luminanceMomentsTexture_destinationTexture_sourceTexture2_luminanceMomentsTexture2_destinationTexture2_frameCountTexture_depthNormalTextureSelector = mkSelector "encodeVarianceEstimationToCommandBuffer:sourceTexture:luminanceMomentsTexture:destinationTexture:sourceTexture2:luminanceMomentsTexture2:destinationTexture2:frameCountTexture:depthNormalTexture:"

-- | @Selector@ for @encodeBilateralFilterToCommandBuffer:stepDistance:sourceTexture:destinationTexture:depthNormalTexture:@
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTextureSelector :: Selector '[RawId, CULong, RawId, RawId, RawId] ()
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_depthNormalTextureSelector = mkSelector "encodeBilateralFilterToCommandBuffer:stepDistance:sourceTexture:destinationTexture:depthNormalTexture:"

-- | @Selector@ for @encodeBilateralFilterToCommandBuffer:stepDistance:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:depthNormalTexture:@
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTextureSelector :: Selector '[RawId, CULong, RawId, RawId, RawId, RawId, RawId] ()
encodeBilateralFilterToCommandBuffer_stepDistance_sourceTexture_destinationTexture_sourceTexture2_destinationTexture2_depthNormalTextureSelector = mkSelector "encodeBilateralFilterToCommandBuffer:stepDistance:sourceTexture:destinationTexture:sourceTexture2:destinationTexture2:depthNormalTexture:"

-- | @Selector@ for @depthWeight@
depthWeightSelector :: Selector '[] CFloat
depthWeightSelector = mkSelector "depthWeight"

-- | @Selector@ for @setDepthWeight:@
setDepthWeightSelector :: Selector '[CFloat] ()
setDepthWeightSelector = mkSelector "setDepthWeight:"

-- | @Selector@ for @normalWeight@
normalWeightSelector :: Selector '[] CFloat
normalWeightSelector = mkSelector "normalWeight"

-- | @Selector@ for @setNormalWeight:@
setNormalWeightSelector :: Selector '[CFloat] ()
setNormalWeightSelector = mkSelector "setNormalWeight:"

-- | @Selector@ for @luminanceWeight@
luminanceWeightSelector :: Selector '[] CFloat
luminanceWeightSelector = mkSelector "luminanceWeight"

-- | @Selector@ for @setLuminanceWeight:@
setLuminanceWeightSelector :: Selector '[CFloat] ()
setLuminanceWeightSelector = mkSelector "setLuminanceWeight:"

-- | @Selector@ for @temporalWeighting@
temporalWeightingSelector :: Selector '[] MPSTemporalWeighting
temporalWeightingSelector = mkSelector "temporalWeighting"

-- | @Selector@ for @setTemporalWeighting:@
setTemporalWeightingSelector :: Selector '[MPSTemporalWeighting] ()
setTemporalWeightingSelector = mkSelector "setTemporalWeighting:"

-- | @Selector@ for @temporalReprojectionBlendFactor@
temporalReprojectionBlendFactorSelector :: Selector '[] CFloat
temporalReprojectionBlendFactorSelector = mkSelector "temporalReprojectionBlendFactor"

-- | @Selector@ for @setTemporalReprojectionBlendFactor:@
setTemporalReprojectionBlendFactorSelector :: Selector '[CFloat] ()
setTemporalReprojectionBlendFactorSelector = mkSelector "setTemporalReprojectionBlendFactor:"

-- | @Selector@ for @reprojectionThreshold@
reprojectionThresholdSelector :: Selector '[] CFloat
reprojectionThresholdSelector = mkSelector "reprojectionThreshold"

-- | @Selector@ for @setReprojectionThreshold:@
setReprojectionThresholdSelector :: Selector '[CFloat] ()
setReprojectionThresholdSelector = mkSelector "setReprojectionThreshold:"

-- | @Selector@ for @minimumFramesForVarianceEstimation@
minimumFramesForVarianceEstimationSelector :: Selector '[] CULong
minimumFramesForVarianceEstimationSelector = mkSelector "minimumFramesForVarianceEstimation"

-- | @Selector@ for @setMinimumFramesForVarianceEstimation:@
setMinimumFramesForVarianceEstimationSelector :: Selector '[CULong] ()
setMinimumFramesForVarianceEstimationSelector = mkSelector "setMinimumFramesForVarianceEstimation:"

-- | @Selector@ for @varianceEstimationRadius@
varianceEstimationRadiusSelector :: Selector '[] CULong
varianceEstimationRadiusSelector = mkSelector "varianceEstimationRadius"

-- | @Selector@ for @setVarianceEstimationRadius:@
setVarianceEstimationRadiusSelector :: Selector '[CULong] ()
setVarianceEstimationRadiusSelector = mkSelector "setVarianceEstimationRadius:"

-- | @Selector@ for @varianceEstimationSigma@
varianceEstimationSigmaSelector :: Selector '[] CFloat
varianceEstimationSigmaSelector = mkSelector "varianceEstimationSigma"

-- | @Selector@ for @setVarianceEstimationSigma:@
setVarianceEstimationSigmaSelector :: Selector '[CFloat] ()
setVarianceEstimationSigmaSelector = mkSelector "setVarianceEstimationSigma:"

-- | @Selector@ for @variancePrefilterSigma@
variancePrefilterSigmaSelector :: Selector '[] CFloat
variancePrefilterSigmaSelector = mkSelector "variancePrefilterSigma"

-- | @Selector@ for @setVariancePrefilterSigma:@
setVariancePrefilterSigmaSelector :: Selector '[CFloat] ()
setVariancePrefilterSigmaSelector = mkSelector "setVariancePrefilterSigma:"

-- | @Selector@ for @variancePrefilterRadius@
variancePrefilterRadiusSelector :: Selector '[] CULong
variancePrefilterRadiusSelector = mkSelector "variancePrefilterRadius"

-- | @Selector@ for @setVariancePrefilterRadius:@
setVariancePrefilterRadiusSelector :: Selector '[CULong] ()
setVariancePrefilterRadiusSelector = mkSelector "setVariancePrefilterRadius:"

-- | @Selector@ for @bilateralFilterSigma@
bilateralFilterSigmaSelector :: Selector '[] CFloat
bilateralFilterSigmaSelector = mkSelector "bilateralFilterSigma"

-- | @Selector@ for @setBilateralFilterSigma:@
setBilateralFilterSigmaSelector :: Selector '[CFloat] ()
setBilateralFilterSigmaSelector = mkSelector "setBilateralFilterSigma:"

-- | @Selector@ for @bilateralFilterRadius@
bilateralFilterRadiusSelector :: Selector '[] CULong
bilateralFilterRadiusSelector = mkSelector "bilateralFilterRadius"

-- | @Selector@ for @setBilateralFilterRadius:@
setBilateralFilterRadiusSelector :: Selector '[CULong] ()
setBilateralFilterRadiusSelector = mkSelector "setBilateralFilterRadius:"

-- | @Selector@ for @channelCount@
channelCountSelector :: Selector '[] CULong
channelCountSelector = mkSelector "channelCount"

-- | @Selector@ for @setChannelCount:@
setChannelCountSelector :: Selector '[CULong] ()
setChannelCountSelector = mkSelector "setChannelCount:"

-- | @Selector@ for @channelCount2@
channelCount2Selector :: Selector '[] CULong
channelCount2Selector = mkSelector "channelCount2"

-- | @Selector@ for @setChannelCount2:@
setChannelCount2Selector :: Selector '[CULong] ()
setChannelCount2Selector = mkSelector "setChannelCount2:"

