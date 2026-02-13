{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageFindKeypoints
--
-- The MPSImageFindKeypoints kernel is used to find a list of keypoints whose values are >= minimumPixelThresholdValue              in MPSImageKeypointRangeInfo. The keypoints are generated for a specified region in the image.                The pixel format of the source image must be MTLPixelFormatR8Unorm.
--
-- Generated bindings for @MPSImageFindKeypoints@.
module ObjC.MetalPerformanceShaders.MPSImageFindKeypoints
  ( MPSImageFindKeypoints
  , IsMPSImageFindKeypoints(..)
  , initWithDevice_info
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffset
  , encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffsetSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_infoSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Specifies information to find keypoints in an image.
--
-- @device@ — The device the filter will run on
--
-- @info@ — Pointer to the MPSImageKeypointRangeInfo struct
--
-- Returns: A valid MPSImageFindKeypoints object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:info:@
initWithDevice_info :: IsMPSImageFindKeypoints mpsImageFindKeypoints => mpsImageFindKeypoints -> RawId -> Const RawId -> IO (Id MPSImageFindKeypoints)
initWithDevice_info mpsImageFindKeypoints device info =
  sendOwnedMessage mpsImageFindKeypoints initWithDevice_infoSelector device info

-- | @- initWithDevice:@
initWithDevice :: IsMPSImageFindKeypoints mpsImageFindKeypoints => mpsImageFindKeypoints -> RawId -> IO (Id MPSImageFindKeypoints)
initWithDevice mpsImageFindKeypoints device =
  sendOwnedMessage mpsImageFindKeypoints initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSImageFindKeypoints mpsImageFindKeypoints, IsNSCoder aDecoder) => mpsImageFindKeypoints -> aDecoder -> RawId -> IO (Id MPSImageFindKeypoints)
initWithCoder_device mpsImageFindKeypoints aDecoder device =
  sendOwnedMessage mpsImageFindKeypoints initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode the filter to a command buffer using a MTLComputeCommandEncoder.
--
-- The filter will not begin to execute until after the command  buffer has been enqueued and committed.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @source@ — A valid MTLTexture containing the source image for the filter.
--
-- @regions@ — An array of rectangles that describe regions in the image.                                      The list of keypoints is generated for each individual rectangle specifed.
--
-- @keypointCountBuffer@ — The list of keypoints for each specified region
--
-- @keypointCountBufferOffset@ — Byte offset into keypointCountBufferOffset buffer at which to write the keypoint results.                                      Must be a multiple of 32 bytes.
--
-- @keypointDataBuffer@ — A valid MTLBuffer to receive the keypoint data results for each rectangle.                                      The keypoint data for keypoints in each rectangle are stored consecutively.                                      The keypoint data for each rectangle starts at the following offset:                                          MPSImageKeypointRangeInfo.maximumKeyPoints * rectangle index
--
-- @keypointDataBufferOffset@ — Byte offset into keypointData buffer at which to write the keypoint results.                                      Must be a multiple of 32 bytes.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceTexture:regions:numberOfRegions:keypointCountBuffer:keypointCountBufferOffset:keypointDataBuffer:keypointDataBufferOffset:@
encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffset :: IsMPSImageFindKeypoints mpsImageFindKeypoints => mpsImageFindKeypoints -> RawId -> RawId -> Const RawId -> CULong -> RawId -> CULong -> RawId -> CULong -> IO ()
encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffset mpsImageFindKeypoints commandBuffer source regions numberOfRegions keypointCountBuffer keypointCountBufferOffset keypointDataBuffer keypointDataBufferOffset =
  sendMessage mpsImageFindKeypoints encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffsetSelector commandBuffer source regions numberOfRegions keypointCountBuffer keypointCountBufferOffset keypointDataBuffer keypointDataBufferOffset

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:info:@
initWithDevice_infoSelector :: Selector '[RawId, Const RawId] (Id MPSImageFindKeypoints)
initWithDevice_infoSelector = mkSelector "initWithDevice:info:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSImageFindKeypoints)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageFindKeypoints)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceTexture:regions:numberOfRegions:keypointCountBuffer:keypointCountBufferOffset:keypointDataBuffer:keypointDataBufferOffset:@
encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffsetSelector :: Selector '[RawId, RawId, Const RawId, CULong, RawId, CULong, RawId, CULong] ()
encodeToCommandBuffer_sourceTexture_regions_numberOfRegions_keypointCountBuffer_keypointCountBufferOffset_keypointDataBuffer_keypointDataBufferOffsetSelector = mkSelector "encodeToCommandBuffer:sourceTexture:regions:numberOfRegions:keypointCountBuffer:keypointCountBufferOffset:keypointDataBuffer:keypointDataBufferOffset:"

