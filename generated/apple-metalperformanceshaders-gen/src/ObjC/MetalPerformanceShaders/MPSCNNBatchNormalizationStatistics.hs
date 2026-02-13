{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBatchNormalizationStatistics
--
-- This depends on Metal.framework
--
-- MPSCNNBatchNormalizationStatistics updates a MPSCNNBatchNormalizationState              with the batch statistics necessary to perform a batch normalization.              MPSCNNBatchNormalizationStatistics may be executed multiple times with              multiple images to accumulate all the statistics necessary to perform              a batch normalization as described in  https://arxiv.org/pdf/1502.03167v3.pdf.
--
-- Generated bindings for @MPSCNNBatchNormalizationStatistics@.
module ObjC.MetalPerformanceShaders.MPSCNNBatchNormalizationStatistics
  ( MPSCNNBatchNormalizationStatistics
  , IsMPSCNNBatchNormalizationStatistics(..)
  , initWithDevice
  , initWithCoder_device
  , encodeBatchToCommandBuffer_sourceImages_batchNormalizationState
  , encodeBatchToCommandBuffer_sourceImages_destinationImages
  , encodeToCommandBuffer_sourceImage_destinationImage
  , encodeToCommandBuffer_sourceImage
  , encodeBatchToCommandBuffer_sourceImages
  , encodeBatchToCommandBuffer_sourceImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector
  , encodeToCommandBuffer_sourceImageSelector
  , encodeToCommandBuffer_sourceImage_destinationImageSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize this kernel on a device.
--
-- @device@ — The MTLDevice on which to initialize the kernel.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics => mpscnnBatchNormalizationStatistics -> RawId -> IO (Id MPSCNNBatchNormalizationStatistics)
initWithDevice mpscnnBatchNormalizationStatistics device =
  sendOwnedMessage mpscnnBatchNormalizationStatistics initWithDeviceSelector device

-- | NSSecureCoding compatability
--
-- While the standard NSSecureCoding/NSCoding method              -initWithCoder: should work, since the file can't              know which device your data is allocated on, we              have to guess and may guess incorrectly.  To avoid              that problem, use initWithCoder:device instead.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSKernel
--
-- @device@ — The MTLDevice on which to make the MPSKernel
--
-- Returns: A new MPSCNNBatchNormalizationStatistics object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics, IsNSCoder aDecoder) => mpscnnBatchNormalizationStatistics -> aDecoder -> RawId -> IO (Id MPSCNNBatchNormalizationStatistics)
initWithCoder_device mpscnnBatchNormalizationStatistics aDecoder device =
  sendOwnedMessage mpscnnBatchNormalizationStatistics initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode this operation to a command buffer.
--
-- @commandBuffer@ — The command buffer.
--
-- @sourceImages@ — An MPSImageBatch containing the source images.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which                                          will be updated with the image batch statistics.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState :: (IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics, IsMPSCNNBatchNormalizationState batchNormalizationState) => mpscnnBatchNormalizationStatistics -> RawId -> RawId -> batchNormalizationState -> IO ()
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState mpscnnBatchNormalizationStatistics commandBuffer sourceImages batchNormalizationState =
  sendMessage mpscnnBatchNormalizationStatistics encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector commandBuffer sourceImages (toMPSCNNBatchNormalizationState batchNormalizationState)

-- | @- encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImages :: IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics => mpscnnBatchNormalizationStatistics -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationImages mpscnnBatchNormalizationStatistics commandBuffer sourceImages destinationImages =
  sendMessage mpscnnBatchNormalizationStatistics encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector commandBuffer sourceImages destinationImages

-- | @- encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImage :: (IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics, IsMPSImage sourceImage, IsMPSImage destinationImage) => mpscnnBatchNormalizationStatistics -> RawId -> sourceImage -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationImage mpscnnBatchNormalizationStatistics commandBuffer sourceImage destinationImage =
  sendMessage mpscnnBatchNormalizationStatistics encodeToCommandBuffer_sourceImage_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSImage destinationImage)

-- | @- encodeToCommandBuffer:sourceImage:@
encodeToCommandBuffer_sourceImage :: (IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics, IsMPSImage sourceImage) => mpscnnBatchNormalizationStatistics -> RawId -> sourceImage -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage mpscnnBatchNormalizationStatistics commandBuffer sourceImage =
  sendMessage mpscnnBatchNormalizationStatistics encodeToCommandBuffer_sourceImageSelector commandBuffer (toMPSImage sourceImage)

-- | @- encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImages :: IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics => mpscnnBatchNormalizationStatistics -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages mpscnnBatchNormalizationStatistics commandBuffer sourceImages =
  sendMessage mpscnnBatchNormalizationStatistics encodeBatchToCommandBuffer_sourceImagesSelector commandBuffer sourceImages

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNBatchNormalizationStatistics)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNBatchNormalizationStatistics)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector :: Selector '[RawId, RawId, Id MPSCNNBatchNormalizationState] ()
encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector :: Selector '[RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage] ()
encodeToCommandBuffer_sourceImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:@
encodeToCommandBuffer_sourceImageSelector :: Selector '[RawId, Id MPSImage] (Id MPSImage)
encodeToCommandBuffer_sourceImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImagesSelector :: Selector '[RawId, RawId] RawId
encodeBatchToCommandBuffer_sourceImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:"

