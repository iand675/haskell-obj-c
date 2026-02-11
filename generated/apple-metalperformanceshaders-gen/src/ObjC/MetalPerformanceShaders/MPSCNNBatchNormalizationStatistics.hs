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
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector
  , encodeToCommandBuffer_sourceImage_destinationImageSelector
  , encodeToCommandBuffer_sourceImageSelector
  , encodeBatchToCommandBuffer_sourceImagesSelector


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

-- | Initialize this kernel on a device.
--
-- @device@ — The MTLDevice on which to initialize the kernel.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics => mpscnnBatchNormalizationStatistics -> RawId -> IO (Id MPSCNNBatchNormalizationStatistics)
initWithDevice mpscnnBatchNormalizationStatistics  device =
    sendMsg mpscnnBatchNormalizationStatistics (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device mpscnnBatchNormalizationStatistics  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnBatchNormalizationStatistics (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeBatchToCommandBuffer_sourceImages_batchNormalizationState mpscnnBatchNormalizationStatistics  commandBuffer sourceImages batchNormalizationState =
  withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
      sendMsg mpscnnBatchNormalizationStatistics (mkSelector "encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ())]

-- | @- encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImages :: IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics => mpscnnBatchNormalizationStatistics -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationImages mpscnnBatchNormalizationStatistics  commandBuffer sourceImages destinationImages =
    sendMsg mpscnnBatchNormalizationStatistics (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId destinationImages) :: Ptr ())]

-- | @- encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImage :: (IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics, IsMPSImage sourceImage, IsMPSImage destinationImage) => mpscnnBatchNormalizationStatistics -> RawId -> sourceImage -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationImage mpscnnBatchNormalizationStatistics  commandBuffer sourceImage destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnBatchNormalizationStatistics (mkSelector "encodeToCommandBuffer:sourceImage:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | @- encodeToCommandBuffer:sourceImage:@
encodeToCommandBuffer_sourceImage :: (IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics, IsMPSImage sourceImage) => mpscnnBatchNormalizationStatistics -> RawId -> sourceImage -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage mpscnnBatchNormalizationStatistics  commandBuffer sourceImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
      sendMsg mpscnnBatchNormalizationStatistics (mkSelector "encodeToCommandBuffer:sourceImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImages :: IsMPSCNNBatchNormalizationStatistics mpscnnBatchNormalizationStatistics => mpscnnBatchNormalizationStatistics -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages mpscnnBatchNormalizationStatistics  commandBuffer sourceImages =
    fmap (RawId . castPtr) $ sendMsg mpscnnBatchNormalizationStatistics (mkSelector "encodeBatchToCommandBuffer:sourceImages:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:@
encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_batchNormalizationStateSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:batchNormalizationState:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:@
encodeToCommandBuffer_sourceImageSelector :: Selector
encodeToCommandBuffer_sourceImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:"

