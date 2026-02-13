{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSImageCopyToMatrix
--
-- The MPSImageCopyToMatrix copies image data to a MPSMatrix.              The image data is stored in a row of a matrix.  The dataLayout              specifies the order in which the feature channels in the MPSImage              get stored in the matrix.  If MPSImage stores a batch of images,              the images are copied into multiple rows, one row per image.
--
-- The number of elements in a row in the matrix must be >= image width *               image height * number of featureChannels in the image.
--
-- Generated bindings for @MPSImageCopyToMatrix@.
module ObjC.MetalPerformanceShaders.MPSImageCopyToMatrix
  ( MPSImageCopyToMatrix
  , IsMPSImageCopyToMatrix(..)
  , initWithDevice_dataLayout
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_destinationMatrix
  , encodeBatchToCommandBuffer_sourceImages_destinationMatrix
  , destinationMatrixBatchIndex
  , setDestinationMatrixBatchIndex
  , dataLayout
  , dataLayoutSelector
  , destinationMatrixBatchIndexSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector
  , encodeToCommandBuffer_sourceImage_destinationMatrixSelector
  , initWithCoder_deviceSelector
  , initWithDevice_dataLayoutSelector
  , setDestinationMatrixBatchIndexSelector

  -- * Enum types
  , MPSDataLayout(MPSDataLayout)
  , pattern MPSDataLayoutHeightxWidthxFeatureChannels
  , pattern MPSDataLayoutFeatureChannelsxHeightxWidth

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

-- | Initialize a MPSMatrixCopy object on a device
--
-- @device@ — The device the kernel will run on
--
-- @dataLayout@ — The data layout
--
-- Returns: A valid MPSMatrixCopy object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:dataLayout:@
initWithDevice_dataLayout :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> RawId -> MPSDataLayout -> IO (Id MPSImageCopyToMatrix)
initWithDevice_dataLayout mpsImageCopyToMatrix device dataLayout =
  sendOwnedMessage mpsImageCopyToMatrix initWithDevice_dataLayoutSelector device dataLayout

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
initWithCoder_device :: (IsMPSImageCopyToMatrix mpsImageCopyToMatrix, IsNSCoder aDecoder) => mpsImageCopyToMatrix -> aDecoder -> RawId -> IO (Id MPSImageCopyToMatrix)
initWithCoder_device mpsImageCopyToMatrix aDecoder device =
  sendOwnedMessage mpsImageCopyToMatrix initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a kernel that copies a MPSImage to a MPSMatrix into a command buffer            using a MTLComputeCommandEncoder.
--
-- The kernel copies feature channels from sourceImage to the buffer              associated with destinationMatrix.  The kernel will not begin to execute until              after the command buffer has been enqueued and committed.
--
-- NOTE: The destinationMatrix.dataType must match the feature channel data type in sourceImage.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @sourceImage@ — A valid MPSImage describing the image to copy from.
--
-- @destinationMatrix@ — A valid MPSMatrix or MPSTemporaryMatrix object describing the matrix to copy to.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:destinationMatrix:@
encodeToCommandBuffer_sourceImage_destinationMatrix :: (IsMPSImageCopyToMatrix mpsImageCopyToMatrix, IsMPSImage sourceImage, IsMPSMatrix destinationMatrix) => mpsImageCopyToMatrix -> RawId -> sourceImage -> destinationMatrix -> IO ()
encodeToCommandBuffer_sourceImage_destinationMatrix mpsImageCopyToMatrix commandBuffer sourceImage destinationMatrix =
  sendMessage mpsImageCopyToMatrix encodeToCommandBuffer_sourceImage_destinationMatrixSelector commandBuffer (toMPSImage sourceImage) (toMPSMatrix destinationMatrix)

-- | Encode a kernel that copies a MPSImageBatch to a MPSMatrix into a command buffer            using a MTLComputeCommandEncoder.
--
-- The kernel copies feature channels from sourceImage to the buffer              associated with destinationMatrix.  The kernel will not begin to execute until              after the command buffer has been enqueued and committed.              Each image will be copied to its own row in the matrix, starting with row              destinationMatrixOrigin.x.
--
-- NOTE: The destinationMatrix.dataType must match the feature channel data type in sourceImage.              NOTE: All the images in the source batch should be of the same size and have numberOfImages = 1.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @sourceImages@ — A valid MPSImageBatch describing the images to copy from.
--
-- @destinationMatrix@ — A valid MPSMatrix or MPSTemporaryMatrix object describing the matrix to copy to.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationMatrix:@
encodeBatchToCommandBuffer_sourceImages_destinationMatrix :: (IsMPSImageCopyToMatrix mpsImageCopyToMatrix, IsMPSMatrix destinationMatrix) => mpsImageCopyToMatrix -> RawId -> RawId -> destinationMatrix -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationMatrix mpsImageCopyToMatrix commandBuffer sourceImages destinationMatrix =
  sendMessage mpsImageCopyToMatrix encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector commandBuffer sourceImages (toMPSMatrix destinationMatrix)

-- | destinationMatrixBatchIndex
--
-- The index of the destination matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.
--
-- ObjC selector: @- destinationMatrixBatchIndex@
destinationMatrixBatchIndex :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> IO CULong
destinationMatrixBatchIndex mpsImageCopyToMatrix =
  sendMessage mpsImageCopyToMatrix destinationMatrixBatchIndexSelector

-- | destinationMatrixBatchIndex
--
-- The index of the destination matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.
--
-- ObjC selector: @- setDestinationMatrixBatchIndex:@
setDestinationMatrixBatchIndex :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> CULong -> IO ()
setDestinationMatrixBatchIndex mpsImageCopyToMatrix value =
  sendMessage mpsImageCopyToMatrix setDestinationMatrixBatchIndexSelector value

-- | dataLayout
--
-- The data layout to use
--
-- Returns the data layout.  When copying from a MPSImage to a MPSMatrix, this              describes the order in which the image values are stored in the buffer associated              with the MPSMatrix.              Default: MPSDataLayoutFeatureChannelsxHeightxWidth
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> IO MPSDataLayout
dataLayout mpsImageCopyToMatrix =
  sendMessage mpsImageCopyToMatrix dataLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataLayout:@
initWithDevice_dataLayoutSelector :: Selector '[RawId, MPSDataLayout] (Id MPSImageCopyToMatrix)
initWithDevice_dataLayoutSelector = mkSelector "initWithDevice:dataLayout:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSImageCopyToMatrix)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationMatrix:@
encodeToCommandBuffer_sourceImage_destinationMatrixSelector :: Selector '[RawId, Id MPSImage, Id MPSMatrix] ()
encodeToCommandBuffer_sourceImage_destinationMatrixSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationMatrix:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationMatrix:@
encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector :: Selector '[RawId, RawId, Id MPSMatrix] ()
encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationMatrix:"

-- | @Selector@ for @destinationMatrixBatchIndex@
destinationMatrixBatchIndexSelector :: Selector '[] CULong
destinationMatrixBatchIndexSelector = mkSelector "destinationMatrixBatchIndex"

-- | @Selector@ for @setDestinationMatrixBatchIndex:@
setDestinationMatrixBatchIndexSelector :: Selector '[CULong] ()
setDestinationMatrixBatchIndexSelector = mkSelector "setDestinationMatrixBatchIndex:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector '[] MPSDataLayout
dataLayoutSelector = mkSelector "dataLayout"

