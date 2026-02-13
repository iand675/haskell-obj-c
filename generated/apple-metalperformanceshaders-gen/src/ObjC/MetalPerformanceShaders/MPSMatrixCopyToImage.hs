{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixCopyToImage
--
-- The MPSMatrixCopyToImage copies matrix data to a MPSImage.              The operation is the reverse of MPSImageCopyToMatrix.
--
-- Generated bindings for @MPSMatrixCopyToImage@.
module ObjC.MetalPerformanceShaders.MPSMatrixCopyToImage
  ( MPSMatrixCopyToImage
  , IsMPSMatrixCopyToImage(..)
  , initWithDevice_dataLayout
  , initWithCoder_device
  , encodeToCommandBuffer_sourceMatrix_destinationImage
  , encodeBatchToCommandBuffer_sourceMatrix_destinationImages
  , sourceMatrixBatchIndex
  , setSourceMatrixBatchIndex
  , dataLayout
  , dataLayoutSelector
  , encodeBatchToCommandBuffer_sourceMatrix_destinationImagesSelector
  , encodeToCommandBuffer_sourceMatrix_destinationImageSelector
  , initWithCoder_deviceSelector
  , initWithDevice_dataLayoutSelector
  , setSourceMatrixBatchIndexSelector
  , sourceMatrixBatchIndexSelector

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

-- | Initialize a MPSMatrixCopyToImage object on a device
--
-- @device@ — The device the kernel will run on
--
-- @dataLayout@ — The data layout
--
-- Returns: A valid MPSMatrixCopyToImage object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:dataLayout:@
initWithDevice_dataLayout :: IsMPSMatrixCopyToImage mpsMatrixCopyToImage => mpsMatrixCopyToImage -> RawId -> MPSDataLayout -> IO (Id MPSMatrixCopyToImage)
initWithDevice_dataLayout mpsMatrixCopyToImage device dataLayout =
  sendOwnedMessage mpsMatrixCopyToImage initWithDevice_dataLayoutSelector device dataLayout

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
initWithCoder_device :: (IsMPSMatrixCopyToImage mpsMatrixCopyToImage, IsNSCoder aDecoder) => mpsMatrixCopyToImage -> aDecoder -> RawId -> IO (Id MPSMatrixCopyToImage)
initWithCoder_device mpsMatrixCopyToImage aDecoder device =
  sendOwnedMessage mpsMatrixCopyToImage initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a kernel that copies a MPSMatrix to a MPSImage into a command buffer            using a MTLComputeCommandEncoder.
--
-- The kernel copies feature channels from sourceMatrix to the destinationImage.              The kernel will not begin to execute until              after the command buffer has been enqueued and committed.
--
-- NOTE: The sourceMatrix.dataType must match the feature channel data type in destinationImage.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @sourceMatrix@ — A valid MPSMatrix or MPSTemporaryMatrix object describing the source matrix.
--
-- @destinationImage@ — A valid MPSImage describing the image to copy to.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceMatrix:destinationImage:@
encodeToCommandBuffer_sourceMatrix_destinationImage :: (IsMPSMatrixCopyToImage mpsMatrixCopyToImage, IsMPSMatrix sourceMatrix, IsMPSImage destinationImage) => mpsMatrixCopyToImage -> RawId -> sourceMatrix -> destinationImage -> IO ()
encodeToCommandBuffer_sourceMatrix_destinationImage mpsMatrixCopyToImage commandBuffer sourceMatrix destinationImage =
  sendMessage mpsMatrixCopyToImage encodeToCommandBuffer_sourceMatrix_destinationImageSelector commandBuffer (toMPSMatrix sourceMatrix) (toMPSImage destinationImage)

-- | Encode a kernel that copies a MPSMatrix to a MPSImageBatch into a command buffer            using a MTLComputeCommandEncoder.
--
-- The kernel copies feature channels from sourceImage to the buffer              associated with destinationMatrix.  The kernel will not begin to execute until              after the command buffer has been enqueued and committed.              Each image will be copied to its own row in the matrix, starting with row              destinationMatrixOrigin.x.
--
-- NOTE: The destinationMatrix.dataType must match the feature channel data type in sourceImage.              NOTE: All the images in the source batch should be of the same size and have numberOfImages = 1.
--
-- @commandBuffer@ — A valid MTLCommandBuffer.
--
-- @sourceMatrix@ — A valid MPSMatrix or MPSTemporaryMatrix object describing the source matrix.
--
-- @destinationImages@ — A valid MPSImageBatch describing the images to copy to.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceMatrix:destinationImages:@
encodeBatchToCommandBuffer_sourceMatrix_destinationImages :: (IsMPSMatrixCopyToImage mpsMatrixCopyToImage, IsMPSMatrix sourceMatrix) => mpsMatrixCopyToImage -> RawId -> sourceMatrix -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceMatrix_destinationImages mpsMatrixCopyToImage commandBuffer sourceMatrix destinationImages =
  sendMessage mpsMatrixCopyToImage encodeBatchToCommandBuffer_sourceMatrix_destinationImagesSelector commandBuffer (toMPSMatrix sourceMatrix) destinationImages

-- | sourceMatrixBatchIndex
--
-- The index of the source matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.
--
-- ObjC selector: @- sourceMatrixBatchIndex@
sourceMatrixBatchIndex :: IsMPSMatrixCopyToImage mpsMatrixCopyToImage => mpsMatrixCopyToImage -> IO CULong
sourceMatrixBatchIndex mpsMatrixCopyToImage =
  sendMessage mpsMatrixCopyToImage sourceMatrixBatchIndexSelector

-- | sourceMatrixBatchIndex
--
-- The index of the source matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.
--
-- ObjC selector: @- setSourceMatrixBatchIndex:@
setSourceMatrixBatchIndex :: IsMPSMatrixCopyToImage mpsMatrixCopyToImage => mpsMatrixCopyToImage -> CULong -> IO ()
setSourceMatrixBatchIndex mpsMatrixCopyToImage value =
  sendMessage mpsMatrixCopyToImage setSourceMatrixBatchIndexSelector value

-- | dataLayout
--
-- The data layout to use
--
-- Returns the data layout.  When copying from a MPSMatrix to a MPSImage, this              describes the order in which the image values are to be stored in the buffer associated              with the MPSMatrix.              Default: MPSDataLayoutFeatureChannelsxHeightxWidth
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSMatrixCopyToImage mpsMatrixCopyToImage => mpsMatrixCopyToImage -> IO MPSDataLayout
dataLayout mpsMatrixCopyToImage =
  sendMessage mpsMatrixCopyToImage dataLayoutSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataLayout:@
initWithDevice_dataLayoutSelector :: Selector '[RawId, MPSDataLayout] (Id MPSMatrixCopyToImage)
initWithDevice_dataLayoutSelector = mkSelector "initWithDevice:dataLayout:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixCopyToImage)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceMatrix:destinationImage:@
encodeToCommandBuffer_sourceMatrix_destinationImageSelector :: Selector '[RawId, Id MPSMatrix, Id MPSImage] ()
encodeToCommandBuffer_sourceMatrix_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceMatrix:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceMatrix:destinationImages:@
encodeBatchToCommandBuffer_sourceMatrix_destinationImagesSelector :: Selector '[RawId, Id MPSMatrix, RawId] ()
encodeBatchToCommandBuffer_sourceMatrix_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceMatrix:destinationImages:"

-- | @Selector@ for @sourceMatrixBatchIndex@
sourceMatrixBatchIndexSelector :: Selector '[] CULong
sourceMatrixBatchIndexSelector = mkSelector "sourceMatrixBatchIndex"

-- | @Selector@ for @setSourceMatrixBatchIndex:@
setSourceMatrixBatchIndexSelector :: Selector '[CULong] ()
setSourceMatrixBatchIndexSelector = mkSelector "setSourceMatrixBatchIndex:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector '[] MPSDataLayout
dataLayoutSelector = mkSelector "dataLayout"

