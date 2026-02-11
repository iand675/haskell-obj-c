{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDevice_dataLayoutSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_destinationMatrixSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector
  , destinationMatrixBatchIndexSelector
  , setDestinationMatrixBatchIndexSelector
  , dataLayoutSelector

  -- * Enum types
  , MPSDataLayout(MPSDataLayout)
  , pattern MPSDataLayoutHeightxWidthxFeatureChannels
  , pattern MPSDataLayoutFeatureChannelsxHeightxWidth

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
initWithDevice_dataLayout mpsImageCopyToMatrix  device dataLayout =
    sendMsg mpsImageCopyToMatrix (mkSelector "initWithDevice:dataLayout:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong (coerce dataLayout)] >>= ownedObject . castPtr

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
initWithCoder_device mpsImageCopyToMatrix  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpsImageCopyToMatrix (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceImage_destinationMatrix mpsImageCopyToMatrix  commandBuffer sourceImage destinationMatrix =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr destinationMatrix $ \raw_destinationMatrix ->
        sendMsg mpsImageCopyToMatrix (mkSelector "encodeToCommandBuffer:sourceImage:destinationMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_destinationMatrix :: Ptr ())]

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
encodeBatchToCommandBuffer_sourceImages_destinationMatrix mpsImageCopyToMatrix  commandBuffer sourceImages destinationMatrix =
  withObjCPtr destinationMatrix $ \raw_destinationMatrix ->
      sendMsg mpsImageCopyToMatrix (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr raw_destinationMatrix :: Ptr ())]

-- | destinationMatrixBatchIndex
--
-- The index of the destination matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.
--
-- ObjC selector: @- destinationMatrixBatchIndex@
destinationMatrixBatchIndex :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> IO CULong
destinationMatrixBatchIndex mpsImageCopyToMatrix  =
    sendMsg mpsImageCopyToMatrix (mkSelector "destinationMatrixBatchIndex") retCULong []

-- | destinationMatrixBatchIndex
--
-- The index of the destination matrix in the batch.  This property is              modifiable and defaults to 0 at initialization time.
--
-- ObjC selector: @- setDestinationMatrixBatchIndex:@
setDestinationMatrixBatchIndex :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> CULong -> IO ()
setDestinationMatrixBatchIndex mpsImageCopyToMatrix  value =
    sendMsg mpsImageCopyToMatrix (mkSelector "setDestinationMatrixBatchIndex:") retVoid [argCULong value]

-- | dataLayout
--
-- The data layout to use
--
-- Returns the data layout.  When copying from a MPSImage to a MPSMatrix, this              describes the order in which the image values are stored in the buffer associated              with the MPSMatrix.              Default: MPSDataLayoutFeatureChannelsxHeightxWidth
--
-- ObjC selector: @- dataLayout@
dataLayout :: IsMPSImageCopyToMatrix mpsImageCopyToMatrix => mpsImageCopyToMatrix -> IO MPSDataLayout
dataLayout mpsImageCopyToMatrix  =
    fmap (coerce :: CULong -> MPSDataLayout) $ sendMsg mpsImageCopyToMatrix (mkSelector "dataLayout") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:dataLayout:@
initWithDevice_dataLayoutSelector :: Selector
initWithDevice_dataLayoutSelector = mkSelector "initWithDevice:dataLayout:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationMatrix:@
encodeToCommandBuffer_sourceImage_destinationMatrixSelector :: Selector
encodeToCommandBuffer_sourceImage_destinationMatrixSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationMatrix:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationMatrix:@
encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationMatrixSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationMatrix:"

-- | @Selector@ for @destinationMatrixBatchIndex@
destinationMatrixBatchIndexSelector :: Selector
destinationMatrixBatchIndexSelector = mkSelector "destinationMatrixBatchIndex"

-- | @Selector@ for @setDestinationMatrixBatchIndex:@
setDestinationMatrixBatchIndexSelector :: Selector
setDestinationMatrixBatchIndexSelector = mkSelector "setDestinationMatrixBatchIndex:"

-- | @Selector@ for @dataLayout@
dataLayoutSelector :: Selector
dataLayoutSelector = mkSelector "dataLayout"

