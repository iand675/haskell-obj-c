{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNMultiaryKernel
--
-- This depends on Metal.framework
--
-- Describes a  neural network kernel with multiple image sources.
--
-- A MPSCNNKernel consumes multiple MPSImages, possibly a MPSState, and produces one MPSImage.
--
-- Generated bindings for @MPSCNNMultiaryKernel@.
module ObjC.MetalPerformanceShaders.MPSCNNMultiaryKernel
  ( MPSCNNMultiaryKernel
  , IsMPSCNNMultiaryKernel(..)
  , initWithDevice_sourceCount
  , initWithDevice
  , sourceFeatureChannelOffsetAtIndex
  , setSourceFeatureChannelOffset_atIndex
  , sourceFeatureChannelMaxCountAtIndex
  , setSourceFeatureChannelMaxCount_atIndex
  , edgeModeAtIndex
  , setEdgeMode_atIndex
  , kernelWidthAtIndex
  , setKernelWidth_atIndex
  , kernelHeightAtIndex
  , setKernelHeight_atIndex
  , strideInPixelsXatIndex
  , setStrideInPixelsX_atIndex
  , strideInPixelsYatIndex
  , setStrideInPixelsY_atIndex
  , dilationRateXatIndex
  , setDilationRateX_atIndex
  , dilationRateYatIndex
  , setDilationRateY_atIndex
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImages_destinationImage
  , encodeBatchToCommandBuffer_sourceImages_destinationImages
  , encodeToCommandBuffer_sourceImages
  , encodeBatchToCommandBuffer_sourceImages
  , encodeToCommandBuffer_sourceImages_destinationState_destinationStateIsTemporary
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary
  , isResultStateReusedAcrossBatch
  , appendBatchBarrier
  , resultStateForSourceImages_sourceStates_destinationImage
  , resultStateBatchForSourceImages_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImages_sourceStates_destinationImage
  , temporaryResultStateBatchForCommandBuffer_sourceImages_sourceStates_destinationImage
  , destinationImageDescriptorForSourceImages_sourceStates
  , sourceCount
  , destinationFeatureChannelOffset
  , setDestinationFeatureChannelOffset
  , isBackwards
  , isStateModified
  , padding
  , setPadding
  , destinationImageAllocator
  , setDestinationImageAllocator
  , initWithDevice_sourceCountSelector
  , initWithDeviceSelector
  , sourceFeatureChannelOffsetAtIndexSelector
  , setSourceFeatureChannelOffset_atIndexSelector
  , sourceFeatureChannelMaxCountAtIndexSelector
  , setSourceFeatureChannelMaxCount_atIndexSelector
  , edgeModeAtIndexSelector
  , setEdgeMode_atIndexSelector
  , kernelWidthAtIndexSelector
  , setKernelWidth_atIndexSelector
  , kernelHeightAtIndexSelector
  , setKernelHeight_atIndexSelector
  , strideInPixelsXatIndexSelector
  , setStrideInPixelsX_atIndexSelector
  , strideInPixelsYatIndexSelector
  , setStrideInPixelsY_atIndexSelector
  , dilationRateXatIndexSelector
  , setDilationRateX_atIndexSelector
  , dilationRateYatIndexSelector
  , setDilationRateY_atIndexSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImages_destinationImageSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector
  , encodeToCommandBuffer_sourceImagesSelector
  , encodeBatchToCommandBuffer_sourceImagesSelector
  , encodeToCommandBuffer_sourceImages_destinationState_destinationStateIsTemporarySelector
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector
  , isResultStateReusedAcrossBatchSelector
  , appendBatchBarrierSelector
  , resultStateForSourceImages_sourceStates_destinationImageSelector
  , resultStateBatchForSourceImages_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImages_sourceStates_destinationImageSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImages_sourceStates_destinationImageSelector
  , destinationImageDescriptorForSourceImages_sourceStatesSelector
  , sourceCountSelector
  , destinationFeatureChannelOffsetSelector
  , setDestinationFeatureChannelOffsetSelector
  , isBackwardsSelector
  , isStateModifiedSelector
  , paddingSelector
  , setPaddingSelector
  , destinationImageAllocatorSelector
  , setDestinationImageAllocatorSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

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

-- | Standard init with default properties per filter type
--
-- @device@ — The device that the filter will be used on. May not be NULL.
--
-- @sourceCount@ — The number of source images or MPSImageBatches
--
-- Returns: A pointer to the newly initialized object. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- initWithDevice:sourceCount:@
initWithDevice_sourceCount :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> RawId -> CULong -> IO (Id MPSCNNMultiaryKernel)
initWithDevice_sourceCount mpscnnMultiaryKernel  device sourceCount =
    sendMsg mpscnnMultiaryKernel (mkSelector "initWithDevice:sourceCount:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCULong sourceCount] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> RawId -> IO (Id MPSCNNMultiaryKernel)
initWithDevice mpscnnMultiaryKernel  device =
    sendMsg mpscnnMultiaryKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | The number of channels in the source MPSImage to skip before reading the input.
--
-- This is the starting offset into the  source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set sourceFeatureChannelOffset[0] = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least primarySourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              primarySourceFeatureChannelOffset > 32.
--
-- @index@ — The index of the source image that the feature channel offset describes
--
-- Returns: The source feature channel offset
--
-- ObjC selector: @- sourceFeatureChannelOffsetAtIndex:@
sourceFeatureChannelOffsetAtIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
sourceFeatureChannelOffsetAtIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "sourceFeatureChannelOffsetAtIndex:") retCULong [argCULong index]

-- | Set the number of channels in the source MPSImage to skip before reading the input.
--
-- This is the starting offset into the  source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set sourceFeatureChannelOffset[0] = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least primarySourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              primarySourceFeatureChannelOffset > 32.
--
-- @index@ — The index of the source image that the feature channel offset describes
--
-- @offset@ — The source feature channel offset
--
-- ObjC selector: @- setSourceFeatureChannelOffset:atIndex:@
setSourceFeatureChannelOffset_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setSourceFeatureChannelOffset_atIndex mpscnnMultiaryKernel  offset index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setSourceFeatureChannelOffset:atIndex:") retVoid [argCULong offset, argCULong index]

-- | The maximum number of channels in the source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- @index@ — The index of the source image to which the max count refers
--
-- Returns: The source feature channel max count
--
-- ObjC selector: @- sourceFeatureChannelMaxCountAtIndex:@
sourceFeatureChannelMaxCountAtIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
sourceFeatureChannelMaxCountAtIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "sourceFeatureChannelMaxCountAtIndex:") retCULong [argCULong index]

-- | Set the maximum number of channels in the source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- @count@ — The new source feature channel max count
--
-- @index@ — The index of the source image to which the max count refers
--
-- ObjC selector: @- setSourceFeatureChannelMaxCount:atIndex:@
setSourceFeatureChannelMaxCount_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setSourceFeatureChannelMaxCount_atIndex mpscnnMultiaryKernel  count index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setSourceFeatureChannelMaxCount:atIndex:") retVoid [argCULong count, argCULong index]

-- | The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: subsubsection_edgemode
--
-- @index@ — The index of the source image to which the edge mode refers
--
-- Returns: The edge mode for that source image
--
-- ObjC selector: @- edgeModeAtIndex:@
edgeModeAtIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO MPSImageEdgeMode
edgeModeAtIndex mpscnnMultiaryKernel  index =
    fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpscnnMultiaryKernel (mkSelector "edgeModeAtIndex:") retCULong [argCULong index]

-- | Set the MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: subsubsection_edgemode
--
-- @edgeMode@ — The new edge mode to use
--
-- @index@ — The index of the source image to which the edge mode refers
--
-- ObjC selector: @- setEdgeMode:atIndex:@
setEdgeMode_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> MPSImageEdgeMode -> CULong -> IO ()
setEdgeMode_atIndex mpscnnMultiaryKernel  edgeMode index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setEdgeMode:atIndex:") retVoid [argCULong (coerce edgeMode), argCULong index]

-- | The width of the kernel filter window
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- @index@ — The index of the source image to which the kernel width refers
--
-- ObjC selector: @- kernelWidthAtIndex:@
kernelWidthAtIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
kernelWidthAtIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "kernelWidthAtIndex:") retCULong [argCULong index]

-- | Set the width of the kernel filter window
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- @width@ — The new width
--
-- @index@ — The index of the source image to which the kernel width refers
--
-- ObjC selector: @- setKernelWidth:atIndex:@
setKernelWidth_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setKernelWidth_atIndex mpscnnMultiaryKernel  width index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setKernelWidth:atIndex:") retVoid [argCULong width, argCULong index]

-- | The height of the kernel filter window
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- @index@ — The index of the source image to which the kernel width refers
--
-- ObjC selector: @- kernelHeightAtIndex:@
kernelHeightAtIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
kernelHeightAtIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "kernelHeightAtIndex:") retCULong [argCULong index]

-- | Set the height of the kernel filter window
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- @height@ — The new width
--
-- @index@ — The index of the source image to which the kernel width refers
--
-- ObjC selector: @- setKernelHeight:atIndex:@
setKernelHeight_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setKernelHeight_atIndex mpscnnMultiaryKernel  height index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setKernelHeight:atIndex:") retVoid [argCULong height, argCULong index]

-- | The downsampling factor in the horizontal dimension for the source image
--
-- @index@ — The index of the source Image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- Returns: The stride
--
-- ObjC selector: @- strideInPixelsXatIndex:@
strideInPixelsXatIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
strideInPixelsXatIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "strideInPixelsXatIndex:") retCULong [argCULong index]

-- | The downsampling factor in the horizontal dimension for the source image
--
-- If the filter does not do up or downsampling, 1 is returned.  Default: 1
--
-- @index@ — The index of the source Image
--
-- @stride@ — The stride for the source image
--
-- ObjC selector: @- setStrideInPixelsX:atIndex:@
setStrideInPixelsX_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setStrideInPixelsX_atIndex mpscnnMultiaryKernel  stride index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setStrideInPixelsX:atIndex:") retVoid [argCULong stride, argCULong index]

-- | The downsampling factor in the vertical dimension for the source image
--
-- @index@ — The index of the source Image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- Returns: The stride
--
-- ObjC selector: @- strideInPixelsYatIndex:@
strideInPixelsYatIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
strideInPixelsYatIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "strideInPixelsYatIndex:") retCULong [argCULong index]

-- | The downsampling factor in the vertical dimension for the source image
--
-- If the filter does not do up or downsampling, 1 is returned.  Default: 1
--
-- @index@ — The index of the source Image
--
-- @stride@ — The stride for the source image
--
-- ObjC selector: @- setStrideInPixelsY:atIndex:@
setStrideInPixelsY_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setStrideInPixelsY_atIndex mpscnnMultiaryKernel  stride index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setStrideInPixelsY:atIndex:") retVoid [argCULong stride, argCULong index]

-- | Stride in source coordinates from one kernel tap to the next in the X dimension.
--
-- @index@ — The index of the source image to which the dilation rate applies
--
-- Returns: The dilation rate
--
-- ObjC selector: @- dilationRateXatIndex:@
dilationRateXatIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
dilationRateXatIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "dilationRateXatIndex:") retCULong [argCULong index]

-- | Set the stride in source coordinates from one kernel tap to the next in the X dimension.
--
-- @index@ — The index of the source image to which the dilation rate applies
--
-- @dilationRate@ — The dilation rate
--
-- ObjC selector: @- setDilationRateX:atIndex:@
setDilationRateX_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setDilationRateX_atIndex mpscnnMultiaryKernel  dilationRate index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setDilationRateX:atIndex:") retVoid [argCULong dilationRate, argCULong index]

-- | Stride in source coordinates from one kernel tap to the next in the Y dimension.
--
-- @index@ — The index of the source image to which the dilation rate applies
--
-- Returns: The dilation rate
--
-- ObjC selector: @- dilationRateYatIndex:@
dilationRateYatIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO CULong
dilationRateYatIndex mpscnnMultiaryKernel  index =
    sendMsg mpscnnMultiaryKernel (mkSelector "dilationRateYatIndex:") retCULong [argCULong index]

-- | Set the stride in source coordinates from one kernel tap to the next in the Y dimension.
--
-- @index@ — The index of the source image to which the dilation rate applies
--
-- @dilationRate@ — The dilation rate
--
-- ObjC selector: @- setDilationRateY:atIndex:@
setDilationRateY_atIndex :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> CULong -> IO ()
setDilationRateY_atIndex mpscnnMultiaryKernel  dilationRate index =
    sendMsg mpscnnMultiaryKernel (mkSelector "setDilationRateY:atIndex:") retVoid [argCULong dilationRate, argCULong index]

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
initWithCoder_device :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSCoder aDecoder) => mpscnnMultiaryKernel -> aDecoder -> RawId -> IO (Id MPSCNNMultiaryKernel)
initWithCoder_device mpscnnMultiaryKernel  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnMultiaryKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSCNNKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImages@ — An array containing the source images
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. destinationImage may not alias primarySourceImage or secondarySourceImage.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImages:destinationImage:@
encodeToCommandBuffer_sourceImages_destinationImage :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages, IsMPSImage destinationImage) => mpscnnMultiaryKernel -> RawId -> sourceImages -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImages_destinationImage mpscnnMultiaryKernel  commandBuffer sourceImages destinationImage =
  withObjCPtr sourceImages $ \raw_sourceImages ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceImages:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | Encode a MPSCNNKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method. Multiple images are processed concurrently.              All images must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImages@ — An array of image batches containing the source images.
--
-- @destinationImages@ — An array of MPSImage objects to contain the result images.                                    destinationImages may not alias primarySourceImages or secondarySourceImages                                    in any manner.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImages :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages) => mpscnnMultiaryKernel -> RawId -> sourceImages -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationImages mpscnnMultiaryKernel  commandBuffer sourceImages destinationImages =
  withObjCPtr sourceImages $ \raw_sourceImages ->
      sendMsg mpscnnMultiaryKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr (unRawId destinationImages) :: Ptr ())]

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture to hold the result and return it.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property.  See discussion in MPSNeuralNetworkTypes.h.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImages@ — An array of MPSImages to use as the source images for the filter.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImages:@
encodeToCommandBuffer_sourceImages :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages) => mpscnnMultiaryKernel -> RawId -> sourceImages -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImages mpscnnMultiaryKernel  commandBuffer sourceImages =
  withObjCPtr sourceImages $ \raw_sourceImages ->
      sendMsg mpscnnMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceImages:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImages :: Ptr ())] >>= retainedObject . castPtr

-- | Encode a MPSCNNKernel into a command Buffer. Create textures to hold the results and return them.
--
-- In the first iteration on this method, encodeBatchToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property.  See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImageBatches@ — An array of image batches to use as the source images for the filter.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImages :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImageBatches) => mpscnnMultiaryKernel -> RawId -> sourceImageBatches -> IO RawId
encodeBatchToCommandBuffer_sourceImages mpscnnMultiaryKernel  commandBuffer sourceImageBatches =
  withObjCPtr sourceImageBatches $ \raw_sourceImageBatches ->
      fmap (RawId . castPtr) $ sendMsg mpscnnMultiaryKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImageBatches :: Ptr ())]

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture and state to hold the results and return them.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationState:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImages@ — An array of MPSImages to use as the source images for the filter.
--
-- @outState@ — The address of location to write the pointer to the result state of the operation
--
-- @isTemporary@ — YES if the outState should be a temporary object
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImages:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImages_destinationState_destinationStateIsTemporary :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages, IsMPSState outState) => mpscnnMultiaryKernel -> RawId -> sourceImages -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImages_destinationState_destinationStateIsTemporary mpscnnMultiaryKernel  commandBuffer sourceImages outState isTemporary =
  withObjCPtr sourceImages $ \raw_sourceImages ->
    withObjCPtr outState $ \raw_outState ->
        sendMsg mpscnnMultiaryKernel (mkSelector "encodeToCommandBuffer:sourceImages:destinationState:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_outState :: Ptr ()), argCULong (if isTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture and state to hold the results and return them.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationState:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImageBatches@ — An array of batches to use as the source images for the filter.
--
-- @outState@ — A new state object is returned here.
--
-- @isTemporary@ — YES if the outState should be a temporary object
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImageBatches) => mpscnnMultiaryKernel -> RawId -> sourceImageBatches -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary mpscnnMultiaryKernel  commandBuffer sourceImageBatches outState isTemporary =
  withObjCPtr sourceImageBatches $ \raw_sourceImageBatches ->
      fmap (RawId . castPtr) $ sendMsg mpscnnMultiaryKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImageBatches :: Ptr ()), argPtr (castPtr (unRawId outState) :: Ptr ()), argCULong (if isTemporary then 1 else 0)]

-- | Returns YES if the same state is used for every operation in a batch
--
-- If NO, then each image in a MPSImageBatch will need a corresponding              (and different) state to go with it. Set to YES to avoid allocating              redundant state in the case when the same state is used all the time.              Default: NO
--
-- ObjC selector: @- isResultStateReusedAcrossBatch@
isResultStateReusedAcrossBatch :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO Bool
isResultStateReusedAcrossBatch mpscnnMultiaryKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnMultiaryKernel (mkSelector "isResultStateReusedAcrossBatch") retCULong []

-- | Returns YES if the filter must be run over the entire batch before its              results may be used
--
-- Nearly all filters do not need to see the entire batch all at once and can              operate correctly with partial batches. This allows the graph to              strip-mine the problem, processing the graph top to bottom on a subset              of the batch at a time, dramatically reducing memory usage. As the full              nominal working set for a graph is often so large that it may not fit              in memory, sub-batching may be required forward progress.
--
-- Batch normalization statistics on the other hand must complete the batch              before the statistics may be used to normalize the images in the batch              in the ensuing normalization filter. Consequently, batch normalization statistics              requests the graph insert a batch barrier following it by returning              YES from -appendBatchBarrier. This tells the graph to complete the batch              before any dependent filters can start. Note that the filter itself may              still be subject to sub-batching in its operation. All filters must be able to              function without seeing the entire batch in a single -encode call. Carry              over state that is accumulated across sub-batches is commonly carried in              a shared MPSState containing a MTLBuffer. See -isResultStateReusedAcrossBatch.
--
-- Caution: on most supported devices, the working set may be so large              that the graph may be forced to throw away and recalculate most              intermediate images in cases where strip-mining can not occur because              -appendBatchBarrier returns YES. A single batch barrier can commonly              cause a memory size increase and/or performance reduction by many fold              over the entire graph.  Filters of this variety should be avoided.
--
-- Default: NO
--
-- ObjC selector: @- appendBatchBarrier@
appendBatchBarrier :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO Bool
appendBatchBarrier mpscnnMultiaryKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnMultiaryKernel (mkSelector "appendBatchBarrier") retCULong []

-- | Allocate a MPSState (subclass) to hold the results from a -encodeBatchToCommandBuffer... operation
--
-- A graph may need to allocate storage up front before executing.  This may be              necessary to avoid using too much memory and to manage large batches.  The function              should allocate any MPSState objects that will be produced by an -encode call              with the indicated sourceImages and sourceStates inputs. Though the states              can be further adjusted in the ensuing -encode call, the states should              be initialized with all important data and all MTLResource storage allocated.              The data stored in the MTLResource need not be initialized, unless the ensuing              -encode call expects it to be.
--
-- The MTLDevice used by the result is derived from the source image.              The padding policy will be applied to the filter before this is called              to give it the chance to configure any properties like MPSCNNKernel.offset.
--
-- CAUTION:              The kernel must have all properties set to values that will ultimately be              passed to the -encode call that writes to the state, before              -resultStateForSourceImages:sourceStates:destinationImage: is called or behavior is undefined.              Please note that -destinationImageDescriptorForSourceImages:sourceStates:              will alter some of these properties automatically based on the padding policy.              If you intend to call that to make the destination image, then you should              call that before -resultStateForSourceImages:sourceStates:destinationImage:. This will ensure the              properties used in the encode call and in the destination image creation              match those used to configure the state.
--
-- The following order is recommended:
--
-- // Configure MPSCNNKernel properties first                  kernel.edgeMode = MPSImageEdgeModeZero;                  kernel.destinationFeatureChannelOffset = 128; // concatenation without the copy                  ...
--
-- // ALERT: will change MPSCNNKernel properties                  MPSImageDescriptor * d = [kernel destinationImageDescriptorForSourceImage: source                                                                               sourceStates: states];                  MPSTemporaryImage * dest = [MPSTemporaryImage temporaryImageWithCommandBuffer: cmdBuf                                                                                imageDescriptor: d];
--
-- // Now that all properties are configured properly, we can make the result state                  // and call encode.                  MPSState * __nullable destState = [kernel resultStateForSourceImage: source                                                                         sourceStates: states                                                                     destinationImage: dest];
--
-- // This form of -encode will be declared by the MPSCNNKernel subclass                  [kernel encodeToCommandBuffer: cmdBuf                                    sourceImage: source                               destinationState: destState                               destinationImage: dest ];
--
-- Default: returns nil
--
-- @sourceImages@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSStates consumed by the associated -encode call,                                  for a batch size of 1.
--
-- @destinationImage@ — The destination image for the encode call
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              When the batch size is not 1, this function will be called repeatedly unless              -isResultStateReusedAcrossBatch returns YES. If  -isResultStateReusedAcrossBatch              returns YES, then it will be called once per batch and the MPSStateBatch array will              contain MPSStateBatch.length references to the same object.
--
-- ObjC selector: @- resultStateForSourceImages:sourceStates:destinationImage:@
resultStateForSourceImages_sourceStates_destinationImage :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnMultiaryKernel -> sourceImages -> sourceStates -> destinationImage -> IO (Id MPSState)
resultStateForSourceImages_sourceStates_destinationImage mpscnnMultiaryKernel  sourceImages sourceStates destinationImage =
  withObjCPtr sourceImages $ \raw_sourceImages ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnMultiaryKernel (mkSelector "resultStateForSourceImages:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- resultStateBatchForSourceImages:sourceStates:destinationImage:@
resultStateBatchForSourceImages_sourceStates_destinationImage :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages, IsNSArray sourceStates) => mpscnnMultiaryKernel -> sourceImages -> sourceStates -> RawId -> IO RawId
resultStateBatchForSourceImages_sourceStates_destinationImage mpscnnMultiaryKernel  sourceImages sourceStates destinationImage =
  withObjCPtr sourceImages $ \raw_sourceImages ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
        fmap (RawId . castPtr) $ sendMsg mpscnnMultiaryKernel (mkSelector "resultStateBatchForSourceImages:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | Allocate a temporary MPSState (subclass) to hold the results from a -encodeBatchToCommandBuffer... operation
--
-- A graph may need to allocate storage up front before executing.  This may be              necessary to avoid using too much memory and to manage large batches.  The function              should allocate any MPSState objects that will be produced by an -encode call              with the indicated sourceImages and sourceStates inputs. Though the states              can be further adjusted in the ensuing -encode call, the states should              be initialized with all important data and all MTLResource storage allocated.              The data stored in the MTLResource need not be initialized, unless the ensuing              -encode call expects it to be.
--
-- The MTLDevice used by the result is derived from the command buffer.              The padding policy will be applied to the filter before this is called              to give it the chance to configure any properties like MPSCNNKernel.offset.
--
-- CAUTION:              The kernel must have all properties set to values that will ultimately be              passed to the -encode call that writes to the state, before              -resultStateForSourceImages:sourceStates:destinationImage: is called or behavior is undefined.              Please note that -destinationImageDescriptorForSourceImages:sourceStates:destinationImage:              will alter some of these properties automatically based on the padding policy.              If you intend to call that to make the destination image, then you should              call that before -resultStateForSourceImages:sourceStates:destinationImage:.  This will ensure the              properties used in the encode call and in the destination image creation              match those used to configure the state.
--
-- The following order is recommended:
--
-- // Configure MPSCNNKernel properties first                  kernel.edgeMode = MPSImageEdgeModeZero;                  kernel.destinationFeatureChannelOffset = 128; // concatenation without the copy                  ...
--
-- // ALERT: will change MPSCNNKernel properties                  MPSImageDescriptor * d = [kernel destinationImageDescriptorForSourceImage: source                                                                               sourceStates: states];                  MPSTemporaryImage * dest = [MPSTemporaryImage temporaryImageWithCommandBuffer: cmdBuf                                                                                imageDescriptor: d];
--
-- // Now that all properties are configured properly, we can make the result state                  // and call encode.                  MPSState * __nullable destState = [kernel temporaryResultStateForCommandBuffer: cmdBuf                                                                                     sourceImage: source                                                                                    sourceStates: states];
--
-- // This form of -encode will be declared by the MPSCNNKernel subclass                  [kernel encodeToCommandBuffer: cmdBuf                                    sourceImage: source                               destinationState: destState                               destinationImage: dest ];
--
-- Default: returns nil
--
-- @commandBuffer@ — The command buffer to allocate the temporary storage against                                  The state will only be valid on this command buffer.
--
-- @sourceImage@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSStates consumed by the associated -encode call,                                  for a batch size of 1.
--
-- @destinationImage@ — The destination image for the encode call
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              When the batch size is not 1, this function will be called repeatedly unless              -isResultStateReusedAcrossBatch returns YES. If  -isResultStateReusedAcrossBatch              returns YES, then it will be called once per batch and the MPSStateBatch array will              contain MPSStateBatch.length references to the same object.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImages:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImages_sourceStates_destinationImage :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnMultiaryKernel -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSState)
temporaryResultStateForCommandBuffer_sourceImages_sourceStates_destinationImage mpscnnMultiaryKernel  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnMultiaryKernel (mkSelector "temporaryResultStateForCommandBuffer:sourceImages:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImages:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImages_sourceStates_destinationImage :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImage, IsNSArray sourceStates) => mpscnnMultiaryKernel -> RawId -> sourceImage -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImages_sourceStates_destinationImage mpscnnMultiaryKernel  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
        fmap (RawId . castPtr) $ sendMsg mpscnnMultiaryKernel (mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImages:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | Get a suggested destination image descriptor for a source image
--
-- Your application is certainly free to pass in any destinationImage              it likes to encodeToCommandBuffer:sourceImage:destinationImage,              within reason. This is the basic design for iOS 10. This method              is therefore not required.
--
-- However, calculating the MPSImage size and MPSCNNKernel properties              for each filter can be tedious and complicated work, so this method              is made available to automate the process. The application may              modify the properties of the descriptor before a MPSImage is made from              it, so long as the choice is sensible for the kernel in question.              Please see individual kernel descriptions for restrictions.
--
-- The expected timeline for use is as follows:
--
-- 1) This method is called:                  a) The default MPS padding calculation is applied. It                     uses the MPSNNPaddingMethod of the .padding property to                     provide a consistent addressing scheme over the graph.                     It creates the MPSImageDescriptor and adjusts the .offset                     property of the MPSNNKernel. When using a MPSNNGraph, the                     padding is set using the MPSNNFilterNode as a proxy.
--
-- b) This method may be overridden by MPSCNNKernel subclass                     to achieve any customization appropriate to the object type.
--
-- c) Source states are then applied in order. These may modify the                     descriptor and may update other object properties. See:                      -destinationImageDescriptorForSourceImages:sourceStates:                       forKernel:suggestedDescriptor:  This is the typical way                      in which MPS may attempt to influence the operation of                      its kernels.
--
-- d) If the .padding property has a custom padding policy method                      of the same name, it is called. Similarly, it may also adjust                      the descriptor and any MPSCNNKernel properties. This is the                      typical way in which your application may attempt to influence                      the operation of the MPS kernels.
--
-- 2) A result is returned from this method and the caller                     may further adjust the descriptor and kernel properties                     directly.
--
-- 3) The caller uses the descriptor to make a new MPSImage to                   use as the destination image for the -encode call in step 5.
--
-- 4) The caller calls -resultStateForSourceImage:sourceStates:destinationImage:                    to make any result states needed for the kernel. If there isn't                    one, it will return nil. A variant is available to return a                    temporary state instead.
--
-- 5) a -encode method is called to encode the kernel.
--
-- The entire process 1-5 is more simply achieved by just calling an -encode...              method that returns a MPSImage out the left hand sid of the method. Simpler              still, use the MPSNNGraph to coordinate the entire process from end to end.              Opportunities to influence the process are of course reduced, as (2) is no longer              possible with either method. Your application may opt to use the five step method              if it requires greater customization as described, or if it would like to estimate              storage in advance based on the sum of MPSImageDescriptors before processing              a graph. Storage estimation is done by using the MPSImageDescriptor to create              a MPSImage (without passing it a texture), and then call -resourceSize. As long              as the MPSImage is not used in an encode call and the .texture property is not              invoked, the underlying MTLTexture is not created.
--
-- No destination state or destination image is provided as an argument to this              function because it is expected they will be made / configured after this              is called. This method is expected to auto-configure important object properties              that may be needed in the ensuing destination image and state creation steps.
--
-- @sourceImages@ — A array of source images that will be passed into the -encode call                              Since MPSCNNKernel is a unary kernel, it is an array of length 1.
--
-- @sourceStates@ — An optional array of source states that will be passed into the -encode call
--
-- Returns: an image descriptor allocated on the autorelease pool
--
-- ObjC selector: @- destinationImageDescriptorForSourceImages:sourceStates:@
destinationImageDescriptorForSourceImages_sourceStates :: (IsMPSCNNMultiaryKernel mpscnnMultiaryKernel, IsNSArray sourceImages, IsNSArray sourceStates) => mpscnnMultiaryKernel -> sourceImages -> sourceStates -> IO (Id MPSImageDescriptor)
destinationImageDescriptorForSourceImages_sourceStates mpscnnMultiaryKernel  sourceImages sourceStates =
  withObjCPtr sourceImages $ \raw_sourceImages ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
        sendMsg mpscnnMultiaryKernel (mkSelector "destinationImageDescriptorForSourceImages:sourceStates:") (retPtr retVoid) [argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ())] >>= retainedObject . castPtr

-- | The number of source images accepted by the kernel
--
-- ObjC selector: @- sourceCount@
sourceCount :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO CULong
sourceCount mpscnnMultiaryKernel  =
    sendMsg mpscnnMultiaryKernel (mkSelector "sourceCount") retCULong []

-- | destinationFeatureChannelOffset
--
-- The number of channels in the destination MPSImage to skip before writing output.
--
-- This is the starting offset into the destination image in the feature channel dimension              at which destination data is written.              This allows an application to pass a subset of all the channels in MPSImage as output of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel outputs 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as output, we can set destinationFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel outputs N channels,              destination image MUST have at least destinationFeatureChannelOffset + N channels. Using a destination              image with insufficient number of feature channels result in an error.              E.g. if the MPSCNNConvolution outputs 32 channels, and destination has 64 channels, then it is an error to set              destinationFeatureChannelOffset > 32.
--
-- ObjC selector: @- destinationFeatureChannelOffset@
destinationFeatureChannelOffset :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO CULong
destinationFeatureChannelOffset mpscnnMultiaryKernel  =
    sendMsg mpscnnMultiaryKernel (mkSelector "destinationFeatureChannelOffset") retCULong []

-- | destinationFeatureChannelOffset
--
-- The number of channels in the destination MPSImage to skip before writing output.
--
-- This is the starting offset into the destination image in the feature channel dimension              at which destination data is written.              This allows an application to pass a subset of all the channels in MPSImage as output of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel outputs 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as output, we can set destinationFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel outputs N channels,              destination image MUST have at least destinationFeatureChannelOffset + N channels. Using a destination              image with insufficient number of feature channels result in an error.              E.g. if the MPSCNNConvolution outputs 32 channels, and destination has 64 channels, then it is an error to set              destinationFeatureChannelOffset > 32.
--
-- ObjC selector: @- setDestinationFeatureChannelOffset:@
setDestinationFeatureChannelOffset :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> CULong -> IO ()
setDestinationFeatureChannelOffset mpscnnMultiaryKernel  value =
    sendMsg mpscnnMultiaryKernel (mkSelector "setDestinationFeatureChannelOffset:") retVoid [argCULong value]

-- | isBackwards
--
-- YES if the filter operates backwards.
--
-- This influences how strideInPixelsX/Y should be interpreted.
--
-- ObjC selector: @- isBackwards@
isBackwards :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO Bool
isBackwards mpscnnMultiaryKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnMultiaryKernel (mkSelector "isBackwards") retCULong []

-- | Returns true if the -encode call modifies the state object it accepts.
--
-- ObjC selector: @- isStateModified@
isStateModified :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO Bool
isStateModified mpscnnMultiaryKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnMultiaryKernel (mkSelector "isStateModified") retCULong []

-- | padding
--
-- The padding method used by the filter
--
-- This influences how strideInPixelsX/Y should be interpreted.              Default:  MPSNNPaddingMethodAlignCentered | MPSNNPaddingMethodAddRemainderToTopLeft | MPSNNPaddingMethodSizeSame              Some object types (e.g. MPSCNNFullyConnected) may override this default with something appropriate to its operation.
--
-- ObjC selector: @- padding@
padding :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO RawId
padding mpscnnMultiaryKernel  =
    fmap (RawId . castPtr) $ sendMsg mpscnnMultiaryKernel (mkSelector "padding") (retPtr retVoid) []

-- | padding
--
-- The padding method used by the filter
--
-- This influences how strideInPixelsX/Y should be interpreted.              Default:  MPSNNPaddingMethodAlignCentered | MPSNNPaddingMethodAddRemainderToTopLeft | MPSNNPaddingMethodSizeSame              Some object types (e.g. MPSCNNFullyConnected) may override this default with something appropriate to its operation.
--
-- ObjC selector: @- setPadding:@
setPadding :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> RawId -> IO ()
setPadding mpscnnMultiaryKernel  value =
    sendMsg mpscnnMultiaryKernel (mkSelector "setPadding:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- destinationImageAllocator@
destinationImageAllocator :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> IO RawId
destinationImageAllocator mpscnnMultiaryKernel  =
    fmap (RawId . castPtr) $ sendMsg mpscnnMultiaryKernel (mkSelector "destinationImageAllocator") (retPtr retVoid) []

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- setDestinationImageAllocator:@
setDestinationImageAllocator :: IsMPSCNNMultiaryKernel mpscnnMultiaryKernel => mpscnnMultiaryKernel -> RawId -> IO ()
setDestinationImageAllocator mpscnnMultiaryKernel  value =
    sendMsg mpscnnMultiaryKernel (mkSelector "setDestinationImageAllocator:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:sourceCount:@
initWithDevice_sourceCountSelector :: Selector
initWithDevice_sourceCountSelector = mkSelector "initWithDevice:sourceCount:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @sourceFeatureChannelOffsetAtIndex:@
sourceFeatureChannelOffsetAtIndexSelector :: Selector
sourceFeatureChannelOffsetAtIndexSelector = mkSelector "sourceFeatureChannelOffsetAtIndex:"

-- | @Selector@ for @setSourceFeatureChannelOffset:atIndex:@
setSourceFeatureChannelOffset_atIndexSelector :: Selector
setSourceFeatureChannelOffset_atIndexSelector = mkSelector "setSourceFeatureChannelOffset:atIndex:"

-- | @Selector@ for @sourceFeatureChannelMaxCountAtIndex:@
sourceFeatureChannelMaxCountAtIndexSelector :: Selector
sourceFeatureChannelMaxCountAtIndexSelector = mkSelector "sourceFeatureChannelMaxCountAtIndex:"

-- | @Selector@ for @setSourceFeatureChannelMaxCount:atIndex:@
setSourceFeatureChannelMaxCount_atIndexSelector :: Selector
setSourceFeatureChannelMaxCount_atIndexSelector = mkSelector "setSourceFeatureChannelMaxCount:atIndex:"

-- | @Selector@ for @edgeModeAtIndex:@
edgeModeAtIndexSelector :: Selector
edgeModeAtIndexSelector = mkSelector "edgeModeAtIndex:"

-- | @Selector@ for @setEdgeMode:atIndex:@
setEdgeMode_atIndexSelector :: Selector
setEdgeMode_atIndexSelector = mkSelector "setEdgeMode:atIndex:"

-- | @Selector@ for @kernelWidthAtIndex:@
kernelWidthAtIndexSelector :: Selector
kernelWidthAtIndexSelector = mkSelector "kernelWidthAtIndex:"

-- | @Selector@ for @setKernelWidth:atIndex:@
setKernelWidth_atIndexSelector :: Selector
setKernelWidth_atIndexSelector = mkSelector "setKernelWidth:atIndex:"

-- | @Selector@ for @kernelHeightAtIndex:@
kernelHeightAtIndexSelector :: Selector
kernelHeightAtIndexSelector = mkSelector "kernelHeightAtIndex:"

-- | @Selector@ for @setKernelHeight:atIndex:@
setKernelHeight_atIndexSelector :: Selector
setKernelHeight_atIndexSelector = mkSelector "setKernelHeight:atIndex:"

-- | @Selector@ for @strideInPixelsXatIndex:@
strideInPixelsXatIndexSelector :: Selector
strideInPixelsXatIndexSelector = mkSelector "strideInPixelsXatIndex:"

-- | @Selector@ for @setStrideInPixelsX:atIndex:@
setStrideInPixelsX_atIndexSelector :: Selector
setStrideInPixelsX_atIndexSelector = mkSelector "setStrideInPixelsX:atIndex:"

-- | @Selector@ for @strideInPixelsYatIndex:@
strideInPixelsYatIndexSelector :: Selector
strideInPixelsYatIndexSelector = mkSelector "strideInPixelsYatIndex:"

-- | @Selector@ for @setStrideInPixelsY:atIndex:@
setStrideInPixelsY_atIndexSelector :: Selector
setStrideInPixelsY_atIndexSelector = mkSelector "setStrideInPixelsY:atIndex:"

-- | @Selector@ for @dilationRateXatIndex:@
dilationRateXatIndexSelector :: Selector
dilationRateXatIndexSelector = mkSelector "dilationRateXatIndex:"

-- | @Selector@ for @setDilationRateX:atIndex:@
setDilationRateX_atIndexSelector :: Selector
setDilationRateX_atIndexSelector = mkSelector "setDilationRateX:atIndex:"

-- | @Selector@ for @dilationRateYatIndex:@
dilationRateYatIndexSelector :: Selector
dilationRateYatIndexSelector = mkSelector "dilationRateYatIndex:"

-- | @Selector@ for @setDilationRateY:atIndex:@
setDilationRateY_atIndexSelector :: Selector
setDilationRateY_atIndexSelector = mkSelector "setDilationRateY:atIndex:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImages:destinationImage:@
encodeToCommandBuffer_sourceImages_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImages_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImages:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImages:@
encodeToCommandBuffer_sourceImagesSelector :: Selector
encodeToCommandBuffer_sourceImagesSelector = mkSelector "encodeToCommandBuffer:sourceImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImages:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImages_destinationState_destinationStateIsTemporarySelector :: Selector
encodeToCommandBuffer_sourceImages_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceImages:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:"

-- | @Selector@ for @isResultStateReusedAcrossBatch@
isResultStateReusedAcrossBatchSelector :: Selector
isResultStateReusedAcrossBatchSelector = mkSelector "isResultStateReusedAcrossBatch"

-- | @Selector@ for @appendBatchBarrier@
appendBatchBarrierSelector :: Selector
appendBatchBarrierSelector = mkSelector "appendBatchBarrier"

-- | @Selector@ for @resultStateForSourceImages:sourceStates:destinationImage:@
resultStateForSourceImages_sourceStates_destinationImageSelector :: Selector
resultStateForSourceImages_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImages:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForSourceImages:sourceStates:destinationImage:@
resultStateBatchForSourceImages_sourceStates_destinationImageSelector :: Selector
resultStateBatchForSourceImages_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForSourceImages:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImages:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImages_sourceStates_destinationImageSelector :: Selector
temporaryResultStateForCommandBuffer_sourceImages_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImages:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:sourceImages:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImages_sourceStates_destinationImageSelector :: Selector
temporaryResultStateBatchForCommandBuffer_sourceImages_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImages:sourceStates:destinationImage:"

-- | @Selector@ for @destinationImageDescriptorForSourceImages:sourceStates:@
destinationImageDescriptorForSourceImages_sourceStatesSelector :: Selector
destinationImageDescriptorForSourceImages_sourceStatesSelector = mkSelector "destinationImageDescriptorForSourceImages:sourceStates:"

-- | @Selector@ for @sourceCount@
sourceCountSelector :: Selector
sourceCountSelector = mkSelector "sourceCount"

-- | @Selector@ for @destinationFeatureChannelOffset@
destinationFeatureChannelOffsetSelector :: Selector
destinationFeatureChannelOffsetSelector = mkSelector "destinationFeatureChannelOffset"

-- | @Selector@ for @setDestinationFeatureChannelOffset:@
setDestinationFeatureChannelOffsetSelector :: Selector
setDestinationFeatureChannelOffsetSelector = mkSelector "setDestinationFeatureChannelOffset:"

-- | @Selector@ for @isBackwards@
isBackwardsSelector :: Selector
isBackwardsSelector = mkSelector "isBackwards"

-- | @Selector@ for @isStateModified@
isStateModifiedSelector :: Selector
isStateModifiedSelector = mkSelector "isStateModified"

-- | @Selector@ for @padding@
paddingSelector :: Selector
paddingSelector = mkSelector "padding"

-- | @Selector@ for @setPadding:@
setPaddingSelector :: Selector
setPaddingSelector = mkSelector "setPadding:"

-- | @Selector@ for @destinationImageAllocator@
destinationImageAllocatorSelector :: Selector
destinationImageAllocatorSelector = mkSelector "destinationImageAllocator"

-- | @Selector@ for @setDestinationImageAllocator:@
setDestinationImageAllocatorSelector :: Selector
setDestinationImageAllocatorSelector = mkSelector "setDestinationImageAllocator:"

