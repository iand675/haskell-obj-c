{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNBinaryKernel
--
-- This depends on Metal.framework
--
-- Describes a convolution neural network kernel.
--
-- A MPSCNNKernel consumes two MPSImages, primary and secondary, and produces one MPSImage.
--
-- Generated bindings for @MPSCNNBinaryKernel@.
module ObjC.MetalPerformanceShaders.MPSCNNBinaryKernel
  ( MPSCNNBinaryKernel
  , IsMPSCNNBinaryKernel(..)
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages
  , encodeToCommandBuffer_primaryImage_secondaryImage
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporary
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporary
  , resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImage
  , resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImage
  , temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImage
  , isResultStateReusedAcrossBatch
  , appendBatchBarrier
  , destinationImageDescriptorForSourceImages_sourceStates
  , encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImage
  , batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImage
  , destinationFeatureChannelOffset
  , setDestinationFeatureChannelOffset
  , primarySourceFeatureChannelOffset
  , setPrimarySourceFeatureChannelOffset
  , secondarySourceFeatureChannelOffset
  , setSecondarySourceFeatureChannelOffset
  , primarySourceFeatureChannelMaxCount
  , setPrimarySourceFeatureChannelMaxCount
  , secondarySourceFeatureChannelMaxCount
  , setSecondarySourceFeatureChannelMaxCount
  , primaryEdgeMode
  , setPrimaryEdgeMode
  , secondaryEdgeMode
  , setSecondaryEdgeMode
  , primaryKernelWidth
  , primaryKernelHeight
  , secondaryKernelWidth
  , secondaryKernelHeight
  , primaryStrideInPixelsX
  , setPrimaryStrideInPixelsX
  , primaryStrideInPixelsY
  , setPrimaryStrideInPixelsY
  , secondaryStrideInPixelsX
  , setSecondaryStrideInPixelsX
  , secondaryStrideInPixelsY
  , setSecondaryStrideInPixelsY
  , primaryDilationRateX
  , primaryDilationRateY
  , secondaryDilationRateX
  , secondaryDilationRateY
  , isBackwards
  , isStateModified
  , padding
  , setPadding
  , destinationImageAllocator
  , setDestinationImageAllocator
  , appendBatchBarrierSelector
  , batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector
  , destinationFeatureChannelOffsetSelector
  , destinationImageAllocatorSelector
  , destinationImageDescriptorForSourceImages_sourceStatesSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector
  , encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporarySelector
  , encodeToCommandBuffer_primaryImage_secondaryImageSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector
  , encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporarySelector
  , encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , isBackwardsSelector
  , isResultStateReusedAcrossBatchSelector
  , isStateModifiedSelector
  , paddingSelector
  , primaryDilationRateXSelector
  , primaryDilationRateYSelector
  , primaryEdgeModeSelector
  , primaryKernelHeightSelector
  , primaryKernelWidthSelector
  , primarySourceFeatureChannelMaxCountSelector
  , primarySourceFeatureChannelOffsetSelector
  , primaryStrideInPixelsXSelector
  , primaryStrideInPixelsYSelector
  , resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector
  , resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector
  , secondaryDilationRateXSelector
  , secondaryDilationRateYSelector
  , secondaryEdgeModeSelector
  , secondaryKernelHeightSelector
  , secondaryKernelWidthSelector
  , secondarySourceFeatureChannelMaxCountSelector
  , secondarySourceFeatureChannelOffsetSelector
  , secondaryStrideInPixelsXSelector
  , secondaryStrideInPixelsYSelector
  , setDestinationFeatureChannelOffsetSelector
  , setDestinationImageAllocatorSelector
  , setPaddingSelector
  , setPrimaryEdgeModeSelector
  , setPrimarySourceFeatureChannelMaxCountSelector
  , setPrimarySourceFeatureChannelOffsetSelector
  , setPrimaryStrideInPixelsXSelector
  , setPrimaryStrideInPixelsYSelector
  , setSecondaryEdgeModeSelector
  , setSecondarySourceFeatureChannelMaxCountSelector
  , setSecondarySourceFeatureChannelOffsetSelector
  , setSecondaryStrideInPixelsXSelector
  , setSecondaryStrideInPixelsYSelector
  , temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector

  -- * Enum types
  , MPSImageEdgeMode(MPSImageEdgeMode)
  , pattern MPSImageEdgeModeZero
  , pattern MPSImageEdgeModeClamp
  , pattern MPSImageEdgeModeMirror
  , pattern MPSImageEdgeModeMirrorWithEdge
  , pattern MPSImageEdgeModeConstant

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

-- | Standard init with default properties per filter type
--
-- @device@ — The device that the filter will be used on. May not be NULL.
--
-- Returns: A pointer to the newly initialized object. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> RawId -> IO (Id MPSCNNBinaryKernel)
initWithDevice mpscnnBinaryKernel device =
  sendOwnedMessage mpscnnBinaryKernel initWithDeviceSelector device

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
initWithCoder_device :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsNSCoder aDecoder) => mpscnnBinaryKernel -> aDecoder -> RawId -> IO (Id MPSCNNBinaryKernel)
initWithCoder_device mpscnnBinaryKernel aDecoder device =
  sendOwnedMessage mpscnnBinaryKernel initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a MPSCNNKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @primaryImage@ — A valid MPSImage object containing the primary source image.
--
-- @secondaryImage@ — A valid MPSImage object containing the secondary source image.
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. destinationImage may not alias primarySourceImage or secondarySourceImage.
--
-- ObjC selector: @- encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsMPSImage destinationImage) => mpscnnBinaryKernel -> RawId -> primaryImage -> secondaryImage -> destinationImage -> IO ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImage mpscnnBinaryKernel commandBuffer primaryImage secondaryImage destinationImage =
  sendMessage mpscnnBinaryKernel encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage) (toMPSImage destinationImage)

-- | Encode a MPSCNNKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method. Multiple images are processed concurrently.              All images must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @primaryImages@ — An array of MPSImage objects containing the primary source images.
--
-- @secondaryImages@ — An array MPSImage objects containing the secondary source images.
--
-- @destinationImages@ — An array of MPSImage objects to contain the result images.                                    destinationImages may not alias primarySourceImages or secondarySourceImages                                    in any manner.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImages mpscnnBinaryKernel commandBuffer primaryImages secondaryImages destinationImages =
  sendMessage mpscnnBinaryKernel encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector commandBuffer primaryImages secondaryImages destinationImages

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture to hold the result and return it.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property.  See discussion in MPSNeuralNetworkTypes.h.
--
-- @commandBuffer@ — The command buffer
--
-- @primaryImage@ — A MPSImages to use as the primary source images for the filter.
--
-- @secondaryImage@ — A MPSImages to use as the secondary source images for the filter.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:primaryImage:secondaryImage:@
encodeToCommandBuffer_primaryImage_secondaryImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage) => mpscnnBinaryKernel -> RawId -> primaryImage -> secondaryImage -> IO (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImage mpscnnBinaryKernel commandBuffer primaryImage secondaryImage =
  sendMessage mpscnnBinaryKernel encodeToCommandBuffer_primaryImage_secondaryImageSelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage)

-- | Encode a MPSCNNKernel into a command Buffer. Create textures to hold the results and return them.
--
-- In the first iteration on this method, encodeBatchToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property.  See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @primaryImage@ — A MPSImages to use as the primary source images for the filter.
--
-- @secondaryImage@ — A MPSImages to use as the secondary source images for the filter.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImages mpscnnBinaryKernel commandBuffer primaryImage secondaryImage =
  sendMessage mpscnnBinaryKernel encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector commandBuffer primaryImage secondaryImage

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture and state to hold the results and return them.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationState:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @primaryImage@ — A MPSImage to use as the source images for the filter.
--
-- @secondaryImage@ — A MPSImage to use as the source images for the filter.
--
-- @outState@ — The address of location to write the pointer to the result state of the operation
--
-- @isTemporary@ — YES if the outState should be a temporary object
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporary :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsMPSState outState) => mpscnnBinaryKernel -> RawId -> primaryImage -> secondaryImage -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporary mpscnnBinaryKernel commandBuffer primaryImage secondaryImage outState isTemporary =
  sendMessage mpscnnBinaryKernel encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporarySelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage) (toMPSState outState) isTemporary

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture and state to hold the results and return them.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationState:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @primaryImages@ — A MPSImage to use as the source images for the filter.
--
-- @secondaryImages@ — A MPSImage to use as the source images for the filter.
--
-- @outState@ — A new state object is returned here.
--
-- @isTemporary@ — YES if the outState should be a temporary object
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporary :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> RawId -> RawId -> RawId -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporary mpscnnBinaryKernel commandBuffer primaryImages secondaryImages outState isTemporary =
  sendMessage mpscnnBinaryKernel encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporarySelector commandBuffer primaryImages secondaryImages outState isTemporary

-- | Allocate a MPSState (subclass) to hold the results from a -encodeBatchToCommandBuffer... operation
--
-- A graph may need to allocate storage up front before executing.  This may be              necessary to avoid using too much memory and to manage large batches.  The function              should allocate a MPSState object (if any) that will be produced by an -encode call              with the indicated sourceImages and sourceStates inputs. Though the states              can be further adjusted in the ensuing -encode call, the states should              be initialized with all important data and all MTLResource storage allocated.              The data stored in the MTLResource need not be initialized, unless the ensuing              -encode call expects it to be.
--
-- The MTLDevice used by the result is derived from the source image.              The padding policy will be applied to the filter before this is called              to give it the chance to configure any properties like MPSCNNKernel.offset.
--
-- CAUTION: the result state should be made after the kernel properties are                       configured for the -encode call that will write to the state, and                       after -destinationImageDescriptorForSourceImages:sourceStates:                       is called (if it is called). Otherwise, behavior is undefined.                       Please see the description of                       -[MPSCNNKernel resultStateForSourceImage:sourceStates:destinationImage:] for more.
--
-- Default: returns nil
--
-- @primaryImage@ — The MPSImage consumed by the associated -encode call.
--
-- @secondaryImage@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSStates consumed by the associated -encode call,                                  for a batch size of 1.
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              When the batch size is not 1, this function will be called repeatedly unless              -isResultStateReusedAcrossBatch returns YES. If  -isResultStateReusedAcrossBatch              returns YES, then it will be called once per batch and the MPSStateBatch array will              contain MPSStateBatch.length references to the same object.
--
-- ObjC selector: @- resultStateForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBinaryKernel -> primaryImage -> secondaryImage -> sourceStates -> destinationImage -> IO (Id MPSState)
resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImage mpscnnBinaryKernel primaryImage secondaryImage sourceStates destinationImage =
  sendMessage mpscnnBinaryKernel resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector (toMPSImage primaryImage) (toMPSImage secondaryImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- resultStateBatchForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsNSArray sourceStates) => mpscnnBinaryKernel -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImage mpscnnBinaryKernel primaryImage secondaryImage sourceStates destinationImage =
  sendMessage mpscnnBinaryKernel resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector primaryImage secondaryImage (toNSArray sourceStates) destinationImage

-- | Allocate a temporary MPSState (subclass) to hold the results from a -encodeBatchToCommandBuffer... operation
--
-- A graph may need to allocate storage up front before executing.  This may be              necessary to avoid using too much memory and to manage large batches.  The function              should allocate any MPSState objects that will be produced by an -encode call              with the indicated sourceImages and sourceStates inputs. Though the states              can be further adjusted in the ensuing -encode call, the states should              be initialized with all important data and all MTLResource storage allocated.              The data stored in the MTLResource need not be initialized, unless the ensuing              -encode call expects it to be.
--
-- The MTLDevice used by the result is derived from the command buffer.              The padding policy will be applied to the filter before this is called              to give it the chance to configure any properties like MPSCNNKernel.offset.
--
-- CAUTION: the result state should be made after the kernel properties are                       configured for the -encode call that will write to the state, and                       after -destinationImageDescriptorForSourceImages:sourceStates:                       is called (if it is called). Otherwise, behavior is undefined.                       Please see the description of                       -[MPSCNNKernel resultStateForSourceImage:sourceStates:destinationImage] for more.
--
-- Default: returns nil
--
-- @commandBuffer@ — The command buffer to allocate the temporary storage against                                  The state will only be valid on this command buffer.
--
-- @primaryImage@ — The MPSImage consumed by the associated -encode call.
--
-- @secondaryImage@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSStates consumed by the associated -encode call,                                  for a batch size of 1.
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              When the batch size is not 1, this function will be called repeatedly unless              -isResultStateReusedAcrossBatch returns YES. If  -isResultStateReusedAcrossBatch              returns YES, then it will be called once per batch and the MPSStateBatch array will              contain MPSStateBatch.length references to the same object.
--
-- ObjC selector: @- temporaryResultStateForCommandBuffer:primaryImage:secondaryImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBinaryKernel -> RawId -> primaryImage -> secondaryImage -> sourceStates -> destinationImage -> IO (Id MPSState)
temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImage mpscnnBinaryKernel commandBuffer primaryImage secondaryImage sourceStates destinationImage =
  sendMessage mpscnnBinaryKernel temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector commandBuffer (toMPSImage primaryImage) (toMPSImage secondaryImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | @- temporaryResultStateBatchForCommandBuffer:primaryImage:secondaryImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsNSArray sourceStates) => mpscnnBinaryKernel -> RawId -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImage mpscnnBinaryKernel commandBuffer primaryImage secondaryImage sourceStates destinationImage =
  sendMessage mpscnnBinaryKernel temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector commandBuffer primaryImage secondaryImage (toNSArray sourceStates) destinationImage

-- | Returns YES if the same state is used for every operation in a batch
--
-- If NO, then each image in a MPSImageBatch will need a corresponding              (and different) state to go with it. Set to YES to avoid allocating              redundant state in the case when the same state is used all the time.              Default: NO
--
-- ObjC selector: @- isResultStateReusedAcrossBatch@
isResultStateReusedAcrossBatch :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO Bool
isResultStateReusedAcrossBatch mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel isResultStateReusedAcrossBatchSelector

-- | Returns YES if the filter must be run over the entire batch before its              results may be considered complete
--
-- The MPSNNGraph may split batches into sub-batches to save memory. However,              some filters, like batch statistics calculations, need to operate over              the entire batch to calculate a valid result, in this case, the mean and              variance per channel over the set of images.
--
-- In such cases, the accumulated result is commonly stored in a MPSState              containing a MTLBuffer. (MTLTextures may not be able to be read from              and written to in the same filter on some devices.) -isResultStateReusedAcrossBatch              is set to YES, so that the state is allocated once and passed in for each              sub-batch and the filter accumulates its results into it, one sub-batch              at a time. Note that sub-batches may frequently be as small as 1.
--
-- Default: NO
--
-- ObjC selector: @- appendBatchBarrier@
appendBatchBarrier :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO Bool
appendBatchBarrier mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel appendBatchBarrierSelector

-- | Get a suggested destination image descriptor for a source image
--
-- Your application is certainly free to pass in any destinationImage              it likes to encodeToCommandBuffer:sourceImage:destinationImage,              within reason. This is the basic design for iOS 10. This method              is therefore not required.
--
-- However, calculating the MPSImage size and MPSCNNBinaryKernel properties              for each filter can be tedious and complicated work, so this method              is made available to automate the process. The application may              modify the properties of the descriptor before a MPSImage is made from              it, so long as the choice is sensible for the kernel in question.              Please see individual kernel descriptions for restrictions.
--
-- The expected timeline for use is as follows:
--
-- 1) This method is called:                  a) The default MPS padding calculation is applied. It                     uses the MPSNNPaddingMethod of the .padding property to                     provide a consistent addressing scheme over the graph.                     It creates the MPSImageDescriptor and adjusts the .offset                     property of the MPSNNKernel. When using a MPSNNGraph, the                     padding is set using the MPSNNFilterNode as a proxy.
--
-- b) This method may be overridden by MPSCNNBinaryKernel subclass                     to achieve any customization appropriate to the object type.
--
-- c) Source states are then applied in order. These may modify the                     descriptor and may update other object properties. See:                      -destinationImageDescriptorForSourceImages:sourceStates:                       forKernel:suggestedDescriptor:  This is the typical way                      in which MPS may attempt to influence the operation of                      its kernels.
--
-- d) If the .padding property has a custom padding policy method                      of the same name, it is called. Similarly, it may also adjust                      the descriptor and any MPSCNNBinaryKernel properties. This is the                      typical way in which your application may attempt to influence                      the operation of the MPS kernels.
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
-- @sourceImages@ — A array of source images that will be passed into the -encode call                              Since MPSCNNBinaryKernel is a binary kernel, it is an array of length 2.
--
-- @sourceStates@ — An optional array of source states that will be passed into the -encode call
--
-- Returns: an image descriptor allocated on the autorelease pool
--
-- ObjC selector: @- destinationImageDescriptorForSourceImages:sourceStates:@
destinationImageDescriptorForSourceImages_sourceStates :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsNSArray sourceImages, IsNSArray sourceStates) => mpscnnBinaryKernel -> sourceImages -> sourceStates -> IO (Id MPSImageDescriptor)
destinationImageDescriptorForSourceImages_sourceStates mpscnnBinaryKernel sourceImages sourceStates =
  sendMessage mpscnnBinaryKernel destinationImageDescriptorForSourceImages_sourceStatesSelector (toNSArray sourceImages) (toNSArray sourceStates)

-- | The size of extra MPS heap storage allocated while the kernel is encoding
--
-- This is best effort and just describes things that are likely to end up on the MPS heap. It does not              describe all allocation done by the -encode call.  It is intended for use with high water calculations              for MTLHeap sizing. Allocations are typically for temporary storage needed for multipass algorithms.              This interface should not be used to detect multipass algorithms.
--
-- ObjC selector: @- encodingStorageSizeForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsMPSImage primaryImage, IsMPSImage secondaryImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnBinaryKernel -> primaryImage -> secondaryImage -> sourceStates -> destinationImage -> IO CULong
encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImage mpscnnBinaryKernel primaryImage secondaryImage sourceStates destinationImage =
  sendMessage mpscnnBinaryKernel encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector (toMPSImage primaryImage) (toMPSImage secondaryImage) (toNSArray sourceStates) (toMPSImage destinationImage)

-- | The size of extra MPS heap storage allocated while the kernel is encoding a batch
--
-- This is best effort and just describes things that are likely to end up on the MPS heap. It does not              describe all allocation done by the -encode call.  It is intended for use with high water calculations              for MTLHeap sizing. Allocations are typically for temporary storage needed for multipass algorithms.              This interface should not be used to detect multipass algorithms.
--
-- ObjC selector: @- batchEncodingStorageSizeForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImage :: (IsMPSCNNBinaryKernel mpscnnBinaryKernel, IsNSArray sourceStates) => mpscnnBinaryKernel -> RawId -> RawId -> sourceStates -> RawId -> IO CULong
batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImage mpscnnBinaryKernel primaryImage secondaryImage sourceStates destinationImage =
  sendMessage mpscnnBinaryKernel batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector primaryImage secondaryImage (toNSArray sourceStates) destinationImage

-- | destinationFeatureChannelOffset
--
-- The number of channels in the destination MPSImage to skip before writing output.
--
-- This is the starting offset into the destination image in the feature channel dimension              at which destination data is written.              This allows an application to pass a subset of all the channels in MPSImage as output of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel outputs 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as output, we can set destinationFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel outputs N channels,              destination image MUST have at least destinationFeatureChannelOffset + N channels. Using a destination              image with insufficient number of feature channels result in an error.              E.g. if the MPSCNNConvolution outputs 32 channels, and destination has 64 channels, then it is an error to set              destinationFeatureChannelOffset > 32.
--
-- ObjC selector: @- destinationFeatureChannelOffset@
destinationFeatureChannelOffset :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
destinationFeatureChannelOffset mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel destinationFeatureChannelOffsetSelector

-- | destinationFeatureChannelOffset
--
-- The number of channels in the destination MPSImage to skip before writing output.
--
-- This is the starting offset into the destination image in the feature channel dimension              at which destination data is written.              This allows an application to pass a subset of all the channels in MPSImage as output of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel outputs 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as output, we can set destinationFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel outputs N channels,              destination image MUST have at least destinationFeatureChannelOffset + N channels. Using a destination              image with insufficient number of feature channels result in an error.              E.g. if the MPSCNNConvolution outputs 32 channels, and destination has 64 channels, then it is an error to set              destinationFeatureChannelOffset > 32.
--
-- ObjC selector: @- setDestinationFeatureChannelOffset:@
setDestinationFeatureChannelOffset :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setDestinationFeatureChannelOffset mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setDestinationFeatureChannelOffsetSelector value

-- | primarySourceFeatureChannelOffset
--
-- The number of channels in the primary source MPSImage to skip before reading the input.
--
-- This is the starting offset into the primary source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set primarySourceFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least primarySourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              primarySourceFeatureChannelOffset > 32.
--
-- ObjC selector: @- primarySourceFeatureChannelOffset@
primarySourceFeatureChannelOffset :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primarySourceFeatureChannelOffset mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primarySourceFeatureChannelOffsetSelector

-- | primarySourceFeatureChannelOffset
--
-- The number of channels in the primary source MPSImage to skip before reading the input.
--
-- This is the starting offset into the primary source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set primarySourceFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least primarySourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              primarySourceFeatureChannelOffset > 32.
--
-- ObjC selector: @- setPrimarySourceFeatureChannelOffset:@
setPrimarySourceFeatureChannelOffset :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setPrimarySourceFeatureChannelOffset mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setPrimarySourceFeatureChannelOffsetSelector value

-- | secondarySourceFeatureChannelOffset
--
-- The number of channels in the secondary source MPSImage to skip before reading the input.
--
-- This is the starting offset into the secondary source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set secondarySourceFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least primarySourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              primarySourceFeatureChannelOffset > 32.
--
-- ObjC selector: @- secondarySourceFeatureChannelOffset@
secondarySourceFeatureChannelOffset :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondarySourceFeatureChannelOffset mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondarySourceFeatureChannelOffsetSelector

-- | secondarySourceFeatureChannelOffset
--
-- The number of channels in the secondary source MPSImage to skip before reading the input.
--
-- This is the starting offset into the secondary source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set secondarySourceFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least primarySourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              primarySourceFeatureChannelOffset > 32.
--
-- ObjC selector: @- setSecondarySourceFeatureChannelOffset:@
setSecondarySourceFeatureChannelOffset :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setSecondarySourceFeatureChannelOffset mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setSecondarySourceFeatureChannelOffsetSelector value

-- | primarySourceFeatureChannelMaxCount
--
-- The maximum number of channels in the primary source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- ObjC selector: @- primarySourceFeatureChannelMaxCount@
primarySourceFeatureChannelMaxCount :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primarySourceFeatureChannelMaxCount mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primarySourceFeatureChannelMaxCountSelector

-- | primarySourceFeatureChannelMaxCount
--
-- The maximum number of channels in the primary source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- ObjC selector: @- setPrimarySourceFeatureChannelMaxCount:@
setPrimarySourceFeatureChannelMaxCount :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setPrimarySourceFeatureChannelMaxCount mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setPrimarySourceFeatureChannelMaxCountSelector value

-- | secondarySourceFeatureChannelMaxCount
--
-- The maximum number of channels in the secondary source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- ObjC selector: @- secondarySourceFeatureChannelMaxCount@
secondarySourceFeatureChannelMaxCount :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondarySourceFeatureChannelMaxCount mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondarySourceFeatureChannelMaxCountSelector

-- | secondarySourceFeatureChannelMaxCount
--
-- The maximum number of channels in the secondary source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- ObjC selector: @- setSecondarySourceFeatureChannelMaxCount:@
setSecondarySourceFeatureChannelMaxCount :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setSecondarySourceFeatureChannelMaxCount mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setSecondarySourceFeatureChannelMaxCountSelector value

-- | primaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: subsubsection_edgemode
--
-- ObjC selector: @- primaryEdgeMode@
primaryEdgeMode :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO MPSImageEdgeMode
primaryEdgeMode mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryEdgeModeSelector

-- | primaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: subsubsection_edgemode
--
-- ObjC selector: @- setPrimaryEdgeMode:@
setPrimaryEdgeMode :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> MPSImageEdgeMode -> IO ()
setPrimaryEdgeMode mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setPrimaryEdgeModeSelector value

-- | secondaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: subsubsection_edgemode
--
-- ObjC selector: @- secondaryEdgeMode@
secondaryEdgeMode :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO MPSImageEdgeMode
secondaryEdgeMode mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryEdgeModeSelector

-- | secondaryEdgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of the primary source image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: subsubsection_edgemode
--
-- ObjC selector: @- setSecondaryEdgeMode:@
setSecondaryEdgeMode :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> MPSImageEdgeMode -> IO ()
setSecondaryEdgeMode mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setSecondaryEdgeModeSelector value

-- | primaryKernelWidth
--
-- The width of the MPSCNNBinaryKernel filter window
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- ObjC selector: @- primaryKernelWidth@
primaryKernelWidth :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primaryKernelWidth mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryKernelWidthSelector

-- | primaryKernelHeight
--
-- The height of the MPSCNNBinaryKernel filter window
--
-- This is the vertical diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- ObjC selector: @- primaryKernelHeight@
primaryKernelHeight :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primaryKernelHeight mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryKernelHeightSelector

-- | kernelWidth
--
-- The width of the MPSCNNBinaryKernel filter window for the second image source
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNBinaryKernel does not have a filter window, then              1 will be returned.
--
-- ObjC selector: @- secondaryKernelWidth@
secondaryKernelWidth :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondaryKernelWidth mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryKernelWidthSelector

-- | kernelHeight
--
-- The height of the MPSCNNBinaryKernel filter window for the second image source
--
-- This is the vertical diameter of the region read by the filter for each              result pixel. If the MPSCNNBinaryKernel does not have a filter window, then              1 will be returned.
--
-- ObjC selector: @- secondaryKernelHeight@
secondaryKernelHeight :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondaryKernelHeight mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryKernelHeightSelector

-- | primaryStrideInPixelsX
--
-- The downsampling (or upsampling if a backwards filter) factor in the horizontal dimension              for the primary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- primaryStrideInPixelsX@
primaryStrideInPixelsX :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primaryStrideInPixelsX mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryStrideInPixelsXSelector

-- | primaryStrideInPixelsX
--
-- The downsampling (or upsampling if a backwards filter) factor in the horizontal dimension              for the primary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- setPrimaryStrideInPixelsX:@
setPrimaryStrideInPixelsX :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setPrimaryStrideInPixelsX mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setPrimaryStrideInPixelsXSelector value

-- | primaryStrideInPixelsY
--
-- The downsampling (or upsampling if a backwards filter) factor in the vertical dimension              for the primary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- primaryStrideInPixelsY@
primaryStrideInPixelsY :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primaryStrideInPixelsY mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryStrideInPixelsYSelector

-- | primaryStrideInPixelsY
--
-- The downsampling (or upsampling if a backwards filter) factor in the vertical dimension              for the primary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- setPrimaryStrideInPixelsY:@
setPrimaryStrideInPixelsY :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setPrimaryStrideInPixelsY mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setPrimaryStrideInPixelsYSelector value

-- | secondaryStrideInPixelsX
--
-- The downsampling (or upsampling if a backwards filter) factor in the horizontal dimension              for the secondary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- secondaryStrideInPixelsX@
secondaryStrideInPixelsX :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondaryStrideInPixelsX mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryStrideInPixelsXSelector

-- | secondaryStrideInPixelsX
--
-- The downsampling (or upsampling if a backwards filter) factor in the horizontal dimension              for the secondary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsX :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setSecondaryStrideInPixelsX mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setSecondaryStrideInPixelsXSelector value

-- | secondaryStrideInPixelsY
--
-- The downsampling (or upsampling if a backwards filter) factor in the vertical dimension              for the secondary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- secondaryStrideInPixelsY@
secondaryStrideInPixelsY :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondaryStrideInPixelsY mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryStrideInPixelsYSelector

-- | secondaryStrideInPixelsY
--
-- The downsampling (or upsampling if a backwards filter) factor in the vertical dimension              for the secondary source image
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- ObjC selector: @- setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsY :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> CULong -> IO ()
setSecondaryStrideInPixelsY mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setSecondaryStrideInPixelsYSelector value

-- | dilationRateX
--
-- Stride in source coordinates from one kernel tap to the next in the X dimension.
--
-- ObjC selector: @- primaryDilationRateX@
primaryDilationRateX :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primaryDilationRateX mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryDilationRateXSelector

-- | dilationRate
--
-- Stride in source coordinates from one kernel tap to the next in the Y dimension.
--
-- ObjC selector: @- primaryDilationRateY@
primaryDilationRateY :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
primaryDilationRateY mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel primaryDilationRateYSelector

-- | dilationRateX
--
-- Stride in source coordinates from one kernel tap to the next in the X dimension.
--
-- As applied to the secondary source image.
--
-- ObjC selector: @- secondaryDilationRateX@
secondaryDilationRateX :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondaryDilationRateX mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryDilationRateXSelector

-- | dilationRate
--
-- Stride in source coordinates from one kernel tap to the next in the Y dimension.
--
-- As applied to the secondary source image.
--
-- ObjC selector: @- secondaryDilationRateY@
secondaryDilationRateY :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO CULong
secondaryDilationRateY mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel secondaryDilationRateYSelector

-- | isBackwards
--
-- YES if the filter operates backwards.
--
-- This influences how strideInPixelsX/Y should be interpreted.
--
-- ObjC selector: @- isBackwards@
isBackwards :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO Bool
isBackwards mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel isBackwardsSelector

-- | Returns true if the -encode call modifies the state object it accepts.
--
-- ObjC selector: @- isStateModified@
isStateModified :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO Bool
isStateModified mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel isStateModifiedSelector

-- | padding
--
-- The padding method used by the filter
--
-- This influences how strideInPixelsX/Y should be interpreted.              Default:  MPSNNPaddingMethodAlignCentered | MPSNNPaddingMethodAddRemainderToTopLeft | MPSNNPaddingMethodSizeSame              Some object types (e.g. MPSCNNFullyConnected) may override this default with something appropriate to its operation.
--
-- ObjC selector: @- padding@
padding :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO RawId
padding mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel paddingSelector

-- | padding
--
-- The padding method used by the filter
--
-- This influences how strideInPixelsX/Y should be interpreted.              Default:  MPSNNPaddingMethodAlignCentered | MPSNNPaddingMethodAddRemainderToTopLeft | MPSNNPaddingMethodSizeSame              Some object types (e.g. MPSCNNFullyConnected) may override this default with something appropriate to its operation.
--
-- ObjC selector: @- setPadding:@
setPadding :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> RawId -> IO ()
setPadding mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setPaddingSelector value

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- destinationImageAllocator@
destinationImageAllocator :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> IO RawId
destinationImageAllocator mpscnnBinaryKernel =
  sendMessage mpscnnBinaryKernel destinationImageAllocatorSelector

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- setDestinationImageAllocator:@
setDestinationImageAllocator :: IsMPSCNNBinaryKernel mpscnnBinaryKernel => mpscnnBinaryKernel -> RawId -> IO ()
setDestinationImageAllocator mpscnnBinaryKernel value =
  sendMessage mpscnnBinaryKernel setDestinationImageAllocatorSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNBinaryKernel)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNBinaryKernel)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSImage] ()
encodeToCommandBuffer_primaryImage_secondaryImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:@
encodeToCommandBuffer_primaryImage_secondaryImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage] (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImageSelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:@
encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector :: Selector '[RawId, RawId, RawId] RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImagesSelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:"

-- | @Selector@ for @encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporarySelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id MPSState, Bool] (Id MPSImage)
encodeToCommandBuffer_primaryImage_secondaryImage_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:primaryImage:secondaryImage:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporarySelector :: Selector '[RawId, RawId, RawId, RawId, Bool] RawId
encodeBatchToCommandBuffer_primaryImages_secondaryImages_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:primaryImages:secondaryImages:destinationStates:destinationStateIsTemporary:"

-- | @Selector@ for @resultStateForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSState)
resultStateForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector = mkSelector "resultStateForPrimaryImage:secondaryImage:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector :: Selector '[RawId, RawId, Id NSArray, RawId] RawId
resultStateBatchForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForPrimaryImage:secondaryImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:primaryImage:secondaryImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSImage, Id NSArray, Id MPSImage] (Id MPSState)
temporaryResultStateForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:primaryImage:secondaryImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:primaryImage:secondaryImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector :: Selector '[RawId, RawId, RawId, Id NSArray, RawId] RawId
temporaryResultStateBatchForCommandBuffer_primaryImage_secondaryImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:primaryImage:secondaryImage:sourceStates:destinationImage:"

-- | @Selector@ for @isResultStateReusedAcrossBatch@
isResultStateReusedAcrossBatchSelector :: Selector '[] Bool
isResultStateReusedAcrossBatchSelector = mkSelector "isResultStateReusedAcrossBatch"

-- | @Selector@ for @appendBatchBarrier@
appendBatchBarrierSelector :: Selector '[] Bool
appendBatchBarrierSelector = mkSelector "appendBatchBarrier"

-- | @Selector@ for @destinationImageDescriptorForSourceImages:sourceStates:@
destinationImageDescriptorForSourceImages_sourceStatesSelector :: Selector '[Id NSArray, Id NSArray] (Id MPSImageDescriptor)
destinationImageDescriptorForSourceImages_sourceStatesSelector = mkSelector "destinationImageDescriptorForSourceImages:sourceStates:"

-- | @Selector@ for @encodingStorageSizeForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector :: Selector '[Id MPSImage, Id MPSImage, Id NSArray, Id MPSImage] CULong
encodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector = mkSelector "encodingStorageSizeForPrimaryImage:secondaryImage:sourceStates:destinationImage:"

-- | @Selector@ for @batchEncodingStorageSizeForPrimaryImage:secondaryImage:sourceStates:destinationImage:@
batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector :: Selector '[RawId, RawId, Id NSArray, RawId] CULong
batchEncodingStorageSizeForPrimaryImage_secondaryImage_sourceStates_destinationImageSelector = mkSelector "batchEncodingStorageSizeForPrimaryImage:secondaryImage:sourceStates:destinationImage:"

-- | @Selector@ for @destinationFeatureChannelOffset@
destinationFeatureChannelOffsetSelector :: Selector '[] CULong
destinationFeatureChannelOffsetSelector = mkSelector "destinationFeatureChannelOffset"

-- | @Selector@ for @setDestinationFeatureChannelOffset:@
setDestinationFeatureChannelOffsetSelector :: Selector '[CULong] ()
setDestinationFeatureChannelOffsetSelector = mkSelector "setDestinationFeatureChannelOffset:"

-- | @Selector@ for @primarySourceFeatureChannelOffset@
primarySourceFeatureChannelOffsetSelector :: Selector '[] CULong
primarySourceFeatureChannelOffsetSelector = mkSelector "primarySourceFeatureChannelOffset"

-- | @Selector@ for @setPrimarySourceFeatureChannelOffset:@
setPrimarySourceFeatureChannelOffsetSelector :: Selector '[CULong] ()
setPrimarySourceFeatureChannelOffsetSelector = mkSelector "setPrimarySourceFeatureChannelOffset:"

-- | @Selector@ for @secondarySourceFeatureChannelOffset@
secondarySourceFeatureChannelOffsetSelector :: Selector '[] CULong
secondarySourceFeatureChannelOffsetSelector = mkSelector "secondarySourceFeatureChannelOffset"

-- | @Selector@ for @setSecondarySourceFeatureChannelOffset:@
setSecondarySourceFeatureChannelOffsetSelector :: Selector '[CULong] ()
setSecondarySourceFeatureChannelOffsetSelector = mkSelector "setSecondarySourceFeatureChannelOffset:"

-- | @Selector@ for @primarySourceFeatureChannelMaxCount@
primarySourceFeatureChannelMaxCountSelector :: Selector '[] CULong
primarySourceFeatureChannelMaxCountSelector = mkSelector "primarySourceFeatureChannelMaxCount"

-- | @Selector@ for @setPrimarySourceFeatureChannelMaxCount:@
setPrimarySourceFeatureChannelMaxCountSelector :: Selector '[CULong] ()
setPrimarySourceFeatureChannelMaxCountSelector = mkSelector "setPrimarySourceFeatureChannelMaxCount:"

-- | @Selector@ for @secondarySourceFeatureChannelMaxCount@
secondarySourceFeatureChannelMaxCountSelector :: Selector '[] CULong
secondarySourceFeatureChannelMaxCountSelector = mkSelector "secondarySourceFeatureChannelMaxCount"

-- | @Selector@ for @setSecondarySourceFeatureChannelMaxCount:@
setSecondarySourceFeatureChannelMaxCountSelector :: Selector '[CULong] ()
setSecondarySourceFeatureChannelMaxCountSelector = mkSelector "setSecondarySourceFeatureChannelMaxCount:"

-- | @Selector@ for @primaryEdgeMode@
primaryEdgeModeSelector :: Selector '[] MPSImageEdgeMode
primaryEdgeModeSelector = mkSelector "primaryEdgeMode"

-- | @Selector@ for @setPrimaryEdgeMode:@
setPrimaryEdgeModeSelector :: Selector '[MPSImageEdgeMode] ()
setPrimaryEdgeModeSelector = mkSelector "setPrimaryEdgeMode:"

-- | @Selector@ for @secondaryEdgeMode@
secondaryEdgeModeSelector :: Selector '[] MPSImageEdgeMode
secondaryEdgeModeSelector = mkSelector "secondaryEdgeMode"

-- | @Selector@ for @setSecondaryEdgeMode:@
setSecondaryEdgeModeSelector :: Selector '[MPSImageEdgeMode] ()
setSecondaryEdgeModeSelector = mkSelector "setSecondaryEdgeMode:"

-- | @Selector@ for @primaryKernelWidth@
primaryKernelWidthSelector :: Selector '[] CULong
primaryKernelWidthSelector = mkSelector "primaryKernelWidth"

-- | @Selector@ for @primaryKernelHeight@
primaryKernelHeightSelector :: Selector '[] CULong
primaryKernelHeightSelector = mkSelector "primaryKernelHeight"

-- | @Selector@ for @secondaryKernelWidth@
secondaryKernelWidthSelector :: Selector '[] CULong
secondaryKernelWidthSelector = mkSelector "secondaryKernelWidth"

-- | @Selector@ for @secondaryKernelHeight@
secondaryKernelHeightSelector :: Selector '[] CULong
secondaryKernelHeightSelector = mkSelector "secondaryKernelHeight"

-- | @Selector@ for @primaryStrideInPixelsX@
primaryStrideInPixelsXSelector :: Selector '[] CULong
primaryStrideInPixelsXSelector = mkSelector "primaryStrideInPixelsX"

-- | @Selector@ for @setPrimaryStrideInPixelsX:@
setPrimaryStrideInPixelsXSelector :: Selector '[CULong] ()
setPrimaryStrideInPixelsXSelector = mkSelector "setPrimaryStrideInPixelsX:"

-- | @Selector@ for @primaryStrideInPixelsY@
primaryStrideInPixelsYSelector :: Selector '[] CULong
primaryStrideInPixelsYSelector = mkSelector "primaryStrideInPixelsY"

-- | @Selector@ for @setPrimaryStrideInPixelsY:@
setPrimaryStrideInPixelsYSelector :: Selector '[CULong] ()
setPrimaryStrideInPixelsYSelector = mkSelector "setPrimaryStrideInPixelsY:"

-- | @Selector@ for @secondaryStrideInPixelsX@
secondaryStrideInPixelsXSelector :: Selector '[] CULong
secondaryStrideInPixelsXSelector = mkSelector "secondaryStrideInPixelsX"

-- | @Selector@ for @setSecondaryStrideInPixelsX:@
setSecondaryStrideInPixelsXSelector :: Selector '[CULong] ()
setSecondaryStrideInPixelsXSelector = mkSelector "setSecondaryStrideInPixelsX:"

-- | @Selector@ for @secondaryStrideInPixelsY@
secondaryStrideInPixelsYSelector :: Selector '[] CULong
secondaryStrideInPixelsYSelector = mkSelector "secondaryStrideInPixelsY"

-- | @Selector@ for @setSecondaryStrideInPixelsY:@
setSecondaryStrideInPixelsYSelector :: Selector '[CULong] ()
setSecondaryStrideInPixelsYSelector = mkSelector "setSecondaryStrideInPixelsY:"

-- | @Selector@ for @primaryDilationRateX@
primaryDilationRateXSelector :: Selector '[] CULong
primaryDilationRateXSelector = mkSelector "primaryDilationRateX"

-- | @Selector@ for @primaryDilationRateY@
primaryDilationRateYSelector :: Selector '[] CULong
primaryDilationRateYSelector = mkSelector "primaryDilationRateY"

-- | @Selector@ for @secondaryDilationRateX@
secondaryDilationRateXSelector :: Selector '[] CULong
secondaryDilationRateXSelector = mkSelector "secondaryDilationRateX"

-- | @Selector@ for @secondaryDilationRateY@
secondaryDilationRateYSelector :: Selector '[] CULong
secondaryDilationRateYSelector = mkSelector "secondaryDilationRateY"

-- | @Selector@ for @isBackwards@
isBackwardsSelector :: Selector '[] Bool
isBackwardsSelector = mkSelector "isBackwards"

-- | @Selector@ for @isStateModified@
isStateModifiedSelector :: Selector '[] Bool
isStateModifiedSelector = mkSelector "isStateModified"

-- | @Selector@ for @padding@
paddingSelector :: Selector '[] RawId
paddingSelector = mkSelector "padding"

-- | @Selector@ for @setPadding:@
setPaddingSelector :: Selector '[RawId] ()
setPaddingSelector = mkSelector "setPadding:"

-- | @Selector@ for @destinationImageAllocator@
destinationImageAllocatorSelector :: Selector '[] RawId
destinationImageAllocatorSelector = mkSelector "destinationImageAllocator"

-- | @Selector@ for @setDestinationImageAllocator:@
setDestinationImageAllocatorSelector :: Selector '[RawId] ()
setDestinationImageAllocatorSelector = mkSelector "setDestinationImageAllocator:"

