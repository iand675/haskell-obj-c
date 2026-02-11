{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNKernel
--
-- This depends on Metal.framework
--
-- Describes a convolution neural network kernel.
--
-- A MPSCNNKernel consumes one MPSImage and produces one MPSImage.
--
-- The region overwritten in the destination MPSImage is described              by the clipRect.  The top left corner of the region consumed (ignoring              adjustments for filter size -- e.g. convolution filter size) is given              by the offset. The size of the region consumed is a function of the              clipRect size and any subsampling caused by pixel strides at work,              e.g. MPSCNNPooling.strideInPixelsX/Y.  Where the offset + clipRect              would cause a {x,y} pixel address not in the image to be read, the              edgeMode is used to determine what value to read there.
--
-- The Z/depth component of the offset, clipRect.origin and clipRect.size              indexes which images to use. If the MPSImage contains only a single image              then these should be offset.z = 0, clipRect.origin.z = 0              and clipRect.size.depth = 1. If the MPSImage contains multiple images,              clipRect.size.depth refers to number of images to process. Both source              and destination MPSImages must have at least this many images. offset.z              refers to starting source image index. Thus offset.z + clipRect.size.depth must              be <= source.numberOfImages. Similarly, clipRect.origin.z refers to starting              image index in destination. So clipRect.origin.z + clipRect.size.depth must be              <= destination.numberOfImage.
--
-- destinationFeatureChannelOffset property can be used to control where the MPSKernel will              start writing in feature channel dimension. For example, if the destination image has              64 channels, and MPSKernel outputs 32 channels, by default channels 0-31 of destination              will be populated by MPSKernel. But if we want this MPSKernel to populate channel 32-63              of the destination, we can set destinationFeatureChannelOffset = 32.              A good example of this is concat (concatenation) operation in Tensor Flow. Suppose              we have a src = w x h x Ni which goes through CNNConvolution_0 which produces              output O0 = w x h x N0 and CNNConvolution_1 which produces output O1 = w x h x N1 followed              by concatenation which produces O = w x h x (N0 + N1). We can achieve this by creating              an MPSImage with dimensions O = w x h x (N0 + N1) and using this as destination of              both convolutions as follows                  CNNConvolution0: destinationFeatureChannelOffset = 0, this will output N0 channels starting at                                   channel 0 of destination thus populating [0,N0-1] channels.                  CNNConvolution1: destinationFeatureChannelOffset = N0, this will output N1 channels starting at                                   channel N0 of destination thus populating [N0,N0+N1-1] channels.
--
-- A MPSCNNKernel can be saved to disk / network using NSCoders such as NSKeyedArchiver.               When decoding, the system default MTLDevice will be chosen unless the NSCoder adopts               the <MPSDeviceProvider> protocol.  To accomplish this you will likely need to subclass your              unarchiver to add this method.
--
-- Generated bindings for @MPSCNNKernel@.
module ObjC.MetalPerformanceShaders.MPSCNNKernel
  ( MPSCNNKernel
  , IsMPSCNNKernel(..)
  , initWithDevice
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_destinationImage
  , encodeToCommandBuffer_sourceImage_destinationState_destinationImage
  , encodeBatchToCommandBuffer_sourceImages_destinationImages
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImages
  , encodeToCommandBuffer_sourceImage
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary
  , encodeBatchToCommandBuffer_sourceImages
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary
  , resultStateForSourceImage_sourceStates_destinationImage
  , resultStateBatchForSourceImage_sourceStates_destinationImage
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage
  , isResultStateReusedAcrossBatch
  , appendBatchBarrier
  , destinationImageDescriptorForSourceImages_sourceStates
  , encodingStorageSizeForSourceImage_sourceStates_destinationImage
  , batchEncodingStorageSizeForSourceImage_sourceStates_destinationImage
  , destinationFeatureChannelOffset
  , setDestinationFeatureChannelOffset
  , sourceFeatureChannelOffset
  , setSourceFeatureChannelOffset
  , sourceFeatureChannelMaxCount
  , setSourceFeatureChannelMaxCount
  , edgeMode
  , setEdgeMode
  , kernelWidth
  , kernelHeight
  , strideInPixelsX
  , strideInPixelsY
  , dilationRateX
  , dilationRateY
  , isBackwards
  , isStateModified
  , padding
  , setPadding
  , destinationImageAllocator
  , setDestinationImageAllocator
  , initWithDeviceSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector
  , encodeToCommandBuffer_sourceImageSelector
  , encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector
  , encodeBatchToCommandBuffer_sourceImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector
  , resultStateForSourceImage_sourceStates_destinationImageSelector
  , resultStateBatchForSourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector
  , isResultStateReusedAcrossBatchSelector
  , appendBatchBarrierSelector
  , destinationImageDescriptorForSourceImages_sourceStatesSelector
  , encodingStorageSizeForSourceImage_sourceStates_destinationImageSelector
  , batchEncodingStorageSizeForSourceImage_sourceStates_destinationImageSelector
  , destinationFeatureChannelOffsetSelector
  , setDestinationFeatureChannelOffsetSelector
  , sourceFeatureChannelOffsetSelector
  , setSourceFeatureChannelOffsetSelector
  , sourceFeatureChannelMaxCountSelector
  , setSourceFeatureChannelMaxCountSelector
  , edgeModeSelector
  , setEdgeModeSelector
  , kernelWidthSelector
  , kernelHeightSelector
  , strideInPixelsXSelector
  , strideInPixelsYSelector
  , dilationRateXSelector
  , dilationRateYSelector
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
-- Returns: A pointer to the newly initialized object. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- initWithDevice:@
initWithDevice :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> IO (Id MPSCNNKernel)
initWithDevice mpscnnKernel  device =
    sendMsg mpscnnKernel (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithCoder_device :: (IsMPSCNNKernel mpscnnKernel, IsNSCoder aDecoder) => mpscnnKernel -> aDecoder -> RawId -> IO (Id MPSCNNKernel)
initWithCoder_device mpscnnKernel  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnKernel (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSCNNKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImage@ — A valid MPSImage object containing the source image.
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. destinationImage may not alias sourceImage.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage, IsMPSImage destinationImage) => mpscnnKernel -> RawId -> sourceImage -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationImage mpscnnKernel  commandBuffer sourceImage destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnKernel (mkSelector "encodeToCommandBuffer:sourceImage:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | Encode a MPSCNNKernel with a destination state into a command Buffer.
--
-- This is typically used during training. The state is commonly a MPSNNGradientState.              Please see -resultStateForSourceImages:SourceStates: and batch+temporary variants.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImage@ — A valid MPSImage object containing the source image.
--
-- @destinationState@ — A state to be overwritten by additional state information.
--
-- @destinationImage@ — A valid MPSImage to be overwritten by result image. destinationImage may not alias sourceImage.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:destinationState:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationState_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage, IsMPSState destinationState, IsMPSImage destinationImage) => mpscnnKernel -> RawId -> sourceImage -> destinationState -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_destinationState_destinationImage mpscnnKernel  commandBuffer sourceImage destinationState destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr destinationState $ \raw_destinationState ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnKernel (mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_destinationState :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | Encode a MPSCNNKernel into a command Buffer.  The operation shall proceed out-of-place.
--
-- This is the older style of encode which reads the offset, doesn't change it,              and ignores the padding method.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImages@ — A valid MPSImage object containing the source images.
--
-- @destinationImages@ — A valid MPSImage to be overwritten by result images.                                  destinationImages may not alias sourceImages, even at different                                  indices.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImages :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationImages mpscnnKernel  commandBuffer sourceImages destinationImages =
    sendMsg mpscnnKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId destinationImages) :: Ptr ())]

-- | Encode a MPSCNNKernel with a destination state into a command Buffer.
--
-- This is typically used during training. The state is commonly a MPSNNGradientState.              Please see -resultStateForSourceImages:SourceStates:destinationImage and batch+temporary variants.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImages@ — A valid MPSImage object containing the source images.
--
-- @destinationStates@ — A list of states to be overwritten by results
--
-- @destinationImages@ — A valid MPSImage to be overwritten by result images.                                  destinationImages may not alias sourceImages, even at different                                  indices.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImages :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImages mpscnnKernel  commandBuffer sourceImages destinationStates destinationImages =
    sendMsg mpscnnKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId destinationStates) :: Ptr ()), argPtr (castPtr (unRawId destinationImages) :: Ptr ())]

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture to hold the result and return it.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                   destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImage@ — A MPSImage to use as the source images for the filter.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:@
encodeToCommandBuffer_sourceImage :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage) => mpscnnKernel -> RawId -> sourceImage -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage mpscnnKernel  commandBuffer sourceImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
      sendMsg mpscnnKernel (mkSelector "encodeToCommandBuffer:sourceImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ())] >>= retainedObject . castPtr

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture and state to hold the results and return them.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationState:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImage@ — A MPSImage to use as the source images for the filter.
--
-- @outState@ — A new state object is returned here.
--
-- Returns: A MPSImage or MPSTemporaryImage allocated per the destinationImageAllocator containing the output of the graph.                  The offset property will be adjusted to reflect the offset used during the encode.                  The returned image will be automatically released when the command buffer completes. If you want to                  keep it around for longer, retain the image. (ARC will do this for you if you use it later.)
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage, IsMPSState outState) => mpscnnKernel -> RawId -> sourceImage -> outState -> Bool -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporary mpscnnKernel  commandBuffer sourceImage outState isTemporary =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr outState $ \raw_outState ->
        sendMsg mpscnnKernel (mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_outState :: Ptr ()), argCULong (if isTemporary then 1 else 0)] >>= retainedObject . castPtr

-- | Encode a MPSCNNKernel into a command Buffer. Create a texture to hold the result and return it.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImages@ — A MPSImages to use as the source images for the filter.
--
-- Returns: An array of MPSImages or MPSTemporaryImages allocated per the destinationImageAllocator                  containing the output of the graph. The offset property will be adjusted to reflect the                  offset used during the encode. The returned images will be automatically released when                  the command buffer completes. If you want to keep them around for longer, retain the images.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImages :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages mpscnnKernel  commandBuffer sourceImages =
    fmap (RawId . castPtr) $ sendMsg mpscnnKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ())]

-- | Encode a MPSCNNKernel into a command Buffer. Create a MPSImageBatch and MPSStateBatch to hold the results and return them.
--
-- In the first iteration on this method, encodeToCommandBuffer:sourceImage:destinationImage:                  some work was left for the developer to do in the form of correctly setting the offset property                  and sizing the result buffer. With the introduction of the padding policy (see padding property)                  the filter can do this work itself. If you would like to have some input into what sort of MPSImage                  (e.g. temporary vs. regular) or what size it is or where it is allocated, you may set the                  destinationImageAllocator to allocate the image yourself.
--
-- This method uses the MPSNNPadding padding property to figure out how to size                  the result image and to set the offset property. See discussion in MPSNeuralNetworkTypes.h.                  All images in a batch must have MPSImage.numberOfImages = 1.
--
-- Usage:
--
-- MPSStateBatch * outStates = nil;    // autoreleased
-- MPSImageBatch * result = [k encodeBatchToCommandBuffer: cmdBuf
-- sourceImages: sourceImages
-- destinationStates: &outStates ];
--
-- @commandBuffer@ — The command buffer
--
-- @sourceImages@ — A MPSImages to use as the source images for the filter.
--
-- @outStates@ — A pointer to storage to hold a MPSStateBatch* where output states are returned
--
-- Returns: An array of MPSImages or MPSTemporaryImages allocated per the destinationImageAllocator                  containing the output of the graph. The offset property will be adjusted to reflect the                  offset used during the encode. The returned images will be automatically released when                  the command buffer completes. If you want to keep them around for longer, retain the images.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> RawId -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporary mpscnnKernel  commandBuffer sourceImages outStates isTemporary =
    fmap (RawId . castPtr) $ sendMsg mpscnnKernel (mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImages) :: Ptr ()), argPtr (castPtr (unRawId outStates) :: Ptr ()), argCULong (if isTemporary then 1 else 0)]

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
-- @sourceImage@ — The MPSImage consumed by the associated -encode call.
--
-- @sourceStates@ — The list of MPSStates consumed by the associated -encode call,                                  for a batch size of 1.
--
-- @destinationImage@ — The destination image for the encode call
--
-- Returns: The list of states produced by the -encode call for batch size of 1.              When the batch size is not 1, this function will be called repeatedly unless              -isResultStateReusedAcrossBatch returns YES. If  -isResultStateReusedAcrossBatch              returns YES, then it will be called once per batch and the MPSStateBatch array will              contain MPSStateBatch.length references to the same object.
--
-- ObjC selector: @- resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnKernel -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSState)
resultStateForSourceImage_sourceStates_destinationImage mpscnnKernel  sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnKernel (mkSelector "resultStateForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsNSArray sourceStates) => mpscnnKernel -> RawId -> sourceStates -> RawId -> IO RawId
resultStateBatchForSourceImage_sourceStates_destinationImage mpscnnKernel  sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      fmap (RawId . castPtr) $ sendMsg mpscnnKernel (mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

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
-- ObjC selector: @- temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnKernel -> RawId -> sourceImage -> sourceStates -> destinationImage -> IO (Id MPSState)
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnKernel  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnKernel (mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())] >>= retainedObject . castPtr

-- | @- temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsNSArray sourceStates) => mpscnnKernel -> RawId -> RawId -> sourceStates -> RawId -> IO RawId
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImage mpscnnKernel  commandBuffer sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      fmap (RawId . castPtr) $ sendMsg mpscnnKernel (mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | Returns YES if the same state is used for every operation in a batch
--
-- If NO, then each image in a MPSImageBatch will need a corresponding              (and different) state to go with it. Set to YES to avoid allocating              redundant state in the case when the same state is used all the time.              Default: NO
--
-- ObjC selector: @- isResultStateReusedAcrossBatch@
isResultStateReusedAcrossBatch :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO Bool
isResultStateReusedAcrossBatch mpscnnKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnKernel (mkSelector "isResultStateReusedAcrossBatch") retCULong []

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
appendBatchBarrier :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO Bool
appendBatchBarrier mpscnnKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnKernel (mkSelector "appendBatchBarrier") retCULong []

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
destinationImageDescriptorForSourceImages_sourceStates :: (IsMPSCNNKernel mpscnnKernel, IsNSArray sourceImages, IsNSArray sourceStates) => mpscnnKernel -> sourceImages -> sourceStates -> IO (Id MPSImageDescriptor)
destinationImageDescriptorForSourceImages_sourceStates mpscnnKernel  sourceImages sourceStates =
  withObjCPtr sourceImages $ \raw_sourceImages ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
        sendMsg mpscnnKernel (mkSelector "destinationImageDescriptorForSourceImages:sourceStates:") (retPtr retVoid) [argPtr (castPtr raw_sourceImages :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ())] >>= retainedObject . castPtr

-- | The size of extra MPS heap storage allocated while the kernel is encoding
--
-- This is best effort and just describes things that are likely to end up on the MPS heap. It does not              describe all allocation done by the -encode call.  It is intended for use with high water calculations              for MTLHeap sizing. Allocations are typically for temporary storage needed for multipass algorithms.              This interface should not be used to detect multipass algorithms.
--
-- ObjC selector: @- encodingStorageSizeForSourceImage:sourceStates:destinationImage:@
encodingStorageSizeForSourceImage_sourceStates_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsMPSImage sourceImage, IsNSArray sourceStates, IsMPSImage destinationImage) => mpscnnKernel -> sourceImage -> sourceStates -> destinationImage -> IO CULong
encodingStorageSizeForSourceImage_sourceStates_destinationImage mpscnnKernel  sourceImage sourceStates destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr sourceStates $ \raw_sourceStates ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnKernel (mkSelector "encodingStorageSizeForSourceImage:sourceStates:destinationImage:") retCULong [argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

-- | The size of extra MPS heap storage allocated while the kernel is encoding a batch
--
-- This is best effort and just describes things that are likely to end up on the MPS heap. It does not              describe all allocation done by the -encode call.  It is intended for use with high water calculations              for MTLHeap sizing. Allocations are typically for temporary storage needed for multipass algorithms.              This interface should not be used to detect multipass algorithms.
--
-- ObjC selector: @- batchEncodingStorageSizeForSourceImage:sourceStates:destinationImage:@
batchEncodingStorageSizeForSourceImage_sourceStates_destinationImage :: (IsMPSCNNKernel mpscnnKernel, IsNSArray sourceStates) => mpscnnKernel -> RawId -> sourceStates -> RawId -> IO CULong
batchEncodingStorageSizeForSourceImage_sourceStates_destinationImage mpscnnKernel  sourceImage sourceStates destinationImage =
  withObjCPtr sourceStates $ \raw_sourceStates ->
      sendMsg mpscnnKernel (mkSelector "batchEncodingStorageSizeForSourceImage:sourceStates:destinationImage:") retCULong [argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr raw_sourceStates :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | destinationFeatureChannelOffset
--
-- The number of channels in the destination MPSImage to skip before writing output.
--
-- This is the starting offset into the destination image in the feature channel dimension              at which destination data is written.              This allows an application to pass a subset of all the channels in MPSImage as output of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel outputs 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as output, we can set destinationFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel outputs N channels,              the destination image MUST have at least destinationFeatureChannelOffset + N channels. Using a destination              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution outputs 32 channels, and the destination has 64 channels, then it is an error to set              destinationFeatureChannelOffset > 32.
--
-- ObjC selector: @- destinationFeatureChannelOffset@
destinationFeatureChannelOffset :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
destinationFeatureChannelOffset mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "destinationFeatureChannelOffset") retCULong []

-- | destinationFeatureChannelOffset
--
-- The number of channels in the destination MPSImage to skip before writing output.
--
-- This is the starting offset into the destination image in the feature channel dimension              at which destination data is written.              This allows an application to pass a subset of all the channels in MPSImage as output of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel outputs 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as output, we can set destinationFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel outputs N channels,              the destination image MUST have at least destinationFeatureChannelOffset + N channels. Using a destination              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution outputs 32 channels, and the destination has 64 channels, then it is an error to set              destinationFeatureChannelOffset > 32.
--
-- ObjC selector: @- setDestinationFeatureChannelOffset:@
setDestinationFeatureChannelOffset :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> CULong -> IO ()
setDestinationFeatureChannelOffset mpscnnKernel  value =
    sendMsg mpscnnKernel (mkSelector "setDestinationFeatureChannelOffset:") retVoid [argCULong value]

-- | sourceFeatureChannelOffset
--
-- The number of channels in the source MPSImage to skip before reading the input.
--
-- This is the starting offset into the source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set sourceFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least sourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              sourceFeatureChannelOffset > 32.
--
-- ObjC selector: @- sourceFeatureChannelOffset@
sourceFeatureChannelOffset :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
sourceFeatureChannelOffset mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "sourceFeatureChannelOffset") retCULong []

-- | sourceFeatureChannelOffset
--
-- The number of channels in the source MPSImage to skip before reading the input.
--
-- This is the starting offset into the source image in the feature channel dimension              at which source data is read. Unit: feature channels              This allows an application to read a subset of all the channels in MPSImage as input of MPSKernel.              E.g. Suppose MPSImage has 24 channels and a MPSKernel needs to read 8 channels. If              we want channels 8 to 15 of this MPSImage to be used as input, we can set sourceFeatureChannelOffset = 8.              Note that this offset applies independently to each image when the MPSImage              is a container for multiple images and the MPSCNNKernel is processing multiple images (clipRect.size.depth > 1).              The default value is 0 and any value specifed shall be a multiple of 4. If MPSKernel inputs N channels,              the source image MUST have at least sourceFeatureChannelOffset + N channels. Using a source              image with insufficient number of feature channels will result in an error.              E.g. if the MPSCNNConvolution inputs 32 channels, and the source has 64 channels, then it is an error to set              sourceFeatureChannelOffset > 32.
--
-- ObjC selector: @- setSourceFeatureChannelOffset:@
setSourceFeatureChannelOffset :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> CULong -> IO ()
setSourceFeatureChannelOffset mpscnnKernel  value =
    sendMsg mpscnnKernel (mkSelector "setSourceFeatureChannelOffset:") retVoid [argCULong value]

-- | sourceFeatureChannelMaxCount
--
-- The maximum number of channels in the source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- ObjC selector: @- sourceFeatureChannelMaxCount@
sourceFeatureChannelMaxCount :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
sourceFeatureChannelMaxCount mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "sourceFeatureChannelMaxCount") retCULong []

-- | sourceFeatureChannelMaxCount
--
-- The maximum number of channels in the source MPSImage to use
--
-- Most filters can insert a slice operation into the filter for free.              Use this to limit the size of the feature channel slice taken from              the input image. If the value is too large, it is truncated to be              the remaining size in the image after the sourceFeatureChannelOffset              is taken into account.  Default: ULONG_MAX
--
-- ObjC selector: @- setSourceFeatureChannelMaxCount:@
setSourceFeatureChannelMaxCount :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> CULong -> IO ()
setSourceFeatureChannelMaxCount mpscnnKernel  value =
    sendMsg mpscnnKernel (mkSelector "setSourceFeatureChannelMaxCount:") retVoid [argCULong value]

-- | edgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of an image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: MetalPerformanceShaders.h subsubsection_edgemode              Note: For MPSCNNPoolingAverage specifying edge mode MPSImageEdgeModeClamp                      is interpreted as a "shrink-to-edge" operation, which shrinks the effective                      filtering window to remain within the source image borders.
--
-- ObjC selector: @- edgeMode@
edgeMode :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO MPSImageEdgeMode
edgeMode mpscnnKernel  =
    fmap (coerce :: CULong -> MPSImageEdgeMode) $ sendMsg mpscnnKernel (mkSelector "edgeMode") retCULong []

-- | edgeMode
--
-- The MPSImageEdgeMode to use when texture reads stray off the edge of an image
--
-- Most MPSKernel objects can read off the edge of the source image. This can happen              because of a negative offset property, because the offset + clipRect.size is larger              than the source image or because the filter looks at neighboring pixels, such as a              Convolution filter.   Default:  MPSImageEdgeModeZero.
--
-- See Also: MetalPerformanceShaders.h subsubsection_edgemode              Note: For MPSCNNPoolingAverage specifying edge mode MPSImageEdgeModeClamp                      is interpreted as a "shrink-to-edge" operation, which shrinks the effective                      filtering window to remain within the source image borders.
--
-- ObjC selector: @- setEdgeMode:@
setEdgeMode :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> MPSImageEdgeMode -> IO ()
setEdgeMode mpscnnKernel  value =
    sendMsg mpscnnKernel (mkSelector "setEdgeMode:") retVoid [argCULong (coerce value)]

-- | kernelWidth
--
-- The width of the MPSCNNKernel filter window
--
-- This is the horizontal diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then              1 will be returned.
--
-- Warning: This property was lowered to this class in ios/tvos 11                       The property may not be available on iOS/tvOS 10 for                       all subclasses of MPSCNNKernel
--
-- ObjC selector: @- kernelWidth@
kernelWidth :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
kernelWidth mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "kernelWidth") retCULong []

-- | kernelHeight
--
-- The height of the MPSCNNKernel filter window
--
-- This is the vertical diameter of the region read by the filter for each              result pixel. If the MPSCNNKernel does not have a filter window, then               1 will be returned.
--
-- Warning: This property was lowered to this class in ios/tvos 11                       The property may not be available on iOS/tvOS 10 for                       all subclasses of MPSCNNKernel
--
-- ObjC selector: @- kernelHeight@
kernelHeight :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
kernelHeight mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "kernelHeight") retCULong []

-- | strideInPixelsX
--
-- The downsampling (or upsampling if a backwards filter) factor in the horizontal dimension
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- Warning: This property was lowered to this class in ios/tvos 11                       The property may not be available on iOS/tvOS 10 for                       all subclasses of MPSCNNKernel
--
-- ObjC selector: @- strideInPixelsX@
strideInPixelsX :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
strideInPixelsX mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "strideInPixelsX") retCULong []

-- | strideInPixelsY
--
-- The downsampling (or upsampling if a backwards filter) factor in the vertical dimension
--
-- If the filter does not do up or downsampling, 1 is returned.
--
-- Warning: This property was lowered to this class in ios/tvos 11                       The property may not be available on iOS/tvOS 10 for                       all subclasses of MPSCNNKernel
--
-- ObjC selector: @- strideInPixelsY@
strideInPixelsY :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
strideInPixelsY mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "strideInPixelsY") retCULong []

-- | dilationRateX
--
-- Stride in source coordinates from one kernel tap to the next in the X dimension.
--
-- ObjC selector: @- dilationRateX@
dilationRateX :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
dilationRateX mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "dilationRateX") retCULong []

-- | dilationRate
--
-- Stride in source coordinates from one kernel tap to the next in the Y dimension.
--
-- ObjC selector: @- dilationRateY@
dilationRateY :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO CULong
dilationRateY mpscnnKernel  =
    sendMsg mpscnnKernel (mkSelector "dilationRateY") retCULong []

-- | isBackwards
--
-- YES if the filter operates backwards.
--
-- This influences how strideInPixelsX/Y should be interpreted.               Most filters either have stride 1 or are reducing, meaning that              the result image is smaller than the original by roughly a factor              of the stride.  A few "backward" filters (e.g convolution transpose) are intended              to "undo" the effects of an earlier forward filter, and so               enlarge the image. The stride is in the destination coordinate frame              rather than the source coordinate frame.
--
-- ObjC selector: @- isBackwards@
isBackwards :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO Bool
isBackwards mpscnnKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnKernel (mkSelector "isBackwards") retCULong []

-- | Returns true if the -encode call modifies the state object it accepts.
--
-- ObjC selector: @- isStateModified@
isStateModified :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO Bool
isStateModified mpscnnKernel  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnKernel (mkSelector "isStateModified") retCULong []

-- | padding
--
-- The padding method used by the filter
--
-- This influences how the destination image is sized and how              the offset into the source image is set.  It is used by the              -encode methods that return a MPSImage from the left hand side.
--
-- ObjC selector: @- padding@
padding :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO RawId
padding mpscnnKernel  =
    fmap (RawId . castPtr) $ sendMsg mpscnnKernel (mkSelector "padding") (retPtr retVoid) []

-- | padding
--
-- The padding method used by the filter
--
-- This influences how the destination image is sized and how              the offset into the source image is set.  It is used by the              -encode methods that return a MPSImage from the left hand side.
--
-- ObjC selector: @- setPadding:@
setPadding :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> IO ()
setPadding mpscnnKernel  value =
    sendMsg mpscnnKernel (mkSelector "setPadding:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- destinationImageAllocator@
destinationImageAllocator :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> IO RawId
destinationImageAllocator mpscnnKernel  =
    fmap (RawId . castPtr) $ sendMsg mpscnnKernel (mkSelector "destinationImageAllocator") (retPtr retVoid) []

-- | Method to allocate the result image for -encodeToCommandBuffer:sourceImage:
--
-- Default: MPSTemporaryImage.defaultAllocator
--
-- ObjC selector: @- setDestinationImageAllocator:@
setDestinationImageAllocator :: IsMPSCNNKernel mpscnnKernel => mpscnnKernel -> RawId -> IO ()
setDestinationImageAllocator mpscnnKernel  value =
    sendMsg mpscnnKernel (mkSelector "setDestinationImageAllocator:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationImage:@
encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_destinationState_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationImage:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationImages:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:@
encodeToCommandBuffer_sourceImageSelector :: Selector
encodeToCommandBuffer_sourceImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:@
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector :: Selector
encodeToCommandBuffer_sourceImage_destinationState_destinationStateIsTemporarySelector = mkSelector "encodeToCommandBuffer:sourceImage:destinationState:destinationStateIsTemporary:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:@
encodeBatchToCommandBuffer_sourceImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector :: Selector
encodeBatchToCommandBuffer_sourceImages_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:destinationStates:destinationStateIsTemporary:"

-- | @Selector@ for @resultStateForSourceImage:sourceStates:destinationImage:@
resultStateForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @resultStateBatchForSourceImage:sourceStates:destinationImage:@
resultStateBatchForSourceImage_sourceStates_destinationImageSelector :: Selector
resultStateBatchForSourceImage_sourceStates_destinationImageSelector = mkSelector "resultStateBatchForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:@
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector :: Selector
temporaryResultStateBatchForCommandBuffer_sourceImage_sourceStates_destinationImageSelector = mkSelector "temporaryResultStateBatchForCommandBuffer:sourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @isResultStateReusedAcrossBatch@
isResultStateReusedAcrossBatchSelector :: Selector
isResultStateReusedAcrossBatchSelector = mkSelector "isResultStateReusedAcrossBatch"

-- | @Selector@ for @appendBatchBarrier@
appendBatchBarrierSelector :: Selector
appendBatchBarrierSelector = mkSelector "appendBatchBarrier"

-- | @Selector@ for @destinationImageDescriptorForSourceImages:sourceStates:@
destinationImageDescriptorForSourceImages_sourceStatesSelector :: Selector
destinationImageDescriptorForSourceImages_sourceStatesSelector = mkSelector "destinationImageDescriptorForSourceImages:sourceStates:"

-- | @Selector@ for @encodingStorageSizeForSourceImage:sourceStates:destinationImage:@
encodingStorageSizeForSourceImage_sourceStates_destinationImageSelector :: Selector
encodingStorageSizeForSourceImage_sourceStates_destinationImageSelector = mkSelector "encodingStorageSizeForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @batchEncodingStorageSizeForSourceImage:sourceStates:destinationImage:@
batchEncodingStorageSizeForSourceImage_sourceStates_destinationImageSelector :: Selector
batchEncodingStorageSizeForSourceImage_sourceStates_destinationImageSelector = mkSelector "batchEncodingStorageSizeForSourceImage:sourceStates:destinationImage:"

-- | @Selector@ for @destinationFeatureChannelOffset@
destinationFeatureChannelOffsetSelector :: Selector
destinationFeatureChannelOffsetSelector = mkSelector "destinationFeatureChannelOffset"

-- | @Selector@ for @setDestinationFeatureChannelOffset:@
setDestinationFeatureChannelOffsetSelector :: Selector
setDestinationFeatureChannelOffsetSelector = mkSelector "setDestinationFeatureChannelOffset:"

-- | @Selector@ for @sourceFeatureChannelOffset@
sourceFeatureChannelOffsetSelector :: Selector
sourceFeatureChannelOffsetSelector = mkSelector "sourceFeatureChannelOffset"

-- | @Selector@ for @setSourceFeatureChannelOffset:@
setSourceFeatureChannelOffsetSelector :: Selector
setSourceFeatureChannelOffsetSelector = mkSelector "setSourceFeatureChannelOffset:"

-- | @Selector@ for @sourceFeatureChannelMaxCount@
sourceFeatureChannelMaxCountSelector :: Selector
sourceFeatureChannelMaxCountSelector = mkSelector "sourceFeatureChannelMaxCount"

-- | @Selector@ for @setSourceFeatureChannelMaxCount:@
setSourceFeatureChannelMaxCountSelector :: Selector
setSourceFeatureChannelMaxCountSelector = mkSelector "setSourceFeatureChannelMaxCount:"

-- | @Selector@ for @edgeMode@
edgeModeSelector :: Selector
edgeModeSelector = mkSelector "edgeMode"

-- | @Selector@ for @setEdgeMode:@
setEdgeModeSelector :: Selector
setEdgeModeSelector = mkSelector "setEdgeMode:"

-- | @Selector@ for @kernelWidth@
kernelWidthSelector :: Selector
kernelWidthSelector = mkSelector "kernelWidth"

-- | @Selector@ for @kernelHeight@
kernelHeightSelector :: Selector
kernelHeightSelector = mkSelector "kernelHeight"

-- | @Selector@ for @strideInPixelsX@
strideInPixelsXSelector :: Selector
strideInPixelsXSelector = mkSelector "strideInPixelsX"

-- | @Selector@ for @strideInPixelsY@
strideInPixelsYSelector :: Selector
strideInPixelsYSelector = mkSelector "strideInPixelsY"

-- | @Selector@ for @dilationRateX@
dilationRateXSelector :: Selector
dilationRateXSelector = mkSelector "dilationRateX"

-- | @Selector@ for @dilationRateY@
dilationRateYSelector :: Selector
dilationRateYSelector = mkSelector "dilationRateY"

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

