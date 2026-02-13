{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNImageInferenceLayer
--
-- This depends on Metal.framework
--
-- The MPSRNNImageInferenceLayer specifies a recurrent neural network layer for inference on MPSImages.              Currently two types of recurrent layers are supported: ones that operate with convolutions on              images: MPSRNNImageInferenceLayer and one that operates on matrices: MPSRNNMatrixInferenceLayer.              The former can be often used to implement the latter by using 1x1-images, but due to              image size restrictions and performance, it is advisable to use MPSRNNMatrixInferenceLayer for              linear recurrent layers.              A MPSRNNImageInferenceLayer is initialized using a MPSRNNLayerDescriptor, which further specifies the              recurrent network layer, or an array of MPSRNNLayerDescriptors, which specifies a stack              of recurrent layers, that can operate in parallel a subset of the inputs in a sequence of inputs and              recurrent outputs. Note that currently stacks with bidirectionally traversing encode functions do not support starting              from a previous set of recurrent states, but this can be achieved quite easily by defining two separate              unidirectional stacks of layers, and running the same input sequence on them separately (one forwards and one backwards)              and ultimately combining the two result sequences as desired with auxiliary functions.
--
-- Generated bindings for @MPSRNNImageInferenceLayer@.
module ObjC.MetalPerformanceShaders.MPSRNNImageInferenceLayer
  ( MPSRNNImageInferenceLayer
  , IsMPSRNNImageInferenceLayer(..)
  , initWithDevice_rnnDescriptor
  , initWithDevice_rnnDescriptors
  , initWithDevice
  , encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStates
  , encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImages
  , initWithCoder_device
  , copyWithZone_device
  , inputFeatureChannels
  , outputFeatureChannels
  , numberOfLayers
  , recurrentOutputIsTemporary
  , setRecurrentOutputIsTemporary
  , storeAllIntermediateStates
  , setStoreAllIntermediateStates
  , bidirectionalCombineMode
  , setBidirectionalCombineMode
  , bidirectionalCombineModeSelector
  , copyWithZone_deviceSelector
  , encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImagesSelector
  , encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStatesSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_rnnDescriptorSelector
  , initWithDevice_rnnDescriptorsSelector
  , inputFeatureChannelsSelector
  , numberOfLayersSelector
  , outputFeatureChannelsSelector
  , recurrentOutputIsTemporarySelector
  , setBidirectionalCombineModeSelector
  , setRecurrentOutputIsTemporarySelector
  , setStoreAllIntermediateStatesSelector
  , storeAllIntermediateStatesSelector

  -- * Enum types
  , MPSRNNBidirectionalCombineMode(MPSRNNBidirectionalCombineMode)
  , pattern MPSRNNBidirectionalCombineModeNone
  , pattern MPSRNNBidirectionalCombineModeAdd
  , pattern MPSRNNBidirectionalCombineModeConcatenate

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

-- | Initializes a convolutional RNN kernel
--
-- @device@ — The MTLDevice on which this MPSRNNImageLayer filter will be used
--
-- @rnnDescriptor@ — The descriptor that defines the RNN layer
--
-- Returns: A valid MPSRNNImageInferenceLayer object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rnnDescriptor:@
initWithDevice_rnnDescriptor :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> RawId -> Const (Id MPSRNNDescriptor) -> IO (Id MPSRNNImageInferenceLayer)
initWithDevice_rnnDescriptor mpsrnnImageInferenceLayer device rnnDescriptor =
  sendOwnedMessage mpsrnnImageInferenceLayer initWithDevice_rnnDescriptorSelector device rnnDescriptor

-- | Initializes a kernel that implements a stack of convolutional RNN layers
--
-- @device@ — The MTLDevice on which this MPSRNNImageLayer filter will be used
--
-- @rnnDescriptors@ — An array of RNN descriptors that defines a stack of RNN layers, starting at index zero.                                                  The number of layers in stack is the number of entries in the array.                                                  All entries in the array must be valid MPSRNNDescriptors.
--
-- Returns: A valid MPSRNNImageInferenceLayer object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rnnDescriptors:@
initWithDevice_rnnDescriptors :: (IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer, IsNSArray rnnDescriptors) => mpsrnnImageInferenceLayer -> RawId -> rnnDescriptors -> IO (Id MPSRNNImageInferenceLayer)
initWithDevice_rnnDescriptors mpsrnnImageInferenceLayer device rnnDescriptors =
  sendOwnedMessage mpsrnnImageInferenceLayer initWithDevice_rnnDescriptorsSelector device (toNSArray rnnDescriptors)

-- | @- initWithDevice:@
initWithDevice :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> RawId -> IO (Id MPSRNNImageInferenceLayer)
initWithDevice mpsrnnImageInferenceLayer device =
  sendOwnedMessage mpsrnnImageInferenceLayer initWithDeviceSelector device

-- | Encode an MPSRNNImageInferenceLayer kernel (stack) for a sequence of inputs into a command buffer.                  Note that when encoding using this function the
--
-- See: layerSequenceDirection is ignored and the layer stack operates as                  if all layers were forward feeding layers. In order to run bidirectional sequences                  use encodeBidirectionalSequenceToCommandBuffer:sourceSequence: or alternatively run two layer stacks and combine                  results at the end using utility functions.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceImages@ — An array of valid MPSImage objects containing the sequence of source images.
--
-- @destinationImages@ — An array valid MPSImages to be overwritten by result image sequence. destinationImages may not alias sourceImages.
--
-- @recurrentInputState@ — An optional state containing the output images and memory cells (for LSTMs)                                                  of the layer obtained from the previous input images in a sequence of inputs.                                                  Has to be the output of a previous call to this function or nil (assumed zero).                                                  Note: can be one of the states returned in recurrentOutputStates.
--
-- @recurrentOutputStates@ — An optional array that will contain the recurrent output states. If nil then                                                  the recurrent output state is discarded.                                                  If storeAllIntermediateStates is YES, then all intermediate states of the sequence                                                  are returned in the array, the first one corresponding to the first input in the sequence,                                                  otherwise only the last recurrent output state is returned.                                                  If recurrentOutputIsTemporary is YES and then all returned recurrent states                                                  will be temporary.
--
-- See: MPSState:isTemporary.                                                  Example: In order to get a new state one can do the following:
--
-- MPSRNNRecurrentImageState* recurrent0 = nil;
-- [filter encodeToCommandBuffer: cmdBuf
-- sourceImage: source0
-- destinationImage: destination0
-- recurrentInputState: nil
-- recurrentOutputState: &recurrent0];
--
-- Then use it for the next input in sequence:
--
-- [filter encodeToCommandBuffer: cmdBuf
-- sourceImage: source1
-- destinationImage: destination1
-- recurrentInputState: recurrent0
-- recurrentOutputState: &recurrent0];
--
-- And discard recurrent output of the third input:
--
-- [filter encodeToCommandBuffer: cmdBuf
-- sourceImage: source2
-- destinationImage: destination2
-- recurrentInputState: recurrent0
-- recurrentOutputState: nil];
--
-- ObjC selector: @- encodeSequenceToCommandBuffer:sourceImages:destinationImages:recurrentInputState:recurrentOutputStates:@
encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStates :: (IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer, IsNSArray sourceImages, IsNSArray destinationImages, IsMPSRNNRecurrentImageState recurrentInputState, IsNSMutableArray recurrentOutputStates) => mpsrnnImageInferenceLayer -> RawId -> sourceImages -> destinationImages -> recurrentInputState -> recurrentOutputStates -> IO ()
encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStates mpsrnnImageInferenceLayer commandBuffer sourceImages destinationImages recurrentInputState recurrentOutputStates =
  sendMessage mpsrnnImageInferenceLayer encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStatesSelector commandBuffer (toNSArray sourceImages) (toNSArray destinationImages) (toMPSRNNRecurrentImageState recurrentInputState) (toNSMutableArray recurrentOutputStates)

-- | Encode an MPSRNNImageInferenceLayer kernel stack for an input image sequences into a command buffer bidirectionally.                  The operation proceeds as follows: The first source image x0 is passed through all forward traversing layers in the stack,                  ie. those that were initialized with MPSRNNSequenceDirectionForward, recurrent input is assumed zero.                  This produces forward output yf0 and recurrent states hf00, hf01, hf02, ... hf0n, one for each forward layer.                  Then x1 is passed to forward layers together with recurrent state hf00, hf01, ..., hf0n, which produces yf1, and hf10,...                  This procedure is iterated until the last image in the input sequence x_(N-1), which produces forward output yf(N-1).                  The backwards layers iterate the same sequence backwards, starting from input x_(N-1) (recurrent state zero),                  that produces yb(N-1) and recurrent output hb(N-1)0, hf(N-1)1, ... hb(N-1)m, one for each backwards traversing layer.                  Then the backwards layers handle input x_(N-2) using recurrent state hb(N-1)0, ..., et cetera, until the                  first image of the sequence is computed, producing output yb0. The result of the operation is either pair of sequences                  ({yf0, yf1, ... , yf(N-1)},  {yb0, yb1, ... , yb(N-1)}) or a combined sequence, {(yf0 + yb0), ... , (yf(N-1) + yb(N-1)) },                  where '+' stands either for sum, or concatenation along feature channels, as specified by bidirectionalCombineMode.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceSequence@ — An array of valid MPSImage objects containing the source image sequence (x0, x1, ... x_n-1).
--
-- @destinationForwardImages@ — An array of valid MPSImages to be overwritten by result from forward input images. If bidirectionalCombineMode                                                  is either MPSRNNBidirectionalCombineModeAdd or MPSRNNBidirectionalCombineModeConcatenate, then will                                                  contain the combined results. destinationForwardImage may not alias with any of the source images.
--
-- @destinationBackwardImages@ — If bidirectionalCombineMode is MPSRNNBidirectionalCombineModeNone, then must be a valid MPSImage                                                  that will be  overwritten by result from backward input image. Otherwise this parameter is ignored                                                  and can be nil. destinationBackwardImages may not alias to any of the source images.
--
-- ObjC selector: @- encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardImages:destinationBackwardImages:@
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImages :: (IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer, IsNSArray sourceSequence, IsNSArray destinationForwardImages, IsNSArray destinationBackwardImages) => mpsrnnImageInferenceLayer -> RawId -> sourceSequence -> destinationForwardImages -> destinationBackwardImages -> IO ()
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImages mpsrnnImageInferenceLayer commandBuffer sourceSequence destinationForwardImages destinationBackwardImages =
  sendMessage mpsrnnImageInferenceLayer encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImagesSelector commandBuffer (toNSArray sourceSequence) (toNSArray destinationForwardImages) (toNSArray destinationBackwardImages)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSRNNImageInferenceLayer
--
-- @device@ — The MTLDevice on which to make the MPSRNNImageInferenceLayer
--
-- Returns: A new MPSRNNImageInferenceLayer object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer, IsNSCoder aDecoder) => mpsrnnImageInferenceLayer -> aDecoder -> RawId -> IO (Id MPSRNNImageInferenceLayer)
initWithCoder_device mpsrnnImageInferenceLayer aDecoder device =
  sendOwnedMessage mpsrnnImageInferenceLayer initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Make a copy of this kernel for a new device -
--
-- See: MPSKernel
--
-- @zone@ — The NSZone in which to allocate the object
--
-- @device@ — The device for the new MPSKernel. If nil, then use                          self.device.
--
-- Returns: a pointer to a copy of this MPSKernel. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> Ptr () -> RawId -> IO (Id MPSRNNImageInferenceLayer)
copyWithZone_device mpsrnnImageInferenceLayer zone device =
  sendOwnedMessage mpsrnnImageInferenceLayer copyWithZone_deviceSelector zone device

-- | inputFeatureChannels
--
-- The number of feature channels per pixel in the input image.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> IO CULong
inputFeatureChannels mpsrnnImageInferenceLayer =
  sendMessage mpsrnnImageInferenceLayer inputFeatureChannelsSelector

-- | outputFeatureChannels
--
-- The number of feature channels per pixel in the output image.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> IO CULong
outputFeatureChannels mpsrnnImageInferenceLayer =
  sendMessage mpsrnnImageInferenceLayer outputFeatureChannelsSelector

-- | numberOfLayers
--
-- Number of layers in the filter-stack. This will be one when using initWithDevice:rnnDescriptor to initialize                  this filter and the number of entries in the array 'rnnDescriptors' when initializing this filter with                  initWithDevice:rnnDescriptors.
--
-- ObjC selector: @- numberOfLayers@
numberOfLayers :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> IO CULong
numberOfLayers mpsrnnImageInferenceLayer =
  sendMessage mpsrnnImageInferenceLayer numberOfLayersSelector

-- | recurrentOutputIsTemporary
--
-- How output states from encodeSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- recurrentOutputIsTemporary@
recurrentOutputIsTemporary :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> IO Bool
recurrentOutputIsTemporary mpsrnnImageInferenceLayer =
  sendMessage mpsrnnImageInferenceLayer recurrentOutputIsTemporarySelector

-- | recurrentOutputIsTemporary
--
-- How output states from encodeSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- setRecurrentOutputIsTemporary:@
setRecurrentOutputIsTemporary :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> Bool -> IO ()
setRecurrentOutputIsTemporary mpsrnnImageInferenceLayer value =
  sendMessage mpsrnnImageInferenceLayer setRecurrentOutputIsTemporarySelector value

-- | storeAllIntermediateStates
--
-- If YES then calls to encodeSequenceToCommandBuffer return every recurrent state              in the array: recurrentOutputStates.              Defaults to NO.
--
-- ObjC selector: @- storeAllIntermediateStates@
storeAllIntermediateStates :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> IO Bool
storeAllIntermediateStates mpsrnnImageInferenceLayer =
  sendMessage mpsrnnImageInferenceLayer storeAllIntermediateStatesSelector

-- | storeAllIntermediateStates
--
-- If YES then calls to encodeSequenceToCommandBuffer return every recurrent state              in the array: recurrentOutputStates.              Defaults to NO.
--
-- ObjC selector: @- setStoreAllIntermediateStates:@
setStoreAllIntermediateStates :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> Bool -> IO ()
setStoreAllIntermediateStates mpsrnnImageInferenceLayer value =
  sendMessage mpsrnnImageInferenceLayer setStoreAllIntermediateStatesSelector value

-- | bidirectionalCombineMode
--
-- Defines how to combine the output-results, when encoding bidirectional layers using              encodeBidirectionalSequenceToCommandBuffer.              Defaults to MPSRNNBidirectionalCombineModeNone.
--
-- ObjC selector: @- bidirectionalCombineMode@
bidirectionalCombineMode :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> IO MPSRNNBidirectionalCombineMode
bidirectionalCombineMode mpsrnnImageInferenceLayer =
  sendMessage mpsrnnImageInferenceLayer bidirectionalCombineModeSelector

-- | bidirectionalCombineMode
--
-- Defines how to combine the output-results, when encoding bidirectional layers using              encodeBidirectionalSequenceToCommandBuffer.              Defaults to MPSRNNBidirectionalCombineModeNone.
--
-- ObjC selector: @- setBidirectionalCombineMode:@
setBidirectionalCombineMode :: IsMPSRNNImageInferenceLayer mpsrnnImageInferenceLayer => mpsrnnImageInferenceLayer -> MPSRNNBidirectionalCombineMode -> IO ()
setBidirectionalCombineMode mpsrnnImageInferenceLayer value =
  sendMessage mpsrnnImageInferenceLayer setBidirectionalCombineModeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:rnnDescriptor:@
initWithDevice_rnnDescriptorSelector :: Selector '[RawId, Const (Id MPSRNNDescriptor)] (Id MPSRNNImageInferenceLayer)
initWithDevice_rnnDescriptorSelector = mkSelector "initWithDevice:rnnDescriptor:"

-- | @Selector@ for @initWithDevice:rnnDescriptors:@
initWithDevice_rnnDescriptorsSelector :: Selector '[RawId, Id NSArray] (Id MPSRNNImageInferenceLayer)
initWithDevice_rnnDescriptorsSelector = mkSelector "initWithDevice:rnnDescriptors:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSRNNImageInferenceLayer)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeSequenceToCommandBuffer:sourceImages:destinationImages:recurrentInputState:recurrentOutputStates:@
encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStatesSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id MPSRNNRecurrentImageState, Id NSMutableArray] ()
encodeSequenceToCommandBuffer_sourceImages_destinationImages_recurrentInputState_recurrentOutputStatesSelector = mkSelector "encodeSequenceToCommandBuffer:sourceImages:destinationImages:recurrentInputState:recurrentOutputStates:"

-- | @Selector@ for @encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardImages:destinationBackwardImages:@
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImagesSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id NSArray] ()
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardImages_destinationBackwardImagesSelector = mkSelector "encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardImages:destinationBackwardImages:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSRNNImageInferenceLayer)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSRNNImageInferenceLayer)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector '[] CULong
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector '[] CULong
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @numberOfLayers@
numberOfLayersSelector :: Selector '[] CULong
numberOfLayersSelector = mkSelector "numberOfLayers"

-- | @Selector@ for @recurrentOutputIsTemporary@
recurrentOutputIsTemporarySelector :: Selector '[] Bool
recurrentOutputIsTemporarySelector = mkSelector "recurrentOutputIsTemporary"

-- | @Selector@ for @setRecurrentOutputIsTemporary:@
setRecurrentOutputIsTemporarySelector :: Selector '[Bool] ()
setRecurrentOutputIsTemporarySelector = mkSelector "setRecurrentOutputIsTemporary:"

-- | @Selector@ for @storeAllIntermediateStates@
storeAllIntermediateStatesSelector :: Selector '[] Bool
storeAllIntermediateStatesSelector = mkSelector "storeAllIntermediateStates"

-- | @Selector@ for @setStoreAllIntermediateStates:@
setStoreAllIntermediateStatesSelector :: Selector '[Bool] ()
setStoreAllIntermediateStatesSelector = mkSelector "setStoreAllIntermediateStates:"

-- | @Selector@ for @bidirectionalCombineMode@
bidirectionalCombineModeSelector :: Selector '[] MPSRNNBidirectionalCombineMode
bidirectionalCombineModeSelector = mkSelector "bidirectionalCombineMode"

-- | @Selector@ for @setBidirectionalCombineMode:@
setBidirectionalCombineModeSelector :: Selector '[MPSRNNBidirectionalCombineMode] ()
setBidirectionalCombineModeSelector = mkSelector "setBidirectionalCombineMode:"

