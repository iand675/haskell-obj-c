{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNConvolutionWeightsAndBiasesState
--
-- The MPSCNNConvolutionWeightsAndBiasesState is returned by exportWeightsAndBiasesWithCommandBuffer: method on MPSCNNConvolution object.              This is mainly used for GPU side weights/biases update process.              During training, application can keep a copy of weights, velocity, momentum MTLBuffers in its data source, update the weights (in-place or out of place)              with gradients obtained from MPSCNNConvolutionGradientState and call [MPSCNNConvolution reloadWeightsAndBiasesWithCommandBuffer] with resulting updated              MTLBuffer. If application does not want to keep a copy of weights/biases, it can call [MPSCNNConvolution exportWeightsAndBiasesWithCommandBuffer:] to get              the current weights from convolution itself, do the updated and call reloadWithCommandBuffer.
--
-- Generated bindings for @MPSCNNConvolutionWeightsAndBiasesState@.
module ObjC.MetalPerformanceShaders.MPSCNNConvolutionWeightsAndBiasesState
  ( MPSCNNConvolutionWeightsAndBiasesState
  , IsMPSCNNConvolutionWeightsAndBiasesState(..)
  , initWithWeights_biases
  , initWithDevice_cnnConvolutionDescriptor
  , temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptor
  , initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptor
  , weights
  , biases
  , weightsOffset
  , biasesOffset
  , biasesOffsetSelector
  , biasesSelector
  , initWithDevice_cnnConvolutionDescriptorSelector
  , initWithWeights_biasesSelector
  , initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector
  , temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector
  , weightsOffsetSelector
  , weightsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create and initialize MPSCNNConvolutionWeightsAndBiasesState with application              provided weights and biases buffers.
--
-- This is the convinience API when buffers of exact size i.e.                 [weights length] =  inputFeatureChannels*kernelWidth*kernelHeight*channelMultiplier*sizeof(float)                   // for depthwise convolution                               outputFeatureChannels*kernelWidth*kernelHeight*(inputChannels/groups)*sizeof(float)      // for regular otherwise              and [biases length]  =  outputFeatureChannels*sizeof(float)
--
-- ObjC selector: @- initWithWeights:biases:@
initWithWeights_biases :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> RawId -> RawId -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithWeights_biases mpscnnConvolutionWeightsAndBiasesState weights biases =
  sendOwnedMessage mpscnnConvolutionWeightsAndBiasesState initWithWeights_biasesSelector weights biases

-- | Create and initialize MPSCNNConvolutionWeightsAndBiasesState with application provided convolution descriptor
--
-- Create weights and biases buffers of appropriate size
--
-- ObjC selector: @- initWithDevice:cnnConvolutionDescriptor:@
initWithDevice_cnnConvolutionDescriptor :: (IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState, IsMPSCNNConvolutionDescriptor descriptor) => mpscnnConvolutionWeightsAndBiasesState -> RawId -> descriptor -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithDevice_cnnConvolutionDescriptor mpscnnConvolutionWeightsAndBiasesState device descriptor =
  sendOwnedMessage mpscnnConvolutionWeightsAndBiasesState initWithDevice_cnnConvolutionDescriptorSelector device (toMPSCNNConvolutionDescriptor descriptor)

-- | Create and initialize temporary MPSCNNConvolutionWeightsAndBiasesState with application provided convolution descriptor
--
-- Create weights and biases buffers of appropriate size from command buffer cache.
--
-- ObjC selector: @+ temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:@
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptor :: IsMPSCNNConvolutionDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptor commandBuffer descriptor =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionWeightsAndBiasesState"
    sendClassMessage cls' temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector commandBuffer (toMPSCNNConvolutionDescriptor descriptor)

-- | Create and initialize MPSCNNConvolutionWeightsAndBiasesState with application              provided weights and biases buffers.
--
-- It gives finer allocation control to application e.g. application can pass same buffer for weights and biases with              appropriate offsets. Or offset into some larger buffer from application managed heap etc. Number of weights              and biases or the length of weights and biases buffer this object owns (will read or write to), starting at offset is              determined by MPSCNNConvolutionDescriptor passed in.              weightsLength =  inputFeatureChannels*kernelWidth*kernelHeight*channelMultiplier*sizeof(float)                   // for depthwise convolution                           outputFeatureChannels*kernelWidth*kernelHeight*(inputChannels/groups)*sizeof(float)      // for regular otherwise              biasesLength  =  outputFeatureChannels*sizeof(float)              Thus filters operating on this object will read or write to NSRange(weightsOffset, weightsLength) of weights buffer              and NSRange(biasesOffset, biasesLength) of biases buffer. Thus sizes of buffers provided must be such that                              weightsOffset + weightsLength <= [weights length]                         and     biasesOffset + biasesLength <= [biases length]              Offsets must of sizeof(float) aligned i.e. multiple of 4.
--
-- ObjC selector: @- initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:@
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptor :: (IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState, IsMPSCNNConvolutionDescriptor descriptor) => mpscnnConvolutionWeightsAndBiasesState -> RawId -> CULong -> RawId -> CULong -> descriptor -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptor mpscnnConvolutionWeightsAndBiasesState weights weightsOffset biases biasesOffset descriptor =
  sendOwnedMessage mpscnnConvolutionWeightsAndBiasesState initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector weights weightsOffset biases biasesOffset (toMPSCNNConvolutionDescriptor descriptor)

-- | weights
--
-- A buffer that contains the weights.              Each value in the buffer is a float. The layout of the weights with respect to the weights is the same as              the weights layout provided by data source i.e. it can be interpreted as 4D array
--
-- weights[outputFeatureChannels][kernelHeight][kernelWidth][inputFeatureChannels/groups]              for regular convolution. For depthwise convolution                   weights[outputFeatureChannels][kernelHeight][kernelWidth] as we currently only support channel multiplier of 1.
--
-- ObjC selector: @- weights@
weights :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> IO RawId
weights mpscnnConvolutionWeightsAndBiasesState =
  sendMessage mpscnnConvolutionWeightsAndBiasesState weightsSelector

-- | biases
--
-- A buffer that contains the biases. Each value is float and there are ouputFeatureChannels values.
--
-- ObjC selector: @- biases@
biases :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> IO RawId
biases mpscnnConvolutionWeightsAndBiasesState =
  sendMessage mpscnnConvolutionWeightsAndBiasesState biasesSelector

-- | weightsOffset
--
-- Offset at which weights start in weights buffer              Default value is 0.
--
-- ObjC selector: @- weightsOffset@
weightsOffset :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> IO CULong
weightsOffset mpscnnConvolutionWeightsAndBiasesState =
  sendMessage mpscnnConvolutionWeightsAndBiasesState weightsOffsetSelector

-- | biasesOffset
--
-- Offset at which weights start in biases buffer              Default value is 0.
--
-- ObjC selector: @- biasesOffset@
biasesOffset :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> IO CULong
biasesOffset mpscnnConvolutionWeightsAndBiasesState =
  sendMessage mpscnnConvolutionWeightsAndBiasesState biasesOffsetSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWeights:biases:@
initWithWeights_biasesSelector :: Selector '[RawId, RawId] (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithWeights_biasesSelector = mkSelector "initWithWeights:biases:"

-- | @Selector@ for @initWithDevice:cnnConvolutionDescriptor:@
initWithDevice_cnnConvolutionDescriptorSelector :: Selector '[RawId, Id MPSCNNConvolutionDescriptor] (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithDevice_cnnConvolutionDescriptorSelector = mkSelector "initWithDevice:cnnConvolutionDescriptor:"

-- | @Selector@ for @temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:@
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector :: Selector '[RawId, Id MPSCNNConvolutionDescriptor] (Id MPSCNNConvolutionWeightsAndBiasesState)
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector = mkSelector "temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:"

-- | @Selector@ for @initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:@
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector :: Selector '[RawId, CULong, RawId, CULong, Id MPSCNNConvolutionDescriptor] (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector = mkSelector "initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] RawId
weightsSelector = mkSelector "weights"

-- | @Selector@ for @biases@
biasesSelector :: Selector '[] RawId
biasesSelector = mkSelector "biases"

-- | @Selector@ for @weightsOffset@
weightsOffsetSelector :: Selector '[] CULong
weightsOffsetSelector = mkSelector "weightsOffset"

-- | @Selector@ for @biasesOffset@
biasesOffsetSelector :: Selector '[] CULong
biasesOffsetSelector = mkSelector "biasesOffset"

