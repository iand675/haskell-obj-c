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
  , weightsOffset
  , biasesOffset
  , initWithWeights_biasesSelector
  , initWithDevice_cnnConvolutionDescriptorSelector
  , temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector
  , initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector
  , weightsOffsetSelector
  , biasesOffsetSelector


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

-- | Create and initialize MPSCNNConvolutionWeightsAndBiasesState with application              provided weights and biases buffers.
--
-- This is the convinience API when buffers of exact size i.e.                 [weights length] =  inputFeatureChannels*kernelWidth*kernelHeight*channelMultiplier*sizeof(float)                   // for depthwise convolution                               outputFeatureChannels*kernelWidth*kernelHeight*(inputChannels/groups)*sizeof(float)      // for regular otherwise              and [biases length]  =  outputFeatureChannels*sizeof(float)
--
-- ObjC selector: @- initWithWeights:biases:@
initWithWeights_biases :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> RawId -> RawId -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithWeights_biases mpscnnConvolutionWeightsAndBiasesState  weights biases =
  sendMsg mpscnnConvolutionWeightsAndBiasesState (mkSelector "initWithWeights:biases:") (retPtr retVoid) [argPtr (castPtr (unRawId weights) :: Ptr ()), argPtr (castPtr (unRawId biases) :: Ptr ())] >>= ownedObject . castPtr

-- | Create and initialize MPSCNNConvolutionWeightsAndBiasesState with application provided convolution descriptor
--
-- Create weights and biases buffers of appropriate size
--
-- ObjC selector: @- initWithDevice:cnnConvolutionDescriptor:@
initWithDevice_cnnConvolutionDescriptor :: (IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState, IsMPSCNNConvolutionDescriptor descriptor) => mpscnnConvolutionWeightsAndBiasesState -> RawId -> descriptor -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithDevice_cnnConvolutionDescriptor mpscnnConvolutionWeightsAndBiasesState  device descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpscnnConvolutionWeightsAndBiasesState (mkSelector "initWithDevice:cnnConvolutionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Create and initialize temporary MPSCNNConvolutionWeightsAndBiasesState with application provided convolution descriptor
--
-- Create weights and biases buffers of appropriate size from command buffer cache.
--
-- ObjC selector: @+ temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:@
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptor :: IsMPSCNNConvolutionDescriptor descriptor => RawId -> descriptor -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptor commandBuffer descriptor =
  do
    cls' <- getRequiredClass "MPSCNNConvolutionWeightsAndBiasesState"
    withObjCPtr descriptor $ \raw_descriptor ->
      sendClassMsg cls' (mkSelector "temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | Create and initialize MPSCNNConvolutionWeightsAndBiasesState with application              provided weights and biases buffers.
--
-- It gives finer allocation control to application e.g. application can pass same buffer for weights and biases with              appropriate offsets. Or offset into some larger buffer from application managed heap etc. Number of weights              and biases or the length of weights and biases buffer this object owns (will read or write to), starting at offset is              determined by MPSCNNConvolutionDescriptor passed in.              weightsLength =  inputFeatureChannels*kernelWidth*kernelHeight*channelMultiplier*sizeof(float)                   // for depthwise convolution                           outputFeatureChannels*kernelWidth*kernelHeight*(inputChannels/groups)*sizeof(float)      // for regular otherwise              biasesLength  =  outputFeatureChannels*sizeof(float)              Thus filters operating on this object will read or write to NSRange(weightsOffset, weightsLength) of weights buffer              and NSRange(biasesOffset, biasesLength) of biases buffer. Thus sizes of buffers provided must be such that                              weightsOffset + weightsLength <= [weights length]                         and     biasesOffset + biasesLength <= [biases length]              Offsets must of sizeof(float) aligned i.e. multiple of 4.
--
-- ObjC selector: @- initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:@
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptor :: (IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState, IsMPSCNNConvolutionDescriptor descriptor) => mpscnnConvolutionWeightsAndBiasesState -> RawId -> CULong -> RawId -> CULong -> descriptor -> IO (Id MPSCNNConvolutionWeightsAndBiasesState)
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptor mpscnnConvolutionWeightsAndBiasesState  weights weightsOffset biases biasesOffset descriptor =
withObjCPtr descriptor $ \raw_descriptor ->
    sendMsg mpscnnConvolutionWeightsAndBiasesState (mkSelector "initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId weights) :: Ptr ()), argCULong (fromIntegral weightsOffset), argPtr (castPtr (unRawId biases) :: Ptr ()), argCULong (fromIntegral biasesOffset), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | weightsOffset
--
-- Offset at which weights start in weights buffer              Default value is 0.
--
-- ObjC selector: @- weightsOffset@
weightsOffset :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> IO CULong
weightsOffset mpscnnConvolutionWeightsAndBiasesState  =
  sendMsg mpscnnConvolutionWeightsAndBiasesState (mkSelector "weightsOffset") retCULong []

-- | biasesOffset
--
-- Offset at which weights start in biases buffer              Default value is 0.
--
-- ObjC selector: @- biasesOffset@
biasesOffset :: IsMPSCNNConvolutionWeightsAndBiasesState mpscnnConvolutionWeightsAndBiasesState => mpscnnConvolutionWeightsAndBiasesState -> IO CULong
biasesOffset mpscnnConvolutionWeightsAndBiasesState  =
  sendMsg mpscnnConvolutionWeightsAndBiasesState (mkSelector "biasesOffset") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithWeights:biases:@
initWithWeights_biasesSelector :: Selector
initWithWeights_biasesSelector = mkSelector "initWithWeights:biases:"

-- | @Selector@ for @initWithDevice:cnnConvolutionDescriptor:@
initWithDevice_cnnConvolutionDescriptorSelector :: Selector
initWithDevice_cnnConvolutionDescriptorSelector = mkSelector "initWithDevice:cnnConvolutionDescriptor:"

-- | @Selector@ for @temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:@
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector :: Selector
temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer_cnnConvolutionDescriptorSelector = mkSelector "temporaryCNNConvolutionWeightsAndBiasesStateWithCommandBuffer:cnnConvolutionDescriptor:"

-- | @Selector@ for @initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:@
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector :: Selector
initWithWeights_weightsOffset_biases_biasesOffset_cnnConvolutionDescriptorSelector = mkSelector "initWithWeights:weightsOffset:biases:biasesOffset:cnnConvolutionDescriptor:"

-- | @Selector@ for @weightsOffset@
weightsOffsetSelector :: Selector
weightsOffsetSelector = mkSelector "weightsOffset"

-- | @Selector@ for @biasesOffset@
biasesOffsetSelector :: Selector
biasesOffsetSelector = mkSelector "biasesOffset"

