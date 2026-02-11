{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNMatrixInferenceLayer
--
-- This depends on Metal.framework
--
-- The MPSRNNMatrixInferenceLayer specifies a recurrent neural network layer for inference on MPSMatrices.              Currently two types of recurrent layers are supported: ones that operate with convolutions on              images: MPSRNNImageInferenceLayer and one that operates on matrices: MPSRNNMatrixInferenceLayer.              The former can be often used to implement the latter by using 1x1-matrices, but due to              image size restrictions and performance, it is advisable to use MPSRNNMatrixInferenceLayer for              linear recurrent layers.              A MPSRNNMatrixInferenceLayer is initialized using a MPSRNNLayerDescriptor, which further specifies the              recurrent network layer, or an array of MPSRNNLayerDescriptors, which specifies a stack              of recurrent layers, that can operate in parallel a subset of the inputs in a sequence of inputs and              recurrent outputs. Note that currently stacks with bidirectionally traversing encode functions do not support starting              from a previous set of recurrent states, but this can be achieved quite easily by defining two separate              unidirectional stacks of layers, and running the same input sequence on them separately (one forwards and one backwards)              and ultimately combining the two result sequences as desired with auxiliary functions.              The input and output vectors in encode calls are stored as rows of the input and output matrices and              MPSRNNMatrixInferenceLayer supports matrices with decreasing number of rows: The row-indices identify the different              sequences that may be of different lengths - for example if we have three sequences:                  ( x1, x2, x3 ), ( y1, y2, y3, y4 ) and ( z1, z2 )              of vectors xi, yi and zi, then these can be inserted together as a batch to the sequence encoding kernel by              using the matrices:
--
-- ( y1 )        ( y2 )        ( y3 )        ( y4 )
-- m1 = ( x1 ),  m2 = ( x2 ),  m3 = ( x3 ),  m4 =
-- ( z1 )        ( z2 )
--
-- If a recurrent output state is requested then it will contain the state corresponding to last inputs to each              sequence and if all the intermediate states are requested (see storeAllIntermediateStates),              then the shorter sequences will be propagated by copying the state of the previous output if the              input vector is not present in the sequence - in the example above the output states would be:
--
-- ( s_y1 )        ( s_y2 )        ( s_y3 )        ( s_y4 )
-- s1 = ( s_x1 ),  s2 = ( s_x2 ),  s3 = ( s_x3 ),  s4 = ( s_x3 )
-- ( s_z1 )        ( s_z2 )        ( s_z2 )        ( s_z2 )
--
-- The mathematical operation described in the linear transformations of MPSRNNSingleGateDescriptor              MPSLSTMDescriptor and MPSGRUDescriptor are y^T = W x^T  <=> y = x W^T, where x is the matrix containing              the input vectors as rows, y is the matrix containing the output vectors as rows and W is the weight matrix.
--
-- Generated bindings for @MPSRNNMatrixInferenceLayer@.
module ObjC.MetalPerformanceShaders.MPSRNNMatrixInferenceLayer
  ( MPSRNNMatrixInferenceLayer
  , IsMPSRNNMatrixInferenceLayer(..)
  , initWithDevice_rnnDescriptor
  , initWithDevice_rnnDescriptors
  , initWithDevice
  , encodeSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_recurrentInputState_recurrentOutputStates
  , encodeSequenceToCommandBuffer_sourceMatrices_destinationMatrices_recurrentInputState_recurrentOutputStates
  , encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardMatrices_destinationBackwardMatrices
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
  , initWithDevice_rnnDescriptorSelector
  , initWithDevice_rnnDescriptorsSelector
  , initWithDeviceSelector
  , encodeSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_recurrentInputState_recurrentOutputStatesSelector
  , encodeSequenceToCommandBuffer_sourceMatrices_destinationMatrices_recurrentInputState_recurrentOutputStatesSelector
  , encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardMatrices_destinationBackwardMatricesSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , inputFeatureChannelsSelector
  , outputFeatureChannelsSelector
  , numberOfLayersSelector
  , recurrentOutputIsTemporarySelector
  , setRecurrentOutputIsTemporarySelector
  , storeAllIntermediateStatesSelector
  , setStoreAllIntermediateStatesSelector
  , bidirectionalCombineModeSelector
  , setBidirectionalCombineModeSelector

  -- * Enum types
  , MPSRNNBidirectionalCombineMode(MPSRNNBidirectionalCombineMode)
  , pattern MPSRNNBidirectionalCombineModeNone
  , pattern MPSRNNBidirectionalCombineModeAdd
  , pattern MPSRNNBidirectionalCombineModeConcatenate

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

-- | Initializes a linear (fully connected) RNN kernel
--
-- @device@ — The MTLDevice on which this MPSRNNMatrixLayer filter will be used
--
-- @rnnDescriptor@ — The descriptor that defines the RNN layer
--
-- Returns: A valid MPSRNNMatrixInferenceLayer object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rnnDescriptor:@
initWithDevice_rnnDescriptor :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> RawId -> Const (Id MPSRNNDescriptor) -> IO (Id MPSRNNMatrixInferenceLayer)
initWithDevice_rnnDescriptor mpsrnnMatrixInferenceLayer  device rnnDescriptor =
withObjCPtr rnnDescriptor $ \raw_rnnDescriptor ->
    sendMsg mpsrnnMatrixInferenceLayer (mkSelector "initWithDevice:rnnDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_rnnDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Initializes a kernel that implements a stack of linear (fully connected) RNN layers
--
-- @device@ — The MTLDevice on which this MPSRNNMatrixLayer filter will be used
--
-- @rnnDescriptors@ — An array of RNN descriptors that defines a stack of RNN layers, starting at index zero.                                                  The number of layers in stack is the number of entries in the array.                                                  All entries in the array must be valid MPSRNNDescriptors.
--
-- Returns: A valid MPSRNNMatrixInferenceLayer object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rnnDescriptors:@
initWithDevice_rnnDescriptors :: (IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer, IsNSArray rnnDescriptors) => mpsrnnMatrixInferenceLayer -> RawId -> rnnDescriptors -> IO (Id MPSRNNMatrixInferenceLayer)
initWithDevice_rnnDescriptors mpsrnnMatrixInferenceLayer  device rnnDescriptors =
withObjCPtr rnnDescriptors $ \raw_rnnDescriptors ->
    sendMsg mpsrnnMatrixInferenceLayer (mkSelector "initWithDevice:rnnDescriptors:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_rnnDescriptors :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:@
initWithDevice :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> RawId -> IO (Id MPSRNNMatrixInferenceLayer)
initWithDevice mpsrnnMatrixInferenceLayer  device =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode an MPSRNNMatrixInferenceLayer kernel (stack) for a sequence of inputs into a command buffer.                  Note that when encoding using this function the
--
-- See: layerSequenceDirection is ignored and the layer stack operates as                  if all layers were forward feeding layers. In order to run bidirectional sequences                  use encodeBidirectionalSequenceToCommandBuffer:sourceSequence: or alternatively run two layer stacks and combine                  results at the end using utility functions.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceMatrices@ — An array of valid MPSMatrix objects containing the sequence of source matrices.
--
-- @sourceOffsets@ — An array of byte-offsets into the sourceMatrices, if nil zeros are assumed and                                                  if not nil must contain offset for every matrix in sourceMatrices.
--
-- @destinationMatrices@ — An array valid MPSMatrices to be overwritten by result matrix sequence.                                                  destinationMatrices may not alias sourceMatrices.
--
-- @destinationOffsets@ — An array of byte-offsets into the destinationMatrices, if nil zeros are assumed and                                                  if not nil must contain offset for every matrix in destinationMatrices.
--
-- @recurrentInputState@ — An optional state containing the output matrices and memory cells (for LSTMs)                                                  of the layer obtained from the previous input matrices in a sequence of inputs.                                                  Has to be the output of a previous call to this function or nil (assumed zero).                                                  Note: can be one of the states returned in intermediateRecurrentStates.
--
-- @recurrentOutputStates@ — An optional array that will contain the recurrent output states. If nil then                                                  the recurrent output state is discarded.                                                  If storeAllIntermediateStates is YES, then all intermediate states of the sequence                                                  are returned in the array, the first one corresponding to the first input in the sequence,                                                  otherwise only the last recurrent output state is returned.                                                  If recurrentOutputIsTemporary is YES and then all returned recurrent states                                                  will be temporary.
--
-- See: MPSState:isTemporary.                                                  Example: In order to get a new state one can do the following:
--
-- MPSRNNRecurrentMatrixState* recurrent0 = nil;
-- [filter encodeToCommandBuffer: cmdBuf
-- sourceMatrix: source0
-- destinationMatrix: destination0
-- recurrentInputState: nil
-- recurrentOutputState: &recurrent0];
--
-- Then use it for the next input in sequence:
--
-- [filter encodeToCommandBuffer: cmdBuf
-- sourceMatrix: source1
-- destinationMatrix: destination1
-- recurrentInputState: recurrent0
-- recurrentOutputState: &recurrent0];
--
-- And discard recurrent output of the third input:
--
-- [filter encodeToCommandBuffer: cmdBuf
-- sourceMatrix: source2
-- destinationMatrix: destination2
-- recurrentInputState: recurrent0
-- recurrentOutputState: nil];
--
-- ObjC selector: @- encodeSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:recurrentInputState:recurrentOutputStates:@
encodeSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_recurrentInputState_recurrentOutputStates :: (IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer, IsNSArray sourceMatrices, IsNSArray destinationMatrices, IsMPSRNNRecurrentMatrixState recurrentInputState, IsNSMutableArray recurrentOutputStates) => mpsrnnMatrixInferenceLayer -> RawId -> sourceMatrices -> Ptr CULong -> destinationMatrices -> Ptr CULong -> recurrentInputState -> recurrentOutputStates -> IO ()
encodeSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_recurrentInputState_recurrentOutputStates mpsrnnMatrixInferenceLayer  commandBuffer sourceMatrices sourceOffsets destinationMatrices destinationOffsets recurrentInputState recurrentOutputStates =
withObjCPtr sourceMatrices $ \raw_sourceMatrices ->
  withObjCPtr destinationMatrices $ \raw_destinationMatrices ->
    withObjCPtr recurrentInputState $ \raw_recurrentInputState ->
      withObjCPtr recurrentOutputStates $ \raw_recurrentOutputStates ->
          sendMsg mpsrnnMatrixInferenceLayer (mkSelector "encodeSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:recurrentInputState:recurrentOutputStates:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrices :: Ptr ()), argPtr sourceOffsets, argPtr (castPtr raw_destinationMatrices :: Ptr ()), argPtr destinationOffsets, argPtr (castPtr raw_recurrentInputState :: Ptr ()), argPtr (castPtr raw_recurrentOutputStates :: Ptr ())]

-- | @- encodeSequenceToCommandBuffer:sourceMatrices:destinationMatrices:recurrentInputState:recurrentOutputStates:@
encodeSequenceToCommandBuffer_sourceMatrices_destinationMatrices_recurrentInputState_recurrentOutputStates :: (IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer, IsNSArray sourceMatrices, IsNSArray destinationMatrices, IsMPSRNNRecurrentMatrixState recurrentInputState, IsNSMutableArray recurrentOutputStates) => mpsrnnMatrixInferenceLayer -> RawId -> sourceMatrices -> destinationMatrices -> recurrentInputState -> recurrentOutputStates -> IO ()
encodeSequenceToCommandBuffer_sourceMatrices_destinationMatrices_recurrentInputState_recurrentOutputStates mpsrnnMatrixInferenceLayer  commandBuffer sourceMatrices destinationMatrices recurrentInputState recurrentOutputStates =
withObjCPtr sourceMatrices $ \raw_sourceMatrices ->
  withObjCPtr destinationMatrices $ \raw_destinationMatrices ->
    withObjCPtr recurrentInputState $ \raw_recurrentInputState ->
      withObjCPtr recurrentOutputStates $ \raw_recurrentOutputStates ->
          sendMsg mpsrnnMatrixInferenceLayer (mkSelector "encodeSequenceToCommandBuffer:sourceMatrices:destinationMatrices:recurrentInputState:recurrentOutputStates:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceMatrices :: Ptr ()), argPtr (castPtr raw_destinationMatrices :: Ptr ()), argPtr (castPtr raw_recurrentInputState :: Ptr ()), argPtr (castPtr raw_recurrentOutputStates :: Ptr ())]

-- | Encode an MPSRNNMatrixInferenceLayer kernel stack for an input matrix sequences into a command buffer bidirectionally.                  The operation proceeds as follows: The first source matrix x0 is passed through all forward traversing layers in the stack,                  ie. those that were initialized with MPSRNNSequenceDirectionForward, recurrent input is assumed zero.                  This produces forward output yf0 and recurrent states hf00, hf01, hf02, ... hf0n, one for each forward layer in the stack.                  Then x1 is passed to forward layers together with recurrent state hf00, hf01, ..., hf0n, which produces yf1, and hf10,...                  This procedure is iterated until the last matrix in the input sequence x_(N-1), which produces forward output yf(N-1).                  The backwards layers iterate the same sequence backwards, starting from input x_(N-1) (recurrent state zero),                  that produces yb(N-1) and recurrent output hb(N-1)0, hf(N-1)1, ... hb(N-1)m, one for each backwards traversing layer.                  Then the backwards layers handle input x_(N-2) using recurrent state hb(N-1)0, ..., et cetera, until the                  first matrix of the sequence is computed, producing output yb0. The result of the operation is either pair of sequences                  ({yf0, yf1, ... , yf(N-1)},  {yb0, yb1, ... , yb(N-1)}) or a combined sequence, {(yf0 + yb0), ... , (yf(N-1) + yb(N-1)) },                  where '+' stands either for sum, or concatenation along feature channels, as specified by bidirectionalCombineMode.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceSequence@ — An array of valid MPSMatrix objects containing the source matrix sequence (x0, x1, ... x_n-1).
--
-- @destinationForwardMatrices@ — An array of valid MPSMatrices to be overwritten by result from forward input matrices. If bidirectionalCombineMode                                                  is either MPSRNNBidirectionalCombineModeAdd or MPSRNNBidirectionalCombineModeConcatenate, then will                                                  contain the combined results. destinationForwardMatrix may not alias with any of the source matrices.
--
-- @destinationBackwardMatrices@ — If bidirectionalCombineMode is MPSRNNBidirectionalCombineModeNone, then must be an array of valid MPSMatrices                                                  that will be overwritten by result from backward input matrices. Otherwise this parameter is ignored                                                  and can be nil. destinationBackwardMatrices may not alias to any of the source matrices.
--
-- ObjC selector: @- encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardMatrices:destinationBackwardMatrices:@
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardMatrices_destinationBackwardMatrices :: (IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer, IsNSArray sourceSequence, IsNSArray destinationForwardMatrices, IsNSArray destinationBackwardMatrices) => mpsrnnMatrixInferenceLayer -> RawId -> sourceSequence -> destinationForwardMatrices -> destinationBackwardMatrices -> IO ()
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardMatrices_destinationBackwardMatrices mpsrnnMatrixInferenceLayer  commandBuffer sourceSequence destinationForwardMatrices destinationBackwardMatrices =
withObjCPtr sourceSequence $ \raw_sourceSequence ->
  withObjCPtr destinationForwardMatrices $ \raw_destinationForwardMatrices ->
    withObjCPtr destinationBackwardMatrices $ \raw_destinationBackwardMatrices ->
        sendMsg mpsrnnMatrixInferenceLayer (mkSelector "encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardMatrices:destinationBackwardMatrices:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceSequence :: Ptr ()), argPtr (castPtr raw_destinationForwardMatrices :: Ptr ()), argPtr (castPtr raw_destinationBackwardMatrices :: Ptr ())]

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSRNNMatrixInferenceLayer
--
-- @device@ — The MTLDevice on which to make the MPSRNNMatrixInferenceLayer
--
-- Returns: A new MPSRNNMatrixInferenceLayer object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer, IsNSCoder aDecoder) => mpsrnnMatrixInferenceLayer -> aDecoder -> RawId -> IO (Id MPSRNNMatrixInferenceLayer)
initWithCoder_device mpsrnnMatrixInferenceLayer  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsrnnMatrixInferenceLayer (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
copyWithZone_device :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> Ptr () -> RawId -> IO (Id MPSRNNMatrixInferenceLayer)
copyWithZone_device mpsrnnMatrixInferenceLayer  zone device =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | inputFeatureChannels
--
-- The number of feature channels input vector/matrix.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> IO CULong
inputFeatureChannels mpsrnnMatrixInferenceLayer  =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "inputFeatureChannels") retCULong []

-- | outputFeatureChannels
--
-- The number of feature channels in the output vector/matrix.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> IO CULong
outputFeatureChannels mpsrnnMatrixInferenceLayer  =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "outputFeatureChannels") retCULong []

-- | numberOfLayers
--
-- Number of layers in the filter-stack. This will be one when using initWithDevice:rnnDescriptor to initialize                  this filter and the number of entries in the array 'rnnDescriptors' when initializing this filter with                  initWithDevice:rnnDescriptors.
--
-- ObjC selector: @- numberOfLayers@
numberOfLayers :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> IO CULong
numberOfLayers mpsrnnMatrixInferenceLayer  =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "numberOfLayers") retCULong []

-- | recurrentOutputIsTemporary
--
-- How output states from encodeSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- recurrentOutputIsTemporary@
recurrentOutputIsTemporary :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> IO Bool
recurrentOutputIsTemporary mpsrnnMatrixInferenceLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsrnnMatrixInferenceLayer (mkSelector "recurrentOutputIsTemporary") retCULong []

-- | recurrentOutputIsTemporary
--
-- How output states from encodeSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- setRecurrentOutputIsTemporary:@
setRecurrentOutputIsTemporary :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> Bool -> IO ()
setRecurrentOutputIsTemporary mpsrnnMatrixInferenceLayer  value =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "setRecurrentOutputIsTemporary:") retVoid [argCULong (if value then 1 else 0)]

-- | storeAllIntermediateStates
--
-- If YES then calls to encodeSequenceToCommandBuffer return every recurrent state              in the array: recurrentOutputStates.              Defaults to NO.
--
-- ObjC selector: @- storeAllIntermediateStates@
storeAllIntermediateStates :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> IO Bool
storeAllIntermediateStates mpsrnnMatrixInferenceLayer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsrnnMatrixInferenceLayer (mkSelector "storeAllIntermediateStates") retCULong []

-- | storeAllIntermediateStates
--
-- If YES then calls to encodeSequenceToCommandBuffer return every recurrent state              in the array: recurrentOutputStates.              Defaults to NO.
--
-- ObjC selector: @- setStoreAllIntermediateStates:@
setStoreAllIntermediateStates :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> Bool -> IO ()
setStoreAllIntermediateStates mpsrnnMatrixInferenceLayer  value =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "setStoreAllIntermediateStates:") retVoid [argCULong (if value then 1 else 0)]

-- | bidirectionalCombineMode
--
-- Defines how to combine the output-results, when encoding bidirectional layers using              encodeBidirectionalSequenceToCommandBuffer.              Defaults to MPSRNNBidirectionalCombineModeNone.
--
-- ObjC selector: @- bidirectionalCombineMode@
bidirectionalCombineMode :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> IO MPSRNNBidirectionalCombineMode
bidirectionalCombineMode mpsrnnMatrixInferenceLayer  =
  fmap (coerce :: CULong -> MPSRNNBidirectionalCombineMode) $ sendMsg mpsrnnMatrixInferenceLayer (mkSelector "bidirectionalCombineMode") retCULong []

-- | bidirectionalCombineMode
--
-- Defines how to combine the output-results, when encoding bidirectional layers using              encodeBidirectionalSequenceToCommandBuffer.              Defaults to MPSRNNBidirectionalCombineModeNone.
--
-- ObjC selector: @- setBidirectionalCombineMode:@
setBidirectionalCombineMode :: IsMPSRNNMatrixInferenceLayer mpsrnnMatrixInferenceLayer => mpsrnnMatrixInferenceLayer -> MPSRNNBidirectionalCombineMode -> IO ()
setBidirectionalCombineMode mpsrnnMatrixInferenceLayer  value =
  sendMsg mpsrnnMatrixInferenceLayer (mkSelector "setBidirectionalCombineMode:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:rnnDescriptor:@
initWithDevice_rnnDescriptorSelector :: Selector
initWithDevice_rnnDescriptorSelector = mkSelector "initWithDevice:rnnDescriptor:"

-- | @Selector@ for @initWithDevice:rnnDescriptors:@
initWithDevice_rnnDescriptorsSelector :: Selector
initWithDevice_rnnDescriptorsSelector = mkSelector "initWithDevice:rnnDescriptors:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:recurrentInputState:recurrentOutputStates:@
encodeSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_recurrentInputState_recurrentOutputStatesSelector :: Selector
encodeSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_recurrentInputState_recurrentOutputStatesSelector = mkSelector "encodeSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:recurrentInputState:recurrentOutputStates:"

-- | @Selector@ for @encodeSequenceToCommandBuffer:sourceMatrices:destinationMatrices:recurrentInputState:recurrentOutputStates:@
encodeSequenceToCommandBuffer_sourceMatrices_destinationMatrices_recurrentInputState_recurrentOutputStatesSelector :: Selector
encodeSequenceToCommandBuffer_sourceMatrices_destinationMatrices_recurrentInputState_recurrentOutputStatesSelector = mkSelector "encodeSequenceToCommandBuffer:sourceMatrices:destinationMatrices:recurrentInputState:recurrentOutputStates:"

-- | @Selector@ for @encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardMatrices:destinationBackwardMatrices:@
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardMatrices_destinationBackwardMatricesSelector :: Selector
encodeBidirectionalSequenceToCommandBuffer_sourceSequence_destinationForwardMatrices_destinationBackwardMatricesSelector = mkSelector "encodeBidirectionalSequenceToCommandBuffer:sourceSequence:destinationForwardMatrices:destinationBackwardMatrices:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @numberOfLayers@
numberOfLayersSelector :: Selector
numberOfLayersSelector = mkSelector "numberOfLayers"

-- | @Selector@ for @recurrentOutputIsTemporary@
recurrentOutputIsTemporarySelector :: Selector
recurrentOutputIsTemporarySelector = mkSelector "recurrentOutputIsTemporary"

-- | @Selector@ for @setRecurrentOutputIsTemporary:@
setRecurrentOutputIsTemporarySelector :: Selector
setRecurrentOutputIsTemporarySelector = mkSelector "setRecurrentOutputIsTemporary:"

-- | @Selector@ for @storeAllIntermediateStates@
storeAllIntermediateStatesSelector :: Selector
storeAllIntermediateStatesSelector = mkSelector "storeAllIntermediateStates"

-- | @Selector@ for @setStoreAllIntermediateStates:@
setStoreAllIntermediateStatesSelector :: Selector
setStoreAllIntermediateStatesSelector = mkSelector "setStoreAllIntermediateStates:"

-- | @Selector@ for @bidirectionalCombineMode@
bidirectionalCombineModeSelector :: Selector
bidirectionalCombineModeSelector = mkSelector "bidirectionalCombineMode"

-- | @Selector@ for @setBidirectionalCombineMode:@
setBidirectionalCombineModeSelector :: Selector
setBidirectionalCombineModeSelector = mkSelector "setBidirectionalCombineMode:"

