{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSRNNMatrixTrainingLayer
--
-- This depends on Metal.framework
--
-- The MPSRNNMatrixTrainingLayer specifies a recurrent neural network layer for training on MPSMatrices.
--
-- A MPSRNNMatrixTrainingLayer is initialized using a MPSRNNLayerDescriptor, which further specifies the              recurrent network layer.              The input and output vectors in encode calls are stored as rows of the input and output matrices and              MPSRNNMatrixTrainingLayer supports matrices with decreasing number of rows: The row-indices identify the different              sequences that may be of different lengths - for example if we have three sequences:                  ( x1, x2, x3 ), ( y1, y2, y3, y4 ) and ( z1, z2 )              of vectors xi, yi and zi, then these can be inserted together as a batch to the sequence encoding kernel by              using the matrices:
--
-- ( y1 )        ( y2 )        ( y3 )        ( y4 )
-- m1 = ( x1 ),  m2 = ( x2 ),  m3 = ( x3 ),  m4 =
-- ( z1 )        ( z2 )
--
-- The gradient computation pass is then achieved by passing the corresponding gradient sequence from the              previous layer ( dx1, dx2, dx3 ), ( dy1, dy2, dy3, dy4 ) and ( dz1, dz2 ) as matrices
--
-- ( dy1 )         ( dy2 )         ( dy3 )         ( dy4 )
-- dm1 = ( dx1 ),  dm2 = ( dx2 ),  dm3 = ( dx3 ),  dm4 =
-- ( dz1 )         ( dz2 )
--
-- The mathematical operation described in the linear transformations of MPSRNNSingleGateDescriptor              MPSLSTMDescriptor and MPSGRUDescriptor are y^T = W x^T  <=> y = x W^T, where x is the matrix containing              the input vectors as rows, y is the matrix containing the output vectors as rows and W is the weight matrix.
--
-- Generated bindings for @MPSRNNMatrixTrainingLayer@.
module ObjC.MetalPerformanceShaders.MPSRNNMatrixTrainingLayer
  ( MPSRNNMatrixTrainingLayer
  , IsMPSRNNMatrixTrainingLayer(..)
  , initWithDevice_rnnDescriptor_trainableWeights
  , createWeightGradientMatrices_dataType
  , createTemporaryWeightGradientMatrices_dataType_commandBuffer
  , createWeightMatrices
  , initWithDevice
  , encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weights
  , encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weights
  , encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weights
  , encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weights
  , initWithCoder_device
  , copyWithZone_device
  , inputFeatureChannels
  , outputFeatureChannels
  , storeAllIntermediateStates
  , setStoreAllIntermediateStates
  , recurrentOutputIsTemporary
  , setRecurrentOutputIsTemporary
  , trainingStateIsTemporary
  , setTrainingStateIsTemporary
  , accumulateWeightGradients
  , setAccumulateWeightGradients
  , accumulateWeightGradientsSelector
  , copyWithZone_deviceSelector
  , createTemporaryWeightGradientMatrices_dataType_commandBufferSelector
  , createWeightGradientMatrices_dataTypeSelector
  , createWeightMatricesSelector
  , encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weightsSelector
  , encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector
  , encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector
  , encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weightsSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_rnnDescriptor_trainableWeightsSelector
  , inputFeatureChannelsSelector
  , outputFeatureChannelsSelector
  , recurrentOutputIsTemporarySelector
  , setAccumulateWeightGradientsSelector
  , setRecurrentOutputIsTemporarySelector
  , setStoreAllIntermediateStatesSelector
  , setTrainingStateIsTemporarySelector
  , storeAllIntermediateStatesSelector
  , trainingStateIsTemporarySelector

  -- * Enum types
  , MPSDataType(MPSDataType)
  , pattern MPSDataTypeInvalid
  , pattern MPSDataTypeFloatBit
  , pattern MPSDataTypeFloat32
  , pattern MPSDataTypeFloat16
  , pattern MPSDataTypeComplexBit
  , pattern MPSDataTypeComplexFloat32
  , pattern MPSDataTypeComplexFloat16
  , pattern MPSDataTypeSignedBit
  , pattern MPSDataTypeIntBit
  , pattern MPSDataTypeInt2
  , pattern MPSDataTypeInt4
  , pattern MPSDataTypeInt8
  , pattern MPSDataTypeInt16
  , pattern MPSDataTypeInt32
  , pattern MPSDataTypeInt64
  , pattern MPSDataTypeUInt2
  , pattern MPSDataTypeUInt4
  , pattern MPSDataTypeUInt8
  , pattern MPSDataTypeUInt16
  , pattern MPSDataTypeUInt32
  , pattern MPSDataTypeUInt64
  , pattern MPSDataTypeAlternateEncodingBit
  , pattern MPSDataTypeBool
  , pattern MPSDataTypeBFloat16
  , pattern MPSDataTypeNormalizedBit
  , pattern MPSDataTypeUnorm1
  , pattern MPSDataTypeUnorm8
  , MPSRNNMatrixId(MPSRNNMatrixId)
  , pattern MPSRNNMatrixIdSingleGateInputWeights
  , pattern MPSRNNMatrixIdSingleGateRecurrentWeights
  , pattern MPSRNNMatrixIdSingleGateBiasTerms
  , pattern MPSRNNMatrixIdLSTMInputGateInputWeights
  , pattern MPSRNNMatrixIdLSTMInputGateRecurrentWeights
  , pattern MPSRNNMatrixIdLSTMInputGateMemoryWeights
  , pattern MPSRNNMatrixIdLSTMInputGateBiasTerms
  , pattern MPSRNNMatrixIdLSTMForgetGateInputWeights
  , pattern MPSRNNMatrixIdLSTMForgetGateRecurrentWeights
  , pattern MPSRNNMatrixIdLSTMForgetGateMemoryWeights
  , pattern MPSRNNMatrixIdLSTMForgetGateBiasTerms
  , pattern MPSRNNMatrixIdLSTMMemoryGateInputWeights
  , pattern MPSRNNMatrixIdLSTMMemoryGateRecurrentWeights
  , pattern MPSRNNMatrixIdLSTMMemoryGateMemoryWeights
  , pattern MPSRNNMatrixIdLSTMMemoryGateBiasTerms
  , pattern MPSRNNMatrixIdLSTMOutputGateInputWeights
  , pattern MPSRNNMatrixIdLSTMOutputGateRecurrentWeights
  , pattern MPSRNNMatrixIdLSTMOutputGateMemoryWeights
  , pattern MPSRNNMatrixIdLSTMOutputGateBiasTerms
  , pattern MPSRNNMatrixIdGRUInputGateInputWeights
  , pattern MPSRNNMatrixIdGRUInputGateRecurrentWeights
  , pattern MPSRNNMatrixIdGRUInputGateBiasTerms
  , pattern MPSRNNMatrixIdGRURecurrentGateInputWeights
  , pattern MPSRNNMatrixIdGRURecurrentGateRecurrentWeights
  , pattern MPSRNNMatrixIdGRURecurrentGateBiasTerms
  , pattern MPSRNNMatrixIdGRUOutputGateInputWeights
  , pattern MPSRNNMatrixIdGRUOutputGateRecurrentWeights
  , pattern MPSRNNMatrixIdGRUOutputGateInputGateWeights
  , pattern MPSRNNMatrixIdGRUOutputGateBiasTerms
  , pattern MPSRNNMatrixId_count

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

-- | Initializes a linear (fully connected) RNN kernel for training
--
-- @device@ — The MTLDevice on which this MPSRNNMatrixLayer filter will be used
--
-- @rnnDescriptor@ — The descriptor that defines the RNN layer
--
-- @trainableWeights@ — An array where to store the weights of the layer as MPSMatrices.                                          NOTE: The exact layout and number of matrices may vary between                                          platforms and therefore you should not save out these weights directly,                                          but instead use the function encodeCopyWeightsToCommandBuffer to identify                                          the weights and biases for serialization.                                          Typically you should pass here an initialized but empty NSMutableArray and                                          when this function returns the array will have been populated with the                                          weight matrices needed in the encode-calls, by using initial values from                                          the datasources in rnnDescriptor.
--
-- Returns: A valid MPSRNNMatrixTrainingLayer object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:rnnDescriptor:trainableWeights:@
initWithDevice_rnnDescriptor_trainableWeights :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSMutableArray trainableWeights) => mpsrnnMatrixTrainingLayer -> RawId -> Const (Id MPSRNNDescriptor) -> trainableWeights -> IO (Id MPSRNNMatrixTrainingLayer)
initWithDevice_rnnDescriptor_trainableWeights mpsrnnMatrixTrainingLayer device rnnDescriptor trainableWeights =
  sendOwnedMessage mpsrnnMatrixTrainingLayer initWithDevice_rnnDescriptor_trainableWeightsSelector device rnnDescriptor (toNSMutableArray trainableWeights)

-- | Initializes a set of matrices that can be used in training for weight and bias gradient outputs in
--
-- See: encodeBackwardSequenceToCommandBuffer. Can be also used to easily create auxiliary matrices for example              for ADAM and other advanced optimization schemes. The layout and number of matrices is the same as for the outputs of
--
-- See: initWithDevice, but the data type may differ. NOTE: These matrices cannot be used as weight matrices in the              forward and backward encode calls, but matrices from initWithDevice() or createWeightMatrices() should be used instead.
--
-- @matricesOut@ — An array where the newly created matrices will be stored, will be initialized to zero.
--
-- @dataType@ — Datatype for the entries - currently MPSDataTypeFloat32 and MPSDataTypeFloat16 are supported.
--
-- ObjC selector: @- createWeightGradientMatrices:dataType:@
createWeightGradientMatrices_dataType :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSMutableArray matricesOut) => mpsrnnMatrixTrainingLayer -> matricesOut -> MPSDataType -> IO ()
createWeightGradientMatrices_dataType mpsrnnMatrixTrainingLayer matricesOut dataType =
  sendMessage mpsrnnMatrixTrainingLayer createWeightGradientMatrices_dataTypeSelector (toNSMutableArray matricesOut) dataType

-- | As createWeightGradientMatrices, but the matrices will be temporary with readCount = 1, which means that they              become invalid after the first encode call that reads them. Note also that as the matrices are temporary, their              storage mode will be private which means that you can only access the data using a kernel on the GPU.
--
-- @matricesOut@ — An array where the newly created matrices will be stored, will be initialized to zero.
--
-- @dataType@ — Datatype for the entries - currently MPSDataTypeFloat32 and MPSDataTypeFloat16 are supported.
--
-- @commandBuffer@ — The command buffer that the temporary matrices will live on.
--
-- ObjC selector: @- createTemporaryWeightGradientMatrices:dataType:commandBuffer:@
createTemporaryWeightGradientMatrices_dataType_commandBuffer :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSMutableArray matricesOut) => mpsrnnMatrixTrainingLayer -> matricesOut -> MPSDataType -> RawId -> IO ()
createTemporaryWeightGradientMatrices_dataType_commandBuffer mpsrnnMatrixTrainingLayer matricesOut dataType commandBuffer =
  sendMessage mpsrnnMatrixTrainingLayer createTemporaryWeightGradientMatrices_dataType_commandBufferSelector (toNSMutableArray matricesOut) dataType commandBuffer

-- | Initializes a set of matrices that can be used in training for weight and bias matrices in              the forward and backward passes. The layout, datatype and number of matrices is the same as for the outputs of
--
-- See: initWithDevice.
--
-- @matricesOut@ — An array where the newly created matrices will be stored, will be initialized to zero.
--
-- ObjC selector: @- createWeightMatrices:@
createWeightMatrices :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSMutableArray matricesOut) => mpsrnnMatrixTrainingLayer -> matricesOut -> IO ()
createWeightMatrices mpsrnnMatrixTrainingLayer matricesOut =
  sendMessage mpsrnnMatrixTrainingLayer createWeightMatricesSelector (toNSMutableArray matricesOut)

-- | @- initWithDevice:@
initWithDevice :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> RawId -> IO (Id MPSRNNMatrixTrainingLayer)
initWithDevice mpsrnnMatrixTrainingLayer device =
  sendOwnedMessage mpsrnnMatrixTrainingLayer initWithDeviceSelector device

-- | Encode an MPSRNNMatrixTrainingLayer forward pass kernel for a sequence of inputs into a command buffer.
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
-- @trainingStates@ — An array containing the training states to be passed to the gradient computation                                                  encode function.
--
-- @recurrentInputState@ — An optional state containing the output matrices and memory cells (for LSTMs)                                                  of the layer obtained from the previous input matrices in a sequence of inputs.                                                  Has to be the output of a previous call to this function or nil (assumed zero).
--
-- @recurrentOutputStates@ — An array that will be appended with the recurrent output states. May not be nil.                                                  If recurrentOutputIsTemporary is YES and then all returned recurrent states                                                  will be temporary.
--
-- See: MPSState:isTemporary.
--
-- @weights@ — An array of valid MPSMatrix objects containing the weights, should be the array                                                  that was produced either by
--
-- See: initWithDevice or
--
-- See: createWeightMatrices.
--
-- ObjC selector: @- encodeForwardSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:trainingStates:recurrentInputState:recurrentOutputStates:weights:@
encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weights :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSArray sourceMatrices, IsNSArray destinationMatrices, IsNSMutableArray trainingStates, IsMPSRNNRecurrentMatrixState recurrentInputState, IsNSMutableArray recurrentOutputStates, IsNSArray weights) => mpsrnnMatrixTrainingLayer -> RawId -> sourceMatrices -> Ptr CULong -> destinationMatrices -> Ptr CULong -> trainingStates -> recurrentInputState -> recurrentOutputStates -> weights -> IO ()
encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weights mpsrnnMatrixTrainingLayer commandBuffer sourceMatrices sourceOffsets destinationMatrices destinationOffsets trainingStates recurrentInputState recurrentOutputStates weights =
  sendMessage mpsrnnMatrixTrainingLayer encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector commandBuffer (toNSArray sourceMatrices) sourceOffsets (toNSArray destinationMatrices) destinationOffsets (toNSMutableArray trainingStates) (toMPSRNNRecurrentMatrixState recurrentInputState) (toNSMutableArray recurrentOutputStates) (toNSArray weights)

-- | Encode an MPSRNNMatrixTrainingLayer forward pass kernel for a sequence of inputs into a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @sourceMatrices@ — An array of valid MPSMatrix objects containing the sequence of source matrices.
--
-- @destinationMatrices@ — An array valid MPSMatrices to be overwritten by result matrix sequence.                                                  destinationMatrices may not alias sourceMatrices.
--
-- @trainingStates@ — An array containing the training states to be passed to the gradient computation                                                  encode function.
--
-- @weights@ — An array of valid MPSMatrix objects containing the weights, should be the array                                                  that was produced either by
--
-- See: initWithDevice or
--
-- See: createWeightMatrices.
--
-- ObjC selector: @- encodeForwardSequenceToCommandBuffer:sourceMatrices:destinationMatrices:trainingStates:weights:@
encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weights :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSArray sourceMatrices, IsNSArray destinationMatrices, IsNSMutableArray trainingStates, IsNSArray weights) => mpsrnnMatrixTrainingLayer -> RawId -> sourceMatrices -> destinationMatrices -> trainingStates -> weights -> IO ()
encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weights mpsrnnMatrixTrainingLayer commandBuffer sourceMatrices destinationMatrices trainingStates weights =
  sendMessage mpsrnnMatrixTrainingLayer encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weightsSelector commandBuffer (toNSArray sourceMatrices) (toNSArray destinationMatrices) (toNSMutableArray trainingStates) (toNSArray weights)

-- | Encode an MPSRNNMatrixTrainingLayer gradient pass kernel for a sequence of input gradients into a command buffer.              NOTE: The time sequence indexing follows the array indexing in the inputs: sourceGradients[0] has to contain the              gradients corresponding to the first matrix in the forward pass corresponding to the current subsequence, which is              typically sourceMatrices[0].
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @forwardSources@ — An array of MPSMatrix objects containing the sequence of source matrices of the forward pass.
--
-- @forwardSourceOffsets@ — An array of byte-offsets into the forwardSources, if nil zeros are assumed and                                                  if not nil must contain offset for every matrix in forwardSources.
--
-- @sourceGradients@ — An array of valid MPSMatrix objects containing the sequence of source gradient matrices.
--
-- @sourceGradientOffsets@ — An array of byte-offsets into the sourceGradients, if nil zeros are assumed and                                                  if not nil must contain offset for every matrix in sourceGradients.
--
-- @destinationGradients@ — An array valid MPSMatrix objects that will receive the backpropagated gradients, may be                                                  nil if not needed (for example first layer in graph).
--
-- @destinationOffsets@ — An array of byte-offsets into the destinationGradients, if nil zeros are assumed and                                                  if not nil must contain offset for every matrix in destinationGradients.
--
-- @weightGradients@ — An array of valid MPSMatrix objects that will receive the gradient wrt. weights and                                                  biases of the layer - should be the array that was produced either                                                  by
--
-- See: initWithDevice or
--
-- See: createWeightMatrices. May be nil in which case                                                  the gradients for the weights are not computed.
--
-- @trainingStates@ — An array containing the training states from the forward pass - the array must contain                                                  the states corresponding to the input gradients is sourceGradients.
--
-- @recurrentInputState@ — An optional state containing the output matrices and memory cells (for LSTMs)                                                  of the layer obtained from the previous input gradients in a sequence of inputs.                                                  Has to be the output of a previous call to this function or nil (assumed zero).
--
-- @recurrentOutputStates@ — An array that will be appended with the recurrent output states. Can be nil.                                                  If recurrentOutputIsTemporary is YES and then all returned recurrent states                                                  will be temporary.
--
-- See: MPSState:isTemporary.
--
-- @weights@ — An array of valid MPSMatrix objects containing the weights, should be the array                                                  that was produced either by
--
-- See: initWithDevice or
--
-- See: createWeightMatrices.
--
-- ObjC selector: @- encodeGradientSequenceToCommandBuffer:forwardSources:forwardSourceOffsets:sourceGradients:sourceGradientOffsets:destinationGradients:destinationOffsets:weightGradients:trainingStates:recurrentInputState:recurrentOutputStates:weights:@
encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weights :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSArray forwardSources, IsNSArray sourceGradients, IsNSArray destinationGradients, IsNSArray weightGradients, IsNSArray trainingStates, IsMPSRNNRecurrentMatrixState recurrentInputState, IsNSMutableArray recurrentOutputStates, IsNSArray weights) => mpsrnnMatrixTrainingLayer -> RawId -> forwardSources -> Ptr CULong -> sourceGradients -> Ptr CULong -> destinationGradients -> Ptr CULong -> weightGradients -> trainingStates -> recurrentInputState -> recurrentOutputStates -> weights -> IO ()
encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weights mpsrnnMatrixTrainingLayer commandBuffer forwardSources forwardSourceOffsets sourceGradients sourceGradientOffsets destinationGradients destinationOffsets weightGradients trainingStates recurrentInputState recurrentOutputStates weights =
  sendMessage mpsrnnMatrixTrainingLayer encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector commandBuffer (toNSArray forwardSources) forwardSourceOffsets (toNSArray sourceGradients) sourceGradientOffsets (toNSArray destinationGradients) destinationOffsets (toNSArray weightGradients) (toNSArray trainingStates) (toMPSRNNRecurrentMatrixState recurrentInputState) (toNSMutableArray recurrentOutputStates) (toNSArray weights)

-- | Encode an MPSRNNMatrixTrainingLayer gradient pass kernel for a sequence of input gradients into a command buffer.              NOTE: The time sequence indexing follows the array indexing in the inputs: sourceGradients[0] has to contain the              gradients corresponding to the first matrix in the forward pass corresponding to the current subsequence, which is              typically sourceMatrices[0].
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded filter
--
-- @forwardSources@ — An array of MPSMatrix objects containing the sequence of source matrices of the forward pass.
--
-- @sourceGradients@ — An array of MPSMatrix objects containing the sequence of source gradient matrices.
--
-- @destinationGradients@ — An array valid MPSMatrix objects that will receive the backpropagated gradients, may be                                                  nil if not needed (for example first layer in graph).
--
-- @weightGradients@ — An array valid MPSMatrix objects that will receive the gradient wrt. weights and                                                  biases of the layer - should be the array that was produced either                                                  by
--
-- See: initWithDevice or
--
-- See: createWeightMatrices. May be nil in which case                                                  the gradients for the weights are not computed.                                                  NOTE: The weight gradients are accumulated on top of existing values so
--
-- @trainingStates@ — An array containing the training states from the forward pass - the array must contain                                                  the states corresponding to the input gradients is sourceGradients.
--
-- @weights@ — An array of valid MPSMatrix objects containing the weights, should be the array                                                  that was produced either by
--
-- See: initWithDevice or
--
-- See: createWeightMatrices.
--
-- ObjC selector: @- encodeGradientSequenceToCommandBuffer:forwardSources:sourceGradients:destinationGradients:weightGradients:trainingStates:weights:@
encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weights :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSArray forwardSources, IsNSArray sourceGradients, IsNSArray destinationGradients, IsNSArray weightGradients, IsNSArray trainingStates, IsNSArray weights) => mpsrnnMatrixTrainingLayer -> RawId -> forwardSources -> sourceGradients -> destinationGradients -> weightGradients -> trainingStates -> weights -> IO ()
encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weights mpsrnnMatrixTrainingLayer commandBuffer forwardSources sourceGradients destinationGradients weightGradients trainingStates weights =
  sendMessage mpsrnnMatrixTrainingLayer encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weightsSelector commandBuffer (toNSArray forwardSources) (toNSArray sourceGradients) (toNSArray destinationGradients) (toNSArray weightGradients) (toNSArray trainingStates) (toNSArray weights)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSRNNMatrixTrainingLayer
--
-- @device@ — The MTLDevice on which to make the MPSRNNMatrixTrainingLayer
--
-- Returns: A new MPSRNNMatrixTrainingLayer object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer, IsNSCoder aDecoder) => mpsrnnMatrixTrainingLayer -> aDecoder -> RawId -> IO (Id MPSRNNMatrixTrainingLayer)
initWithCoder_device mpsrnnMatrixTrainingLayer aDecoder device =
  sendOwnedMessage mpsrnnMatrixTrainingLayer initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
copyWithZone_device :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> Ptr () -> RawId -> IO (Id MPSRNNMatrixTrainingLayer)
copyWithZone_device mpsrnnMatrixTrainingLayer zone device =
  sendOwnedMessage mpsrnnMatrixTrainingLayer copyWithZone_deviceSelector zone device

-- | inputFeatureChannels
--
-- The number of feature channels input vector/matrix.
--
-- ObjC selector: @- inputFeatureChannels@
inputFeatureChannels :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> IO CULong
inputFeatureChannels mpsrnnMatrixTrainingLayer =
  sendMessage mpsrnnMatrixTrainingLayer inputFeatureChannelsSelector

-- | outputFeatureChannels
--
-- The number of feature channels in the output vector/matrix.
--
-- ObjC selector: @- outputFeatureChannels@
outputFeatureChannels :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> IO CULong
outputFeatureChannels mpsrnnMatrixTrainingLayer =
  sendMessage mpsrnnMatrixTrainingLayer outputFeatureChannelsSelector

-- | storeAllIntermediateStates
--
-- If YES then calls to functions encodeForwardSequenceToCommandBuffer and              encodeGradientSequenceToCommandBuffer return every recurrent state              in the array: recurrentOutputStates.              Defaults to NO.
--
-- ObjC selector: @- storeAllIntermediateStates@
storeAllIntermediateStates :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> IO Bool
storeAllIntermediateStates mpsrnnMatrixTrainingLayer =
  sendMessage mpsrnnMatrixTrainingLayer storeAllIntermediateStatesSelector

-- | storeAllIntermediateStates
--
-- If YES then calls to functions encodeForwardSequenceToCommandBuffer and              encodeGradientSequenceToCommandBuffer return every recurrent state              in the array: recurrentOutputStates.              Defaults to NO.
--
-- ObjC selector: @- setStoreAllIntermediateStates:@
setStoreAllIntermediateStates :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> Bool -> IO ()
setStoreAllIntermediateStates mpsrnnMatrixTrainingLayer value =
  sendMessage mpsrnnMatrixTrainingLayer setStoreAllIntermediateStatesSelector value

-- | recurrentOutputIsTemporary
--
-- How recurrent output states from encodeForwardSequenceToCommandBuffer              and encodeGradientSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- recurrentOutputIsTemporary@
recurrentOutputIsTemporary :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> IO Bool
recurrentOutputIsTemporary mpsrnnMatrixTrainingLayer =
  sendMessage mpsrnnMatrixTrainingLayer recurrentOutputIsTemporarySelector

-- | recurrentOutputIsTemporary
--
-- How recurrent output states from encodeForwardSequenceToCommandBuffer              and encodeGradientSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- setRecurrentOutputIsTemporary:@
setRecurrentOutputIsTemporary :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> Bool -> IO ()
setRecurrentOutputIsTemporary mpsrnnMatrixTrainingLayer value =
  sendMessage mpsrnnMatrixTrainingLayer setRecurrentOutputIsTemporarySelector value

-- | trainingStateIsTemporary
--
-- How training output states from encodeForwardSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- trainingStateIsTemporary@
trainingStateIsTemporary :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> IO Bool
trainingStateIsTemporary mpsrnnMatrixTrainingLayer =
  sendMessage mpsrnnMatrixTrainingLayer trainingStateIsTemporarySelector

-- | trainingStateIsTemporary
--
-- How training output states from encodeForwardSequenceToCommandBuffer are constructed.              Defaults to NO. For reference
--
-- See: MPSState.
--
-- ObjC selector: @- setTrainingStateIsTemporary:@
setTrainingStateIsTemporary :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> Bool -> IO ()
setTrainingStateIsTemporary mpsrnnMatrixTrainingLayer value =
  sendMessage mpsrnnMatrixTrainingLayer setTrainingStateIsTemporarySelector value

-- | accumulateWeightGradients
--
-- If yes then the computed weight gradients are accumulated on top of existing values in              calls to the gradient computation functions: encodeGradientSequenceToCommandBuffer.              Defaults to NO.
--
-- ObjC selector: @- accumulateWeightGradients@
accumulateWeightGradients :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> IO Bool
accumulateWeightGradients mpsrnnMatrixTrainingLayer =
  sendMessage mpsrnnMatrixTrainingLayer accumulateWeightGradientsSelector

-- | accumulateWeightGradients
--
-- If yes then the computed weight gradients are accumulated on top of existing values in              calls to the gradient computation functions: encodeGradientSequenceToCommandBuffer.              Defaults to NO.
--
-- ObjC selector: @- setAccumulateWeightGradients:@
setAccumulateWeightGradients :: IsMPSRNNMatrixTrainingLayer mpsrnnMatrixTrainingLayer => mpsrnnMatrixTrainingLayer -> Bool -> IO ()
setAccumulateWeightGradients mpsrnnMatrixTrainingLayer value =
  sendMessage mpsrnnMatrixTrainingLayer setAccumulateWeightGradientsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:rnnDescriptor:trainableWeights:@
initWithDevice_rnnDescriptor_trainableWeightsSelector :: Selector '[RawId, Const (Id MPSRNNDescriptor), Id NSMutableArray] (Id MPSRNNMatrixTrainingLayer)
initWithDevice_rnnDescriptor_trainableWeightsSelector = mkSelector "initWithDevice:rnnDescriptor:trainableWeights:"

-- | @Selector@ for @createWeightGradientMatrices:dataType:@
createWeightGradientMatrices_dataTypeSelector :: Selector '[Id NSMutableArray, MPSDataType] ()
createWeightGradientMatrices_dataTypeSelector = mkSelector "createWeightGradientMatrices:dataType:"

-- | @Selector@ for @createTemporaryWeightGradientMatrices:dataType:commandBuffer:@
createTemporaryWeightGradientMatrices_dataType_commandBufferSelector :: Selector '[Id NSMutableArray, MPSDataType, RawId] ()
createTemporaryWeightGradientMatrices_dataType_commandBufferSelector = mkSelector "createTemporaryWeightGradientMatrices:dataType:commandBuffer:"

-- | @Selector@ for @createWeightMatrices:@
createWeightMatricesSelector :: Selector '[Id NSMutableArray] ()
createWeightMatricesSelector = mkSelector "createWeightMatrices:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSRNNMatrixTrainingLayer)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeForwardSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:trainingStates:recurrentInputState:recurrentOutputStates:weights:@
encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector :: Selector '[RawId, Id NSArray, Ptr CULong, Id NSArray, Ptr CULong, Id NSMutableArray, Id MPSRNNRecurrentMatrixState, Id NSMutableArray, Id NSArray] ()
encodeForwardSequenceToCommandBuffer_sourceMatrices_sourceOffsets_destinationMatrices_destinationOffsets_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector = mkSelector "encodeForwardSequenceToCommandBuffer:sourceMatrices:sourceOffsets:destinationMatrices:destinationOffsets:trainingStates:recurrentInputState:recurrentOutputStates:weights:"

-- | @Selector@ for @encodeForwardSequenceToCommandBuffer:sourceMatrices:destinationMatrices:trainingStates:weights:@
encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weightsSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id NSMutableArray, Id NSArray] ()
encodeForwardSequenceToCommandBuffer_sourceMatrices_destinationMatrices_trainingStates_weightsSelector = mkSelector "encodeForwardSequenceToCommandBuffer:sourceMatrices:destinationMatrices:trainingStates:weights:"

-- | @Selector@ for @encodeGradientSequenceToCommandBuffer:forwardSources:forwardSourceOffsets:sourceGradients:sourceGradientOffsets:destinationGradients:destinationOffsets:weightGradients:trainingStates:recurrentInputState:recurrentOutputStates:weights:@
encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector :: Selector '[RawId, Id NSArray, Ptr CULong, Id NSArray, Ptr CULong, Id NSArray, Ptr CULong, Id NSArray, Id NSArray, Id MPSRNNRecurrentMatrixState, Id NSMutableArray, Id NSArray] ()
encodeGradientSequenceToCommandBuffer_forwardSources_forwardSourceOffsets_sourceGradients_sourceGradientOffsets_destinationGradients_destinationOffsets_weightGradients_trainingStates_recurrentInputState_recurrentOutputStates_weightsSelector = mkSelector "encodeGradientSequenceToCommandBuffer:forwardSources:forwardSourceOffsets:sourceGradients:sourceGradientOffsets:destinationGradients:destinationOffsets:weightGradients:trainingStates:recurrentInputState:recurrentOutputStates:weights:"

-- | @Selector@ for @encodeGradientSequenceToCommandBuffer:forwardSources:sourceGradients:destinationGradients:weightGradients:trainingStates:weights:@
encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weightsSelector :: Selector '[RawId, Id NSArray, Id NSArray, Id NSArray, Id NSArray, Id NSArray, Id NSArray] ()
encodeGradientSequenceToCommandBuffer_forwardSources_sourceGradients_destinationGradients_weightGradients_trainingStates_weightsSelector = mkSelector "encodeGradientSequenceToCommandBuffer:forwardSources:sourceGradients:destinationGradients:weightGradients:trainingStates:weights:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSRNNMatrixTrainingLayer)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSRNNMatrixTrainingLayer)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @inputFeatureChannels@
inputFeatureChannelsSelector :: Selector '[] CULong
inputFeatureChannelsSelector = mkSelector "inputFeatureChannels"

-- | @Selector@ for @outputFeatureChannels@
outputFeatureChannelsSelector :: Selector '[] CULong
outputFeatureChannelsSelector = mkSelector "outputFeatureChannels"

-- | @Selector@ for @storeAllIntermediateStates@
storeAllIntermediateStatesSelector :: Selector '[] Bool
storeAllIntermediateStatesSelector = mkSelector "storeAllIntermediateStates"

-- | @Selector@ for @setStoreAllIntermediateStates:@
setStoreAllIntermediateStatesSelector :: Selector '[Bool] ()
setStoreAllIntermediateStatesSelector = mkSelector "setStoreAllIntermediateStates:"

-- | @Selector@ for @recurrentOutputIsTemporary@
recurrentOutputIsTemporarySelector :: Selector '[] Bool
recurrentOutputIsTemporarySelector = mkSelector "recurrentOutputIsTemporary"

-- | @Selector@ for @setRecurrentOutputIsTemporary:@
setRecurrentOutputIsTemporarySelector :: Selector '[Bool] ()
setRecurrentOutputIsTemporarySelector = mkSelector "setRecurrentOutputIsTemporary:"

-- | @Selector@ for @trainingStateIsTemporary@
trainingStateIsTemporarySelector :: Selector '[] Bool
trainingStateIsTemporarySelector = mkSelector "trainingStateIsTemporary"

-- | @Selector@ for @setTrainingStateIsTemporary:@
setTrainingStateIsTemporarySelector :: Selector '[Bool] ()
setTrainingStateIsTemporarySelector = mkSelector "setTrainingStateIsTemporary:"

-- | @Selector@ for @accumulateWeightGradients@
accumulateWeightGradientsSelector :: Selector '[] Bool
accumulateWeightGradientsSelector = mkSelector "accumulateWeightGradients"

-- | @Selector@ for @setAccumulateWeightGradients:@
setAccumulateWeightGradientsSelector :: Selector '[Bool] ()
setAccumulateWeightGradientsSelector = mkSelector "setAccumulateWeightGradients:"

