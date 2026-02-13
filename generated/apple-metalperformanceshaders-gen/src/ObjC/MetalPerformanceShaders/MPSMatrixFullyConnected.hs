{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixFullyConnected
--
-- This depends on Metal.framework.
--
-- Applies a fully connected neural network layer by performing a              a matrix multiplication, adding a bias vector, scaling, and applying a              neuron activation function.
--
-- A MPSMatrixFullyConnected object computes:
--
-- y = neuron(alpha * x * W + bias)
--
-- y is the output matrix, x and W are input matrices corresponding              to a collection of input vectors and weights respectively, and bias              is a vector which is broadcast and accumulated to each row              of the product.  alpha is a scale factor applied to the product.
--
-- neuron() is a pointwise function applied to the intermediate result.
--
-- Generated bindings for @MPSMatrixFullyConnected@.
module ObjC.MetalPerformanceShaders.MPSMatrixFullyConnected
  ( MPSMatrixFullyConnected
  , IsMPSMatrixFullyConnected(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , sourceOutputFeatureChannels
  , setSourceOutputFeatureChannels
  , alpha
  , setAlpha
  , alphaSelector
  , copyWithZone_deviceSelector
  , encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , neuronTypeSelector
  , setAlphaSelector
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , setSourceInputFeatureChannelsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , setSourceOutputFeatureChannelsSelector
  , sourceInputFeatureChannelsSelector
  , sourceNumberOfFeatureVectorsSelector
  , sourceOutputFeatureChannelsSelector

  -- * Enum types
  , MPSCNNNeuronType(MPSCNNNeuronType)
  , pattern MPSCNNNeuronTypeNone
  , pattern MPSCNNNeuronTypeReLU
  , pattern MPSCNNNeuronTypeLinear
  , pattern MPSCNNNeuronTypeSigmoid
  , pattern MPSCNNNeuronTypeHardSigmoid
  , pattern MPSCNNNeuronTypeTanH
  , pattern MPSCNNNeuronTypeAbsolute
  , pattern MPSCNNNeuronTypeSoftPlus
  , pattern MPSCNNNeuronTypeSoftSign
  , pattern MPSCNNNeuronTypeELU
  , pattern MPSCNNNeuronTypePReLU
  , pattern MPSCNNNeuronTypeReLUN
  , pattern MPSCNNNeuronTypePower
  , pattern MPSCNNNeuronTypeExponential
  , pattern MPSCNNNeuronTypeLogarithm
  , pattern MPSCNNNeuronTypeGeLU
  , pattern MPSCNNNeuronTypeCount

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

-- | Specifies a neuron activation function to be used.
--
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions.  An MPSMatrixFullyConnected kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
--
-- @neuronType@ — Type of neuron activation function. For full list see MPSCNNNeuronType.h
--
-- @parameterA@ — parameterA of neuron activation that is shared across all output values.
--
-- @parameterB@ — parameterB of neuron activation that is shared across all output values.
--
-- @parameterC@ — parameterC of neuron activation that is shared across all output values.
--
-- ObjC selector: @- setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixFullyConnected neuronType parameterA parameterB parameterC =
  sendMessage mpsMatrixFullyConnected setNeuronType_parameterA_parameterB_parameterCSelector neuronType parameterA parameterB parameterC

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO MPSCNNNeuronType
neuronType mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected neuronTypeSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CFloat
neuronParameterA mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected neuronParameterASelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CFloat
neuronParameterB mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected neuronParameterBSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CFloat
neuronParameterC mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected neuronParameterCSelector

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> RawId -> IO (Id MPSMatrixFullyConnected)
initWithDevice mpsMatrixFullyConnected device =
  sendOwnedMessage mpsMatrixFullyConnected initWithDeviceSelector device

-- | Encode a MPSMatrixFullyConnected object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input array.
--
-- @weightMatrix@ — A valid MPSMatrix object which specifies the weight array.
--
-- @biasVector@ — A valid MPSVector object which specifies the bias values, or                              a null object to indicate that no bias is to be applied.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the output array.
--
-- Encodes the operation to the specified command buffer.  resultMatrix              must be large enough to hold a                  MIN(sourceNumberOfInputs,                      inputMatrix.rows - primarySourceMatrixOrigin.x)                  x                  MIN(sourceOutputFeatureChannels,                      weightMatrix.columns - secondarySourceMatrixOrigin.y) array.
--
-- The bias vector must contain at least                  MIN(sourceOutputFeatureChannels, weightMatrix.columns - secondarySourceMatrixOrigin.y) elements.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrix :: (IsMPSMatrixFullyConnected mpsMatrixFullyConnected, IsMPSMatrix inputMatrix, IsMPSMatrix weightMatrix, IsMPSVector biasVector, IsMPSMatrix resultMatrix) => mpsMatrixFullyConnected -> RawId -> inputMatrix -> weightMatrix -> biasVector -> resultMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrix mpsMatrixFullyConnected commandBuffer inputMatrix weightMatrix biasVector resultMatrix =
  sendMessage mpsMatrixFullyConnected encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector commandBuffer (toMPSMatrix inputMatrix) (toMPSMatrix weightMatrix) (toMPSVector biasVector) (toMPSMatrix resultMatrix)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixFullyConnected
--
-- @device@ — The MTLDevice on which to make the MPSMatrixFullyConnected object.
--
-- Returns: A new MPSMatrixFullyConnected object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixFullyConnected mpsMatrixFullyConnected, IsNSCoder aDecoder) => mpsMatrixFullyConnected -> aDecoder -> RawId -> IO (Id MPSMatrixFullyConnected)
initWithCoder_device mpsMatrixFullyConnected aDecoder device =
  sendOwnedMessage mpsMatrixFullyConnected initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Make a copy of this kernel for a new device -
--
-- See: MPSKernel
--
-- @zone@ — The NSZone in which to allocate the object
--
-- @device@ — The device for the new MPSKernel. If nil, then use                          self.device.
--
-- Returns: A pointer to a copy of this MPSKernel. This will fail, returning              nil if the device is not supported. Devices must be              MTLFeatureSet_iOS_GPUFamily2_v1 or later.
--
-- ObjC selector: @- copyWithZone:device:@
copyWithZone_device :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> Ptr () -> RawId -> IO (Id MPSMatrixFullyConnected)
copyWithZone_device mpsMatrixFullyConnected zone device =
  sendOwnedMessage mpsMatrixFullyConnected copyWithZone_deviceSelector zone device

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at primarySourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected sourceNumberOfFeatureVectorsSelector

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at primarySourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixFullyConnected value =
  sendMessage mpsMatrixFullyConnected setSourceNumberOfFeatureVectorsSelector value

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns and the number of rows in the primary (input array) and              secondary (weight array) source matrices respectively.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the input array (beginning at primarySourceMatrixOrigin.y) and all              available rows in the weight array (beginning at secondarySourceMatrixOrigin.x)              should be considered.              Note: The value used in the operation will be              MIN(MIN(inputMatrix.columns - primarySourceMatrixOrigin.y,                      weightMatrix.rows - secondarySourceMatrixOrigin.x),                  sourceInputFeatureChannels)
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CULong
sourceInputFeatureChannels mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected sourceInputFeatureChannelsSelector

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns and the number of rows in the primary (input array) and              secondary (weight array) source matrices respectively.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the input array (beginning at primarySourceMatrixOrigin.y) and all              available rows in the weight array (beginning at secondarySourceMatrixOrigin.x)              should be considered.              Note: The value used in the operation will be              MIN(MIN(inputMatrix.columns - primarySourceMatrixOrigin.y,                      weightMatrix.rows - secondarySourceMatrixOrigin.x),                  sourceInputFeatureChannels)
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixFullyConnected value =
  sendMessage mpsMatrixFullyConnected setSourceInputFeatureChannelsSelector value

-- | sourceOutputFeatureChannels
--
-- The output size to to use in the operation.  This is equivalent to the              number of columns to consider in the weight array, or the secondary source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available output size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the weight array (beginning at secondarySourceMatrixOrigin.y)              should be considered.
--
-- ObjC selector: @- sourceOutputFeatureChannels@
sourceOutputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CULong
sourceOutputFeatureChannels mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected sourceOutputFeatureChannelsSelector

-- | sourceOutputFeatureChannels
--
-- The output size to to use in the operation.  This is equivalent to the              number of columns to consider in the weight array, or the secondary source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available output size is used.              The value of NSUIntegerMax thus indicates that all available              columns in the weight array (beginning at secondarySourceMatrixOrigin.y)              should be considered.
--
-- ObjC selector: @- setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannels :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CULong -> IO ()
setSourceOutputFeatureChannels mpsMatrixFullyConnected value =
  sendMessage mpsMatrixFullyConnected setSourceOutputFeatureChannelsSelector value

-- | alpha
--
-- The scale factor to apply to the product.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> IO CDouble
alpha mpsMatrixFullyConnected =
  sendMessage mpsMatrixFullyConnected alphaSelector

-- | alpha
--
-- The scale factor to apply to the product.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixFullyConnected mpsMatrixFullyConnected => mpsMatrixFullyConnected -> CDouble -> IO ()
setAlpha mpsMatrixFullyConnected value =
  sendMessage mpsMatrixFullyConnected setAlphaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterCSelector :: Selector '[MPSCNNNeuronType, CFloat, CFloat, CFloat] ()
setNeuronType_parameterA_parameterB_parameterCSelector = mkSelector "setNeuronType:parameterA:parameterB:parameterC:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector '[] MPSCNNNeuronType
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector '[] CFloat
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector '[] CFloat
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector '[] CFloat
neuronParameterCSelector = mkSelector "neuronParameterC"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixFullyConnected)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSVector, Id MPSMatrix] ()
encodeToCommandBuffer_inputMatrix_weightMatrix_biasVector_resultMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:weightMatrix:biasVector:resultMatrix:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixFullyConnected)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixFullyConnected)
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectorsSelector :: Selector '[] CULong
sourceNumberOfFeatureVectorsSelector = mkSelector "sourceNumberOfFeatureVectors"

-- | @Selector@ for @setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectorsSelector :: Selector '[CULong] ()
setSourceNumberOfFeatureVectorsSelector = mkSelector "setSourceNumberOfFeatureVectors:"

-- | @Selector@ for @sourceInputFeatureChannels@
sourceInputFeatureChannelsSelector :: Selector '[] CULong
sourceInputFeatureChannelsSelector = mkSelector "sourceInputFeatureChannels"

-- | @Selector@ for @setSourceInputFeatureChannels:@
setSourceInputFeatureChannelsSelector :: Selector '[CULong] ()
setSourceInputFeatureChannelsSelector = mkSelector "setSourceInputFeatureChannels:"

-- | @Selector@ for @sourceOutputFeatureChannels@
sourceOutputFeatureChannelsSelector :: Selector '[] CULong
sourceOutputFeatureChannelsSelector = mkSelector "sourceOutputFeatureChannels"

-- | @Selector@ for @setSourceOutputFeatureChannels:@
setSourceOutputFeatureChannelsSelector :: Selector '[CULong] ()
setSourceOutputFeatureChannelsSelector = mkSelector "setSourceOutputFeatureChannels:"

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

