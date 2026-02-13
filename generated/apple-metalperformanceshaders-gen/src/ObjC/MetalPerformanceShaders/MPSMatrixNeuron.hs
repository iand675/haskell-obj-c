{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixNeuron
--
-- This depends on Metal.framework.
--
-- A neuron activation kernel that operates on matrices.
--
-- A MPSMatrixNeuron object computes:
--
-- y = neuron(alpha * x + bias)
--
-- y is the output matrix, x is the input matrix corresponding              to a collection of input vectors and bias is a vector which is broadcast              and accumulated to each row of the intermediate result.              alpha is a scale factor applied to the input.
--
-- neuron() defines the pointwise function that is applied to the intermediate result.
--
-- Note: This function computes the same result as MPSMatrixFullyConnected that has                      unit weight matrix.
--
-- Generated bindings for @MPSMatrixNeuron@.
module ObjC.MetalPerformanceShaders.MPSMatrixNeuron
  ( MPSMatrixNeuron
  , IsMPSMatrixNeuron(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , setNeuronToPReLUWithParametersA
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_biasVector_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , alpha
  , setAlpha
  , alphaSelector
  , copyWithZone_deviceSelector
  , encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , neuronTypeSelector
  , setAlphaSelector
  , setNeuronToPReLUWithParametersASelector
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , setSourceInputFeatureChannelsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , sourceInputFeatureChannelsSelector
  , sourceNumberOfFeatureVectorsSelector

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
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. For those kind of neuron activation functions,              use appropriate setter functions.  An MPSMatrixNeuron kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
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
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixNeuron neuronType parameterA parameterB parameterC =
  sendMessage mpsMatrixNeuron setNeuronType_parameterA_parameterB_parameterCSelector neuronType parameterA parameterB parameterC

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO MPSCNNNeuronType
neuronType mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron neuronTypeSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CFloat
neuronParameterA mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron neuronParameterASelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CFloat
neuronParameterB mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron neuronParameterBSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CFloat
neuronParameterC mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron neuronParameterCSelector

-- | Add per output value neuron parameters A for PReLu neuron activation functions.
--
-- This method sets the neuron to PReLU, zeros parameters A and B and sets the per output value              neuron parameters A to an array containing a unique value of A for each output value.
--
-- If the neuron function is f(v,a,b), it will apply
--
-- resultMatrix(i, j) = f( input(i, j), A[j], B[j] )                  where j in [0, sourceInputFeatureChannels]
--
-- See https://arxiv.org/pdf/1502.01852.pdf for details.
--
-- All other neuron types, where parameter A              and parameter B are shared across output values must be set using              -setNeuronType:parameterA:parameterB:
--
-- @A@ — An array containing float values for neuron parameter A.                      Number of entries must be equal to MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersA :: (IsMPSMatrixNeuron mpsMatrixNeuron, IsNSData a) => mpsMatrixNeuron -> a -> IO ()
setNeuronToPReLUWithParametersA mpsMatrixNeuron a =
  sendMessage mpsMatrixNeuron setNeuronToPReLUWithParametersASelector (toNSData a)

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> RawId -> IO (Id MPSMatrixNeuron)
initWithDevice mpsMatrixNeuron device =
  sendOwnedMessage mpsMatrixNeuron initWithDeviceSelector device

-- | Encode a MPSMatrixNeuron object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input array.
--
-- @biasVector@ — A valid MPSVector object which specifies the bias values, or                              a null object to indicate that no bias is to be applied.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the output array.
--
-- Encodes the operation to the specified command buffer.  resultMatrix              must be large enough to hold a                  MIN(sourceNumberOfFeatureVectors, inputMatrix.rows - sourceMatrixOrigin.x)                  x                  MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels) array.
--
-- The bias vector must contain at least                  MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels) elements.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrix :: (IsMPSMatrixNeuron mpsMatrixNeuron, IsMPSMatrix inputMatrix, IsMPSVector biasVector, IsMPSMatrix resultMatrix) => mpsMatrixNeuron -> RawId -> inputMatrix -> biasVector -> resultMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrix mpsMatrixNeuron commandBuffer inputMatrix biasVector resultMatrix =
  sendMessage mpsMatrixNeuron encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector commandBuffer (toMPSMatrix inputMatrix) (toMPSVector biasVector) (toMPSMatrix resultMatrix)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixNeuron
--
-- @device@ — The MTLDevice on which to make the MPSMatrixNeuron object.
--
-- Returns: A new MPSMatrixNeuron object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixNeuron mpsMatrixNeuron, IsNSCoder aDecoder) => mpsMatrixNeuron -> aDecoder -> RawId -> IO (Id MPSMatrixNeuron)
initWithCoder_device mpsMatrixNeuron aDecoder device =
  sendOwnedMessage mpsMatrixNeuron initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
copyWithZone_device :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> Ptr () -> RawId -> IO (Id MPSMatrixNeuron)
copyWithZone_device mpsMatrixNeuron zone device =
  sendOwnedMessage mpsMatrixNeuron copyWithZone_deviceSelector zone device

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at sourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron sourceNumberOfFeatureVectorsSelector

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at sourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixNeuron value =
  sendMessage mpsMatrixNeuron setSourceNumberOfFeatureVectorsSelector value

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns in the primary (input array) source matrix to consider              and the number of channels to produce for the output matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available columns in              the input array (beginning at sourceMatrixOrigin.y) should be considered.              Defines also the number of output feature channels.              Note: The value used in the operation will be              MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CULong
sourceInputFeatureChannels mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron sourceInputFeatureChannelsSelector

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns in the primary (input array) source matrix to consider              and the number of channels to produce for the output matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available columns in              the input array (beginning at sourceMatrixOrigin.y) should be considered.              Defines also the number of output feature channels.              Note: The value used in the operation will be              MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixNeuron value =
  sendMessage mpsMatrixNeuron setSourceInputFeatureChannelsSelector value

-- | alpha
--
-- The scale factor to apply to the input.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> IO CDouble
alpha mpsMatrixNeuron =
  sendMessage mpsMatrixNeuron alphaSelector

-- | alpha
--
-- The scale factor to apply to the input.  Specified in double              precision.  Will be converted to the appropriate precision in the              implementation subject to rounding and/or clamping as necessary.              Defaults to 1.0 at initialization time.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixNeuron mpsMatrixNeuron => mpsMatrixNeuron -> CDouble -> IO ()
setAlpha mpsMatrixNeuron value =
  sendMessage mpsMatrixNeuron setAlphaSelector value

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

-- | @Selector@ for @setNeuronToPReLUWithParametersA:@
setNeuronToPReLUWithParametersASelector :: Selector '[Id NSData] ()
setNeuronToPReLUWithParametersASelector = mkSelector "setNeuronToPReLUWithParametersA:"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixNeuron)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSVector, Id MPSMatrix] ()
encodeToCommandBuffer_inputMatrix_biasVector_resultMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:biasVector:resultMatrix:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixNeuron)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixNeuron)
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

-- | @Selector@ for @alpha@
alphaSelector :: Selector '[] CDouble
alphaSelector = mkSelector "alpha"

-- | @Selector@ for @setAlpha:@
setAlphaSelector :: Selector '[CDouble] ()
setAlphaSelector = mkSelector "setAlpha:"

