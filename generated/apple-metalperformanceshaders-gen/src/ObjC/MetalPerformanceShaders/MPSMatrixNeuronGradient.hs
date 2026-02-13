{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixNeuronGradient
--
-- This depends on Metal.framework.
--
-- A neuron gradient activation kernel that operates on matrices.
--
-- A MPSMatrixNeuronGradient object computes the results of backpropagating              the gradients of a loss function with respect to the outputs of an              MPSMatrixNeuron object.  The corresponding properties and data used by              the MPSMatrixNeuronGradient object should correspond to those used by              the forward MPSMatrixNeuron object for which the gradient is being computed.
--
-- Generated bindings for @MPSMatrixNeuronGradient@.
module ObjC.MetalPerformanceShaders.MPSMatrixNeuronGradient
  ( MPSMatrixNeuronGradient
  , IsMPSMatrixNeuronGradient(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , setNeuronToPReLUWithParametersA
  , initWithDevice
  , encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVector
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
  , encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVectorSelector
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
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixNeuronGradient neuronType parameterA parameterB parameterC =
  sendMessage mpsMatrixNeuronGradient setNeuronType_parameterA_parameterB_parameterCSelector neuronType parameterA parameterB parameterC

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO MPSCNNNeuronType
neuronType mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient neuronTypeSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO CFloat
neuronParameterA mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient neuronParameterASelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO CFloat
neuronParameterB mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient neuronParameterBSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO CFloat
neuronParameterC mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient neuronParameterCSelector

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
setNeuronToPReLUWithParametersA :: (IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient, IsNSData a) => mpsMatrixNeuronGradient -> a -> IO ()
setNeuronToPReLUWithParametersA mpsMatrixNeuronGradient a =
  sendMessage mpsMatrixNeuronGradient setNeuronToPReLUWithParametersASelector (toNSData a)

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> RawId -> IO (Id MPSMatrixNeuronGradient)
initWithDevice mpsMatrixNeuronGradient device =
  sendOwnedMessage mpsMatrixNeuronGradient initWithDeviceSelector device

-- | Encode a MPSMatrixNeuronGradient object to a command buffer and compute              its gradient with respect to its input data.
--
-- @commandBuffer@ — The commandBuffer on which to encode the operation.
--
-- @gradientMatrix@ — A matrix whose values represent the gradient of a                                          loss function with respect to the results of a forward                                          MPSMatrixNeuron operation.
--
-- @inputMatrix@ — A matrix containing the inputs to a forward MPSMatrixNeuron                                          operation for which the gradient values are to be computed.
--
-- @biasVector@ — A vector containing the bias terms.
--
-- @resultGradientForDataMatrix@ — The matrix containing the resulting gradient values.
--
-- @resultGradientForBiasVector@ — If non-NULL the vector containing gradients for the bias                                          terms.
--
-- ObjC selector: @- encodeToCommandBuffer:gradientMatrix:inputMatrix:biasVector:resultGradientForDataMatrix:resultGradientForBiasVector:@
encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVector :: (IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient, IsMPSMatrix gradientMatrix, IsMPSMatrix inputMatrix, IsMPSVector biasVector, IsMPSMatrix resultGradientForDataMatrix, IsMPSVector resultGradientForBiasVector) => mpsMatrixNeuronGradient -> RawId -> gradientMatrix -> inputMatrix -> biasVector -> resultGradientForDataMatrix -> resultGradientForBiasVector -> IO ()
encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVector mpsMatrixNeuronGradient commandBuffer gradientMatrix inputMatrix biasVector resultGradientForDataMatrix resultGradientForBiasVector =
  sendMessage mpsMatrixNeuronGradient encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVectorSelector commandBuffer (toMPSMatrix gradientMatrix) (toMPSMatrix inputMatrix) (toMPSVector biasVector) (toMPSMatrix resultGradientForDataMatrix) (toMPSVector resultGradientForBiasVector)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixNeuronGradient
--
-- @device@ — The MTLDevice on which to make the MPSMatrixNeuronGradient object.
--
-- Returns: A new MPSMatrixNeuronGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient, IsNSCoder aDecoder) => mpsMatrixNeuronGradient -> aDecoder -> RawId -> IO (Id MPSMatrixNeuronGradient)
initWithCoder_device mpsMatrixNeuronGradient aDecoder device =
  sendOwnedMessage mpsMatrixNeuronGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
copyWithZone_device :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> Ptr () -> RawId -> IO (Id MPSMatrixNeuronGradient)
copyWithZone_device mpsMatrixNeuronGradient zone device =
  sendOwnedMessage mpsMatrixNeuronGradient copyWithZone_deviceSelector zone device

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient sourceNumberOfFeatureVectorsSelector

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixNeuronGradient value =
  sendMessage mpsMatrixNeuronGradient setSourceNumberOfFeatureVectorsSelector value

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input vectors.
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO CULong
sourceInputFeatureChannels mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient sourceInputFeatureChannelsSelector

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input vectors.
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixNeuronGradient value =
  sendMessage mpsMatrixNeuronGradient setSourceInputFeatureChannelsSelector value

-- | alpha
--
-- The scale factor to apply to the input.
--
-- ObjC selector: @- alpha@
alpha :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> IO CDouble
alpha mpsMatrixNeuronGradient =
  sendMessage mpsMatrixNeuronGradient alphaSelector

-- | alpha
--
-- The scale factor to apply to the input.
--
-- ObjC selector: @- setAlpha:@
setAlpha :: IsMPSMatrixNeuronGradient mpsMatrixNeuronGradient => mpsMatrixNeuronGradient -> CDouble -> IO ()
setAlpha mpsMatrixNeuronGradient value =
  sendMessage mpsMatrixNeuronGradient setAlphaSelector value

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
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixNeuronGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:gradientMatrix:inputMatrix:biasVector:resultGradientForDataMatrix:resultGradientForBiasVector:@
encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVectorSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSVector, Id MPSMatrix, Id MPSVector] ()
encodeToCommandBuffer_gradientMatrix_inputMatrix_biasVector_resultGradientForDataMatrix_resultGradientForBiasVectorSelector = mkSelector "encodeToCommandBuffer:gradientMatrix:inputMatrix:biasVector:resultGradientForDataMatrix:resultGradientForBiasVector:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixNeuronGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixNeuronGradient)
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

