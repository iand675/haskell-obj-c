{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixBatchNormalizationGradient
--
-- This depends on Metal.framework.
--
-- A kernel to compute the gradient of the batch normalization operation.
--
-- A MPSMatrixBatchNormalizationGradient object computes the results of backpropagating              the gradients of a loss function with respect to the outputs of an              MPSMatrixBatchNormalization object.  The corresponding properties and data used by              the MPSMatrixBatchNormalizationGradient object should correspond to those used by              the forward MPSMatrixBatchNormalization object for which the gradient is being computed.
--
-- Generated bindings for @MPSMatrixBatchNormalizationGradient@.
module ObjC.MetalPerformanceShaders.MPSMatrixBatchNormalizationGradient
  ( MPSMatrixBatchNormalizationGradient
  , IsMPSMatrixBatchNormalizationGradient(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , initWithDevice
  , encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVector
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , epsilon
  , setEpsilon
  , copyWithZone_deviceSelector
  , encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , neuronTypeSelector
  , setEpsilonSelector
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
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values. An MPSMatrixBatchNormalizationGradient kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
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
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixBatchNormalizationGradient neuronType parameterA parameterB parameterC =
  sendMessage mpsMatrixBatchNormalizationGradient setNeuronType_parameterA_parameterB_parameterCSelector neuronType parameterA parameterB parameterC

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO MPSCNNNeuronType
neuronType mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient neuronTypeSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
neuronParameterA mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient neuronParameterASelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
neuronParameterB mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient neuronParameterBSelector

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
neuronParameterC mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient neuronParameterCSelector

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> RawId -> IO (Id MPSMatrixBatchNormalizationGradient)
initWithDevice mpsMatrixBatchNormalizationGradient device =
  sendOwnedMessage mpsMatrixBatchNormalizationGradient initWithDeviceSelector device

-- | Encode a MPSMatrixBatchNormalizationGradient object to a command buffer and compute              its gradient with respect to its input data.
--
-- @commandBuffer@ — The commandBuffer on which to encode the operation.
--
-- @gradientMatrix@ — A matrix whose values represent the gradient of a                                              loss function with respect to the results of a forward                                              MPSMatrixBatchNormalization operation.
--
-- @inputMatrix@ — A matrix containing the inputs to a forward MPSMatrixBatchNormalization                                              operation for which the gradient values are to be computed.
--
-- @meanVector@ — A vector containing the batch mean values.  Should contain either the specified                                              values used to compute the forward result, or the computed values resulting from                                              the forward kernel execution.
--
-- @varianceVector@ — A vector containing the batch variance values.  Should contain either the specified                                              values used to compute the forward result, or the computed values resulting from                                              the forward kernel execution.
--
-- @gammaVector@ — A vector containing the gamma terms.  Should be the same values as used                                              when computing the forward result.
--
-- @betaVector@ — A vector containing the beta terms.  Should be the same values as used when                                              computing the forward result.
--
-- @resultGradientForDataMatrix@ — The matrix containing the resulting gradient values.
--
-- @resultGradientForGammaVector@ — If non-NULL the vector containing gradients for the gamma                                              terms.
--
-- @resultGradientForBetaVector@ — If non-NULL the vector containing gradients for the beta                                              terms.
--
-- ObjC selector: @- encodeToCommandBuffer:gradientMatrix:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultGradientForDataMatrix:resultGradientForGammaVector:resultGradientForBetaVector:@
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVector :: (IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient, IsMPSMatrix gradientMatrix, IsMPSMatrix inputMatrix, IsMPSVector meanVector, IsMPSVector varianceVector, IsMPSVector gammaVector, IsMPSVector betaVector, IsMPSMatrix resultGradientForDataMatrix, IsMPSVector resultGradientForGammaVector, IsMPSVector resultGradientForBetaVector) => mpsMatrixBatchNormalizationGradient -> RawId -> gradientMatrix -> inputMatrix -> meanVector -> varianceVector -> gammaVector -> betaVector -> resultGradientForDataMatrix -> resultGradientForGammaVector -> resultGradientForBetaVector -> IO ()
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVector mpsMatrixBatchNormalizationGradient commandBuffer gradientMatrix inputMatrix meanVector varianceVector gammaVector betaVector resultGradientForDataMatrix resultGradientForGammaVector resultGradientForBetaVector =
  sendMessage mpsMatrixBatchNormalizationGradient encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector commandBuffer (toMPSMatrix gradientMatrix) (toMPSMatrix inputMatrix) (toMPSVector meanVector) (toMPSVector varianceVector) (toMPSVector gammaVector) (toMPSVector betaVector) (toMPSMatrix resultGradientForDataMatrix) (toMPSVector resultGradientForGammaVector) (toMPSVector resultGradientForBetaVector)

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixBatchNormalizationGradient
--
-- @device@ — The MTLDevice on which to make the MPSMatrixBatchNormalizationGradient object.
--
-- Returns: A new MPSMatrixBatchNormalizationGradient object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient, IsNSCoder aDecoder) => mpsMatrixBatchNormalizationGradient -> aDecoder -> RawId -> IO (Id MPSMatrixBatchNormalizationGradient)
initWithCoder_device mpsMatrixBatchNormalizationGradient aDecoder device =
  sendOwnedMessage mpsMatrixBatchNormalizationGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

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
copyWithZone_device :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> Ptr () -> RawId -> IO (Id MPSMatrixBatchNormalizationGradient)
copyWithZone_device mpsMatrixBatchNormalizationGradient zone device =
  sendOwnedMessage mpsMatrixBatchNormalizationGradient copyWithZone_deviceSelector zone device

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient sourceNumberOfFeatureVectorsSelector

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixBatchNormalizationGradient value =
  sendMessage mpsMatrixBatchNormalizationGradient setSourceNumberOfFeatureVectorsSelector value

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input vectors.
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CULong
sourceInputFeatureChannels mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient sourceInputFeatureChannelsSelector

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input vectors.
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixBatchNormalizationGradient value =
  sendMessage mpsMatrixBatchNormalizationGradient setSourceInputFeatureChannelsSelector value

-- | epsilon
--
-- A small term added to the variance when normalizing the input.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
epsilon mpsMatrixBatchNormalizationGradient =
  sendMessage mpsMatrixBatchNormalizationGradient epsilonSelector

-- | epsilon
--
-- A small term added to the variance when normalizing the input.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> CFloat -> IO ()
setEpsilon mpsMatrixBatchNormalizationGradient value =
  sendMessage mpsMatrixBatchNormalizationGradient setEpsilonSelector value

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
initWithDeviceSelector :: Selector '[RawId] (Id MPSMatrixBatchNormalizationGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:gradientMatrix:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultGradientForDataMatrix:resultGradientForGammaVector:resultGradientForBetaVector:@
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSVector, Id MPSVector, Id MPSVector, Id MPSVector, Id MPSMatrix, Id MPSVector, Id MPSVector] ()
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector = mkSelector "encodeToCommandBuffer:gradientMatrix:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultGradientForDataMatrix:resultGradientForGammaVector:resultGradientForBetaVector:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSMatrixBatchNormalizationGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector '[Ptr (), RawId] (Id MPSMatrixBatchNormalizationGradient)
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

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector '[CFloat] ()
setEpsilonSelector = mkSelector "setEpsilon:"

