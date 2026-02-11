{-# LANGUAGE PatternSynonyms #-}
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
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , neuronTypeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , sourceNumberOfFeatureVectorsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , sourceInputFeatureChannelsSelector
  , setSourceInputFeatureChannelsSelector
  , epsilonSelector
  , setEpsilonSelector

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
setNeuronType_parameterA_parameterB_parameterC mpsMatrixBatchNormalizationGradient  neuronType parameterA parameterB parameterC =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "setNeuronType:parameterA:parameterB:parameterC:") retVoid [argCInt (coerce neuronType), argCFloat (fromIntegral parameterA), argCFloat (fromIntegral parameterB), argCFloat (fromIntegral parameterC)]

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO MPSCNNNeuronType
neuronType mpsMatrixBatchNormalizationGradient  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "neuronType") retCInt []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
neuronParameterA mpsMatrixBatchNormalizationGradient  =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "neuronParameterA") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
neuronParameterB mpsMatrixBatchNormalizationGradient  =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "neuronParameterB") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
neuronParameterC mpsMatrixBatchNormalizationGradient  =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "neuronParameterC") retCFloat []

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> RawId -> IO (Id MPSMatrixBatchNormalizationGradient)
initWithDevice mpsMatrixBatchNormalizationGradient  device =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVector mpsMatrixBatchNormalizationGradient  commandBuffer gradientMatrix inputMatrix meanVector varianceVector gammaVector betaVector resultGradientForDataMatrix resultGradientForGammaVector resultGradientForBetaVector =
withObjCPtr gradientMatrix $ \raw_gradientMatrix ->
  withObjCPtr inputMatrix $ \raw_inputMatrix ->
    withObjCPtr meanVector $ \raw_meanVector ->
      withObjCPtr varianceVector $ \raw_varianceVector ->
        withObjCPtr gammaVector $ \raw_gammaVector ->
          withObjCPtr betaVector $ \raw_betaVector ->
            withObjCPtr resultGradientForDataMatrix $ \raw_resultGradientForDataMatrix ->
              withObjCPtr resultGradientForGammaVector $ \raw_resultGradientForGammaVector ->
                withObjCPtr resultGradientForBetaVector $ \raw_resultGradientForBetaVector ->
                    sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "encodeToCommandBuffer:gradientMatrix:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultGradientForDataMatrix:resultGradientForGammaVector:resultGradientForBetaVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_gradientMatrix :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_meanVector :: Ptr ()), argPtr (castPtr raw_varianceVector :: Ptr ()), argPtr (castPtr raw_gammaVector :: Ptr ()), argPtr (castPtr raw_betaVector :: Ptr ()), argPtr (castPtr raw_resultGradientForDataMatrix :: Ptr ()), argPtr (castPtr raw_resultGradientForGammaVector :: Ptr ()), argPtr (castPtr raw_resultGradientForBetaVector :: Ptr ())]

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
initWithCoder_device mpsMatrixBatchNormalizationGradient  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
copyWithZone_device mpsMatrixBatchNormalizationGradient  zone device =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixBatchNormalizationGradient  =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "sourceNumberOfFeatureVectors") retCULong []

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixBatchNormalizationGradient  value =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "setSourceNumberOfFeatureVectors:") retVoid [argCULong (fromIntegral value)]

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input vectors.
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CULong
sourceInputFeatureChannels mpsMatrixBatchNormalizationGradient  =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "sourceInputFeatureChannels") retCULong []

-- | sourceInputFeatureChannels
--
-- The number of feature channels in the input vectors.
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixBatchNormalizationGradient  value =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "setSourceInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | epsilon
--
-- A small term added to the variance when normalizing the input.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> IO CFloat
epsilon mpsMatrixBatchNormalizationGradient  =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "epsilon") retCFloat []

-- | epsilon
--
-- A small term added to the variance when normalizing the input.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSMatrixBatchNormalizationGradient mpsMatrixBatchNormalizationGradient => mpsMatrixBatchNormalizationGradient -> CFloat -> IO ()
setEpsilon mpsMatrixBatchNormalizationGradient  value =
  sendMsg mpsMatrixBatchNormalizationGradient (mkSelector "setEpsilon:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setNeuronType:parameterA:parameterB:parameterC:@
setNeuronType_parameterA_parameterB_parameterCSelector :: Selector
setNeuronType_parameterA_parameterB_parameterCSelector = mkSelector "setNeuronType:parameterA:parameterB:parameterC:"

-- | @Selector@ for @neuronType@
neuronTypeSelector :: Selector
neuronTypeSelector = mkSelector "neuronType"

-- | @Selector@ for @neuronParameterA@
neuronParameterASelector :: Selector
neuronParameterASelector = mkSelector "neuronParameterA"

-- | @Selector@ for @neuronParameterB@
neuronParameterBSelector :: Selector
neuronParameterBSelector = mkSelector "neuronParameterB"

-- | @Selector@ for @neuronParameterC@
neuronParameterCSelector :: Selector
neuronParameterCSelector = mkSelector "neuronParameterC"

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @encodeToCommandBuffer:gradientMatrix:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultGradientForDataMatrix:resultGradientForGammaVector:resultGradientForBetaVector:@
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector :: Selector
encodeToCommandBuffer_gradientMatrix_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultGradientForDataMatrix_resultGradientForGammaVector_resultGradientForBetaVectorSelector = mkSelector "encodeToCommandBuffer:gradientMatrix:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultGradientForDataMatrix:resultGradientForGammaVector:resultGradientForBetaVector:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @copyWithZone:device:@
copyWithZone_deviceSelector :: Selector
copyWithZone_deviceSelector = mkSelector "copyWithZone:device:"

-- | @Selector@ for @sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectorsSelector :: Selector
sourceNumberOfFeatureVectorsSelector = mkSelector "sourceNumberOfFeatureVectors"

-- | @Selector@ for @setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectorsSelector :: Selector
setSourceNumberOfFeatureVectorsSelector = mkSelector "setSourceNumberOfFeatureVectors:"

-- | @Selector@ for @sourceInputFeatureChannels@
sourceInputFeatureChannelsSelector :: Selector
sourceInputFeatureChannelsSelector = mkSelector "sourceInputFeatureChannels"

-- | @Selector@ for @setSourceInputFeatureChannels:@
setSourceInputFeatureChannelsSelector :: Selector
setSourceInputFeatureChannelsSelector = mkSelector "setSourceInputFeatureChannels:"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector
setEpsilonSelector = mkSelector "setEpsilon:"

