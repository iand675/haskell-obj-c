{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSMatrixBatchNormalization
--
-- This depends on Metal.framework.
--
-- Applies a batch normalization to a matrix.
--
-- A MPSMatrixBatchNormalization object computes the batch normalization              of a collection of feature vectors stored in an MPSMatrix.
--
-- Feature vectors are stored in a row of the supplied input matrix and the              normalization is performed along columns:
--
-- y[i,j] = gamma[j] * (x[i,j] - mean(x[:,j])) / (variance(x[:,j]) + epsilon) + beta[j]
--
-- where gamma and beta are supplied weight and bias factors and epsilon is a small value added              to the variance.
--
-- Optionally a neuron activation function may be applied to the result.
--
-- Generated bindings for @MPSMatrixBatchNormalization@.
module ObjC.MetalPerformanceShaders.MPSMatrixBatchNormalization
  ( MPSMatrixBatchNormalization
  , IsMPSMatrixBatchNormalization(..)
  , setNeuronType_parameterA_parameterB_parameterC
  , neuronType
  , neuronParameterA
  , neuronParameterB
  , neuronParameterC
  , initWithDevice
  , encodeToCommandBuffer_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultMatrix
  , initWithCoder_device
  , copyWithZone_device
  , sourceNumberOfFeatureVectors
  , setSourceNumberOfFeatureVectors
  , sourceInputFeatureChannels
  , setSourceInputFeatureChannels
  , epsilon
  , setEpsilon
  , computeStatistics
  , setComputeStatistics
  , setNeuronType_parameterA_parameterB_parameterCSelector
  , neuronTypeSelector
  , neuronParameterASelector
  , neuronParameterBSelector
  , neuronParameterCSelector
  , initWithDeviceSelector
  , encodeToCommandBuffer_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultMatrixSelector
  , initWithCoder_deviceSelector
  , copyWithZone_deviceSelector
  , sourceNumberOfFeatureVectorsSelector
  , setSourceNumberOfFeatureVectorsSelector
  , sourceInputFeatureChannelsSelector
  , setSourceInputFeatureChannelsSelector
  , epsilonSelector
  , setEpsilonSelector
  , computeStatisticsSelector
  , setComputeStatisticsSelector

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
-- This method can be used to add a neuron activation funtion of given type with              associated scalar parameters A, B, and C that are shared across all output values.              Note that this method can only be used to specify neurons which are specified by three (or fewer)              parameters shared across all output values (or channels, in CNN nomenclature). It is an error to call              this method for neuron activation functions like MPSCNNNeuronTypePReLU,              which require per-channel parameter values.  An MPSMatrixNeuron kernel is initialized              with a default neuron function of MPSCNNNeuronTypeNone.
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
setNeuronType_parameterA_parameterB_parameterC :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> MPSCNNNeuronType -> CFloat -> CFloat -> CFloat -> IO ()
setNeuronType_parameterA_parameterB_parameterC mpsMatrixBatchNormalization  neuronType parameterA parameterB parameterC =
  sendMsg mpsMatrixBatchNormalization (mkSelector "setNeuronType:parameterA:parameterB:parameterC:") retVoid [argCInt (coerce neuronType), argCFloat (fromIntegral parameterA), argCFloat (fromIntegral parameterB), argCFloat (fromIntegral parameterC)]

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronType@
neuronType :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO MPSCNNNeuronType
neuronType mpsMatrixBatchNormalization  =
  fmap (coerce :: CInt -> MPSCNNNeuronType) $ sendMsg mpsMatrixBatchNormalization (mkSelector "neuronType") retCInt []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterA@
neuronParameterA :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO CFloat
neuronParameterA mpsMatrixBatchNormalization  =
  sendMsg mpsMatrixBatchNormalization (mkSelector "neuronParameterA") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterB@
neuronParameterB :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO CFloat
neuronParameterB mpsMatrixBatchNormalization  =
  sendMsg mpsMatrixBatchNormalization (mkSelector "neuronParameterB") retCFloat []

-- | Getter funtion for neuronType set using setNeuronType:parameterA:parameterB:parameterC method
--
-- ObjC selector: @- neuronParameterC@
neuronParameterC :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO CFloat
neuronParameterC mpsMatrixBatchNormalization  =
  sendMsg mpsMatrixBatchNormalization (mkSelector "neuronParameterC") retCFloat []

-- | @- initWithDevice:@
initWithDevice :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> RawId -> IO (Id MPSMatrixBatchNormalization)
initWithDevice mpsMatrixBatchNormalization  device =
  sendMsg mpsMatrixBatchNormalization (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Encode a MPSMatrixBatchNormalization object to a command buffer.
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputMatrix@ — A valid MPSMatrix object which specifies the input array.
--
-- @meanVector@ — A valid MPSVector object containing batch mean values to be used                                  to normalize the inputs if computeStatistics is NO.  If                                  computeStatistics is YES the resulting batch mean values                                  will be returned in this array.
--
-- @varianceVector@ — A valid MPSVector object containing batch variance values to be used                                  to normalize the inputs if computeStatistics is NO.  If                                  computeStatistics is YES the resulting batch variance values                                  will be returned in this array.
--
-- @gammaVector@ — A valid MPSVector object which specifies the gamma terms, or                                  a null object to indicate that no scaling is to be applied.
--
-- @betaVector@ — A valid MPSVector object which specifies the beta terms, or                                  a null object to indicate that no values are to be added.
--
-- @resultMatrix@ — A valid MPSMatrix object which specifies the output array.
--
-- Encodes the operation to the specified command buffer.  resultMatrix              must be large enough to hold a                  MIN(sourceNumberOfFeatureVectors, inputMatrix.rows - sourceMatrixOrigin.x)                  x                  MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels) array.
--
-- Let numChannels = MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- The gamma, beta, mean, and variance vectors must contain at least numChannels elements.
--
-- ObjC selector: @- encodeToCommandBuffer:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultMatrix :: (IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization, IsMPSMatrix inputMatrix, IsMPSVector meanVector, IsMPSVector varianceVector, IsMPSVector gammaVector, IsMPSVector betaVector, IsMPSMatrix resultMatrix) => mpsMatrixBatchNormalization -> RawId -> inputMatrix -> meanVector -> varianceVector -> gammaVector -> betaVector -> resultMatrix -> IO ()
encodeToCommandBuffer_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultMatrix mpsMatrixBatchNormalization  commandBuffer inputMatrix meanVector varianceVector gammaVector betaVector resultMatrix =
withObjCPtr inputMatrix $ \raw_inputMatrix ->
  withObjCPtr meanVector $ \raw_meanVector ->
    withObjCPtr varianceVector $ \raw_varianceVector ->
      withObjCPtr gammaVector $ \raw_gammaVector ->
        withObjCPtr betaVector $ \raw_betaVector ->
          withObjCPtr resultMatrix $ \raw_resultMatrix ->
              sendMsg mpsMatrixBatchNormalization (mkSelector "encodeToCommandBuffer:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputMatrix :: Ptr ()), argPtr (castPtr raw_meanVector :: Ptr ()), argPtr (castPtr raw_varianceVector :: Ptr ()), argPtr (castPtr raw_gammaVector :: Ptr ()), argPtr (castPtr raw_betaVector :: Ptr ()), argPtr (castPtr raw_resultMatrix :: Ptr ())]

-- | NSSecureCoding compatability
--
-- See MPSKernel#initWithCoder.
--
-- @aDecoder@ — The NSCoder subclass with your serialized MPSMatrixBatchNormalization object.
--
-- @device@ — The MTLDevice on which to make the MPSMatrixBatchNormalization object.
--
-- Returns: A new MPSMatrixBatchNormalization object, or nil if failure.
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization, IsNSCoder aDecoder) => mpsMatrixBatchNormalization -> aDecoder -> RawId -> IO (Id MPSMatrixBatchNormalization)
initWithCoder_device mpsMatrixBatchNormalization  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpsMatrixBatchNormalization (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
copyWithZone_device :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> Ptr () -> RawId -> IO (Id MPSMatrixBatchNormalization)
copyWithZone_device mpsMatrixBatchNormalization  zone device =
  sendMsg mpsMatrixBatchNormalization (mkSelector "copyWithZone:device:") (retPtr retVoid) [argPtr zone, argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at sourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- sourceNumberOfFeatureVectors@
sourceNumberOfFeatureVectors :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO CULong
sourceNumberOfFeatureVectors mpsMatrixBatchNormalization  =
  sendMsg mpsMatrixBatchNormalization (mkSelector "sourceNumberOfFeatureVectors") retCULong []

-- | sourceNumberOfFeatureVectors
--
-- The number of input vectors which make up the input array.  This              is equivalent to the number of rows to consider from the primary              source matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available number of inputs is              used.  The value of NSUIntegerMax thus indicates that all available input              rows (beginning at sourceMatrixOrigin.x) should be considered.
--
-- ObjC selector: @- setSourceNumberOfFeatureVectors:@
setSourceNumberOfFeatureVectors :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> CULong -> IO ()
setSourceNumberOfFeatureVectors mpsMatrixBatchNormalization  value =
  sendMsg mpsMatrixBatchNormalization (mkSelector "setSourceNumberOfFeatureVectors:") retVoid [argCULong (fromIntegral value)]

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns in the primary (input array) source matrix to consider              and the number of channels to produce for the output matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available columns in              the input array (beginning at sourceMatrixOrigin.y) should be considered.              Defines also the number of output feature channels.              Note: The value used in the operation will be              MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- sourceInputFeatureChannels@
sourceInputFeatureChannels :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO CULong
sourceInputFeatureChannels mpsMatrixBatchNormalization  =
  sendMsg mpsMatrixBatchNormalization (mkSelector "sourceInputFeatureChannels") retCULong []

-- | sourceInputFeatureChannels
--
-- The input size to to use in the operation.  This is equivalent to the              number of columns in the primary (input array) source matrix to consider              and the number of channels to produce for the output matrix.              This property is modifiable and defaults to NSUIntegerMax.  At encode              time the larger of this property or the available input size is used.              The value of NSUIntegerMax thus indicates that all available columns in              the input array (beginning at sourceMatrixOrigin.y) should be considered.              Defines also the number of output feature channels.              Note: The value used in the operation will be              MIN(inputMatrix.columns - sourceMatrixOrigin.y, sourceInputFeatureChannels)
--
-- ObjC selector: @- setSourceInputFeatureChannels:@
setSourceInputFeatureChannels :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> CULong -> IO ()
setSourceInputFeatureChannels mpsMatrixBatchNormalization  value =
  sendMsg mpsMatrixBatchNormalization (mkSelector "setSourceInputFeatureChannels:") retVoid [argCULong (fromIntegral value)]

-- | epsilon
--
-- A small value to add to the variance when normalizing the inputs.  Defaults              to FLT_MIN upon initialization.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO CFloat
epsilon mpsMatrixBatchNormalization  =
  sendMsg mpsMatrixBatchNormalization (mkSelector "epsilon") retCFloat []

-- | epsilon
--
-- A small value to add to the variance when normalizing the inputs.  Defaults              to FLT_MIN upon initialization.
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> CFloat -> IO ()
setEpsilon mpsMatrixBatchNormalization  value =
  sendMsg mpsMatrixBatchNormalization (mkSelector "setEpsilon:") retVoid [argCFloat (fromIntegral value)]

-- | computeStatistics
--
-- If YES the batch statistics will be computed prior to performing the normalization.              Otherwise the provided statistics will be used.  Defaults to NO at initialization              time.
--
-- ObjC selector: @- computeStatistics@
computeStatistics :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> IO Bool
computeStatistics mpsMatrixBatchNormalization  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsMatrixBatchNormalization (mkSelector "computeStatistics") retCULong []

-- | computeStatistics
--
-- If YES the batch statistics will be computed prior to performing the normalization.              Otherwise the provided statistics will be used.  Defaults to NO at initialization              time.
--
-- ObjC selector: @- setComputeStatistics:@
setComputeStatistics :: IsMPSMatrixBatchNormalization mpsMatrixBatchNormalization => mpsMatrixBatchNormalization -> Bool -> IO ()
setComputeStatistics mpsMatrixBatchNormalization  value =
  sendMsg mpsMatrixBatchNormalization (mkSelector "setComputeStatistics:") retVoid [argCULong (if value then 1 else 0)]

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

-- | @Selector@ for @encodeToCommandBuffer:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultMatrix:@
encodeToCommandBuffer_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultMatrixSelector :: Selector
encodeToCommandBuffer_inputMatrix_meanVector_varianceVector_gammaVector_betaVector_resultMatrixSelector = mkSelector "encodeToCommandBuffer:inputMatrix:meanVector:varianceVector:gammaVector:betaVector:resultMatrix:"

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

-- | @Selector@ for @computeStatistics@
computeStatisticsSelector :: Selector
computeStatisticsSelector = mkSelector "computeStatistics"

-- | @Selector@ for @setComputeStatistics:@
setComputeStatisticsSelector :: Selector
setComputeStatisticsSelector = mkSelector "setComputeStatistics:"

