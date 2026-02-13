{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNOptimizerStochasticGradientDescent
--
-- The MPSNNOptimizerStochasticGradientDescent performs a gradient descent with an optional momentum Update              RMSProp is also known as root mean square propagation.
--
-- useNesterov == NO:                  m[t]     = momentumScale * m[t-1] + learningRate * g                  variable = variable - m[t]
--
-- useNesterov == YES:                  m[t]     = momentumScale * m[t-1] + g                  variable = variable - (learningRate * (g + m[t] * momentumScale))
--
-- where,                g    is gradient of error wrt variable                m[t] is momentum of gradients it is a state we keep updating every update iteration
--
-- Generated bindings for @MPSNNOptimizerStochasticGradientDescent@.
module ObjC.MetalPerformanceShaders.MPSNNOptimizerStochasticGradientDescent
  ( MPSNNOptimizerStochasticGradientDescent
  , IsMPSNNOptimizerStochasticGradientDescent(..)
  , initWithDevice
  , initWithDevice_learningRate
  , initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptor
  , initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptor
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrix
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultState
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultState
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultState
  , momentumScale
  , useNesterovMomentum
  , useNestrovMomentum
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector
  , initWithDeviceSelector
  , initWithDevice_learningRateSelector
  , initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector
  , initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector
  , momentumScaleSelector
  , useNesterovMomentumSelector
  , useNestrovMomentumSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> RawId -> IO (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice mpsnnOptimizerStochasticGradientDescent device =
  sendOwnedMessage mpsnnOptimizerStochasticGradientDescent initWithDeviceSelector device

-- | Convenience initialization for the momentum update
--
-- @device@ — The device on which the kernel will execute.
--
-- @learningRate@ — The learningRate which will be applied
--
-- Returns: A valid MPSNNOptimizerStochasticGradientDescent object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:learningRate:@
initWithDevice_learningRate :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> RawId -> CFloat -> IO (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_learningRate mpsnnOptimizerStochasticGradientDescent device learningRate =
  sendOwnedMessage mpsnnOptimizerStochasticGradientDescent initWithDevice_learningRateSelector device learningRate

-- | Full initialization for the momentum update
--
-- @device@ — The device on which the kernel will execute.
--
-- @momentumScale@ — The momentumScale to update momentum for values array
--
-- @useNesterovMomentum@ — Use the Nesterov style momentum update
--
-- @optimizerDescriptor@ — The optimizerDescriptor which will have a bunch of properties to be applied
--
-- Returns: A valid MPSNNOptimizerMomentum object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:momentumScale:useNesterovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptor :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSNNOptimizerDescriptor optimizerDescriptor) => mpsnnOptimizerStochasticGradientDescent -> RawId -> CFloat -> Bool -> optimizerDescriptor -> IO (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptor mpsnnOptimizerStochasticGradientDescent device momentumScale useNesterovMomentum optimizerDescriptor =
  sendOwnedMessage mpsnnOptimizerStochasticGradientDescent initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector device momentumScale useNesterovMomentum (toMPSNNOptimizerDescriptor optimizerDescriptor)

-- | @- initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptor :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSNNOptimizerDescriptor optimizerDescriptor) => mpsnnOptimizerStochasticGradientDescent -> RawId -> CFloat -> Bool -> optimizerDescriptor -> IO (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptor mpsnnOptimizerStochasticGradientDescent device momentumScale useNestrovMomentum optimizerDescriptor =
  sendOwnedMessage mpsnnOptimizerStochasticGradientDescent initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector device momentumScale useNestrovMomentum (toMPSNNOptimizerDescriptor optimizerDescriptor)

-- | Encode an MPSNNOptimizerStochasticGradientDescent object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputGradientVector@ — A valid MPSVector object which specifies the input vector of gradients for this update.
--
-- @inputValuesVector@ — A valid MPSVector object which specifies the input vector of values to be updated.
--
-- @inputMomentumVector@ — A valid MPSVector object which specifies the gradient momentum vector which will                                         be updated and overwritten.
--
-- @resultValuesVector@ — A valid MPSVector object which specifies the resultValues vector which will                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- useNesterov == NO:                  m[t]     = momentumScale * m[t-1] + learningRate * g                  variable = variable - m[t]
--
-- useNesterov == YES:                  m[t]     = momentumScale * m[t-1] + g                  variable = variable - (learningRate * (g + m[t] * momentumScale))
--
-- inputMomentumVector == nil                  variable = variable - (learningRate * g)
--
-- where,                g    is gradient of error wrt variable                m[t] is momentum of gradients it is a state we keep updating every update iteration
--
-- ObjC selector: @- encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVector :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSVector inputGradientVector, IsMPSVector inputValuesVector, IsMPSVector inputMomentumVector, IsMPSVector resultValuesVector) => mpsnnOptimizerStochasticGradientDescent -> RawId -> inputGradientVector -> inputValuesVector -> inputMomentumVector -> resultValuesVector -> IO ()
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVector mpsnnOptimizerStochasticGradientDescent commandBuffer inputGradientVector inputValuesVector inputMomentumVector resultValuesVector =
  sendMessage mpsnnOptimizerStochasticGradientDescent encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector commandBuffer (toMPSVector inputGradientVector) (toMPSVector inputValuesVector) (toMPSVector inputMomentumVector) (toMPSVector resultValuesVector)

-- | @- encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrix :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSMatrix inputGradientMatrix, IsMPSMatrix inputValuesMatrix, IsMPSMatrix inputMomentumMatrix, IsMPSMatrix resultValuesMatrix) => mpsnnOptimizerStochasticGradientDescent -> RawId -> inputGradientMatrix -> inputValuesMatrix -> inputMomentumMatrix -> resultValuesMatrix -> IO ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrix mpsnnOptimizerStochasticGradientDescent commandBuffer inputGradientMatrix inputValuesMatrix inputMomentumMatrix resultValuesMatrix =
  sendMessage mpsnnOptimizerStochasticGradientDescent encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector commandBuffer (toMPSMatrix inputGradientMatrix) (toMPSMatrix inputValuesMatrix) (toMPSMatrix inputMomentumMatrix) (toMPSMatrix resultValuesMatrix)

-- | Encode an MPSNNOptimizerStochasticGradientDescent object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @convolutionGradientState@ — A valid MPSCNNConvolutionGradientState object which specifies the input state with gradients for this update.
--
-- @convolutionSourceState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the input state with values to be updated.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated
--
-- @resultState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the resultValues state which will                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- useNesterov == NO:                  m[t]     = momentumScale * m[t-1] + learningRate * g                  variable = variable - m[t]
--
-- useNesterov == YES:                  m[t]     = momentumScale * m[t-1] + g                  variable = variable - (learningRate * (g + m[t] * momentumScale))
--
-- inputMomentumVector == nil                  variable = variable - (learningRate * g)
--
-- where,                g    is gradient of error wrt variable                m[t] is momentum of gradients it is a state we keep updating every update iteration
--
-- ObjC selector: @- encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultState :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSCNNConvolutionWeightsAndBiasesState convolutionSourceState, IsNSArray inputMomentumVectors, IsMPSCNNConvolutionWeightsAndBiasesState resultState) => mpsnnOptimizerStochasticGradientDescent -> RawId -> convolutionGradientState -> convolutionSourceState -> inputMomentumVectors -> resultState -> IO ()
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultState mpsnnOptimizerStochasticGradientDescent commandBuffer convolutionGradientState convolutionSourceState inputMomentumVectors resultState =
  sendMessage mpsnnOptimizerStochasticGradientDescent encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector commandBuffer (toMPSCNNConvolutionGradientState convolutionGradientState) (toMPSCNNConvolutionWeightsAndBiasesState convolutionSourceState) (toNSArray inputMomentumVectors) (toMPSCNNConvolutionWeightsAndBiasesState resultState)

-- | Encode an MPSNNOptimizerStochasticGradientDescent object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients and original gamma/beta for this update.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- useNesterov == NO:                  m[t]     = momentumScale * m[t-1] + learningRate * g                  variable = variable - m[t]
--
-- useNesterov == YES:                  m[t]     = momentumScale * m[t-1] + g                  variable = variable - (learningRate * (g + m[t] * momentumScale))
--
-- inputMomentumVector == nil                  variable = variable - (learningRate * g)
--
-- where,                g    is gradient of error wrt variable                m[t] is momentum of gradients it is a state we keep updating every update iteration
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultState :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSCNNBatchNormalizationState batchNormalizationState, IsNSArray inputMomentumVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerStochasticGradientDescent -> RawId -> batchNormalizationState -> inputMomentumVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultState mpsnnOptimizerStochasticGradientDescent commandBuffer batchNormalizationState inputMomentumVectors resultState =
  sendMessage mpsnnOptimizerStochasticGradientDescent encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector commandBuffer (toMPSCNNBatchNormalizationState batchNormalizationState) (toNSArray inputMomentumVectors) (toMPSCNNNormalizationGammaAndBetaState resultState)

-- | Encode an MPSNNOptimizerStochasticGradientDescent object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationGradientState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients for this update.
--
-- @batchNormalizationSourceState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with original gamma/beta for this update.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- useNesterov == NO:                  m[t]     = momentumScale * m[t-1] + learningRate * g                  variable = variable - m[t]
--
-- useNesterov == YES:                  m[t]     = momentumScale * m[t-1] + g                  variable = variable - (learningRate * (g + m[t] * momentumScale))
--
-- inputMomentumVector == nil                  variable = variable - (learningRate * g)
--
-- where,                g    is gradient of error wrt variable                m[t] is momentum of gradients it is a state we keep updating every update iteration
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultState :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSCNNBatchNormalizationState batchNormalizationGradientState, IsMPSCNNBatchNormalizationState batchNormalizationSourceState, IsNSArray inputMomentumVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerStochasticGradientDescent -> RawId -> batchNormalizationGradientState -> batchNormalizationSourceState -> inputMomentumVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultState mpsnnOptimizerStochasticGradientDescent commandBuffer batchNormalizationGradientState batchNormalizationSourceState inputMomentumVectors resultState =
  sendMessage mpsnnOptimizerStochasticGradientDescent encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector commandBuffer (toMPSCNNBatchNormalizationState batchNormalizationGradientState) (toMPSCNNBatchNormalizationState batchNormalizationSourceState) (toNSArray inputMomentumVectors) (toMPSCNNNormalizationGammaAndBetaState resultState)

-- | momentumScale
--
-- The momentumScale at which we update momentum for values array
--
-- Default value is 0.0
--
-- ObjC selector: @- momentumScale@
momentumScale :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> IO CFloat
momentumScale mpsnnOptimizerStochasticGradientDescent =
  sendMessage mpsnnOptimizerStochasticGradientDescent momentumScaleSelector

-- | useNesterovMomentum
--
-- Nesterov momentum is considered an improvement on the usual momentum update
--
-- Default value is NO
--
-- Note: Maps to old useNestrovMomentum property
--
-- ObjC selector: @- useNesterovMomentum@
useNesterovMomentum :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> IO Bool
useNesterovMomentum mpsnnOptimizerStochasticGradientDescent =
  sendMessage mpsnnOptimizerStochasticGradientDescent useNesterovMomentumSelector

-- | @- useNestrovMomentum@
useNestrovMomentum :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> IO Bool
useNestrovMomentum mpsnnOptimizerStochasticGradientDescent =
  sendMessage mpsnnOptimizerStochasticGradientDescent useNestrovMomentumSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNOptimizerStochasticGradientDescent)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:learningRate:@
initWithDevice_learningRateSelector :: Selector '[RawId, CFloat] (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_learningRateSelector = mkSelector "initWithDevice:learningRate:"

-- | @Selector@ for @initWithDevice:momentumScale:useNesterovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector :: Selector '[RawId, CFloat, Bool, Id MPSNNOptimizerDescriptor] (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector = mkSelector "initWithDevice:momentumScale:useNesterovMomentum:optimizerDescriptor:"

-- | @Selector@ for @initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector :: Selector '[RawId, CFloat, Bool, Id MPSNNOptimizerDescriptor] (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector = mkSelector "initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector :: Selector '[RawId, Id MPSVector, Id MPSVector, Id MPSVector, Id MPSVector] ()
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector = mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:resultValuesVector:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSMatrix, Id MPSMatrix] ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector = mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:"

-- | @Selector@ for @encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector :: Selector '[RawId, Id MPSCNNConvolutionGradientState, Id MPSCNNConvolutionWeightsAndBiasesState, Id NSArray, Id MPSCNNConvolutionWeightsAndBiasesState] ()
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector :: Selector '[RawId, Id MPSCNNBatchNormalizationState, Id NSArray, Id MPSCNNNormalizationGammaAndBetaState] ()
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector :: Selector '[RawId, Id MPSCNNBatchNormalizationState, Id MPSCNNBatchNormalizationState, Id NSArray, Id MPSCNNNormalizationGammaAndBetaState] ()
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:resultState:"

-- | @Selector@ for @momentumScale@
momentumScaleSelector :: Selector '[] CFloat
momentumScaleSelector = mkSelector "momentumScale"

-- | @Selector@ for @useNesterovMomentum@
useNesterovMomentumSelector :: Selector '[] Bool
useNesterovMomentumSelector = mkSelector "useNesterovMomentum"

-- | @Selector@ for @useNestrovMomentum@
useNestrovMomentumSelector :: Selector '[] Bool
useNestrovMomentumSelector = mkSelector "useNestrovMomentum"

