{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNOptimizerRMSProp
--
-- The MPSNNOptimizerRMSProp performs an RMSProp Update              RMSProp is also known as root mean square propagation.
--
-- s[t]     = decay * s[t-1] + (1 - decay) * (g ^ 2)              variable = variable - learningRate * g / (sqrt(s[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                s[t] is weighted sum of squares of gradients
--
-- Generated bindings for @MPSNNOptimizerRMSProp@.
module ObjC.MetalPerformanceShaders.MPSNNOptimizerRMSProp
  ( MPSNNOptimizerRMSProp
  , IsMPSNNOptimizerRMSProp(..)
  , initWithDevice
  , initWithDevice_learningRate
  , initWithDevice_decay_epsilon_optimizerDescriptor
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrix
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultState
  , encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultState
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultState
  , decay
  , epsilon
  , decaySelector
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector
  , epsilonSelector
  , initWithDeviceSelector
  , initWithDevice_decay_epsilon_optimizerDescriptorSelector
  , initWithDevice_learningRateSelector


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
initWithDevice :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> RawId -> IO (Id MPSNNOptimizerRMSProp)
initWithDevice mpsnnOptimizerRMSProp device =
  sendOwnedMessage mpsnnOptimizerRMSProp initWithDeviceSelector device

-- | Convenience initialization for the RMSProp update
--
-- @device@ — The device on which the kernel will execute.
--
-- @learningRate@ — The learningRate which will be applied
--
-- Returns: A valid MPSNNOptimizerRMSProp object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:learningRate:@
initWithDevice_learningRate :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> RawId -> CFloat -> IO (Id MPSNNOptimizerRMSProp)
initWithDevice_learningRate mpsnnOptimizerRMSProp device learningRate =
  sendOwnedMessage mpsnnOptimizerRMSProp initWithDevice_learningRateSelector device learningRate

-- | Full initialization for the rmsProp update
--
-- @device@ — The device on which the kernel will execute.
--
-- @decay@ — The decay to update sumOfSquares
--
-- @epsilon@ — The epsilon which will be applied
--
-- @optimizerDescriptor@ — The optimizerDescriptor which will have a bunch of properties to be applied
--
-- Returns: A valid MPSNNOptimizerRMSProp object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:decay:epsilon:optimizerDescriptor:@
initWithDevice_decay_epsilon_optimizerDescriptor :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSNNOptimizerDescriptor optimizerDescriptor) => mpsnnOptimizerRMSProp -> RawId -> CDouble -> CFloat -> optimizerDescriptor -> IO (Id MPSNNOptimizerRMSProp)
initWithDevice_decay_epsilon_optimizerDescriptor mpsnnOptimizerRMSProp device decay epsilon optimizerDescriptor =
  sendOwnedMessage mpsnnOptimizerRMSProp initWithDevice_decay_epsilon_optimizerDescriptorSelector device decay epsilon (toMPSNNOptimizerDescriptor optimizerDescriptor)

-- | Encode an MPSNNOptimizerRMSProp object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputGradientVector@ — A valid MPSVector object which specifies the input vector of gradients for this update.
--
-- @inputValuesVector@ — A valid MPSVector object which specifies the input vector of values to be updated.
--
-- @inputSumOfSquaresVector@ — A valid MPSVector object which specifies the gradient velocity vector which will                                         be updated and overwritten.
--
-- @resultValuesVector@ — A valid MPSVector object which specifies the resultValues vector which will                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- s[t]     = decay * s[t-1] + (1 - decay) * (g ^ 2)              variable = variable - learningRate * g / (sqrt(s[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                s[t] is weighted sum of squares of gradients
--
-- ObjC selector: @- encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputSumOfSquaresVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVector :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSVector inputGradientVector, IsMPSVector inputValuesVector, IsMPSVector inputSumOfSquaresVector, IsMPSVector resultValuesVector) => mpsnnOptimizerRMSProp -> RawId -> inputGradientVector -> inputValuesVector -> inputSumOfSquaresVector -> resultValuesVector -> IO ()
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVector mpsnnOptimizerRMSProp commandBuffer inputGradientVector inputValuesVector inputSumOfSquaresVector resultValuesVector =
  sendMessage mpsnnOptimizerRMSProp encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector commandBuffer (toMPSVector inputGradientVector) (toMPSVector inputValuesVector) (toMPSVector inputSumOfSquaresVector) (toMPSVector resultValuesVector)

-- | @- encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrix :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSMatrix inputGradientMatrix, IsMPSMatrix inputValuesMatrix, IsMPSMatrix inputSumOfSquaresMatrix, IsMPSMatrix resultValuesMatrix) => mpsnnOptimizerRMSProp -> RawId -> inputGradientMatrix -> inputValuesMatrix -> inputSumOfSquaresMatrix -> resultValuesMatrix -> IO ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrix mpsnnOptimizerRMSProp commandBuffer inputGradientMatrix inputValuesMatrix inputSumOfSquaresMatrix resultValuesMatrix =
  sendMessage mpsnnOptimizerRMSProp encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector commandBuffer (toMPSMatrix inputGradientMatrix) (toMPSMatrix inputValuesMatrix) (toMPSMatrix inputSumOfSquaresMatrix) (toMPSMatrix resultValuesMatrix)

-- | Encode an MPSNNOptimizerRMSProp object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @convolutionGradientState@ — A valid MPSCNNConvolutionGradientState object which specifies the input state with gradients for this update.
--
-- @convolutionSourceState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the input state with values to be updated.
--
-- @inputSumOfSquaresVectors@ — An array MPSVector object which specifies the gradient sumOfSquares vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated
--
-- @resultState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the resultValues state which will                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- s[t]     = decay * s[t-1] + (1 - decay) * (g ^ 2)              variable = variable - learningRate * g / (sqrt(s[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                s[t] is weighted sum of squares of gradients
--
-- ObjC selector: @- encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultState :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSCNNConvolutionWeightsAndBiasesState convolutionSourceState, IsNSArray inputSumOfSquaresVectors, IsMPSCNNConvolutionWeightsAndBiasesState resultState) => mpsnnOptimizerRMSProp -> RawId -> convolutionGradientState -> convolutionSourceState -> inputSumOfSquaresVectors -> resultState -> IO ()
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultState mpsnnOptimizerRMSProp commandBuffer convolutionGradientState convolutionSourceState inputSumOfSquaresVectors resultState =
  sendMessage mpsnnOptimizerRMSProp encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector commandBuffer (toMPSCNNConvolutionGradientState convolutionGradientState) (toMPSCNNConvolutionWeightsAndBiasesState convolutionSourceState) (toNSArray inputSumOfSquaresVectors) (toMPSCNNConvolutionWeightsAndBiasesState resultState)

-- | Encode an MPSNNOptimizerRMSProp object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients and original gamma/beta for this update.
--
-- @inputSumOfSquaresVectors@ — An array MPSVector object which specifies the gradient sumOfSquares vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- s[t]     = decay * s[t-1] + (1 - decay) * (g ^ 2)              variable = variable - learningRate * g / (sqrt(s[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                s[t] is weighted sum of squares of gradients
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultState :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSCNNBatchNormalizationState batchNormalizationState, IsNSArray inputSumOfSquaresVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerRMSProp -> RawId -> batchNormalizationState -> inputSumOfSquaresVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultState mpsnnOptimizerRMSProp commandBuffer batchNormalizationState inputSumOfSquaresVectors resultState =
  sendMessage mpsnnOptimizerRMSProp encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector commandBuffer (toMPSCNNBatchNormalizationState batchNormalizationState) (toNSArray inputSumOfSquaresVectors) (toMPSCNNNormalizationGammaAndBetaState resultState)

-- | Encode an MPSNNOptimizerRMSProp object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationGradientState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients for this update.
--
-- @batchNormalizationSourceState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with original gamma/beta for this update.
--
-- @inputSumOfSquaresVectors@ — An array MPSVector object which specifies the gradient sumOfSquares vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- s[t]     = decay * s[t-1] + (1 - decay) * (g ^ 2)              variable = variable - learningRate * g / (sqrt(s[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                s[t] is weighted sum of squares of gradients
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultState :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSCNNBatchNormalizationState batchNormalizationGradientState, IsMPSCNNBatchNormalizationState batchNormalizationSourceState, IsNSArray inputSumOfSquaresVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerRMSProp -> RawId -> batchNormalizationGradientState -> batchNormalizationSourceState -> inputSumOfSquaresVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultState mpsnnOptimizerRMSProp commandBuffer batchNormalizationGradientState batchNormalizationSourceState inputSumOfSquaresVectors resultState =
  sendMessage mpsnnOptimizerRMSProp encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector commandBuffer (toMPSCNNBatchNormalizationState batchNormalizationGradientState) (toMPSCNNBatchNormalizationState batchNormalizationSourceState) (toNSArray inputSumOfSquaresVectors) (toMPSCNNNormalizationGammaAndBetaState resultState)

-- | decay
--
-- The decay at which we update sumOfSquares
--
-- Default value is 0.9
--
-- ObjC selector: @- decay@
decay :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> IO CDouble
decay mpsnnOptimizerRMSProp =
  sendMessage mpsnnOptimizerRMSProp decaySelector

-- | epsilon
--
-- The epsilon at which we update values
--
-- This value is usually used to ensure to avoid divide by 0, default value is 1e-8
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> IO CFloat
epsilon mpsnnOptimizerRMSProp =
  sendMessage mpsnnOptimizerRMSProp epsilonSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNOptimizerRMSProp)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:learningRate:@
initWithDevice_learningRateSelector :: Selector '[RawId, CFloat] (Id MPSNNOptimizerRMSProp)
initWithDevice_learningRateSelector = mkSelector "initWithDevice:learningRate:"

-- | @Selector@ for @initWithDevice:decay:epsilon:optimizerDescriptor:@
initWithDevice_decay_epsilon_optimizerDescriptorSelector :: Selector '[RawId, CDouble, CFloat, Id MPSNNOptimizerDescriptor] (Id MPSNNOptimizerRMSProp)
initWithDevice_decay_epsilon_optimizerDescriptorSelector = mkSelector "initWithDevice:decay:epsilon:optimizerDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputSumOfSquaresVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector :: Selector '[RawId, Id MPSVector, Id MPSVector, Id MPSVector, Id MPSVector] ()
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector = mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputSumOfSquaresVector:resultValuesVector:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector :: Selector '[RawId, Id MPSMatrix, Id MPSMatrix, Id MPSMatrix, Id MPSMatrix] ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector = mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:"

-- | @Selector@ for @encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector :: Selector '[RawId, Id MPSCNNConvolutionGradientState, Id MPSCNNConvolutionWeightsAndBiasesState, Id NSArray, Id MPSCNNConvolutionWeightsAndBiasesState] ()
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputSumOfSquaresVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector :: Selector '[RawId, Id MPSCNNBatchNormalizationState, Id NSArray, Id MPSCNNNormalizationGammaAndBetaState] ()
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationState:inputSumOfSquaresVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector :: Selector '[RawId, Id MPSCNNBatchNormalizationState, Id MPSCNNBatchNormalizationState, Id NSArray, Id MPSCNNNormalizationGammaAndBetaState] ()
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputSumOfSquaresVectors:resultState:"

-- | @Selector@ for @decay@
decaySelector :: Selector '[] CDouble
decaySelector = mkSelector "decay"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

