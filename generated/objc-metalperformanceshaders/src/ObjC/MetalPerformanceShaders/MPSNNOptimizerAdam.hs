{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNOptimizerAdam
--
-- The MPSNNOptimizerAdam performs an Adam Update
--
-- Initialization time              m[0] = 0 (Initialize initial 1st moment vector aka momentum, user is responsible for this)              v[0] = 0 (Initialize initial 2nd moment vector aka velocity, user is responsible for this)              t    = 0 (Initialize timestep)
--
-- https://arxiv.org/abs/1412.6980
--
-- At update time:              t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              variable = variable - lr[t] * m[t] / (sqrt(v[t]) + epsilon)
--
-- where,                g    is gradient of error wrt variable                v[t] is velocity                m[t] is momentum
--
-- Generated bindings for @MPSNNOptimizerAdam@.
module ObjC.MetalPerformanceShaders.MPSNNOptimizerAdam
  ( MPSNNOptimizerAdam
  , IsMPSNNOptimizerAdam(..)
  , initWithDevice
  , initWithDevice_learningRate
  , initWithDevice_beta1_beta2_epsilon_timeStep_optimizerDescriptor
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_resultValuesVector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_resultValuesMatrix
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_maximumVelocityVector_resultValuesVector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_maximumVelocityMatrix_resultValuesMatrix
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_resultState
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_resultState
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_resultState
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState
  , beta1
  , beta2
  , epsilon
  , timeStep
  , setTimeStep
  , initWithDeviceSelector
  , initWithDevice_learningRateSelector
  , initWithDevice_beta1_beta2_epsilon_timeStep_optimizerDescriptorSelector
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_resultValuesVectorSelector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_resultValuesMatrixSelector
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_maximumVelocityVector_resultValuesVectorSelector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_maximumVelocityMatrix_resultValuesMatrixSelector
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_resultStateSelector
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector
  , beta1Selector
  , beta2Selector
  , epsilonSelector
  , timeStepSelector
  , setTimeStepSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> RawId -> IO (Id MPSNNOptimizerAdam)
initWithDevice mpsnnOptimizerAdam  device =
  sendMsg mpsnnOptimizerAdam (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

-- | Convenience initialization for the adam update
--
-- @device@ — The device on which the kernel will execute.
--
-- @learningRate@ — The learningRate at which we will update values
--
-- Returns: A valid MPSNNOptimizerAdam object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:learningRate:@
initWithDevice_learningRate :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> RawId -> CFloat -> IO (Id MPSNNOptimizerAdam)
initWithDevice_learningRate mpsnnOptimizerAdam  device learningRate =
  sendMsg mpsnnOptimizerAdam (mkSelector "initWithDevice:learningRate:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral learningRate)] >>= ownedObject . castPtr

-- | Full initialization for the adam update
--
-- @device@ — The device on which the kernel will execute.
--
-- @beta1@ — The beta1 to update values
--
-- @beta2@ — The beta2 to update values
--
-- @epsilon@ — The epsilon at which we update values
--
-- @timeStep@ — The timeStep at which values will start updating
--
-- @optimizerDescriptor@ — The optimizerDescriptor which will have a bunch of properties to be applied
--
-- Returns: A valid MPSNNOptimizerAdam object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:beta1:beta2:epsilon:timeStep:optimizerDescriptor:@
initWithDevice_beta1_beta2_epsilon_timeStep_optimizerDescriptor :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSNNOptimizerDescriptor optimizerDescriptor) => mpsnnOptimizerAdam -> RawId -> CDouble -> CDouble -> CFloat -> CULong -> optimizerDescriptor -> IO (Id MPSNNOptimizerAdam)
initWithDevice_beta1_beta2_epsilon_timeStep_optimizerDescriptor mpsnnOptimizerAdam  device beta1 beta2 epsilon timeStep optimizerDescriptor =
withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
    sendMsg mpsnnOptimizerAdam (mkSelector "initWithDevice:beta1:beta2:epsilon:timeStep:optimizerDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCDouble (fromIntegral beta1), argCDouble (fromIntegral beta2), argCFloat (fromIntegral epsilon), argCULong (fromIntegral timeStep), argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Encode an MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputGradientVector@ — A valid MPSVector object which specifies the input vector of gradients for this update.
--
-- @inputValuesVector@ — A valid MPSVector object which specifies the input vector of values to be updated.
--
-- @inputMomentumVector@ — A valid MPSVector object which specifies the gradient momentum vector which will                                     be updated and overwritten.
--
-- @inputVelocityVector@ — A valid MPSVector object which specifies the gradient velocity vector which will                                     be updated and overwritten.
--
-- @resultValuesVector@ — A valid MPSVector object which specifies the resultValues vector which will                                     be updated and overwritten.
--
-- The following operations would be applied
--
-- t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              variable = variable - lr[t] * m[t] / (sqrt(v[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_resultValuesVector :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSVector inputGradientVector, IsMPSVector inputValuesVector, IsMPSVector inputMomentumVector, IsMPSVector inputVelocityVector, IsMPSVector resultValuesVector) => mpsnnOptimizerAdam -> RawId -> inputGradientVector -> inputValuesVector -> inputMomentumVector -> inputVelocityVector -> resultValuesVector -> IO ()
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_resultValuesVector mpsnnOptimizerAdam  commandBuffer inputGradientVector inputValuesVector inputMomentumVector inputVelocityVector resultValuesVector =
withObjCPtr inputGradientVector $ \raw_inputGradientVector ->
  withObjCPtr inputValuesVector $ \raw_inputValuesVector ->
    withObjCPtr inputMomentumVector $ \raw_inputMomentumVector ->
      withObjCPtr inputVelocityVector $ \raw_inputVelocityVector ->
        withObjCPtr resultValuesVector $ \raw_resultValuesVector ->
            sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:resultValuesVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientVector :: Ptr ()), argPtr (castPtr raw_inputValuesVector :: Ptr ()), argPtr (castPtr raw_inputMomentumVector :: Ptr ()), argPtr (castPtr raw_inputVelocityVector :: Ptr ()), argPtr (castPtr raw_resultValuesVector :: Ptr ())]

-- | @- encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_resultValuesMatrix :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSMatrix inputGradientMatrix, IsMPSMatrix inputValuesMatrix, IsMPSMatrix inputMomentumMatrix, IsMPSMatrix inputVelocityMatrix, IsMPSMatrix resultValuesMatrix) => mpsnnOptimizerAdam -> RawId -> inputGradientMatrix -> inputValuesMatrix -> inputMomentumMatrix -> inputVelocityMatrix -> resultValuesMatrix -> IO ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_resultValuesMatrix mpsnnOptimizerAdam  commandBuffer inputGradientMatrix inputValuesMatrix inputMomentumMatrix inputVelocityMatrix resultValuesMatrix =
withObjCPtr inputGradientMatrix $ \raw_inputGradientMatrix ->
  withObjCPtr inputValuesMatrix $ \raw_inputValuesMatrix ->
    withObjCPtr inputMomentumMatrix $ \raw_inputMomentumMatrix ->
      withObjCPtr inputVelocityMatrix $ \raw_inputVelocityMatrix ->
        withObjCPtr resultValuesMatrix $ \raw_resultValuesMatrix ->
            sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:resultValuesMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientMatrix :: Ptr ()), argPtr (castPtr raw_inputValuesMatrix :: Ptr ()), argPtr (castPtr raw_inputMomentumMatrix :: Ptr ()), argPtr (castPtr raw_inputVelocityMatrix :: Ptr ()), argPtr (castPtr raw_resultValuesMatrix :: Ptr ())]

-- | Encode an AMSGrad variant of MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @inputGradientVector@ — A valid MPSVector object which specifies the input vector of gradients for this update.
--
-- @inputValuesVector@ — A valid MPSVector object which specifies the input vector of values to be updated.
--
-- @inputMomentumVector@ — A valid MPSVector object which specifies the gradient momentum vector which will                                     be updated and overwritten.
--
-- @inputVelocityVector@ — A valid MPSVector object which specifies the gradient velocity vector which will                                     be updated and overwritten.
--
-- @maximumVelocityVector@ — A valid MPSVector object which specifies the maximum velocity vector which will                                     be updated and overwritten. May be nil, if nil then normal Adam optimizer behaviour is followed.
--
-- @resultValuesVector@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the resultValues state which will                                     be updated and overwritten.
--
-- The following operations would be applied              At update time:              t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              maxVel[t] = max(maxVel[t-1],v[t])              variable = variable - lr[t] * m[t] / (sqrt(maxVel[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:maximumVelocityVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_maximumVelocityVector_resultValuesVector :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSVector inputGradientVector, IsMPSVector inputValuesVector, IsMPSVector inputMomentumVector, IsMPSVector inputVelocityVector, IsMPSVector maximumVelocityVector, IsMPSVector resultValuesVector) => mpsnnOptimizerAdam -> RawId -> inputGradientVector -> inputValuesVector -> inputMomentumVector -> inputVelocityVector -> maximumVelocityVector -> resultValuesVector -> IO ()
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_maximumVelocityVector_resultValuesVector mpsnnOptimizerAdam  commandBuffer inputGradientVector inputValuesVector inputMomentumVector inputVelocityVector maximumVelocityVector resultValuesVector =
withObjCPtr inputGradientVector $ \raw_inputGradientVector ->
  withObjCPtr inputValuesVector $ \raw_inputValuesVector ->
    withObjCPtr inputMomentumVector $ \raw_inputMomentumVector ->
      withObjCPtr inputVelocityVector $ \raw_inputVelocityVector ->
        withObjCPtr maximumVelocityVector $ \raw_maximumVelocityVector ->
          withObjCPtr resultValuesVector $ \raw_resultValuesVector ->
              sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:maximumVelocityVector:resultValuesVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientVector :: Ptr ()), argPtr (castPtr raw_inputValuesVector :: Ptr ()), argPtr (castPtr raw_inputMomentumVector :: Ptr ()), argPtr (castPtr raw_inputVelocityVector :: Ptr ()), argPtr (castPtr raw_maximumVelocityVector :: Ptr ()), argPtr (castPtr raw_resultValuesVector :: Ptr ())]

-- | @- encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:maximumVelocityMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_maximumVelocityMatrix_resultValuesMatrix :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSMatrix inputGradientMatrix, IsMPSMatrix inputValuesMatrix, IsMPSMatrix inputMomentumMatrix, IsMPSMatrix inputVelocityMatrix, IsMPSMatrix maximumVelocityMatrix, IsMPSMatrix resultValuesMatrix) => mpsnnOptimizerAdam -> RawId -> inputGradientMatrix -> inputValuesMatrix -> inputMomentumMatrix -> inputVelocityMatrix -> maximumVelocityMatrix -> resultValuesMatrix -> IO ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_maximumVelocityMatrix_resultValuesMatrix mpsnnOptimizerAdam  commandBuffer inputGradientMatrix inputValuesMatrix inputMomentumMatrix inputVelocityMatrix maximumVelocityMatrix resultValuesMatrix =
withObjCPtr inputGradientMatrix $ \raw_inputGradientMatrix ->
  withObjCPtr inputValuesMatrix $ \raw_inputValuesMatrix ->
    withObjCPtr inputMomentumMatrix $ \raw_inputMomentumMatrix ->
      withObjCPtr inputVelocityMatrix $ \raw_inputVelocityMatrix ->
        withObjCPtr maximumVelocityMatrix $ \raw_maximumVelocityMatrix ->
          withObjCPtr resultValuesMatrix $ \raw_resultValuesMatrix ->
              sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:maximumVelocityMatrix:resultValuesMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientMatrix :: Ptr ()), argPtr (castPtr raw_inputValuesMatrix :: Ptr ()), argPtr (castPtr raw_inputMomentumMatrix :: Ptr ()), argPtr (castPtr raw_inputVelocityMatrix :: Ptr ()), argPtr (castPtr raw_maximumVelocityMatrix :: Ptr ()), argPtr (castPtr raw_resultValuesMatrix :: Ptr ())]

-- | Encode an MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @convolutionGradientState@ — A valid MPSCNNConvolutionGradientState object which specifies the input state with gradients for this update.
--
-- @convolutionSourceState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the input state with values to be updated.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated
--
-- @inputVelocityVectors@ — An array MPSVector object which specifies the gradient velocity vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated
--
-- @resultState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the resultValues state which will                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              variable = variable - lr[t] * m[t] / (sqrt(v[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_resultState :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSCNNConvolutionWeightsAndBiasesState convolutionSourceState, IsNSArray inputMomentumVectors, IsNSArray inputVelocityVectors, IsMPSCNNConvolutionWeightsAndBiasesState resultState) => mpsnnOptimizerAdam -> RawId -> convolutionGradientState -> convolutionSourceState -> inputMomentumVectors -> inputVelocityVectors -> resultState -> IO ()
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_resultState mpsnnOptimizerAdam  commandBuffer convolutionGradientState convolutionSourceState inputMomentumVectors inputVelocityVectors resultState =
withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
  withObjCPtr convolutionSourceState $ \raw_convolutionSourceState ->
    withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
      withObjCPtr inputVelocityVectors $ \raw_inputVelocityVectors ->
        withObjCPtr resultState $ \raw_resultState ->
            sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr raw_convolutionSourceState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_inputVelocityVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | Encode an AMSGrad variant of MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @convolutionGradientState@ — A valid MPSCNNConvolutionGradientState object which specifies the input state with gradients for this update.
--
-- @convolutionSourceState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the input state with values to be updated.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated
--
-- @inputVelocityVectors@ — An array MPSVector object which specifies the gradient velocity vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated
--
-- @maximumVelocityVectors@ — An array MPSVector object which specifies the maximum velocity vectors which will                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                         size 1 in which case biases won't be updated. May be nil, if nil then normal Adam optimizer behaviour is followed.
--
-- @resultState@ — A valid MPSCNNConvolutionWeightsAndBiasesState object which specifies the resultValues state which will                                         be updated and overwritten.
--
-- The following operations would be applied              At update time:              t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              maxVel[t] = max(maxVel[t-1],v[t])              variable = variable - lr[t] * m[t] / (sqrt(maxVel[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSCNNConvolutionGradientState convolutionGradientState, IsMPSCNNConvolutionWeightsAndBiasesState convolutionSourceState, IsNSArray inputMomentumVectors, IsNSArray inputVelocityVectors, IsNSArray maximumVelocityVectors, IsMPSCNNConvolutionWeightsAndBiasesState resultState) => mpsnnOptimizerAdam -> RawId -> convolutionGradientState -> convolutionSourceState -> inputMomentumVectors -> inputVelocityVectors -> maximumVelocityVectors -> resultState -> IO ()
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState mpsnnOptimizerAdam  commandBuffer convolutionGradientState convolutionSourceState inputMomentumVectors inputVelocityVectors maximumVelocityVectors resultState =
withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
  withObjCPtr convolutionSourceState $ \raw_convolutionSourceState ->
    withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
      withObjCPtr inputVelocityVectors $ \raw_inputVelocityVectors ->
        withObjCPtr maximumVelocityVectors $ \raw_maximumVelocityVectors ->
          withObjCPtr resultState $ \raw_resultState ->
              sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr raw_convolutionSourceState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_inputVelocityVectors :: Ptr ()), argPtr (castPtr raw_maximumVelocityVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | Encode an MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients and original gamma/beta for this update.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @inputVelocityVectors@ — An array MPSVector object which specifies the gradient velocity vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              variable = variable - lr[t] * m[t] / (sqrt(v[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_resultState :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSCNNBatchNormalizationState batchNormalizationState, IsNSArray inputMomentumVectors, IsNSArray inputVelocityVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerAdam -> RawId -> batchNormalizationState -> inputMomentumVectors -> inputVelocityVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_resultState mpsnnOptimizerAdam  commandBuffer batchNormalizationState inputMomentumVectors inputVelocityVectors resultState =
withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
  withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
    withObjCPtr inputVelocityVectors $ \raw_inputVelocityVectors ->
      withObjCPtr resultState $ \raw_resultState ->
          sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_inputVelocityVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | Encode an AMSGrad variant of  MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients and original gamma/beta for this update.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @inputVelocityVectors@ — An array MPSVector object which specifies the gradient velocity vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @maximumVelocityVectors@ — An array MPSVector object which specifies the maximum velocity vectors which will                                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                                         size 1 in which case biases won't be updated. May be nil, if nil then normal Adam optimizer behaviour is followed.
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied              At update time:              t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              maxVel[t] = max(maxVel[t-1],v[t])              variable = variable - lr[t] * m[t] / (sqrt(maxVel[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSCNNBatchNormalizationState batchNormalizationState, IsNSArray inputMomentumVectors, IsNSArray inputVelocityVectors, IsNSArray maximumVelocityVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerAdam -> RawId -> batchNormalizationState -> inputMomentumVectors -> inputVelocityVectors -> maximumVelocityVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState mpsnnOptimizerAdam  commandBuffer batchNormalizationState inputMomentumVectors inputVelocityVectors maximumVelocityVectors resultState =
withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
  withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
    withObjCPtr inputVelocityVectors $ \raw_inputVelocityVectors ->
      withObjCPtr maximumVelocityVectors $ \raw_maximumVelocityVectors ->
        withObjCPtr resultState $ \raw_resultState ->
            sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_inputVelocityVectors :: Ptr ()), argPtr (castPtr raw_maximumVelocityVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | Encode an MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationGradientState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients for this update.
--
-- @batchNormalizationSourceState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with original gamma/beta for this update.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @inputVelocityVectors@ — An array MPSVector object which specifies the gradient velocity vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied
--
-- t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              variable = variable - lr[t] * m[t] / (sqrt(v[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_resultState :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSCNNBatchNormalizationState batchNormalizationGradientState, IsMPSCNNBatchNormalizationState batchNormalizationSourceState, IsNSArray inputMomentumVectors, IsNSArray inputVelocityVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerAdam -> RawId -> batchNormalizationGradientState -> batchNormalizationSourceState -> inputMomentumVectors -> inputVelocityVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_resultState mpsnnOptimizerAdam  commandBuffer batchNormalizationGradientState batchNormalizationSourceState inputMomentumVectors inputVelocityVectors resultState =
withObjCPtr batchNormalizationGradientState $ \raw_batchNormalizationGradientState ->
  withObjCPtr batchNormalizationSourceState $ \raw_batchNormalizationSourceState ->
    withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
      withObjCPtr inputVelocityVectors $ \raw_inputVelocityVectors ->
        withObjCPtr resultState $ \raw_resultState ->
            sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationGradientState :: Ptr ()), argPtr (castPtr raw_batchNormalizationSourceState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_inputVelocityVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | Encode an AMSGrad variant of MPSNNOptimizerAdam object to a command buffer to perform out of place update
--
-- @commandBuffer@ — A valid MTLCommandBuffer to receive the encoded kernel.
--
-- @batchNormalizationGradientState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with gradients for this update.
--
-- @batchNormalizationSourceState@ — A valid MPSCNNBatchNormalizationState object which specifies the input state with original gamma/beta for this update.
--
-- @inputMomentumVectors@ — An array MPSVector object which specifies the gradient momentum vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @inputVelocityVectors@ — An array MPSVector object which specifies the gradient velocity vectors which will                                                         be updated and overwritten. The index 0 corresponds to gamma, index 1 corresponds to beta, array can be of                                                         size 1 in which case beta won't be updated
--
-- @maximumVelocityVectors@ — An array MPSVector object which specifies the maximum velocity vectors which will                                                         be updated and overwritten. The index 0 corresponds to weights, index 1 corresponds to biases, array can be of                                                         size 1 in which case biases won't be updated. May be nil, if nil then normal Adam optimizer behaviour is followed.
--
-- @resultState@ — A valid MPSCNNNormalizationGammaAndBetaState object which specifies the resultValues state which will                                                         be updated and overwritten.
--
-- The following operations would be applied              At update time:              t = t + 1              lr[t] = learningRate * sqrt(1 - beta2^t) / (1 - beta1^t)
--
-- m[t]     = beta1 * m[t-1] + (1 - beta1) * g              v[t]     = beta2 * v[t-1] + (1 - beta2) * (g ^ 2)              maxVel[t] = max(maxVel[t-1],v[t])              variable = variable - lr[t] * m[t] / (sqrt(maxVel[t]) + epsilon)
--
-- ObjC selector: @- encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState :: (IsMPSNNOptimizerAdam mpsnnOptimizerAdam, IsMPSCNNBatchNormalizationState batchNormalizationGradientState, IsMPSCNNBatchNormalizationState batchNormalizationSourceState, IsNSArray inputMomentumVectors, IsNSArray inputVelocityVectors, IsNSArray maximumVelocityVectors, IsMPSCNNNormalizationGammaAndBetaState resultState) => mpsnnOptimizerAdam -> RawId -> batchNormalizationGradientState -> batchNormalizationSourceState -> inputMomentumVectors -> inputVelocityVectors -> maximumVelocityVectors -> resultState -> IO ()
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultState mpsnnOptimizerAdam  commandBuffer batchNormalizationGradientState batchNormalizationSourceState inputMomentumVectors inputVelocityVectors maximumVelocityVectors resultState =
withObjCPtr batchNormalizationGradientState $ \raw_batchNormalizationGradientState ->
  withObjCPtr batchNormalizationSourceState $ \raw_batchNormalizationSourceState ->
    withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
      withObjCPtr inputVelocityVectors $ \raw_inputVelocityVectors ->
        withObjCPtr maximumVelocityVectors $ \raw_maximumVelocityVectors ->
          withObjCPtr resultState $ \raw_resultState ->
              sendMsg mpsnnOptimizerAdam (mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationGradientState :: Ptr ()), argPtr (castPtr raw_batchNormalizationSourceState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_inputVelocityVectors :: Ptr ()), argPtr (castPtr raw_maximumVelocityVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | beta1
--
-- The beta1 at which we update values
--
-- Default value is 0.9
--
-- ObjC selector: @- beta1@
beta1 :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> IO CDouble
beta1 mpsnnOptimizerAdam  =
  sendMsg mpsnnOptimizerAdam (mkSelector "beta1") retCDouble []

-- | beta2
--
-- The beta2 at which we update values
--
-- Default value is 0.999
--
-- ObjC selector: @- beta2@
beta2 :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> IO CDouble
beta2 mpsnnOptimizerAdam  =
  sendMsg mpsnnOptimizerAdam (mkSelector "beta2") retCDouble []

-- | epsilon
--
-- The epsilon at which we update values
--
-- This value is usually used to ensure to avoid divide by 0, default value is 1e-8
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> IO CFloat
epsilon mpsnnOptimizerAdam  =
  sendMsg mpsnnOptimizerAdam (mkSelector "epsilon") retCFloat []

-- | timeStep
--
-- Current timeStep for the update, number of times update has occurred
--
-- ObjC selector: @- timeStep@
timeStep :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> IO CULong
timeStep mpsnnOptimizerAdam  =
  sendMsg mpsnnOptimizerAdam (mkSelector "timeStep") retCULong []

-- | timeStep
--
-- Current timeStep for the update, number of times update has occurred
--
-- ObjC selector: @- setTimeStep:@
setTimeStep :: IsMPSNNOptimizerAdam mpsnnOptimizerAdam => mpsnnOptimizerAdam -> CULong -> IO ()
setTimeStep mpsnnOptimizerAdam  value =
  sendMsg mpsnnOptimizerAdam (mkSelector "setTimeStep:") retVoid [argCULong (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:learningRate:@
initWithDevice_learningRateSelector :: Selector
initWithDevice_learningRateSelector = mkSelector "initWithDevice:learningRate:"

-- | @Selector@ for @initWithDevice:beta1:beta2:epsilon:timeStep:optimizerDescriptor:@
initWithDevice_beta1_beta2_epsilon_timeStep_optimizerDescriptorSelector :: Selector
initWithDevice_beta1_beta2_epsilon_timeStep_optimizerDescriptorSelector = mkSelector "initWithDevice:beta1:beta2:epsilon:timeStep:optimizerDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_resultValuesVectorSelector :: Selector
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_resultValuesVectorSelector = mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:resultValuesVector:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_resultValuesMatrixSelector :: Selector
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_resultValuesMatrixSelector = mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:resultValuesMatrix:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:maximumVelocityVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_maximumVelocityVector_resultValuesVectorSelector :: Selector
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_inputVelocityVector_maximumVelocityVector_resultValuesVectorSelector = mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:inputVelocityVector:maximumVelocityVector:resultValuesVector:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:maximumVelocityMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_maximumVelocityMatrix_resultValuesMatrixSelector :: Selector
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_inputVelocityMatrix_maximumVelocityMatrix_resultValuesMatrixSelector = mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:inputVelocityMatrix:maximumVelocityMatrix:resultValuesMatrix:"

-- | @Selector@ for @encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_resultStateSelector :: Selector
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector :: Selector
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_inputVelocityVectors_maximumVelocityVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:inputVelocityVectors:maximumVelocityVectors:resultState:"

-- | @Selector@ for @beta1@
beta1Selector :: Selector
beta1Selector = mkSelector "beta1"

-- | @Selector@ for @beta2@
beta2Selector :: Selector
beta2Selector = mkSelector "beta2"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @timeStep@
timeStepSelector :: Selector
timeStepSelector = mkSelector "timeStep"

-- | @Selector@ for @setTimeStep:@
setTimeStepSelector :: Selector
setTimeStepSelector = mkSelector "setTimeStep:"

