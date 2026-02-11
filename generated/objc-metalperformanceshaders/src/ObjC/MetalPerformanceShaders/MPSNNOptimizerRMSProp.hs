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
  , initWithDeviceSelector
  , initWithDevice_learningRateSelector
  , initWithDevice_decay_epsilon_optimizerDescriptorSelector
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector
  , decaySelector
  , epsilonSelector


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
initWithDevice :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> RawId -> IO (Id MPSNNOptimizerRMSProp)
initWithDevice mpsnnOptimizerRMSProp  device =
  sendMsg mpsnnOptimizerRMSProp (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_learningRate mpsnnOptimizerRMSProp  device learningRate =
  sendMsg mpsnnOptimizerRMSProp (mkSelector "initWithDevice:learningRate:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral learningRate)] >>= ownedObject . castPtr

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
initWithDevice_decay_epsilon_optimizerDescriptor mpsnnOptimizerRMSProp  device decay epsilon optimizerDescriptor =
withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
    sendMsg mpsnnOptimizerRMSProp (mkSelector "initWithDevice:decay:epsilon:optimizerDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCDouble (fromIntegral decay), argCFloat (fromIntegral epsilon), argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVector mpsnnOptimizerRMSProp  commandBuffer inputGradientVector inputValuesVector inputSumOfSquaresVector resultValuesVector =
withObjCPtr inputGradientVector $ \raw_inputGradientVector ->
  withObjCPtr inputValuesVector $ \raw_inputValuesVector ->
    withObjCPtr inputSumOfSquaresVector $ \raw_inputSumOfSquaresVector ->
      withObjCPtr resultValuesVector $ \raw_resultValuesVector ->
          sendMsg mpsnnOptimizerRMSProp (mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputSumOfSquaresVector:resultValuesVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientVector :: Ptr ()), argPtr (castPtr raw_inputValuesVector :: Ptr ()), argPtr (castPtr raw_inputSumOfSquaresVector :: Ptr ()), argPtr (castPtr raw_resultValuesVector :: Ptr ())]

-- | @- encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrix :: (IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp, IsMPSMatrix inputGradientMatrix, IsMPSMatrix inputValuesMatrix, IsMPSMatrix inputSumOfSquaresMatrix, IsMPSMatrix resultValuesMatrix) => mpsnnOptimizerRMSProp -> RawId -> inputGradientMatrix -> inputValuesMatrix -> inputSumOfSquaresMatrix -> resultValuesMatrix -> IO ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrix mpsnnOptimizerRMSProp  commandBuffer inputGradientMatrix inputValuesMatrix inputSumOfSquaresMatrix resultValuesMatrix =
withObjCPtr inputGradientMatrix $ \raw_inputGradientMatrix ->
  withObjCPtr inputValuesMatrix $ \raw_inputValuesMatrix ->
    withObjCPtr inputSumOfSquaresMatrix $ \raw_inputSumOfSquaresMatrix ->
      withObjCPtr resultValuesMatrix $ \raw_resultValuesMatrix ->
          sendMsg mpsnnOptimizerRMSProp (mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientMatrix :: Ptr ()), argPtr (castPtr raw_inputValuesMatrix :: Ptr ()), argPtr (castPtr raw_inputSumOfSquaresMatrix :: Ptr ()), argPtr (castPtr raw_resultValuesMatrix :: Ptr ())]

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
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultState mpsnnOptimizerRMSProp  commandBuffer convolutionGradientState convolutionSourceState inputSumOfSquaresVectors resultState =
withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
  withObjCPtr convolutionSourceState $ \raw_convolutionSourceState ->
    withObjCPtr inputSumOfSquaresVectors $ \raw_inputSumOfSquaresVectors ->
      withObjCPtr resultState $ \raw_resultState ->
          sendMsg mpsnnOptimizerRMSProp (mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputSumOfSquaresVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr raw_convolutionSourceState :: Ptr ()), argPtr (castPtr raw_inputSumOfSquaresVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

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
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultState mpsnnOptimizerRMSProp  commandBuffer batchNormalizationState inputSumOfSquaresVectors resultState =
withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
  withObjCPtr inputSumOfSquaresVectors $ \raw_inputSumOfSquaresVectors ->
    withObjCPtr resultState $ \raw_resultState ->
        sendMsg mpsnnOptimizerRMSProp (mkSelector "encodeToCommandBuffer:batchNormalizationState:inputSumOfSquaresVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr raw_inputSumOfSquaresVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

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
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultState mpsnnOptimizerRMSProp  commandBuffer batchNormalizationGradientState batchNormalizationSourceState inputSumOfSquaresVectors resultState =
withObjCPtr batchNormalizationGradientState $ \raw_batchNormalizationGradientState ->
  withObjCPtr batchNormalizationSourceState $ \raw_batchNormalizationSourceState ->
    withObjCPtr inputSumOfSquaresVectors $ \raw_inputSumOfSquaresVectors ->
      withObjCPtr resultState $ \raw_resultState ->
          sendMsg mpsnnOptimizerRMSProp (mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputSumOfSquaresVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationGradientState :: Ptr ()), argPtr (castPtr raw_batchNormalizationSourceState :: Ptr ()), argPtr (castPtr raw_inputSumOfSquaresVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | decay
--
-- The decay at which we update sumOfSquares
--
-- Default value is 0.9
--
-- ObjC selector: @- decay@
decay :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> IO CDouble
decay mpsnnOptimizerRMSProp  =
  sendMsg mpsnnOptimizerRMSProp (mkSelector "decay") retCDouble []

-- | epsilon
--
-- The epsilon at which we update values
--
-- This value is usually used to ensure to avoid divide by 0, default value is 1e-8
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSNNOptimizerRMSProp mpsnnOptimizerRMSProp => mpsnnOptimizerRMSProp -> IO CFloat
epsilon mpsnnOptimizerRMSProp  =
  sendMsg mpsnnOptimizerRMSProp (mkSelector "epsilon") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:learningRate:@
initWithDevice_learningRateSelector :: Selector
initWithDevice_learningRateSelector = mkSelector "initWithDevice:learningRate:"

-- | @Selector@ for @initWithDevice:decay:epsilon:optimizerDescriptor:@
initWithDevice_decay_epsilon_optimizerDescriptorSelector :: Selector
initWithDevice_decay_epsilon_optimizerDescriptorSelector = mkSelector "initWithDevice:decay:epsilon:optimizerDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputSumOfSquaresVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector :: Selector
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputSumOfSquaresVector_resultValuesVectorSelector = mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputSumOfSquaresVector:resultValuesVector:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector :: Selector
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputSumOfSquaresMatrix_resultValuesMatrixSelector = mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputSumOfSquaresMatrix:resultValuesMatrix:"

-- | @Selector@ for @encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector :: Selector
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputSumOfSquaresVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputSumOfSquaresVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationState_inputSumOfSquaresVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationState:inputSumOfSquaresVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputSumOfSquaresVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputSumOfSquaresVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputSumOfSquaresVectors:resultState:"

-- | @Selector@ for @decay@
decaySelector :: Selector
decaySelector = mkSelector "decay"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

