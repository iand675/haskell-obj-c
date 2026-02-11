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
  , initWithDeviceSelector
  , initWithDevice_learningRateSelector
  , initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector
  , initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector
  , encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector
  , encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector
  , encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector
  , encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector
  , momentumScaleSelector
  , useNesterovMomentumSelector
  , useNestrovMomentumSelector


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
initWithDevice :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> RawId -> IO (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice mpsnnOptimizerStochasticGradientDescent  device =
  sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_learningRate mpsnnOptimizerStochasticGradientDescent  device learningRate =
  sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "initWithDevice:learningRate:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral learningRate)] >>= ownedObject . castPtr

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
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptor mpsnnOptimizerStochasticGradientDescent  device momentumScale useNesterovMomentum optimizerDescriptor =
withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
    sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "initWithDevice:momentumScale:useNesterovMomentum:optimizerDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral momentumScale), argCULong (if useNesterovMomentum then 1 else 0), argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptor :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSNNOptimizerDescriptor optimizerDescriptor) => mpsnnOptimizerStochasticGradientDescent -> RawId -> CFloat -> Bool -> optimizerDescriptor -> IO (Id MPSNNOptimizerStochasticGradientDescent)
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptor mpsnnOptimizerStochasticGradientDescent  device momentumScale useNestrovMomentum optimizerDescriptor =
withObjCPtr optimizerDescriptor $ \raw_optimizerDescriptor ->
    sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argCFloat (fromIntegral momentumScale), argCULong (if useNestrovMomentum then 1 else 0), argPtr (castPtr raw_optimizerDescriptor :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVector mpsnnOptimizerStochasticGradientDescent  commandBuffer inputGradientVector inputValuesVector inputMomentumVector resultValuesVector =
withObjCPtr inputGradientVector $ \raw_inputGradientVector ->
  withObjCPtr inputValuesVector $ \raw_inputValuesVector ->
    withObjCPtr inputMomentumVector $ \raw_inputMomentumVector ->
      withObjCPtr resultValuesVector $ \raw_resultValuesVector ->
          sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:resultValuesVector:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientVector :: Ptr ()), argPtr (castPtr raw_inputValuesVector :: Ptr ()), argPtr (castPtr raw_inputMomentumVector :: Ptr ()), argPtr (castPtr raw_resultValuesVector :: Ptr ())]

-- | @- encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrix :: (IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent, IsMPSMatrix inputGradientMatrix, IsMPSMatrix inputValuesMatrix, IsMPSMatrix inputMomentumMatrix, IsMPSMatrix resultValuesMatrix) => mpsnnOptimizerStochasticGradientDescent -> RawId -> inputGradientMatrix -> inputValuesMatrix -> inputMomentumMatrix -> resultValuesMatrix -> IO ()
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrix mpsnnOptimizerStochasticGradientDescent  commandBuffer inputGradientMatrix inputValuesMatrix inputMomentumMatrix resultValuesMatrix =
withObjCPtr inputGradientMatrix $ \raw_inputGradientMatrix ->
  withObjCPtr inputValuesMatrix $ \raw_inputValuesMatrix ->
    withObjCPtr inputMomentumMatrix $ \raw_inputMomentumMatrix ->
      withObjCPtr resultValuesMatrix $ \raw_resultValuesMatrix ->
          sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_inputGradientMatrix :: Ptr ()), argPtr (castPtr raw_inputValuesMatrix :: Ptr ()), argPtr (castPtr raw_inputMomentumMatrix :: Ptr ()), argPtr (castPtr raw_resultValuesMatrix :: Ptr ())]

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
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultState mpsnnOptimizerStochasticGradientDescent  commandBuffer convolutionGradientState convolutionSourceState inputMomentumVectors resultState =
withObjCPtr convolutionGradientState $ \raw_convolutionGradientState ->
  withObjCPtr convolutionSourceState $ \raw_convolutionSourceState ->
    withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
      withObjCPtr resultState $ \raw_resultState ->
          sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_convolutionGradientState :: Ptr ()), argPtr (castPtr raw_convolutionSourceState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

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
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultState mpsnnOptimizerStochasticGradientDescent  commandBuffer batchNormalizationState inputMomentumVectors resultState =
withObjCPtr batchNormalizationState $ \raw_batchNormalizationState ->
  withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
    withObjCPtr resultState $ \raw_resultState ->
        sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

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
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultState mpsnnOptimizerStochasticGradientDescent  commandBuffer batchNormalizationGradientState batchNormalizationSourceState inputMomentumVectors resultState =
withObjCPtr batchNormalizationGradientState $ \raw_batchNormalizationGradientState ->
  withObjCPtr batchNormalizationSourceState $ \raw_batchNormalizationSourceState ->
    withObjCPtr inputMomentumVectors $ \raw_inputMomentumVectors ->
      withObjCPtr resultState $ \raw_resultState ->
          sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:resultState:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_batchNormalizationGradientState :: Ptr ()), argPtr (castPtr raw_batchNormalizationSourceState :: Ptr ()), argPtr (castPtr raw_inputMomentumVectors :: Ptr ()), argPtr (castPtr raw_resultState :: Ptr ())]

-- | momentumScale
--
-- The momentumScale at which we update momentum for values array
--
-- Default value is 0.0
--
-- ObjC selector: @- momentumScale@
momentumScale :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> IO CFloat
momentumScale mpsnnOptimizerStochasticGradientDescent  =
  sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "momentumScale") retCFloat []

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
useNesterovMomentum mpsnnOptimizerStochasticGradientDescent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "useNesterovMomentum") retCULong []

-- | @- useNestrovMomentum@
useNestrovMomentum :: IsMPSNNOptimizerStochasticGradientDescent mpsnnOptimizerStochasticGradientDescent => mpsnnOptimizerStochasticGradientDescent -> IO Bool
useNestrovMomentum mpsnnOptimizerStochasticGradientDescent  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnOptimizerStochasticGradientDescent (mkSelector "useNestrovMomentum") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:learningRate:@
initWithDevice_learningRateSelector :: Selector
initWithDevice_learningRateSelector = mkSelector "initWithDevice:learningRate:"

-- | @Selector@ for @initWithDevice:momentumScale:useNesterovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector :: Selector
initWithDevice_momentumScale_useNesterovMomentum_optimizerDescriptorSelector = mkSelector "initWithDevice:momentumScale:useNesterovMomentum:optimizerDescriptor:"

-- | @Selector@ for @initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:@
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector :: Selector
initWithDevice_momentumScale_useNestrovMomentum_optimizerDescriptorSelector = mkSelector "initWithDevice:momentumScale:useNestrovMomentum:optimizerDescriptor:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:resultValuesVector:@
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector :: Selector
encodeToCommandBuffer_inputGradientVector_inputValuesVector_inputMomentumVector_resultValuesVectorSelector = mkSelector "encodeToCommandBuffer:inputGradientVector:inputValuesVector:inputMomentumVector:resultValuesVector:"

-- | @Selector@ for @encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:@
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector :: Selector
encodeToCommandBuffer_inputGradientMatrix_inputValuesMatrix_inputMomentumMatrix_resultValuesMatrixSelector = mkSelector "encodeToCommandBuffer:inputGradientMatrix:inputValuesMatrix:inputMomentumMatrix:resultValuesMatrix:"

-- | @Selector@ for @encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector :: Selector
encodeToCommandBuffer_convolutionGradientState_convolutionSourceState_inputMomentumVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:convolutionGradientState:convolutionSourceState:inputMomentumVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationState_inputMomentumVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationState:inputMomentumVectors:resultState:"

-- | @Selector@ for @encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:resultState:@
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector :: Selector
encodeToCommandBuffer_batchNormalizationGradientState_batchNormalizationSourceState_inputMomentumVectors_resultStateSelector = mkSelector "encodeToCommandBuffer:batchNormalizationGradientState:batchNormalizationSourceState:inputMomentumVectors:resultState:"

-- | @Selector@ for @momentumScale@
momentumScaleSelector :: Selector
momentumScaleSelector = mkSelector "momentumScale"

-- | @Selector@ for @useNesterovMomentum@
useNesterovMomentumSelector :: Selector
useNesterovMomentumSelector = mkSelector "useNesterovMomentum"

-- | @Selector@ for @useNestrovMomentum@
useNestrovMomentumSelector :: Selector
useNestrovMomentumSelector = mkSelector "useNestrovMomentum"

