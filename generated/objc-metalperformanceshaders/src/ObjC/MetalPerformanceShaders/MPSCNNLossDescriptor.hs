{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLossDescriptor
--
-- This depends on Metal.framework.
--
-- The MPSCNNLossDescriptor specifies a loss filter descriptor.              The same descriptor can be used to initialize both the              MPSCNNLoss and the MPSNNLossGradient filters.
--
-- Generated bindings for @MPSCNNLossDescriptor@.
module ObjC.MetalPerformanceShaders.MPSCNNLossDescriptor
  ( MPSCNNLossDescriptor
  , IsMPSCNNLossDescriptor(..)
  , init_
  , cnnLossDescriptorWithType_reductionType
  , lossType
  , setLossType
  , reductionType
  , setReductionType
  , reduceAcrossBatch
  , setReduceAcrossBatch
  , weight
  , setWeight
  , labelSmoothing
  , setLabelSmoothing
  , numberOfClasses
  , setNumberOfClasses
  , epsilon
  , setEpsilon
  , delta
  , setDelta
  , initSelector
  , cnnLossDescriptorWithType_reductionTypeSelector
  , lossTypeSelector
  , setLossTypeSelector
  , reductionTypeSelector
  , setReductionTypeSelector
  , reduceAcrossBatchSelector
  , setReduceAcrossBatchSelector
  , weightSelector
  , setWeightSelector
  , labelSmoothingSelector
  , setLabelSmoothingSelector
  , numberOfClassesSelector
  , setNumberOfClassesSelector
  , epsilonSelector
  , setEpsilonSelector
  , deltaSelector
  , setDeltaSelector

  -- * Enum types
  , MPSCNNLossType(MPSCNNLossType)
  , pattern MPSCNNLossTypeMeanAbsoluteError
  , pattern MPSCNNLossTypeMeanSquaredError
  , pattern MPSCNNLossTypeSoftMaxCrossEntropy
  , pattern MPSCNNLossTypeSigmoidCrossEntropy
  , pattern MPSCNNLossTypeCategoricalCrossEntropy
  , pattern MPSCNNLossTypeHinge
  , pattern MPSCNNLossTypeHuber
  , pattern MPSCNNLossTypeCosineDistance
  , pattern MPSCNNLossTypeLog
  , pattern MPSCNNLossTypeKullbackLeiblerDivergence
  , pattern MPSCNNLossTypeCount
  , MPSCNNReductionType(MPSCNNReductionType)
  , pattern MPSCNNReductionTypeNone
  , pattern MPSCNNReductionTypeSum
  , pattern MPSCNNReductionTypeMean
  , pattern MPSCNNReductionTypeSumByNonZeroWeights
  , pattern MPSCNNReductionTypeCount

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

-- | @- init@
init_ :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO (Id MPSCNNLossDescriptor)
init_ mpscnnLossDescriptor  =
  sendMsg mpscnnLossDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Make a descriptor for a MPSCNNLoss or MPSNNLossGradient object.
--
-- @lossType@ — The type of a loss filter.
--
-- @reductionType@ — The type of a reduction operation to apply.                                          This argument is ignored in the MPSNNLossGradient filter.
--
-- Returns: A valid MPSCNNLossDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnLossDescriptorWithType:reductionType:@
cnnLossDescriptorWithType_reductionType :: MPSCNNLossType -> MPSCNNReductionType -> IO (Id MPSCNNLossDescriptor)
cnnLossDescriptorWithType_reductionType lossType reductionType =
  do
    cls' <- getRequiredClass "MPSCNNLossDescriptor"
    sendClassMsg cls' (mkSelector "cnnLossDescriptorWithType:reductionType:") (retPtr retVoid) [argCUInt (coerce lossType), argCInt (coerce reductionType)] >>= retainedObject . castPtr

-- | lossType
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO MPSCNNLossType
lossType mpscnnLossDescriptor  =
  fmap (coerce :: CUInt -> MPSCNNLossType) $ sendMsg mpscnnLossDescriptor (mkSelector "lossType") retCUInt []

-- | lossType
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setLossType:@
setLossType :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> MPSCNNLossType -> IO ()
setLossType mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setLossType:") retVoid [argCUInt (coerce value)]

-- | reductionType
--
-- The type of a reduction operation performed in the loss filter.
--
-- This parameter specifies the type of a reduction operation              performed in the loss filter.
--
-- ObjC selector: @- reductionType@
reductionType :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO MPSCNNReductionType
reductionType mpscnnLossDescriptor  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpscnnLossDescriptor (mkSelector "reductionType") retCInt []

-- | reductionType
--
-- The type of a reduction operation performed in the loss filter.
--
-- This parameter specifies the type of a reduction operation              performed in the loss filter.
--
-- ObjC selector: @- setReductionType:@
setReductionType :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> MPSCNNReductionType -> IO ()
setReductionType mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setReductionType:") retVoid [argCInt (coerce value)]

-- | reduceAcrossBatch
--
-- If set to YES then the reduction operation is applied also across the batch-index dimension,              ie. the loss value is summed over images in the batch and the result of the reduction is written              on the first loss image in the batch while the other loss images will be set to zero.              If set to NO, then no reductions are performed across the batch dimension and each image in the batch              will contain the loss value associated with that one particular image.              NOTE: If reductionType == MPSCNNReductionTypeNone, then this flag has no effect on results,              that is no reductions are done in this case.              NOTE: If reduceAcrossBatch is set to YES and reductionType == MPSCNNReductionTypeMean then              the final forward loss value is computed by first summing over the components and then by              dividing the result with: number of feature channels * width * height * number of images in the batch.              The default value is NO.
--
-- ObjC selector: @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO Bool
reduceAcrossBatch mpscnnLossDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnLossDescriptor (mkSelector "reduceAcrossBatch") retCULong []

-- | reduceAcrossBatch
--
-- If set to YES then the reduction operation is applied also across the batch-index dimension,              ie. the loss value is summed over images in the batch and the result of the reduction is written              on the first loss image in the batch while the other loss images will be set to zero.              If set to NO, then no reductions are performed across the batch dimension and each image in the batch              will contain the loss value associated with that one particular image.              NOTE: If reductionType == MPSCNNReductionTypeNone, then this flag has no effect on results,              that is no reductions are done in this case.              NOTE: If reduceAcrossBatch is set to YES and reductionType == MPSCNNReductionTypeMean then              the final forward loss value is computed by first summing over the components and then by              dividing the result with: number of feature channels * width * height * number of images in the batch.              The default value is NO.
--
-- ObjC selector: @- setReduceAcrossBatch:@
setReduceAcrossBatch :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> Bool -> IO ()
setReduceAcrossBatch mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setReduceAcrossBatch:") retVoid [argCULong (if value then 1 else 0)]

-- | weight
--
-- The scale factor to apply to each element of a result.
--
-- Each element of a result is multiplied by the weight value.              The default value is 1.0f.
--
-- ObjC selector: @- weight@
weight :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO CFloat
weight mpscnnLossDescriptor  =
  sendMsg mpscnnLossDescriptor (mkSelector "weight") retCFloat []

-- | weight
--
-- The scale factor to apply to each element of a result.
--
-- Each element of a result is multiplied by the weight value.              The default value is 1.0f.
--
-- ObjC selector: @- setWeight:@
setWeight :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> CFloat -> IO ()
setWeight mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setWeight:") retVoid [argCFloat (fromIntegral value)]

-- | labelSmoothing
--
-- The label smoothing parameter. The default value is 0.0f.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossFunctionTypeSoftmaxCrossEntropy, MPSCNNLossFunctionTypeSigmoidCrossEntropy.
--
-- MPSCNNLossFunctionTypeSoftmaxCrossEntropy: given labels (ground truth), it is applied in the following way:              labels = labelSmoothing > 0 ? labels * (1 - labelSmoothing) + labelSmoothing / numberOfClasses : labels
--
-- MPSCNNLossFunctionTypeSigmoidCrossEntropy: given labels (ground truth), it is applied in the following way:              labels = labelSmoothing > 0 ? labels * (1 - labelSmoothing) + 0.5 * labelSmoothing : labels
--
-- ObjC selector: @- labelSmoothing@
labelSmoothing :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO CFloat
labelSmoothing mpscnnLossDescriptor  =
  sendMsg mpscnnLossDescriptor (mkSelector "labelSmoothing") retCFloat []

-- | labelSmoothing
--
-- The label smoothing parameter. The default value is 0.0f.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossFunctionTypeSoftmaxCrossEntropy, MPSCNNLossFunctionTypeSigmoidCrossEntropy.
--
-- MPSCNNLossFunctionTypeSoftmaxCrossEntropy: given labels (ground truth), it is applied in the following way:              labels = labelSmoothing > 0 ? labels * (1 - labelSmoothing) + labelSmoothing / numberOfClasses : labels
--
-- MPSCNNLossFunctionTypeSigmoidCrossEntropy: given labels (ground truth), it is applied in the following way:              labels = labelSmoothing > 0 ? labels * (1 - labelSmoothing) + 0.5 * labelSmoothing : labels
--
-- ObjC selector: @- setLabelSmoothing:@
setLabelSmoothing :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> CFloat -> IO ()
setLabelSmoothing mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setLabelSmoothing:") retVoid [argCFloat (fromIntegral value)]

-- | numberOfClasses
--
-- The number of classes parameter. The default value is 1.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossFunctionTypeSoftmaxCrossEntropy.
--
-- Given labels (ground truth), it is applied in the following way:              labels = labelSmoothing > 0 ? labels * (1 - labelSmoothing) + labelSmoothing / numberOfClasses : labels
--
-- ObjC selector: @- numberOfClasses@
numberOfClasses :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO CULong
numberOfClasses mpscnnLossDescriptor  =
  sendMsg mpscnnLossDescriptor (mkSelector "numberOfClasses") retCULong []

-- | numberOfClasses
--
-- The number of classes parameter. The default value is 1.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossFunctionTypeSoftmaxCrossEntropy.
--
-- Given labels (ground truth), it is applied in the following way:              labels = labelSmoothing > 0 ? labels * (1 - labelSmoothing) + labelSmoothing / numberOfClasses : labels
--
-- ObjC selector: @- setNumberOfClasses:@
setNumberOfClasses :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> CULong -> IO ()
setNumberOfClasses mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setNumberOfClasses:") retVoid [argCULong (fromIntegral value)]

-- | epsilon
--
-- The epsilon parameter. The default value is 1e-7.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossTypeLog.
--
-- Given predictions and labels (ground truth), it is applied in the following way:              -(labels * log(predictions + epsilon)) - ((1 - labels) * log(1 - predictions + epsilon))
--
-- ObjC selector: @- epsilon@
epsilon :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO CFloat
epsilon mpscnnLossDescriptor  =
  sendMsg mpscnnLossDescriptor (mkSelector "epsilon") retCFloat []

-- | epsilon
--
-- The epsilon parameter. The default value is 1e-7.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossTypeLog.
--
-- Given predictions and labels (ground truth), it is applied in the following way:              -(labels * log(predictions + epsilon)) - ((1 - labels) * log(1 - predictions + epsilon))
--
-- ObjC selector: @- setEpsilon:@
setEpsilon :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> CFloat -> IO ()
setEpsilon mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setEpsilon:") retVoid [argCFloat (fromIntegral value)]

-- | delta
--
-- The delta parameter. The default value is 1.0f.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossTypeHuber.
--
-- Given predictions and labels (ground truth), it is applied in the following way:              if (|predictions - labels| <= delta, loss = 0.5f * predictions^2              if (|predictions - labels| >  delta, loss = 0.5 * delta^2 + delta * (|predictions - labels| - delta)
--
-- ObjC selector: @- delta@
delta :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> IO CFloat
delta mpscnnLossDescriptor  =
  sendMsg mpscnnLossDescriptor (mkSelector "delta") retCFloat []

-- | delta
--
-- The delta parameter. The default value is 1.0f.
--
-- This parameter is valid only for the loss functions of the following type(s):              MPSCNNLossTypeHuber.
--
-- Given predictions and labels (ground truth), it is applied in the following way:              if (|predictions - labels| <= delta, loss = 0.5f * predictions^2              if (|predictions - labels| >  delta, loss = 0.5 * delta^2 + delta * (|predictions - labels| - delta)
--
-- ObjC selector: @- setDelta:@
setDelta :: IsMPSCNNLossDescriptor mpscnnLossDescriptor => mpscnnLossDescriptor -> CFloat -> IO ()
setDelta mpscnnLossDescriptor  value =
  sendMsg mpscnnLossDescriptor (mkSelector "setDelta:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @cnnLossDescriptorWithType:reductionType:@
cnnLossDescriptorWithType_reductionTypeSelector :: Selector
cnnLossDescriptorWithType_reductionTypeSelector = mkSelector "cnnLossDescriptorWithType:reductionType:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @setLossType:@
setLossTypeSelector :: Selector
setLossTypeSelector = mkSelector "setLossType:"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @setReductionType:@
setReductionTypeSelector :: Selector
setReductionTypeSelector = mkSelector "setReductionType:"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @setReduceAcrossBatch:@
setReduceAcrossBatchSelector :: Selector
setReduceAcrossBatchSelector = mkSelector "setReduceAcrossBatch:"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector
setWeightSelector = mkSelector "setWeight:"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @setLabelSmoothing:@
setLabelSmoothingSelector :: Selector
setLabelSmoothingSelector = mkSelector "setLabelSmoothing:"

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @setNumberOfClasses:@
setNumberOfClassesSelector :: Selector
setNumberOfClassesSelector = mkSelector "setNumberOfClasses:"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector
setDeltaSelector = mkSelector "setDelta:"

