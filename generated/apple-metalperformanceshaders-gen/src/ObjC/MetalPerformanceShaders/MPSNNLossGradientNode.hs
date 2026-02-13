{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSNNLossGradient kernel
--
-- Generated bindings for @MPSNNLossGradientNode@.
module ObjC.MetalPerformanceShaders.MPSNNLossGradientNode
  ( MPSNNLossGradientNode
  , IsMPSNNLossGradientNode(..)
  , nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter
  , nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter
  , nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilter
  , initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter
  , initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter
  , initWithSources_gradientState_lossDescriptor_isLabelsGradientFilter
  , gradientFilterWithSources
  , lossType
  , reductionType
  , numberOfClasses
  , reduceAcrossBatch
  , weight
  , labelSmoothing
  , epsilon
  , delta
  , isLabelsGradientFilter
  , propertyCallBack
  , setPropertyCallBack
  , deltaSelector
  , epsilonSelector
  , gradientFilterWithSourcesSelector
  , initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , isLabelsGradientFilterSelector
  , labelSmoothingSelector
  , lossTypeSelector
  , nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , numberOfClassesSelector
  , propertyCallBackSelector
  , reduceAcrossBatchSelector
  , reductionTypeSelector
  , setPropertyCallBackSelector
  , weightSelector

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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetalPerformanceShaders.Internal.Classes
import ObjC.MetalPerformanceShaders.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => sourceGradient -> sourceImage -> labels -> weights -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter sourceGradient sourceImage labels weights gradientState descriptor isLabelsGradientFilter =
  do
    cls' <- getRequiredClass "MPSNNLossGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNImageNode labels) (toMPSNNImageNode weights) (toMPSNNGradientStateNode gradientState) (toMPSCNNLossDescriptor descriptor) isLabelsGradientFilter

-- | @+ nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => sourceGradient -> sourceImage -> labels -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter sourceGradient sourceImage labels gradientState descriptor isLabelsGradientFilter =
  do
    cls' <- getRequiredClass "MPSNNLossGradientNode"
    sendClassMessage cls' nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNImageNode labels) (toMPSNNGradientStateNode gradientState) (toMPSCNNLossDescriptor descriptor) isLabelsGradientFilter

-- | Init a gradient loss node from multiple images
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter                                      Node0: logits, Node1: labels, Node2: weights
--
-- Returns: A new MPSNNFilter node.
--
-- ObjC selector: @+ nodeWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsNSArray sourceNodes, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => sourceNodes -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilter sourceNodes gradientState descriptor isLabelsGradientFilter =
  do
    cls' <- getRequiredClass "MPSNNLossGradientNode"
    sendClassMessage cls' nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector (toNSArray sourceNodes) (toMPSNNGradientStateNode gradientState) (toMPSCNNLossDescriptor descriptor) isLabelsGradientFilter

-- | @- initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => mpsnnLossGradientNode -> sourceGradient -> sourceImage -> labels -> weights -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter mpsnnLossGradientNode sourceGradient sourceImage labels weights gradientState descriptor isLabelsGradientFilter =
  sendOwnedMessage mpsnnLossGradientNode initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNImageNode labels) (toMPSNNImageNode weights) (toMPSNNGradientStateNode gradientState) (toMPSCNNLossDescriptor descriptor) isLabelsGradientFilter

-- | @- initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => mpsnnLossGradientNode -> sourceGradient -> sourceImage -> labels -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter mpsnnLossGradientNode sourceGradient sourceImage labels gradientState descriptor isLabelsGradientFilter =
  sendOwnedMessage mpsnnLossGradientNode initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector (toMPSNNImageNode sourceGradient) (toMPSNNImageNode sourceImage) (toMPSNNImageNode labels) (toMPSNNGradientStateNode gradientState) (toMPSCNNLossDescriptor descriptor) isLabelsGradientFilter

-- | Init a gradient loss node from multiple images
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter                                      Node0: input gradients, Node1: logits, Node2: labels, Node3: weights
--
-- Returns: A new MPSNNFilter node.
--
-- ObjC selector: @- initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsNSArray sourceNodes, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => mpsnnLossGradientNode -> sourceNodes -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilter mpsnnLossGradientNode sourceNodes gradientState descriptor isLabelsGradientFilter =
  sendOwnedMessage mpsnnLossGradientNode initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector (toNSArray sourceNodes) (toMPSNNGradientStateNode gradientState) (toMPSCNNLossDescriptor descriptor) isLabelsGradientFilter

-- | This is a gradient filter - there is no support gradients of gradients currently.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsNSArray gradientImages) => mpsnnLossGradientNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnLossGradientNode gradientImages =
  sendMessage mpsnnLossGradientNode gradientFilterWithSourcesSelector (toNSArray gradientImages)

-- | @- lossType@
lossType :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO MPSCNNLossType
lossType mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode lossTypeSelector

-- | @- reductionType@
reductionType :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO MPSCNNReductionType
reductionType mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode reductionTypeSelector

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CULong
numberOfClasses mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode numberOfClassesSelector

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO Bool
reduceAcrossBatch mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode reduceAcrossBatchSelector

-- | @- weight@
weight :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
weight mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode weightSelector

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
labelSmoothing mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode labelSmoothingSelector

-- | @- epsilon@
epsilon :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
epsilon mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode epsilonSelector

-- | @- delta@
delta :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
delta mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode deltaSelector

-- | @- isLabelsGradientFilter@
isLabelsGradientFilter :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO Bool
isLabelsGradientFilter mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode isLabelsGradientFilterSelector

-- | propertyCallBack
--
-- Optional callback option - setting this allows the scalar weight value to be changed dynamically at encode time.              Default value: nil.
--
-- ObjC selector: @- propertyCallBack@
propertyCallBack :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO RawId
propertyCallBack mpsnnLossGradientNode =
  sendMessage mpsnnLossGradientNode propertyCallBackSelector

-- | propertyCallBack
--
-- Optional callback option - setting this allows the scalar weight value to be changed dynamically at encode time.              Default value: nil.
--
-- ObjC selector: @- setPropertyCallBack:@
setPropertyCallBack :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> RawId -> IO ()
setPropertyCallBack mpsnnLossGradientNode value =
  sendMessage mpsnnLossGradientNode setPropertyCallBackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, Id MPSCNNLossDescriptor, Bool] (Id MPSNNLossGradientNode)
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, Id MPSCNNLossDescriptor, Bool] (Id MPSNNLossGradientNode)
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @nodeWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector '[Id NSArray, Id MPSNNGradientStateNode, Id MPSCNNLossDescriptor, Bool] (Id MPSNNLossGradientNode)
nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "nodeWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, Id MPSCNNLossDescriptor, Bool] (Id MPSNNLossGradientNode)
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNGradientStateNode, Id MPSCNNLossDescriptor, Bool] (Id MPSNNLossGradientNode)
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector '[Id NSArray, Id MPSNNGradientStateNode, Id MPSCNNLossDescriptor, Bool] (Id MPSNNLossGradientNode)
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNGradientFilterNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector '[] MPSCNNLossType
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MPSCNNReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector '[] CULong
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector '[] Bool
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CFloat
weightSelector = mkSelector "weight"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector '[] CFloat
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

-- | @Selector@ for @isLabelsGradientFilter@
isLabelsGradientFilterSelector :: Selector '[] Bool
isLabelsGradientFilterSelector = mkSelector "isLabelsGradientFilter"

-- | @Selector@ for @propertyCallBack@
propertyCallBackSelector :: Selector '[] RawId
propertyCallBackSelector = mkSelector "propertyCallBack"

-- | @Selector@ for @setPropertyCallBack:@
setPropertyCallBackSelector :: Selector '[RawId] ()
setPropertyCallBackSelector = mkSelector "setPropertyCallBack:"

