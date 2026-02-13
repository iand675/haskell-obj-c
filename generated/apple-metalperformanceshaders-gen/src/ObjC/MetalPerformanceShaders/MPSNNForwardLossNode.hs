{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Node representing a MPSNNForwardLoss kernel
--
-- Generated bindings for @MPSNNForwardLossNode@.
module ObjC.MetalPerformanceShaders.MPSNNForwardLossNode
  ( MPSNNForwardLossNode
  , IsMPSNNForwardLossNode(..)
  , nodeWithSource_labels_weights_lossDescriptor
  , nodeWithSource_labels_lossDescriptor
  , nodeWithSources_lossDescriptor
  , initWithSource_labels_weights_lossDescriptor
  , initWithSource_labels_lossDescriptor
  , initWithSources_lossDescriptor
  , gradientFilterWithSources
  , gradientFiltersWithSources
  , gradientFilterWithSource
  , gradientFiltersWithSource
  , lossType
  , reductionType
  , numberOfClasses
  , reduceAcrossBatch
  , weight
  , labelSmoothing
  , epsilon
  , delta
  , propertyCallBack
  , setPropertyCallBack
  , deltaSelector
  , epsilonSelector
  , gradientFilterWithSourceSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourceSelector
  , gradientFiltersWithSourcesSelector
  , initWithSource_labels_lossDescriptorSelector
  , initWithSource_labels_weights_lossDescriptorSelector
  , initWithSources_lossDescriptorSelector
  , labelSmoothingSelector
  , lossTypeSelector
  , nodeWithSource_labels_lossDescriptorSelector
  , nodeWithSource_labels_weights_lossDescriptorSelector
  , nodeWithSources_lossDescriptorSelector
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

-- | @+ nodeWithSource:labels:weights:lossDescriptor:@
nodeWithSource_labels_weights_lossDescriptor :: (IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSCNNLossDescriptor descriptor) => source -> labels -> weights -> descriptor -> IO (Id MPSNNForwardLossNode)
nodeWithSource_labels_weights_lossDescriptor source labels weights descriptor =
  do
    cls' <- getRequiredClass "MPSNNForwardLossNode"
    sendClassMessage cls' nodeWithSource_labels_weights_lossDescriptorSelector (toMPSNNImageNode source) (toMPSNNImageNode labels) (toMPSNNImageNode weights) (toMPSCNNLossDescriptor descriptor)

-- | @+ nodeWithSource:labels:lossDescriptor:@
nodeWithSource_labels_lossDescriptor :: (IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSCNNLossDescriptor descriptor) => source -> labels -> descriptor -> IO (Id MPSNNForwardLossNode)
nodeWithSource_labels_lossDescriptor source labels descriptor =
  do
    cls' <- getRequiredClass "MPSNNForwardLossNode"
    sendClassMessage cls' nodeWithSource_labels_lossDescriptorSelector (toMPSNNImageNode source) (toMPSNNImageNode labels) (toMPSCNNLossDescriptor descriptor)

-- | Init a forward loss node from multiple images
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter                                      Node0: logits, Node1: labels, Node2: weights
--
-- Returns: A new MPSNNFilter node.
--
-- ObjC selector: @+ nodeWithSources:lossDescriptor:@
nodeWithSources_lossDescriptor :: (IsNSArray sourceNodes, IsMPSCNNLossDescriptor descriptor) => sourceNodes -> descriptor -> IO (Id MPSNNForwardLossNode)
nodeWithSources_lossDescriptor sourceNodes descriptor =
  do
    cls' <- getRequiredClass "MPSNNForwardLossNode"
    sendClassMessage cls' nodeWithSources_lossDescriptorSelector (toNSArray sourceNodes) (toMPSCNNLossDescriptor descriptor)

-- | @- initWithSource:labels:weights:lossDescriptor:@
initWithSource_labels_weights_lossDescriptor :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSCNNLossDescriptor descriptor) => mpsnnForwardLossNode -> source -> labels -> weights -> descriptor -> IO (Id MPSNNForwardLossNode)
initWithSource_labels_weights_lossDescriptor mpsnnForwardLossNode source labels weights descriptor =
  sendOwnedMessage mpsnnForwardLossNode initWithSource_labels_weights_lossDescriptorSelector (toMPSNNImageNode source) (toMPSNNImageNode labels) (toMPSNNImageNode weights) (toMPSCNNLossDescriptor descriptor)

-- | @- initWithSource:labels:lossDescriptor:@
initWithSource_labels_lossDescriptor :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSCNNLossDescriptor descriptor) => mpsnnForwardLossNode -> source -> labels -> descriptor -> IO (Id MPSNNForwardLossNode)
initWithSource_labels_lossDescriptor mpsnnForwardLossNode source labels descriptor =
  sendOwnedMessage mpsnnForwardLossNode initWithSource_labels_lossDescriptorSelector (toMPSNNImageNode source) (toMPSNNImageNode labels) (toMPSCNNLossDescriptor descriptor)

-- | Init a forward loss node from multiple images
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter                                      Node0: logits, Node1: labels, Node2: weights
--
-- Returns: A new MPSNNFilter node.
--
-- ObjC selector: @- initWithSources:lossDescriptor:@
initWithSources_lossDescriptor :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsNSArray sourceNodes, IsMPSCNNLossDescriptor descriptor) => mpsnnForwardLossNode -> sourceNodes -> descriptor -> IO (Id MPSNNForwardLossNode)
initWithSources_lossDescriptor mpsnnForwardLossNode sourceNodes descriptor =
  sendOwnedMessage mpsnnForwardLossNode initWithSources_lossDescriptorSelector (toNSArray sourceNodes) (toMPSCNNLossDescriptor descriptor)

-- | Returns the gradient filter for predictions, if you want also gradients for labels then use -gradientFiltersWithSource(s):
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsNSArray sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id MPSNNLossGradientNode)
gradientFilterWithSources mpsnnForwardLossNode sourceGradient =
  sendMessage mpsnnForwardLossNode gradientFilterWithSourcesSelector (toNSArray sourceGradient)

-- | @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsNSArray sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSources mpsnnForwardLossNode sourceGradient =
  sendMessage mpsnnForwardLossNode gradientFiltersWithSourcesSelector (toNSArray sourceGradient)

-- | @- gradientFilterWithSource:@
gradientFilterWithSource :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id MPSNNLossGradientNode)
gradientFilterWithSource mpsnnForwardLossNode sourceGradient =
  sendMessage mpsnnForwardLossNode gradientFilterWithSourceSelector (toMPSNNImageNode sourceGradient)

-- | @- gradientFiltersWithSource:@
gradientFiltersWithSource :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSource mpsnnForwardLossNode sourceGradient =
  sendMessage mpsnnForwardLossNode gradientFiltersWithSourceSelector (toMPSNNImageNode sourceGradient)

-- | @- lossType@
lossType :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO MPSCNNLossType
lossType mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode lossTypeSelector

-- | @- reductionType@
reductionType :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO MPSCNNReductionType
reductionType mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode reductionTypeSelector

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CULong
numberOfClasses mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode numberOfClassesSelector

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO Bool
reduceAcrossBatch mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode reduceAcrossBatchSelector

-- | @- weight@
weight :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
weight mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode weightSelector

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
labelSmoothing mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode labelSmoothingSelector

-- | @- epsilon@
epsilon :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
epsilon mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode epsilonSelector

-- | @- delta@
delta :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
delta mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode deltaSelector

-- | propertyCallBack
--
-- Optional callback option - setting this allows the scalar weight value to be changed dynamically at encode time.              Default value: nil.
--
-- ObjC selector: @- propertyCallBack@
propertyCallBack :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO RawId
propertyCallBack mpsnnForwardLossNode =
  sendMessage mpsnnForwardLossNode propertyCallBackSelector

-- | propertyCallBack
--
-- Optional callback option - setting this allows the scalar weight value to be changed dynamically at encode time.              Default value: nil.
--
-- ObjC selector: @- setPropertyCallBack:@
setPropertyCallBack :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> RawId -> IO ()
setPropertyCallBack mpsnnForwardLossNode value =
  sendMessage mpsnnForwardLossNode setPropertyCallBackSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:labels:weights:lossDescriptor:@
nodeWithSource_labels_weights_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNLossDescriptor] (Id MPSNNForwardLossNode)
nodeWithSource_labels_weights_lossDescriptorSelector = mkSelector "nodeWithSource:labels:weights:lossDescriptor:"

-- | @Selector@ for @nodeWithSource:labels:lossDescriptor:@
nodeWithSource_labels_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNLossDescriptor] (Id MPSNNForwardLossNode)
nodeWithSource_labels_lossDescriptorSelector = mkSelector "nodeWithSource:labels:lossDescriptor:"

-- | @Selector@ for @nodeWithSources:lossDescriptor:@
nodeWithSources_lossDescriptorSelector :: Selector '[Id NSArray, Id MPSCNNLossDescriptor] (Id MPSNNForwardLossNode)
nodeWithSources_lossDescriptorSelector = mkSelector "nodeWithSources:lossDescriptor:"

-- | @Selector@ for @initWithSource:labels:weights:lossDescriptor:@
initWithSource_labels_weights_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNLossDescriptor] (Id MPSNNForwardLossNode)
initWithSource_labels_weights_lossDescriptorSelector = mkSelector "initWithSource:labels:weights:lossDescriptor:"

-- | @Selector@ for @initWithSource:labels:lossDescriptor:@
initWithSource_labels_lossDescriptorSelector :: Selector '[Id MPSNNImageNode, Id MPSNNImageNode, Id MPSCNNLossDescriptor] (Id MPSNNForwardLossNode)
initWithSource_labels_lossDescriptorSelector = mkSelector "initWithSource:labels:lossDescriptor:"

-- | @Selector@ for @initWithSources:lossDescriptor:@
initWithSources_lossDescriptorSelector :: Selector '[Id NSArray, Id MPSCNNLossDescriptor] (Id MPSNNForwardLossNode)
initWithSources_lossDescriptorSelector = mkSelector "initWithSources:lossDescriptor:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector '[Id NSArray] (Id MPSNNLossGradientNode)
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector '[Id NSArray] (Id NSArray)
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

-- | @Selector@ for @gradientFilterWithSource:@
gradientFilterWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id MPSNNLossGradientNode)
gradientFilterWithSourceSelector = mkSelector "gradientFilterWithSource:"

-- | @Selector@ for @gradientFiltersWithSource:@
gradientFiltersWithSourceSelector :: Selector '[Id MPSNNImageNode] (Id NSArray)
gradientFiltersWithSourceSelector = mkSelector "gradientFiltersWithSource:"

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

-- | @Selector@ for @propertyCallBack@
propertyCallBackSelector :: Selector '[] RawId
propertyCallBackSelector = mkSelector "propertyCallBack"

-- | @Selector@ for @setPropertyCallBack:@
setPropertyCallBackSelector :: Selector '[RawId] ()
setPropertyCallBackSelector = mkSelector "setPropertyCallBack:"

