{-# LANGUAGE PatternSynonyms #-}
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
  , nodeWithSource_labels_weights_lossDescriptorSelector
  , nodeWithSource_labels_lossDescriptorSelector
  , nodeWithSources_lossDescriptorSelector
  , initWithSource_labels_weights_lossDescriptorSelector
  , initWithSource_labels_lossDescriptorSelector
  , initWithSources_lossDescriptorSelector
  , gradientFilterWithSourcesSelector
  , gradientFiltersWithSourcesSelector
  , gradientFilterWithSourceSelector
  , gradientFiltersWithSourceSelector
  , lossTypeSelector
  , reductionTypeSelector
  , numberOfClassesSelector
  , reduceAcrossBatchSelector
  , weightSelector
  , labelSmoothingSelector
  , epsilonSelector
  , deltaSelector

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

-- | @+ nodeWithSource:labels:weights:lossDescriptor:@
nodeWithSource_labels_weights_lossDescriptor :: (IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSCNNLossDescriptor descriptor) => source -> labels -> weights -> descriptor -> IO (Id MPSNNForwardLossNode)
nodeWithSource_labels_weights_lossDescriptor source labels weights descriptor =
  do
    cls' <- getRequiredClass "MPSNNForwardLossNode"
    withObjCPtr source $ \raw_source ->
      withObjCPtr labels $ \raw_labels ->
        withObjCPtr weights $ \raw_weights ->
          withObjCPtr descriptor $ \raw_descriptor ->
            sendClassMsg cls' (mkSelector "nodeWithSource:labels:weights:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @+ nodeWithSource:labels:lossDescriptor:@
nodeWithSource_labels_lossDescriptor :: (IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSCNNLossDescriptor descriptor) => source -> labels -> descriptor -> IO (Id MPSNNForwardLossNode)
nodeWithSource_labels_lossDescriptor source labels descriptor =
  do
    cls' <- getRequiredClass "MPSNNForwardLossNode"
    withObjCPtr source $ \raw_source ->
      withObjCPtr labels $ \raw_labels ->
        withObjCPtr descriptor $ \raw_descriptor ->
          sendClassMsg cls' (mkSelector "nodeWithSource:labels:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr sourceNodes $ \raw_sourceNodes ->
      withObjCPtr descriptor $ \raw_descriptor ->
        sendClassMsg cls' (mkSelector "nodeWithSources:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithSource:labels:weights:lossDescriptor:@
initWithSource_labels_weights_lossDescriptor :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSCNNLossDescriptor descriptor) => mpsnnForwardLossNode -> source -> labels -> weights -> descriptor -> IO (Id MPSNNForwardLossNode)
initWithSource_labels_weights_lossDescriptor mpsnnForwardLossNode  source labels weights descriptor =
withObjCPtr source $ \raw_source ->
  withObjCPtr labels $ \raw_labels ->
    withObjCPtr weights $ \raw_weights ->
      withObjCPtr descriptor $ \raw_descriptor ->
          sendMsg mpsnnForwardLossNode (mkSelector "initWithSource:labels:weights:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSource:labels:lossDescriptor:@
initWithSource_labels_lossDescriptor :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode source, IsMPSNNImageNode labels, IsMPSCNNLossDescriptor descriptor) => mpsnnForwardLossNode -> source -> labels -> descriptor -> IO (Id MPSNNForwardLossNode)
initWithSource_labels_lossDescriptor mpsnnForwardLossNode  source labels descriptor =
withObjCPtr source $ \raw_source ->
  withObjCPtr labels $ \raw_labels ->
    withObjCPtr descriptor $ \raw_descriptor ->
        sendMsg mpsnnForwardLossNode (mkSelector "initWithSource:labels:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_source :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Init a forward loss node from multiple images
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter                                      Node0: logits, Node1: labels, Node2: weights
--
-- Returns: A new MPSNNFilter node.
--
-- ObjC selector: @- initWithSources:lossDescriptor:@
initWithSources_lossDescriptor :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsNSArray sourceNodes, IsMPSCNNLossDescriptor descriptor) => mpsnnForwardLossNode -> sourceNodes -> descriptor -> IO (Id MPSNNForwardLossNode)
initWithSources_lossDescriptor mpsnnForwardLossNode  sourceNodes descriptor =
withObjCPtr sourceNodes $ \raw_sourceNodes ->
  withObjCPtr descriptor $ \raw_descriptor ->
      sendMsg mpsnnForwardLossNode (mkSelector "initWithSources:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ())] >>= ownedObject . castPtr

-- | Returns the gradient filter for predictions, if you want also gradients for labels then use -gradientFiltersWithSource(s):
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsNSArray sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id MPSNNLossGradientNode)
gradientFilterWithSources mpsnnForwardLossNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnForwardLossNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- gradientFiltersWithSources:@
gradientFiltersWithSources :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsNSArray sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSources mpsnnForwardLossNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnForwardLossNode (mkSelector "gradientFiltersWithSources:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- gradientFilterWithSource:@
gradientFilterWithSource :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id MPSNNLossGradientNode)
gradientFilterWithSource mpsnnForwardLossNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnForwardLossNode (mkSelector "gradientFilterWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- gradientFiltersWithSource:@
gradientFiltersWithSource :: (IsMPSNNForwardLossNode mpsnnForwardLossNode, IsMPSNNImageNode sourceGradient) => mpsnnForwardLossNode -> sourceGradient -> IO (Id NSArray)
gradientFiltersWithSource mpsnnForwardLossNode  sourceGradient =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
    sendMsg mpsnnForwardLossNode (mkSelector "gradientFiltersWithSource:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ())] >>= retainedObject . castPtr

-- | @- lossType@
lossType :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO MPSCNNLossType
lossType mpsnnForwardLossNode  =
  fmap (coerce :: CUInt -> MPSCNNLossType) $ sendMsg mpsnnForwardLossNode (mkSelector "lossType") retCUInt []

-- | @- reductionType@
reductionType :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO MPSCNNReductionType
reductionType mpsnnForwardLossNode  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpsnnForwardLossNode (mkSelector "reductionType") retCInt []

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CULong
numberOfClasses mpsnnForwardLossNode  =
  sendMsg mpsnnForwardLossNode (mkSelector "numberOfClasses") retCULong []

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO Bool
reduceAcrossBatch mpsnnForwardLossNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnForwardLossNode (mkSelector "reduceAcrossBatch") retCULong []

-- | @- weight@
weight :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
weight mpsnnForwardLossNode  =
  sendMsg mpsnnForwardLossNode (mkSelector "weight") retCFloat []

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
labelSmoothing mpsnnForwardLossNode  =
  sendMsg mpsnnForwardLossNode (mkSelector "labelSmoothing") retCFloat []

-- | @- epsilon@
epsilon :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
epsilon mpsnnForwardLossNode  =
  sendMsg mpsnnForwardLossNode (mkSelector "epsilon") retCFloat []

-- | @- delta@
delta :: IsMPSNNForwardLossNode mpsnnForwardLossNode => mpsnnForwardLossNode -> IO CFloat
delta mpsnnForwardLossNode  =
  sendMsg mpsnnForwardLossNode (mkSelector "delta") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSource:labels:weights:lossDescriptor:@
nodeWithSource_labels_weights_lossDescriptorSelector :: Selector
nodeWithSource_labels_weights_lossDescriptorSelector = mkSelector "nodeWithSource:labels:weights:lossDescriptor:"

-- | @Selector@ for @nodeWithSource:labels:lossDescriptor:@
nodeWithSource_labels_lossDescriptorSelector :: Selector
nodeWithSource_labels_lossDescriptorSelector = mkSelector "nodeWithSource:labels:lossDescriptor:"

-- | @Selector@ for @nodeWithSources:lossDescriptor:@
nodeWithSources_lossDescriptorSelector :: Selector
nodeWithSources_lossDescriptorSelector = mkSelector "nodeWithSources:lossDescriptor:"

-- | @Selector@ for @initWithSource:labels:weights:lossDescriptor:@
initWithSource_labels_weights_lossDescriptorSelector :: Selector
initWithSource_labels_weights_lossDescriptorSelector = mkSelector "initWithSource:labels:weights:lossDescriptor:"

-- | @Selector@ for @initWithSource:labels:lossDescriptor:@
initWithSource_labels_lossDescriptorSelector :: Selector
initWithSource_labels_lossDescriptorSelector = mkSelector "initWithSource:labels:lossDescriptor:"

-- | @Selector@ for @initWithSources:lossDescriptor:@
initWithSources_lossDescriptorSelector :: Selector
initWithSources_lossDescriptorSelector = mkSelector "initWithSources:lossDescriptor:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

-- | @Selector@ for @gradientFiltersWithSources:@
gradientFiltersWithSourcesSelector :: Selector
gradientFiltersWithSourcesSelector = mkSelector "gradientFiltersWithSources:"

-- | @Selector@ for @gradientFilterWithSource:@
gradientFilterWithSourceSelector :: Selector
gradientFilterWithSourceSelector = mkSelector "gradientFilterWithSource:"

-- | @Selector@ for @gradientFiltersWithSource:@
gradientFiltersWithSourceSelector :: Selector
gradientFiltersWithSourceSelector = mkSelector "gradientFiltersWithSource:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

