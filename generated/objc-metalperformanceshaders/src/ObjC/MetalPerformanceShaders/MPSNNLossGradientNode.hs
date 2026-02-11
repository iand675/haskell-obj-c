{-# LANGUAGE PatternSynonyms #-}
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
  , nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector
  , gradientFilterWithSourcesSelector
  , lossTypeSelector
  , reductionTypeSelector
  , numberOfClassesSelector
  , reduceAcrossBatchSelector
  , weightSelector
  , labelSmoothingSelector
  , epsilonSelector
  , deltaSelector
  , isLabelsGradientFilterSelector

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

-- | @+ nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => sourceGradient -> sourceImage -> labels -> weights -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter sourceGradient sourceImage labels weights gradientState descriptor isLabelsGradientFilter =
  do
    cls' <- getRequiredClass "MPSNNLossGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr labels $ \raw_labels ->
          withObjCPtr weights $ \raw_weights ->
            withObjCPtr gradientState $ \raw_gradientState ->
              withObjCPtr descriptor $ \raw_descriptor ->
                sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (if isLabelsGradientFilter then 1 else 0)] >>= retainedObject . castPtr

-- | @+ nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => sourceGradient -> sourceImage -> labels -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter sourceGradient sourceImage labels gradientState descriptor isLabelsGradientFilter =
  do
    cls' <- getRequiredClass "MPSNNLossGradientNode"
    withObjCPtr sourceGradient $ \raw_sourceGradient ->
      withObjCPtr sourceImage $ \raw_sourceImage ->
        withObjCPtr labels $ \raw_labels ->
          withObjCPtr gradientState $ \raw_gradientState ->
            withObjCPtr descriptor $ \raw_descriptor ->
              sendClassMsg cls' (mkSelector "nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (if isLabelsGradientFilter then 1 else 0)] >>= retainedObject . castPtr

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
    withObjCPtr sourceNodes $ \raw_sourceNodes ->
      withObjCPtr gradientState $ \raw_gradientState ->
        withObjCPtr descriptor $ \raw_descriptor ->
          sendClassMsg cls' (mkSelector "nodeWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (if isLabelsGradientFilter then 1 else 0)] >>= retainedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNImageNode weights, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => mpsnnLossGradientNode -> sourceGradient -> sourceImage -> labels -> weights -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilter mpsnnLossGradientNode  sourceGradient sourceImage labels weights gradientState descriptor isLabelsGradientFilter =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr labels $ \raw_labels ->
      withObjCPtr weights $ \raw_weights ->
        withObjCPtr gradientState $ \raw_gradientState ->
          withObjCPtr descriptor $ \raw_descriptor ->
              sendMsg mpsnnLossGradientNode (mkSelector "initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (if isLabelsGradientFilter then 1 else 0)] >>= ownedObject . castPtr

-- | @- initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsMPSNNImageNode sourceGradient, IsMPSNNImageNode sourceImage, IsMPSNNImageNode labels, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => mpsnnLossGradientNode -> sourceGradient -> sourceImage -> labels -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilter mpsnnLossGradientNode  sourceGradient sourceImage labels gradientState descriptor isLabelsGradientFilter =
withObjCPtr sourceGradient $ \raw_sourceGradient ->
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr labels $ \raw_labels ->
      withObjCPtr gradientState $ \raw_gradientState ->
        withObjCPtr descriptor $ \raw_descriptor ->
            sendMsg mpsnnLossGradientNode (mkSelector "initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceGradient :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (if isLabelsGradientFilter then 1 else 0)] >>= ownedObject . castPtr

-- | Init a gradient loss node from multiple images
--
-- @sourceNodes@ — The MPSNNImageNode representing the source MPSImages for the filter                                      Node0: input gradients, Node1: logits, Node2: labels, Node3: weights
--
-- Returns: A new MPSNNFilter node.
--
-- ObjC selector: @- initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilter :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsNSArray sourceNodes, IsMPSNNGradientStateNode gradientState, IsMPSCNNLossDescriptor descriptor) => mpsnnLossGradientNode -> sourceNodes -> gradientState -> descriptor -> Bool -> IO (Id MPSNNLossGradientNode)
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilter mpsnnLossGradientNode  sourceNodes gradientState descriptor isLabelsGradientFilter =
withObjCPtr sourceNodes $ \raw_sourceNodes ->
  withObjCPtr gradientState $ \raw_gradientState ->
    withObjCPtr descriptor $ \raw_descriptor ->
        sendMsg mpsnnLossGradientNode (mkSelector "initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:") (retPtr retVoid) [argPtr (castPtr raw_sourceNodes :: Ptr ()), argPtr (castPtr raw_gradientState :: Ptr ()), argPtr (castPtr raw_descriptor :: Ptr ()), argCULong (if isLabelsGradientFilter then 1 else 0)] >>= ownedObject . castPtr

-- | This is a gradient filter - there is no support gradients of gradients currently.
--
-- ObjC selector: @- gradientFilterWithSources:@
gradientFilterWithSources :: (IsMPSNNLossGradientNode mpsnnLossGradientNode, IsNSArray gradientImages) => mpsnnLossGradientNode -> gradientImages -> IO (Id MPSNNGradientFilterNode)
gradientFilterWithSources mpsnnLossGradientNode  gradientImages =
withObjCPtr gradientImages $ \raw_gradientImages ->
    sendMsg mpsnnLossGradientNode (mkSelector "gradientFilterWithSources:") (retPtr retVoid) [argPtr (castPtr raw_gradientImages :: Ptr ())] >>= retainedObject . castPtr

-- | @- lossType@
lossType :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO MPSCNNLossType
lossType mpsnnLossGradientNode  =
  fmap (coerce :: CUInt -> MPSCNNLossType) $ sendMsg mpsnnLossGradientNode (mkSelector "lossType") retCUInt []

-- | @- reductionType@
reductionType :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO MPSCNNReductionType
reductionType mpsnnLossGradientNode  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpsnnLossGradientNode (mkSelector "reductionType") retCInt []

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CULong
numberOfClasses mpsnnLossGradientNode  =
  sendMsg mpsnnLossGradientNode (mkSelector "numberOfClasses") retCULong []

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO Bool
reduceAcrossBatch mpsnnLossGradientNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnLossGradientNode (mkSelector "reduceAcrossBatch") retCULong []

-- | @- weight@
weight :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
weight mpsnnLossGradientNode  =
  sendMsg mpsnnLossGradientNode (mkSelector "weight") retCFloat []

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
labelSmoothing mpsnnLossGradientNode  =
  sendMsg mpsnnLossGradientNode (mkSelector "labelSmoothing") retCFloat []

-- | @- epsilon@
epsilon :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
epsilon mpsnnLossGradientNode  =
  sendMsg mpsnnLossGradientNode (mkSelector "epsilon") retCFloat []

-- | @- delta@
delta :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO CFloat
delta mpsnnLossGradientNode  =
  sendMsg mpsnnLossGradientNode (mkSelector "delta") retCFloat []

-- | @- isLabelsGradientFilter@
isLabelsGradientFilter :: IsMPSNNLossGradientNode mpsnnLossGradientNode => mpsnnLossGradientNode -> IO Bool
isLabelsGradientFilter mpsnnLossGradientNode  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpsnnLossGradientNode (mkSelector "isLabelsGradientFilter") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector
nodeWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "nodeWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector
nodeWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "nodeWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @nodeWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector
nodeWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "nodeWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector
initWithSourceGradient_sourceImage_labels_weights_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "initWithSourceGradient:sourceImage:labels:weights:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector
initWithSourceGradient_sourceImage_labels_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "initWithSourceGradient:sourceImage:labels:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:@
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector :: Selector
initWithSources_gradientState_lossDescriptor_isLabelsGradientFilterSelector = mkSelector "initWithSources:gradientState:lossDescriptor:isLabelsGradientFilter:"

-- | @Selector@ for @gradientFilterWithSources:@
gradientFilterWithSourcesSelector :: Selector
gradientFilterWithSourcesSelector = mkSelector "gradientFilterWithSources:"

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

-- | @Selector@ for @isLabelsGradientFilter@
isLabelsGradientFilterSelector :: Selector
isLabelsGradientFilterSelector = mkSelector "isLabelsGradientFilter"

