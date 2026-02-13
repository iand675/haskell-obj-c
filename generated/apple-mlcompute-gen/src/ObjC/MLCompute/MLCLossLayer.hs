{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCLossLayer
--
-- A loss layer
--
-- Generated bindings for @MLCLossLayer@.
module ObjC.MLCompute.MLCLossLayer
  ( MLCLossLayer
  , IsMLCLossLayer(..)
  , layerWithDescriptor
  , layerWithDescriptor_weights
  , softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weight
  , softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weights
  , categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weight
  , categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weights
  , sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weight
  , sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weights
  , logLossWithReductionType_epsilon_weight
  , logLossWithReductionType_epsilon_weights
  , huberLossWithReductionType_delta_weight
  , huberLossWithReductionType_delta_weights
  , meanAbsoluteErrorLossWithReductionType_weight
  , meanAbsoluteErrorLossWithReductionType_weights
  , meanSquaredErrorLossWithReductionType_weight
  , meanSquaredErrorLossWithReductionType_weights
  , hingeLossWithReductionType_weight
  , hingeLossWithReductionType_weights
  , cosineDistanceLossWithReductionType_weight
  , cosineDistanceLossWithReductionType_weights
  , descriptor
  , weights
  , categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector
  , categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector
  , cosineDistanceLossWithReductionType_weightSelector
  , cosineDistanceLossWithReductionType_weightsSelector
  , descriptorSelector
  , hingeLossWithReductionType_weightSelector
  , hingeLossWithReductionType_weightsSelector
  , huberLossWithReductionType_delta_weightSelector
  , huberLossWithReductionType_delta_weightsSelector
  , layerWithDescriptorSelector
  , layerWithDescriptor_weightsSelector
  , logLossWithReductionType_epsilon_weightSelector
  , logLossWithReductionType_epsilon_weightsSelector
  , meanAbsoluteErrorLossWithReductionType_weightSelector
  , meanAbsoluteErrorLossWithReductionType_weightsSelector
  , meanSquaredErrorLossWithReductionType_weightSelector
  , meanSquaredErrorLossWithReductionType_weightsSelector
  , sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector
  , sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector
  , softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector
  , softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector
  , weightsSelector

  -- * Enum types
  , MLCReductionType(MLCReductionType)
  , pattern MLCReductionTypeNone
  , pattern MLCReductionTypeSum
  , pattern MLCReductionTypeMean
  , pattern MLCReductionTypeMax
  , pattern MLCReductionTypeMin
  , pattern MLCReductionTypeArgMax
  , pattern MLCReductionTypeArgMin
  , pattern MLCReductionTypeL1Norm
  , pattern MLCReductionTypeAny
  , pattern MLCReductionTypeAll
  , pattern MLCReductionTypeCount

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.MLCompute.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Create a loss layer
--
-- @lossDescriptor@ — The loss descriptor
--
-- Returns: A new loss layer.
--
-- ObjC selector: @+ layerWithDescriptor:@
layerWithDescriptor :: IsMLCLossDescriptor lossDescriptor => lossDescriptor -> IO (Id MLCLossLayer)
layerWithDescriptor lossDescriptor =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' layerWithDescriptorSelector (toMLCLossDescriptor lossDescriptor)

-- | Create a MLComputeLoss layer
--
-- @lossDescriptor@ — The loss descriptor
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new loss layer.
--
-- ObjC selector: @+ layerWithDescriptor:weights:@
layerWithDescriptor_weights :: (IsMLCLossDescriptor lossDescriptor, IsMLCTensor weights) => lossDescriptor -> weights -> IO (Id MLCLossLayer)
layerWithDescriptor_weights lossDescriptor weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' layerWithDescriptor_weightsSelector (toMLCLossDescriptor lossDescriptor) (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @labelSmoothing@ — Label smoothing value
--
-- @classCount@ — Number of classes
--
-- @weight@ — A scalar floating point value
--
-- Returns: A new softmax cross entropy loss layer.
--
-- ObjC selector: @+ softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:@
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weight :: MLCReductionType -> CFloat -> CULong -> CFloat -> IO (Id MLCLossLayer)
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weight reductionType labelSmoothing classCount weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector reductionType labelSmoothing classCount weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @labelSmoothing@ — Label smoothing value
--
-- @classCount@ — Number of classes
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new softmax cross entropy loss layer.
--
-- ObjC selector: @+ softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:@
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weights :: IsMLCTensor weights => MLCReductionType -> CFloat -> CULong -> weights -> IO (Id MLCLossLayer)
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weights reductionType labelSmoothing classCount weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector reductionType labelSmoothing classCount (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @labelSmoothing@ — Label smoothing value
--
-- @classCount@ — Number of classes
--
-- @weight@ — A scalar floating point value
--
-- Returns: A new categorical cross entropy loss layer.
--
-- ObjC selector: @+ categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:@
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weight :: MLCReductionType -> CFloat -> CULong -> CFloat -> IO (Id MLCLossLayer)
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weight reductionType labelSmoothing classCount weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector reductionType labelSmoothing classCount weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @labelSmoothing@ — Label smoothing value
--
-- @classCount@ — Number of classes
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new categorical cross entropy loss layer.
--
-- ObjC selector: @+ categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:@
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weights :: IsMLCTensor weights => MLCReductionType -> CFloat -> CULong -> weights -> IO (Id MLCLossLayer)
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weights reductionType labelSmoothing classCount weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector reductionType labelSmoothing classCount (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @labelSmoothing@ — Label smoothing value
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new sigmoid cross entropy loss layer.
--
-- ObjC selector: @+ sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weight:@
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weight :: MLCReductionType -> CFloat -> CFloat -> IO (Id MLCLossLayer)
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weight reductionType labelSmoothing weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector reductionType labelSmoothing weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @labelSmoothing@ — Label smoothing value
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new sigmoid cross entropy loss layer.
--
-- ObjC selector: @+ sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weights:@
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weights :: IsMLCTensor weights => MLCReductionType -> CFloat -> weights -> IO (Id MLCLossLayer)
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weights reductionType labelSmoothing weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector reductionType labelSmoothing (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @epsilon@ — The epsilon parameter
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new log loss layer.
--
-- ObjC selector: @+ logLossWithReductionType:epsilon:weight:@
logLossWithReductionType_epsilon_weight :: MLCReductionType -> CFloat -> CFloat -> IO (Id MLCLossLayer)
logLossWithReductionType_epsilon_weight reductionType epsilon weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' logLossWithReductionType_epsilon_weightSelector reductionType epsilon weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @epsilon@ — The epsilon parameter
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new log loss layer.
--
-- ObjC selector: @+ logLossWithReductionType:epsilon:weights:@
logLossWithReductionType_epsilon_weights :: IsMLCTensor weights => MLCReductionType -> CFloat -> weights -> IO (Id MLCLossLayer)
logLossWithReductionType_epsilon_weights reductionType epsilon weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' logLossWithReductionType_epsilon_weightsSelector reductionType epsilon (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @delta@ — The delta parameter
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new huber loss layer.
--
-- ObjC selector: @+ huberLossWithReductionType:delta:weight:@
huberLossWithReductionType_delta_weight :: MLCReductionType -> CFloat -> CFloat -> IO (Id MLCLossLayer)
huberLossWithReductionType_delta_weight reductionType delta weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' huberLossWithReductionType_delta_weightSelector reductionType delta weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @delta@ — The delta parameter
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new huber loss layer.
--
-- ObjC selector: @+ huberLossWithReductionType:delta:weights:@
huberLossWithReductionType_delta_weights :: IsMLCTensor weights => MLCReductionType -> CFloat -> weights -> IO (Id MLCLossLayer)
huberLossWithReductionType_delta_weights reductionType delta weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' huberLossWithReductionType_delta_weightsSelector reductionType delta (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new L1 i.e. mean absolute error loss layer.
--
-- ObjC selector: @+ meanAbsoluteErrorLossWithReductionType:weight:@
meanAbsoluteErrorLossWithReductionType_weight :: MLCReductionType -> CFloat -> IO (Id MLCLossLayer)
meanAbsoluteErrorLossWithReductionType_weight reductionType weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' meanAbsoluteErrorLossWithReductionType_weightSelector reductionType weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new L1 i.e. mean absolute error loss layer.
--
-- ObjC selector: @+ meanAbsoluteErrorLossWithReductionType:weights:@
meanAbsoluteErrorLossWithReductionType_weights :: IsMLCTensor weights => MLCReductionType -> weights -> IO (Id MLCLossLayer)
meanAbsoluteErrorLossWithReductionType_weights reductionType weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' meanAbsoluteErrorLossWithReductionType_weightsSelector reductionType (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new L2 i.e. mean squared error loss layer.
--
-- ObjC selector: @+ meanSquaredErrorLossWithReductionType:weight:@
meanSquaredErrorLossWithReductionType_weight :: MLCReductionType -> CFloat -> IO (Id MLCLossLayer)
meanSquaredErrorLossWithReductionType_weight reductionType weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' meanSquaredErrorLossWithReductionType_weightSelector reductionType weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new L2 i.e. mean squared error loss layer.
--
-- ObjC selector: @+ meanSquaredErrorLossWithReductionType:weights:@
meanSquaredErrorLossWithReductionType_weights :: IsMLCTensor weights => MLCReductionType -> weights -> IO (Id MLCLossLayer)
meanSquaredErrorLossWithReductionType_weights reductionType weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' meanSquaredErrorLossWithReductionType_weightsSelector reductionType (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new hinge loss layer.
--
-- ObjC selector: @+ hingeLossWithReductionType:weight:@
hingeLossWithReductionType_weight :: MLCReductionType -> CFloat -> IO (Id MLCLossLayer)
hingeLossWithReductionType_weight reductionType weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' hingeLossWithReductionType_weightSelector reductionType weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new hinge loss layer.
--
-- ObjC selector: @+ hingeLossWithReductionType:weights:@
hingeLossWithReductionType_weights :: IsMLCTensor weights => MLCReductionType -> weights -> IO (Id MLCLossLayer)
hingeLossWithReductionType_weights reductionType weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' hingeLossWithReductionType_weightsSelector reductionType (toMLCTensor weights)

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weight@ — A scalar floating-point value
--
-- Returns: A new cosine distance loss layer.
--
-- ObjC selector: @+ cosineDistanceLossWithReductionType:weight:@
cosineDistanceLossWithReductionType_weight :: MLCReductionType -> CFloat -> IO (Id MLCLossLayer)
cosineDistanceLossWithReductionType_weight reductionType weight =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' cosineDistanceLossWithReductionType_weightSelector reductionType weight

-- | Create a loss layer
--
-- @reductionType@ — The reduction type to use
--
-- @weights@ — The loss label weights tensor
--
-- Returns: A new cosine distance loss layer.
--
-- ObjC selector: @+ cosineDistanceLossWithReductionType:weights:@
cosineDistanceLossWithReductionType_weights :: IsMLCTensor weights => MLCReductionType -> weights -> IO (Id MLCLossLayer)
cosineDistanceLossWithReductionType_weights reductionType weights =
  do
    cls' <- getRequiredClass "MLCLossLayer"
    sendClassMessage cls' cosineDistanceLossWithReductionType_weightsSelector reductionType (toMLCTensor weights)

-- | descriptor
--
-- The loss descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCLossLayer mlcLossLayer => mlcLossLayer -> IO (Id MLCLossDescriptor)
descriptor mlcLossLayer =
  sendMessage mlcLossLayer descriptorSelector

-- | weights
--
-- The loss label weights tensor
--
-- ObjC selector: @- weights@
weights :: IsMLCLossLayer mlcLossLayer => mlcLossLayer -> IO (Id MLCTensor)
weights mlcLossLayer =
  sendMessage mlcLossLayer weightsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector '[Id MLCLossDescriptor] (Id MLCLossLayer)
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @layerWithDescriptor:weights:@
layerWithDescriptor_weightsSelector :: Selector '[Id MLCLossDescriptor, Id MLCTensor] (Id MLCLossLayer)
layerWithDescriptor_weightsSelector = mkSelector "layerWithDescriptor:weights:"

-- | @Selector@ for @softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:@
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector :: Selector '[MLCReductionType, CFloat, CULong, CFloat] (Id MLCLossLayer)
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector = mkSelector "softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:"

-- | @Selector@ for @softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:@
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector :: Selector '[MLCReductionType, CFloat, CULong, Id MLCTensor] (Id MLCLossLayer)
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector = mkSelector "softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:"

-- | @Selector@ for @categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:@
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector :: Selector '[MLCReductionType, CFloat, CULong, CFloat] (Id MLCLossLayer)
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector = mkSelector "categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:"

-- | @Selector@ for @categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:@
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector :: Selector '[MLCReductionType, CFloat, CULong, Id MLCTensor] (Id MLCLossLayer)
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector = mkSelector "categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:"

-- | @Selector@ for @sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weight:@
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector :: Selector '[MLCReductionType, CFloat, CFloat] (Id MLCLossLayer)
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector = mkSelector "sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weight:"

-- | @Selector@ for @sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weights:@
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector :: Selector '[MLCReductionType, CFloat, Id MLCTensor] (Id MLCLossLayer)
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector = mkSelector "sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weights:"

-- | @Selector@ for @logLossWithReductionType:epsilon:weight:@
logLossWithReductionType_epsilon_weightSelector :: Selector '[MLCReductionType, CFloat, CFloat] (Id MLCLossLayer)
logLossWithReductionType_epsilon_weightSelector = mkSelector "logLossWithReductionType:epsilon:weight:"

-- | @Selector@ for @logLossWithReductionType:epsilon:weights:@
logLossWithReductionType_epsilon_weightsSelector :: Selector '[MLCReductionType, CFloat, Id MLCTensor] (Id MLCLossLayer)
logLossWithReductionType_epsilon_weightsSelector = mkSelector "logLossWithReductionType:epsilon:weights:"

-- | @Selector@ for @huberLossWithReductionType:delta:weight:@
huberLossWithReductionType_delta_weightSelector :: Selector '[MLCReductionType, CFloat, CFloat] (Id MLCLossLayer)
huberLossWithReductionType_delta_weightSelector = mkSelector "huberLossWithReductionType:delta:weight:"

-- | @Selector@ for @huberLossWithReductionType:delta:weights:@
huberLossWithReductionType_delta_weightsSelector :: Selector '[MLCReductionType, CFloat, Id MLCTensor] (Id MLCLossLayer)
huberLossWithReductionType_delta_weightsSelector = mkSelector "huberLossWithReductionType:delta:weights:"

-- | @Selector@ for @meanAbsoluteErrorLossWithReductionType:weight:@
meanAbsoluteErrorLossWithReductionType_weightSelector :: Selector '[MLCReductionType, CFloat] (Id MLCLossLayer)
meanAbsoluteErrorLossWithReductionType_weightSelector = mkSelector "meanAbsoluteErrorLossWithReductionType:weight:"

-- | @Selector@ for @meanAbsoluteErrorLossWithReductionType:weights:@
meanAbsoluteErrorLossWithReductionType_weightsSelector :: Selector '[MLCReductionType, Id MLCTensor] (Id MLCLossLayer)
meanAbsoluteErrorLossWithReductionType_weightsSelector = mkSelector "meanAbsoluteErrorLossWithReductionType:weights:"

-- | @Selector@ for @meanSquaredErrorLossWithReductionType:weight:@
meanSquaredErrorLossWithReductionType_weightSelector :: Selector '[MLCReductionType, CFloat] (Id MLCLossLayer)
meanSquaredErrorLossWithReductionType_weightSelector = mkSelector "meanSquaredErrorLossWithReductionType:weight:"

-- | @Selector@ for @meanSquaredErrorLossWithReductionType:weights:@
meanSquaredErrorLossWithReductionType_weightsSelector :: Selector '[MLCReductionType, Id MLCTensor] (Id MLCLossLayer)
meanSquaredErrorLossWithReductionType_weightsSelector = mkSelector "meanSquaredErrorLossWithReductionType:weights:"

-- | @Selector@ for @hingeLossWithReductionType:weight:@
hingeLossWithReductionType_weightSelector :: Selector '[MLCReductionType, CFloat] (Id MLCLossLayer)
hingeLossWithReductionType_weightSelector = mkSelector "hingeLossWithReductionType:weight:"

-- | @Selector@ for @hingeLossWithReductionType:weights:@
hingeLossWithReductionType_weightsSelector :: Selector '[MLCReductionType, Id MLCTensor] (Id MLCLossLayer)
hingeLossWithReductionType_weightsSelector = mkSelector "hingeLossWithReductionType:weights:"

-- | @Selector@ for @cosineDistanceLossWithReductionType:weight:@
cosineDistanceLossWithReductionType_weightSelector :: Selector '[MLCReductionType, CFloat] (Id MLCLossLayer)
cosineDistanceLossWithReductionType_weightSelector = mkSelector "cosineDistanceLossWithReductionType:weight:"

-- | @Selector@ for @cosineDistanceLossWithReductionType:weights:@
cosineDistanceLossWithReductionType_weightsSelector :: Selector '[MLCReductionType, Id MLCTensor] (Id MLCLossLayer)
cosineDistanceLossWithReductionType_weightsSelector = mkSelector "cosineDistanceLossWithReductionType:weights:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector '[] (Id MLCLossDescriptor)
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector '[] (Id MLCTensor)
weightsSelector = mkSelector "weights"

