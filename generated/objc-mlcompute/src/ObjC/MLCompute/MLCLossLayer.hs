{-# LANGUAGE PatternSynonyms #-}
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
  , layerWithDescriptorSelector
  , layerWithDescriptor_weightsSelector
  , softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector
  , softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector
  , categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector
  , categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector
  , sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector
  , sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector
  , logLossWithReductionType_epsilon_weightSelector
  , logLossWithReductionType_epsilon_weightsSelector
  , huberLossWithReductionType_delta_weightSelector
  , huberLossWithReductionType_delta_weightsSelector
  , meanAbsoluteErrorLossWithReductionType_weightSelector
  , meanAbsoluteErrorLossWithReductionType_weightsSelector
  , meanSquaredErrorLossWithReductionType_weightSelector
  , meanSquaredErrorLossWithReductionType_weightsSelector
  , hingeLossWithReductionType_weightSelector
  , hingeLossWithReductionType_weightsSelector
  , cosineDistanceLossWithReductionType_weightSelector
  , cosineDistanceLossWithReductionType_weightsSelector
  , descriptorSelector
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
    withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
      sendClassMsg cls' (mkSelector "layerWithDescriptor:") (retPtr retVoid) [argPtr (castPtr raw_lossDescriptor :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
      withObjCPtr weights $ \raw_weights ->
        sendClassMsg cls' (mkSelector "layerWithDescriptor:weights:") (retPtr retVoid) [argPtr (castPtr raw_lossDescriptor :: Ptr ()), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral labelSmoothing), argCULong (fromIntegral classCount), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral labelSmoothing), argCULong (fromIntegral classCount), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral labelSmoothing), argCULong (fromIntegral classCount), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral labelSmoothing), argCULong (fromIntegral classCount), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral labelSmoothing), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral labelSmoothing), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "logLossWithReductionType:epsilon:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral epsilon), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "logLossWithReductionType:epsilon:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral epsilon), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "huberLossWithReductionType:delta:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral delta), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "huberLossWithReductionType:delta:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral delta), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "meanAbsoluteErrorLossWithReductionType:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "meanAbsoluteErrorLossWithReductionType:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "meanSquaredErrorLossWithReductionType:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "meanSquaredErrorLossWithReductionType:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "hingeLossWithReductionType:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "hingeLossWithReductionType:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "cosineDistanceLossWithReductionType:weight:") (retPtr retVoid) [argCInt (coerce reductionType), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    withObjCPtr weights $ \raw_weights ->
      sendClassMsg cls' (mkSelector "cosineDistanceLossWithReductionType:weights:") (retPtr retVoid) [argCInt (coerce reductionType), argPtr (castPtr raw_weights :: Ptr ())] >>= retainedObject . castPtr

-- | descriptor
--
-- The loss descriptor
--
-- ObjC selector: @- descriptor@
descriptor :: IsMLCLossLayer mlcLossLayer => mlcLossLayer -> IO (Id MLCLossDescriptor)
descriptor mlcLossLayer  =
  sendMsg mlcLossLayer (mkSelector "descriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | weights
--
-- The loss label weights tensor
--
-- ObjC selector: @- weights@
weights :: IsMLCLossLayer mlcLossLayer => mlcLossLayer -> IO (Id MLCTensor)
weights mlcLossLayer  =
  sendMsg mlcLossLayer (mkSelector "weights") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @layerWithDescriptor:@
layerWithDescriptorSelector :: Selector
layerWithDescriptorSelector = mkSelector "layerWithDescriptor:"

-- | @Selector@ for @layerWithDescriptor:weights:@
layerWithDescriptor_weightsSelector :: Selector
layerWithDescriptor_weightsSelector = mkSelector "layerWithDescriptor:weights:"

-- | @Selector@ for @softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:@
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector :: Selector
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector = mkSelector "softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:"

-- | @Selector@ for @softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:@
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector :: Selector
softmaxCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector = mkSelector "softmaxCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:"

-- | @Selector@ for @categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:@
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector :: Selector
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightSelector = mkSelector "categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weight:"

-- | @Selector@ for @categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:@
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector :: Selector
categoricalCrossEntropyLossWithReductionType_labelSmoothing_classCount_weightsSelector = mkSelector "categoricalCrossEntropyLossWithReductionType:labelSmoothing:classCount:weights:"

-- | @Selector@ for @sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weight:@
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector :: Selector
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightSelector = mkSelector "sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weight:"

-- | @Selector@ for @sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weights:@
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector :: Selector
sigmoidCrossEntropyLossWithReductionType_labelSmoothing_weightsSelector = mkSelector "sigmoidCrossEntropyLossWithReductionType:labelSmoothing:weights:"

-- | @Selector@ for @logLossWithReductionType:epsilon:weight:@
logLossWithReductionType_epsilon_weightSelector :: Selector
logLossWithReductionType_epsilon_weightSelector = mkSelector "logLossWithReductionType:epsilon:weight:"

-- | @Selector@ for @logLossWithReductionType:epsilon:weights:@
logLossWithReductionType_epsilon_weightsSelector :: Selector
logLossWithReductionType_epsilon_weightsSelector = mkSelector "logLossWithReductionType:epsilon:weights:"

-- | @Selector@ for @huberLossWithReductionType:delta:weight:@
huberLossWithReductionType_delta_weightSelector :: Selector
huberLossWithReductionType_delta_weightSelector = mkSelector "huberLossWithReductionType:delta:weight:"

-- | @Selector@ for @huberLossWithReductionType:delta:weights:@
huberLossWithReductionType_delta_weightsSelector :: Selector
huberLossWithReductionType_delta_weightsSelector = mkSelector "huberLossWithReductionType:delta:weights:"

-- | @Selector@ for @meanAbsoluteErrorLossWithReductionType:weight:@
meanAbsoluteErrorLossWithReductionType_weightSelector :: Selector
meanAbsoluteErrorLossWithReductionType_weightSelector = mkSelector "meanAbsoluteErrorLossWithReductionType:weight:"

-- | @Selector@ for @meanAbsoluteErrorLossWithReductionType:weights:@
meanAbsoluteErrorLossWithReductionType_weightsSelector :: Selector
meanAbsoluteErrorLossWithReductionType_weightsSelector = mkSelector "meanAbsoluteErrorLossWithReductionType:weights:"

-- | @Selector@ for @meanSquaredErrorLossWithReductionType:weight:@
meanSquaredErrorLossWithReductionType_weightSelector :: Selector
meanSquaredErrorLossWithReductionType_weightSelector = mkSelector "meanSquaredErrorLossWithReductionType:weight:"

-- | @Selector@ for @meanSquaredErrorLossWithReductionType:weights:@
meanSquaredErrorLossWithReductionType_weightsSelector :: Selector
meanSquaredErrorLossWithReductionType_weightsSelector = mkSelector "meanSquaredErrorLossWithReductionType:weights:"

-- | @Selector@ for @hingeLossWithReductionType:weight:@
hingeLossWithReductionType_weightSelector :: Selector
hingeLossWithReductionType_weightSelector = mkSelector "hingeLossWithReductionType:weight:"

-- | @Selector@ for @hingeLossWithReductionType:weights:@
hingeLossWithReductionType_weightsSelector :: Selector
hingeLossWithReductionType_weightsSelector = mkSelector "hingeLossWithReductionType:weights:"

-- | @Selector@ for @cosineDistanceLossWithReductionType:weight:@
cosineDistanceLossWithReductionType_weightSelector :: Selector
cosineDistanceLossWithReductionType_weightSelector = mkSelector "cosineDistanceLossWithReductionType:weight:"

-- | @Selector@ for @cosineDistanceLossWithReductionType:weights:@
cosineDistanceLossWithReductionType_weightsSelector :: Selector
cosineDistanceLossWithReductionType_weightsSelector = mkSelector "cosineDistanceLossWithReductionType:weights:"

-- | @Selector@ for @descriptor@
descriptorSelector :: Selector
descriptorSelector = mkSelector "descriptor"

-- | @Selector@ for @weights@
weightsSelector :: Selector
weightsSelector = mkSelector "weights"

