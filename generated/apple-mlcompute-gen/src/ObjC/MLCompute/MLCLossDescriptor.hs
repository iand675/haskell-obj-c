{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCLossDescriptor
--
-- The MLCLossDescriptor specifies a loss filter descriptor.
--
-- Generated bindings for @MLCLossDescriptor@.
module ObjC.MLCompute.MLCLossDescriptor
  ( MLCLossDescriptor
  , IsMLCLossDescriptor(..)
  , new
  , init_
  , descriptorWithType_reductionType
  , descriptorWithType_reductionType_weight
  , descriptorWithType_reductionType_weight_labelSmoothing_classCount
  , descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_delta
  , lossType
  , reductionType
  , weight
  , labelSmoothing
  , classCount
  , epsilon
  , delta
  , classCountSelector
  , deltaSelector
  , descriptorWithType_reductionTypeSelector
  , descriptorWithType_reductionType_weightSelector
  , descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector
  , descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector
  , epsilonSelector
  , initSelector
  , labelSmoothingSelector
  , lossTypeSelector
  , newSelector
  , reductionTypeSelector
  , weightSelector

  -- * Enum types
  , MLCLossType(MLCLossType)
  , pattern MLCLossTypeMeanAbsoluteError
  , pattern MLCLossTypeMeanSquaredError
  , pattern MLCLossTypeSoftmaxCrossEntropy
  , pattern MLCLossTypeSigmoidCrossEntropy
  , pattern MLCLossTypeCategoricalCrossEntropy
  , pattern MLCLossTypeHinge
  , pattern MLCLossTypeHuber
  , pattern MLCLossTypeCosineDistance
  , pattern MLCLossTypeLog
  , pattern MLCLossTypeCount
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

-- | @+ new@
new :: IO (Id MLCLossDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCLossDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO (Id MLCLossDescriptor)
init_ mlcLossDescriptor =
  sendOwnedMessage mlcLossDescriptor initSelector

-- | Create a loss descriptor object
--
-- @lossType@ — The loss function.
--
-- @reductionType@ — The reduction operation
--
-- Returns: A new MLCLossDescriptor object
--
-- ObjC selector: @+ descriptorWithType:reductionType:@
descriptorWithType_reductionType :: MLCLossType -> MLCReductionType -> IO (Id MLCLossDescriptor)
descriptorWithType_reductionType lossType reductionType =
  do
    cls' <- getRequiredClass "MLCLossDescriptor"
    sendClassMessage cls' descriptorWithType_reductionTypeSelector lossType reductionType

-- | Create a loss descriptor object
--
-- @lossType@ — The loss function.
--
-- @reductionType@ — The reduction operation
--
-- @weight@ — The scale factor to apply to each element of a result.
--
-- Returns: A new MLCLossDescriptor object
--
-- ObjC selector: @+ descriptorWithType:reductionType:weight:@
descriptorWithType_reductionType_weight :: MLCLossType -> MLCReductionType -> CFloat -> IO (Id MLCLossDescriptor)
descriptorWithType_reductionType_weight lossType reductionType weight =
  do
    cls' <- getRequiredClass "MLCLossDescriptor"
    sendClassMessage cls' descriptorWithType_reductionType_weightSelector lossType reductionType weight

-- | Create a loss descriptor object
--
-- @lossType@ — The loss function.
--
-- @reductionType@ — The reduction operation
--
-- @weight@ — The scale factor to apply to each element of a result.
--
-- @labelSmoothing@ — The label smoothing parameter.
--
-- @classCount@ — The number of classes parameter.
--
-- Returns: A new MLCLossDescriptor object
--
-- ObjC selector: @+ descriptorWithType:reductionType:weight:labelSmoothing:classCount:@
descriptorWithType_reductionType_weight_labelSmoothing_classCount :: MLCLossType -> MLCReductionType -> CFloat -> CFloat -> CULong -> IO (Id MLCLossDescriptor)
descriptorWithType_reductionType_weight_labelSmoothing_classCount lossType reductionType weight labelSmoothing classCount =
  do
    cls' <- getRequiredClass "MLCLossDescriptor"
    sendClassMessage cls' descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector lossType reductionType weight labelSmoothing classCount

-- | Create a loss descriptor object
--
-- @lossType@ — The loss function.
--
-- @reductionType@ — The reduction operation
--
-- @weight@ — The scale factor to apply to each element of a result.
--
-- @labelSmoothing@ — The label smoothing parameter.
--
-- @classCount@ — The number of classes parameter.
--
-- @epsilon@ — The epsilon used by LogLoss
--
-- @delta@ — The delta parameter used by Huber loss
--
-- Returns: A new MLCLossDescriptor object
--
-- ObjC selector: @+ descriptorWithType:reductionType:weight:labelSmoothing:classCount:epsilon:delta:@
descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_delta :: MLCLossType -> MLCReductionType -> CFloat -> CFloat -> CULong -> CFloat -> CFloat -> IO (Id MLCLossDescriptor)
descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_delta lossType reductionType weight labelSmoothing classCount epsilon delta =
  do
    cls' <- getRequiredClass "MLCLossDescriptor"
    sendClassMessage cls' descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector lossType reductionType weight labelSmoothing classCount epsilon delta

-- | lossType
--
-- Specifies the loss function.
--
-- ObjC selector: @- lossType@
lossType :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO MLCLossType
lossType mlcLossDescriptor =
  sendMessage mlcLossDescriptor lossTypeSelector

-- | reductionType
--
-- The reduction operation performed by the loss function.
--
-- ObjC selector: @- reductionType@
reductionType :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO MLCReductionType
reductionType mlcLossDescriptor =
  sendMessage mlcLossDescriptor reductionTypeSelector

-- | weight
--
-- The scale factor to apply to each element of a result.  The default value is 1.0.
--
-- ObjC selector: @- weight@
weight :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
weight mlcLossDescriptor =
  sendMessage mlcLossDescriptor weightSelector

-- | labelSmoothing
--
-- The label smoothing parameter. The default value is 0.0.
--
-- This parameter is valid only for the loss functions of the following type(s):                     MLCLossTypeSoftmaxCrossEntropy and MLCLossTypeSigmoidCrossEntropy.
--
-- ObjC selector: @- labelSmoothing@
labelSmoothing :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
labelSmoothing mlcLossDescriptor =
  sendMessage mlcLossDescriptor labelSmoothingSelector

-- | numberOfClasses
--
-- The number of classes parameter. The default value is 1.
--
-- This parameter is valid only for the loss function MLCLossTypeSoftmaxCrossEntropy.
--
-- ObjC selector: @- classCount@
classCount :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CULong
classCount mlcLossDescriptor =
  sendMessage mlcLossDescriptor classCountSelector

-- | epsilon
--
-- The epsilon parameter. The default value is 1e-7.
--
-- This parameter is valid only for the loss function MLCLossTypeLog.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
epsilon mlcLossDescriptor =
  sendMessage mlcLossDescriptor epsilonSelector

-- | delta
--
-- The delta parameter. The default value is 1.0f.
--
-- This parameter is valid only for the loss function MLCLossTypeHuber.
--
-- ObjC selector: @- delta@
delta :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
delta mlcLossDescriptor =
  sendMessage mlcLossDescriptor deltaSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCLossDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCLossDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithType:reductionType:@
descriptorWithType_reductionTypeSelector :: Selector '[MLCLossType, MLCReductionType] (Id MLCLossDescriptor)
descriptorWithType_reductionTypeSelector = mkSelector "descriptorWithType:reductionType:"

-- | @Selector@ for @descriptorWithType:reductionType:weight:@
descriptorWithType_reductionType_weightSelector :: Selector '[MLCLossType, MLCReductionType, CFloat] (Id MLCLossDescriptor)
descriptorWithType_reductionType_weightSelector = mkSelector "descriptorWithType:reductionType:weight:"

-- | @Selector@ for @descriptorWithType:reductionType:weight:labelSmoothing:classCount:@
descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector :: Selector '[MLCLossType, MLCReductionType, CFloat, CFloat, CULong] (Id MLCLossDescriptor)
descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector = mkSelector "descriptorWithType:reductionType:weight:labelSmoothing:classCount:"

-- | @Selector@ for @descriptorWithType:reductionType:weight:labelSmoothing:classCount:epsilon:delta:@
descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector :: Selector '[MLCLossType, MLCReductionType, CFloat, CFloat, CULong, CFloat, CFloat] (Id MLCLossDescriptor)
descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector = mkSelector "descriptorWithType:reductionType:weight:labelSmoothing:classCount:epsilon:delta:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector '[] MLCLossType
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MLCReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CFloat
weightSelector = mkSelector "weight"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector '[] CFloat
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @classCount@
classCountSelector :: Selector '[] CULong
classCountSelector = mkSelector "classCount"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

