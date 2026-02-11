{-# LANGUAGE PatternSynonyms #-}
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
  , newSelector
  , initSelector
  , descriptorWithType_reductionTypeSelector
  , descriptorWithType_reductionType_weightSelector
  , descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector
  , descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector
  , lossTypeSelector
  , reductionTypeSelector
  , weightSelector
  , labelSmoothingSelector
  , classCountSelector
  , epsilonSelector
  , deltaSelector

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

-- | @+ new@
new :: IO (Id MLCLossDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCLossDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO (Id MLCLossDescriptor)
init_ mlcLossDescriptor  =
  sendMsg mlcLossDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:reductionType:") (retPtr retVoid) [argCInt (coerce lossType), argCInt (coerce reductionType)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:reductionType:weight:") (retPtr retVoid) [argCInt (coerce lossType), argCInt (coerce reductionType), argCFloat (fromIntegral weight)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:reductionType:weight:labelSmoothing:classCount:") (retPtr retVoid) [argCInt (coerce lossType), argCInt (coerce reductionType), argCFloat (fromIntegral weight), argCFloat (fromIntegral labelSmoothing), argCULong (fromIntegral classCount)] >>= retainedObject . castPtr

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
    sendClassMsg cls' (mkSelector "descriptorWithType:reductionType:weight:labelSmoothing:classCount:epsilon:delta:") (retPtr retVoid) [argCInt (coerce lossType), argCInt (coerce reductionType), argCFloat (fromIntegral weight), argCFloat (fromIntegral labelSmoothing), argCULong (fromIntegral classCount), argCFloat (fromIntegral epsilon), argCFloat (fromIntegral delta)] >>= retainedObject . castPtr

-- | lossType
--
-- Specifies the loss function.
--
-- ObjC selector: @- lossType@
lossType :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO MLCLossType
lossType mlcLossDescriptor  =
  fmap (coerce :: CInt -> MLCLossType) $ sendMsg mlcLossDescriptor (mkSelector "lossType") retCInt []

-- | reductionType
--
-- The reduction operation performed by the loss function.
--
-- ObjC selector: @- reductionType@
reductionType :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO MLCReductionType
reductionType mlcLossDescriptor  =
  fmap (coerce :: CInt -> MLCReductionType) $ sendMsg mlcLossDescriptor (mkSelector "reductionType") retCInt []

-- | weight
--
-- The scale factor to apply to each element of a result.  The default value is 1.0.
--
-- ObjC selector: @- weight@
weight :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
weight mlcLossDescriptor  =
  sendMsg mlcLossDescriptor (mkSelector "weight") retCFloat []

-- | labelSmoothing
--
-- The label smoothing parameter. The default value is 0.0.
--
-- This parameter is valid only for the loss functions of the following type(s):                     MLCLossTypeSoftmaxCrossEntropy and MLCLossTypeSigmoidCrossEntropy.
--
-- ObjC selector: @- labelSmoothing@
labelSmoothing :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
labelSmoothing mlcLossDescriptor  =
  sendMsg mlcLossDescriptor (mkSelector "labelSmoothing") retCFloat []

-- | numberOfClasses
--
-- The number of classes parameter. The default value is 1.
--
-- This parameter is valid only for the loss function MLCLossTypeSoftmaxCrossEntropy.
--
-- ObjC selector: @- classCount@
classCount :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CULong
classCount mlcLossDescriptor  =
  sendMsg mlcLossDescriptor (mkSelector "classCount") retCULong []

-- | epsilon
--
-- The epsilon parameter. The default value is 1e-7.
--
-- This parameter is valid only for the loss function MLCLossTypeLog.
--
-- ObjC selector: @- epsilon@
epsilon :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
epsilon mlcLossDescriptor  =
  sendMsg mlcLossDescriptor (mkSelector "epsilon") retCFloat []

-- | delta
--
-- The delta parameter. The default value is 1.0f.
--
-- This parameter is valid only for the loss function MLCLossTypeHuber.
--
-- ObjC selector: @- delta@
delta :: IsMLCLossDescriptor mlcLossDescriptor => mlcLossDescriptor -> IO CFloat
delta mlcLossDescriptor  =
  sendMsg mlcLossDescriptor (mkSelector "delta") retCFloat []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithType:reductionType:@
descriptorWithType_reductionTypeSelector :: Selector
descriptorWithType_reductionTypeSelector = mkSelector "descriptorWithType:reductionType:"

-- | @Selector@ for @descriptorWithType:reductionType:weight:@
descriptorWithType_reductionType_weightSelector :: Selector
descriptorWithType_reductionType_weightSelector = mkSelector "descriptorWithType:reductionType:weight:"

-- | @Selector@ for @descriptorWithType:reductionType:weight:labelSmoothing:classCount:@
descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector :: Selector
descriptorWithType_reductionType_weight_labelSmoothing_classCountSelector = mkSelector "descriptorWithType:reductionType:weight:labelSmoothing:classCount:"

-- | @Selector@ for @descriptorWithType:reductionType:weight:labelSmoothing:classCount:epsilon:delta:@
descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector :: Selector
descriptorWithType_reductionType_weight_labelSmoothing_classCount_epsilon_deltaSelector = mkSelector "descriptorWithType:reductionType:weight:labelSmoothing:classCount:epsilon:delta:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @weight@
weightSelector :: Selector
weightSelector = mkSelector "weight"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @classCount@
classCountSelector :: Selector
classCountSelector = mkSelector "classCount"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

