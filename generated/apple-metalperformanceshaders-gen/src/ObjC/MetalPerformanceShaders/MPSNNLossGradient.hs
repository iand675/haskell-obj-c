{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNLossGradient
--
-- This depends on Metal.framework.
--
-- The MPSNNLossGradient filter specifies the gradient filter for MPSNNForwardLoss.
--
-- Generated bindings for @MPSNNLossGradient@.
module ObjC.MetalPerformanceShaders.MPSNNLossGradient
  ( MPSNNLossGradient
  , IsMPSNNLossGradient(..)
  , initWithDevice
  , initWithDevice_lossDescriptor
  , initWithCoder_device
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradients
  , lossType
  , reductionType
  , reduceAcrossBatch
  , numberOfClasses
  , weight
  , setWeight
  , labelSmoothing
  , setLabelSmoothing
  , epsilon
  , setEpsilon
  , delta
  , setDelta
  , computeLabelGradients
  , setComputeLabelGradients
  , computeLabelGradientsSelector
  , deltaSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStatesSelector
  , encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradientsSelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , labelSmoothingSelector
  , lossTypeSelector
  , numberOfClassesSelector
  , reduceAcrossBatchSelector
  , reductionTypeSelector
  , setComputeLabelGradientsSelector
  , setDeltaSelector
  , setEpsilonSelector
  , setLabelSmoothingSelector
  , setWeightSelector
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

-- | @- initWithDevice:@
initWithDevice :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> RawId -> IO (Id MPSNNLossGradient)
initWithDevice mpsnnLossGradient device =
  sendOwnedMessage mpsnnLossGradient initWithDeviceSelector device

-- | Initialize the loss gradient filter with a loss descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @lossDescriptor@ — The loss descriptor.
--
-- Returns: A valid MPSNNLossGradient object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptor :: (IsMPSNNLossGradient mpsnnLossGradient, IsMPSCNNLossDescriptor lossDescriptor) => mpsnnLossGradient -> RawId -> lossDescriptor -> IO (Id MPSNNLossGradient)
initWithDevice_lossDescriptor mpsnnLossGradient device lossDescriptor =
  sendOwnedMessage mpsnnLossGradient initWithDevice_lossDescriptorSelector device (toMPSCNNLossDescriptor lossDescriptor)

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNLossGradient mpsnnLossGradient, IsNSCoder aDecoder) => mpsnnLossGradient -> aDecoder -> RawId -> IO (Id MPSNNLossGradient)
initWithCoder_device mpsnnLossGradient aDecoder device =
  sendOwnedMessage mpsnnLossGradient initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode the loss gradient filter and return a gradient
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode
--
-- @sourceGradients@ — The gradient images from the "next" filter in the graph
--
-- @sourceImages@ — The images used as source image from the forward pass
--
-- @labels@ — The source images that contains the labels (targets).
--
-- @weights@ — The object containing weights for the labels. Optional.
--
-- @sourceStates@ — Optional gradient state - carries dynamical property values from the forward pass                                  (weight, labelSmoothing, epsilon, delta).
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:labels:weights:sourceStates:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates mpsnnLossGradient commandBuffer sourceGradients sourceImages labels weights sourceStates =
  sendMessage mpsnnLossGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStatesSelector commandBuffer sourceGradients sourceImages labels weights sourceStates

-- | Encode the loss gradient filter and return a gradient
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode
--
-- @sourceGradients@ — The gradient images from the "next" filter in the graph
--
-- @sourceImages@ — The image used as source images from the forward pass
--
-- @labels@ — The source images that contains the labels (targets).
--
-- @weights@ — The object containing weights for the labels. Optional.
--
-- @sourceStates@ — Optional gradient state - carries dynamical property values from the forward pass                                  (weight, labelSmoothing, epsilon, delta).
--
-- @destinationGradients@ — The MPSImages into which to write the filter result
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceGradients:sourceImages:labels:weights:sourceStates:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradients :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradients mpsnnLossGradient commandBuffer sourceGradients sourceImages labels weights sourceStates destinationGradients =
  sendMessage mpsnnLossGradient encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradientsSelector commandBuffer sourceGradients sourceImages labels weights sourceStates destinationGradients

-- | See MPSCNNLossDescriptor for information about the following properties.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO MPSCNNLossType
lossType mpsnnLossGradient =
  sendMessage mpsnnLossGradient lossTypeSelector

-- | @- reductionType@
reductionType :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO MPSCNNReductionType
reductionType mpsnnLossGradient =
  sendMessage mpsnnLossGradient reductionTypeSelector

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO Bool
reduceAcrossBatch mpsnnLossGradient =
  sendMessage mpsnnLossGradient reduceAcrossBatchSelector

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CULong
numberOfClasses mpsnnLossGradient =
  sendMessage mpsnnLossGradient numberOfClassesSelector

-- | @- weight@
weight :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
weight mpsnnLossGradient =
  sendMessage mpsnnLossGradient weightSelector

-- | @- setWeight:@
setWeight :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setWeight mpsnnLossGradient value =
  sendMessage mpsnnLossGradient setWeightSelector value

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
labelSmoothing mpsnnLossGradient =
  sendMessage mpsnnLossGradient labelSmoothingSelector

-- | @- setLabelSmoothing:@
setLabelSmoothing :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setLabelSmoothing mpsnnLossGradient value =
  sendMessage mpsnnLossGradient setLabelSmoothingSelector value

-- | @- epsilon@
epsilon :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
epsilon mpsnnLossGradient =
  sendMessage mpsnnLossGradient epsilonSelector

-- | @- setEpsilon:@
setEpsilon :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setEpsilon mpsnnLossGradient value =
  sendMessage mpsnnLossGradient setEpsilonSelector value

-- | @- delta@
delta :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO CFloat
delta mpsnnLossGradient =
  sendMessage mpsnnLossGradient deltaSelector

-- | @- setDelta:@
setDelta :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> CFloat -> IO ()
setDelta mpsnnLossGradient value =
  sendMessage mpsnnLossGradient setDeltaSelector value

-- | computeLabelGradients
--
-- The computeLabelGradients property is used to control whether the loss gradient              filter computes gradients for the primary (predictions) or secondary (labels) source image from the forward pass.              Default: NO.
--
-- ObjC selector: @- computeLabelGradients@
computeLabelGradients :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> IO Bool
computeLabelGradients mpsnnLossGradient =
  sendMessage mpsnnLossGradient computeLabelGradientsSelector

-- | computeLabelGradients
--
-- The computeLabelGradients property is used to control whether the loss gradient              filter computes gradients for the primary (predictions) or secondary (labels) source image from the forward pass.              Default: NO.
--
-- ObjC selector: @- setComputeLabelGradients:@
setComputeLabelGradients :: IsMPSNNLossGradient mpsnnLossGradient => mpsnnLossGradient -> Bool -> IO ()
setComputeLabelGradients mpsnnLossGradient value =
  sendMessage mpsnnLossGradient setComputeLabelGradientsSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNLossGradient)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptorSelector :: Selector '[RawId, Id MPSCNNLossDescriptor] (Id MPSNNLossGradient)
initWithDevice_lossDescriptorSelector = mkSelector "initWithDevice:lossDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNLossGradient)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:labels:weights:sourceStates:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStatesSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId] RawId
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStatesSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:labels:weights:sourceStates:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceGradients:sourceImages:labels:weights:sourceStates:destinationGradients:@
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradientsSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceGradients_sourceImages_labels_weights_sourceStates_destinationGradientsSelector = mkSelector "encodeBatchToCommandBuffer:sourceGradients:sourceImages:labels:weights:sourceStates:destinationGradients:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector '[] MPSCNNLossType
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MPSCNNReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector '[] Bool
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector '[] CULong
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CFloat
weightSelector = mkSelector "weight"

-- | @Selector@ for @setWeight:@
setWeightSelector :: Selector '[CFloat] ()
setWeightSelector = mkSelector "setWeight:"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector '[] CFloat
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @setLabelSmoothing:@
setLabelSmoothingSelector :: Selector '[CFloat] ()
setLabelSmoothingSelector = mkSelector "setLabelSmoothing:"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @setEpsilon:@
setEpsilonSelector :: Selector '[CFloat] ()
setEpsilonSelector = mkSelector "setEpsilon:"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

-- | @Selector@ for @setDelta:@
setDeltaSelector :: Selector '[CFloat] ()
setDeltaSelector = mkSelector "setDelta:"

-- | @Selector@ for @computeLabelGradients@
computeLabelGradientsSelector :: Selector '[] Bool
computeLabelGradientsSelector = mkSelector "computeLabelGradients"

-- | @Selector@ for @setComputeLabelGradients:@
setComputeLabelGradientsSelector :: Selector '[Bool] ()
setComputeLabelGradientsSelector = mkSelector "setComputeLabelGradients:"

