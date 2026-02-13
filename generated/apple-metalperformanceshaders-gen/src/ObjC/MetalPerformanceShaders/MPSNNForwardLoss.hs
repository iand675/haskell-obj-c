{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSNNForwardLoss
--
-- This depends on Metal.framework.
--
-- The MPSNNForwardLoss filter specifies a version of the loss filter which separates the forward              computation from the gradient computation. In order to compute gradients for the loss filter              use MPSNNLossGradient filter and in order to start the gradient computation of an arbitrary              image use the MPSNNInitialGradient filter.              NOTE: This filter does not support non-default offset or cliprects and setting them to other              than default values will result in undefined results.
--
-- Generated bindings for @MPSNNForwardLoss@.
module ObjC.MetalPerformanceShaders.MPSNNForwardLoss
  ( MPSNNForwardLoss
  , IsMPSNNForwardLoss(..)
  , initWithDevice
  , initWithDevice_lossDescriptor
  , initWithCoder_device
  , encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImages
  , encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporary
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
  , deltaSelector
  , encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporarySelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , labelSmoothingSelector
  , lossTypeSelector
  , numberOfClassesSelector
  , reduceAcrossBatchSelector
  , reductionTypeSelector
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
initWithDevice :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> RawId -> IO (Id MPSNNForwardLoss)
initWithDevice mpsnnForwardLoss device =
  sendOwnedMessage mpsnnForwardLoss initWithDeviceSelector device

-- | Initialize the loss forward pass filter with a loss descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @lossDescriptor@ — The loss descriptor.
--
-- Returns: A valid MPSNNForwardLoss object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptor :: (IsMPSNNForwardLoss mpsnnForwardLoss, IsMPSCNNLossDescriptor lossDescriptor) => mpsnnForwardLoss -> RawId -> lossDescriptor -> IO (Id MPSNNForwardLoss)
initWithDevice_lossDescriptor mpsnnForwardLoss device lossDescriptor =
  sendOwnedMessage mpsnnForwardLoss initWithDevice_lossDescriptorSelector device (toMPSCNNLossDescriptor lossDescriptor)

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSNNForwardLoss mpsnnForwardLoss, IsNSCoder aDecoder) => mpsnnForwardLoss -> aDecoder -> RawId -> IO (Id MPSNNForwardLoss)
initWithCoder_device mpsnnForwardLoss aDecoder device =
  sendOwnedMessage mpsnnForwardLoss initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a MPSNNForwardLoss filter and return the result in the destinationImage.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode.
--
-- @sourceImages@ — The source images that contains the network prediction (logits).
--
-- @labels@ — The source images that contains the labels (targets).
--
-- @weights@ — The object containing weights for the labels. Optional.
--
-- @destinationStates@ — Optional gradient state - carries dynamical property values to the gradient pass                                  (weight, labelSmoothing, epsilon, delta). Create state using resultStateBatchForSourceImage: or                                  temporaryResultStateBatchForCommandBuffer:.
--
-- @destinationImages@ — The MPSImages into which to write the loss results.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:labels:weights:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImages :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> RawId -> RawId -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImages mpsnnForwardLoss commandBuffer sourceImages labels weights destinationStates destinationImages =
  sendMessage mpsnnForwardLoss encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImagesSelector commandBuffer sourceImages labels weights destinationStates destinationImages

-- | Encode a MPSNNForwardLoss filter and return the loss result image(s).
--
-- This -encode call is similar to the encodeBatchToCommandBuffer:sourceImages:labels:destinationImages: above,              except that it creates and returns the MPSImages with the loss result.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode.
--
-- @sourceImages@ — The source images that contains the network prediction (logits).
--
-- @labels@ — The source images that contains the labels (targets).
--
-- @weights@ — The object containing weights for the labels. Optional.
--
-- @outStates@ — Optional gradient state - carries dynamical property values to the gradient pass                                  (weight, labelSmoothing, epsilon, delta).
--
-- @isTemporary@ — Whether the returned state (if any) should be temporary or not.
--
-- Returns: The MPSImages containing the loss computation results.
--
-- ObjC selector: @- encodeBatchToCommandBuffer:sourceImages:labels:weights:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporary :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> RawId -> RawId -> RawId -> RawId -> RawId -> Bool -> IO RawId
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporary mpsnnForwardLoss commandBuffer sourceImages labels weights outStates isTemporary =
  sendMessage mpsnnForwardLoss encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporarySelector commandBuffer sourceImages labels weights outStates isTemporary

-- | See MPSCNNLossDescriptor for information about the following properties.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO MPSCNNLossType
lossType mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss lossTypeSelector

-- | @- reductionType@
reductionType :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO MPSCNNReductionType
reductionType mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss reductionTypeSelector

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO Bool
reduceAcrossBatch mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss reduceAcrossBatchSelector

-- | @- numberOfClasses@
numberOfClasses :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CULong
numberOfClasses mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss numberOfClassesSelector

-- | @- weight@
weight :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
weight mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss weightSelector

-- | @- setWeight:@
setWeight :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setWeight mpsnnForwardLoss value =
  sendMessage mpsnnForwardLoss setWeightSelector value

-- | @- labelSmoothing@
labelSmoothing :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
labelSmoothing mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss labelSmoothingSelector

-- | @- setLabelSmoothing:@
setLabelSmoothing :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setLabelSmoothing mpsnnForwardLoss value =
  sendMessage mpsnnForwardLoss setLabelSmoothingSelector value

-- | @- epsilon@
epsilon :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
epsilon mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss epsilonSelector

-- | @- setEpsilon:@
setEpsilon :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setEpsilon mpsnnForwardLoss value =
  sendMessage mpsnnForwardLoss setEpsilonSelector value

-- | @- delta@
delta :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> IO CFloat
delta mpsnnForwardLoss =
  sendMessage mpsnnForwardLoss deltaSelector

-- | @- setDelta:@
setDelta :: IsMPSNNForwardLoss mpsnnForwardLoss => mpsnnForwardLoss -> CFloat -> IO ()
setDelta mpsnnForwardLoss value =
  sendMessage mpsnnForwardLoss setDeltaSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSNNForwardLoss)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptorSelector :: Selector '[RawId, Id MPSCNNLossDescriptor] (Id MPSNNForwardLoss)
initWithDevice_lossDescriptorSelector = mkSelector "initWithDevice:lossDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSNNForwardLoss)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:labels:weights:destinationStates:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:weights:destinationStates:destinationImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:labels:weights:destinationStates:destinationStateIsTemporary:@
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporarySelector :: Selector '[RawId, RawId, RawId, RawId, RawId, Bool] RawId
encodeBatchToCommandBuffer_sourceImages_labels_weights_destinationStates_destinationStateIsTemporarySelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:weights:destinationStates:destinationStateIsTemporary:"

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

