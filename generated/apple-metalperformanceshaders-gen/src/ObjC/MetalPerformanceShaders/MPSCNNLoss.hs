{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNLoss
--
-- This depends on Metal.framework.
--
-- The MPSCNNLoss filter is only used for training. This filter performs both the forward and              backward pass computations. Specifically, it computes the loss between the input (predictions)              and target data (labels) and the loss gradient. The loss value can be a 1 x 1 x 1 image containing              a scalar loss value or an image (of the same size as the input source image) with per feature              channel losses. The loss value is used to determine whether to continue the training operation or              to terminate it, once satisfactory results are achieved. The loss gradient is the first gradient              computed for the backward pass and serves as input to the next gradient filter (in the backward              direction).
--
-- The MPSCNNLoss filter is created with a MPSCNNLossDescriptor describing the type of a loss filter              and the type of a reduction to use for computing the overall loss.
--
-- The MPSCNNLoss filter takes the output of the inference pass (predictions) as input. It also              requires the target data (labels) and optionally, weights for the labels. If per-label weights              are not supplied, there is an option to use a single weight value by setting the 'weight' properly              on the MPSCNNLossDescriptor object. The labels and optional weights need to be supplied by the user              using the MPSCNNLossLabels object. The labels and weights are described via the MPSCNNLossDataDescriptor              objects, which are in turn used to initialize the MPSCNNLossLabels object.
--
-- If the specified reduction operation is MPSCNNReductionTypeNone, the destinationImage should be              at least as large as the specified clipRect. The destinationImage will then contain per-element              losses. Otherse, a reduction operation will be performed, according to the specified reduction              type, and the filter will return a scalar value containing the overall loss. For more information              on the available reduction types, see MPSCNNTypes.h. Also see MPSCNNLossDescriptor for the              description of optional parameters.
--
-- Here is a code example:
--
-- // Setup              MPSCNNLossDataDescriptor* labelsDescriptor =                  [MPSCNNLossDataDescriptor cnnLossDataDescriptorWithData: labelsData                                                                   layout: MPSDataLayoutHeightxWidthxFeatureChannels                                                                     size: labelsDataSize];              MPSCNNLossLabels* labels = [[MPSCNNLossLabels alloc] initWithDevice: device                                                                 labelsDescriptor: labelsDescriptor];              MPSCNNLossDescriptor *lossDescriptor =                  [MPSCNNLossDescriptor cnnLossDescriptorWithType: (MPSCNNLossType)MPSCNNLossTypeMeanAbsoluteError                                                    reductionType: (MPSCNNReductionType)MPSCNNReductionTypeSum];              MPSCNNLoss* lossFilter = [[MPSCNNLoss alloc] initWithDevice: device lossDescriptor: lossDescriptor];
--
-- // Encode loss filter.              // The sourceImage is the output of a previous layer, for example, the SoftMax layer. The lossGradientsImage              // is the sourceGradient input image to the first gradient layer (in the backward direction), for example,              // the SoftMax gradient filter.              [lossFilter encodeToCommandBuffer: commandBuffer sourceImage: sourceImage                                                                    labels: labels                                                          destinationImage: lossGradientsImage];
--
-- // In order to guarantee that the loss image data is correctly synchronized for CPU side access,              // it is the application's responsibility to call the [labels synchronizeOnCommandBuffer:]              // method before accessing the loss image data.              [labels synchronizeOnCommandBuffer:commandBuffer];              MPSImage* lossImage = [labels lossImage];
--
-- For predictions (y) and labels (t), the available loss filter types are the following:
--
-- Mean Absolute Error loss filter. This filter measures the absolute error of the element-wise              difference between the predictions and labels.              This loss function is computed according to the following formulas:                  Compute losses:          losses = |y - t|                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Mean Squared Error loss filter. This filter measures the squared error of the element-wise              difference between the predictions and labels.              This loss function is computed according to the following formulas:                  Compute losses:          losses = (y - t)^2                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- SoftMax Cross Entropy loss filter. This loss filter is applied element-wise.              This loss filter combines the LogSoftMax and Negative Log Likelihood operations in a              single filter. It is useful for training a classification problem with multiple classes.              This loss function is computed according to the following formulas:                  Compute losses:          losses = -t * LogSoftMax(y)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)                                           If reductionType is MPSCNNReductionTypeMean, the accumulated                                           loss value is divided by width * height instead of                                           width * height * featureChannels.
--
-- Sigmoid Cross Entropy loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          losses = max(y, 0) - y * t + log(1 + exp(-|y|))                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Categorical Cross Entropy loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          losses = -t * log(y)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Hinge loss filter. This loss filter is applied element-wise.              The labels are expected to be 0.0 or 1.0.              This loss function is computed according to the following formulas:                  Compute losses:          losses = max(1 - (t * y), 0.0f)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Huber loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          if (|y - t| <= delta, losses = 0.5 * y^2                                           if (|y - t| >  delta, losses = 0.5 * delta^2 + delta * (|y - t| - delta)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Cosine Distance loss filter. This loss filter is applied element-wise.              The only valid reduction type for this loss filter is MPSCNNReductionTypeSum.              This loss function is computed according to the following formulas:                  Compute losses:          loss = 1 - reduce_sum(y * t)                  Compute overall loss:    weighted_loss = weight * loss
--
-- Log loss filter. This loss filter is applied element-wise.              This loss function is computed according to the following formulas:                  Compute losses:          losses = -(t * log(y + epsilon)) - ((1 - t) * log(1 - y + epsilon))                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- Kullback-Leibler Divergence loss filter. This loss filter is applied element-wise.              The input (predictions) is expected to contain log-probabilities.                  This loss function is computed according to the following formulas:                  Compute losses:          losses = t * (log(t) - y)                  Compute weighted losses: weighted_losses = weight(s) * losses                  Compute overall loss:    loss = reduce(weighted_losses, reductionType)
--
-- For predictions (y) and labels (t), the loss gradient for each available loss filter type              is computed as follows:
--
-- Mean Absolute Error loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = (y - t) / |y - t|                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Mean Squared Error loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = 2 * (y - t)                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- SoftMax Cross Entropy loss.              The loss gradient is computed according to the following formulas:                  First, apply the same label smoothing as in the MPSCNNLoss filter.                  Compute gradient:          d/dy = y - t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Sigmoid Cross Entropy loss.              The loss gradient is computed according to the following formulas:              First, apply the same label smoothing as in the MPSCNNLoss filter.                  Compute gradient:          d/dy = (1 / (1 + exp(-y)) - t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Categorical Cross Entropy loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = -t / y                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Hinge loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = ((1 + ((1 - (2 * t)) * y)) > 0) ? 1 - (2 * t) : 0                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Huber loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = |y - t| > delta ? delta : y - t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Cosine Distance loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = -t                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Log loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = (-2 * epsilon * t - t + y + epsilon) / (y * (1 - y) + epsilon * (epsilon + 1))                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- Kullback-Leibler Divergence loss.              The loss gradient is computed according to the following formulas:                  Compute gradient:          d/dy = -t / y                  Compute weighted gradient: weighted_gradient = weight(s) * gradient
--
-- The number of output feature channels remains the same as the number of input feature              channels.
--
-- Generated bindings for @MPSCNNLoss@.
module ObjC.MetalPerformanceShaders.MPSCNNLoss
  ( MPSCNNLoss
  , IsMPSCNNLoss(..)
  , initWithDevice
  , initWithDevice_lossDescriptor
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_labels_destinationImage
  , encodeToCommandBuffer_sourceImage_labels
  , encodeBatchToCommandBuffer_sourceImages_labels_destinationImages
  , encodeBatchToCommandBuffer_sourceImages_labels
  , lossType
  , reductionType
  , weight
  , labelSmoothing
  , numberOfClasses
  , epsilon
  , delta
  , reduceAcrossBatch
  , deltaSelector
  , encodeBatchToCommandBuffer_sourceImages_labelsSelector
  , encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector
  , encodeToCommandBuffer_sourceImage_labelsSelector
  , encodeToCommandBuffer_sourceImage_labels_destinationImageSelector
  , epsilonSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , labelSmoothingSelector
  , lossTypeSelector
  , numberOfClassesSelector
  , reduceAcrossBatchSelector
  , reductionTypeSelector
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
initWithDevice :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> RawId -> IO (Id MPSCNNLoss)
initWithDevice mpscnnLoss device =
  sendOwnedMessage mpscnnLoss initWithDeviceSelector device

-- | Initialize the loss filter with a loss descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @lossDescriptor@ — The loss descriptor.
--
-- Returns: A valid MPSCNNLoss object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptor :: (IsMPSCNNLoss mpscnnLoss, IsMPSCNNLossDescriptor lossDescriptor) => mpscnnLoss -> RawId -> lossDescriptor -> IO (Id MPSCNNLoss)
initWithDevice_lossDescriptor mpscnnLoss device lossDescriptor =
  sendOwnedMessage mpscnnLoss initWithDevice_lossDescriptorSelector device (toMPSCNNLossDescriptor lossDescriptor)

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNLoss mpscnnLoss, IsNSCoder aDecoder) => mpscnnLoss -> aDecoder -> RawId -> IO (Id MPSCNNLoss)
initWithCoder_device mpscnnLoss aDecoder device =
  sendOwnedMessage mpscnnLoss initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a MPSCNNLoss filter and return a gradient in the destinationImage.
--
-- This filter consumes the output of a previous layer, for example, the SoftMax layer containing              predictions, and the MPSCNNLossLabels object containing the target data (labels) and optionally,              weights for the labels. The destinationImage contains the computed gradient for the loss layer.              It serves as a source gradient input image to the first gradient layer (in the backward direction),              in our example, the SoftMax gradient layer.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode.
--
-- @sourceImage@ — The source image from the previous filter in the graph (in the inference direction).
--
-- @labels@ — The object containing the target data (labels) and optionally, weights for the labels.
--
-- @destinationImage@ — The MPSImage into which to write the gradient result.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:labels:destinationImage:@
encodeToCommandBuffer_sourceImage_labels_destinationImage :: (IsMPSCNNLoss mpscnnLoss, IsMPSImage sourceImage, IsMPSCNNLossLabels labels, IsMPSImage destinationImage) => mpscnnLoss -> RawId -> sourceImage -> labels -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_labels_destinationImage mpscnnLoss commandBuffer sourceImage labels destinationImage =
  sendMessage mpscnnLoss encodeToCommandBuffer_sourceImage_labels_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNLossLabels labels) (toMPSImage destinationImage)

-- | Encode a MPSCNNLoss filter and return a gradient.
--
-- This -encode call is similar to the encodeToCommandBuffer:sourceImage:labels:destinationImage: above,              except that it creates and returns the MPSImage with the loss gradient result.
--
-- @commandBuffer@ — The MTLCommandBuffer on which to encode.
--
-- @sourceImage@ — The source image from the previous filter in the graph (in the inference direction).
--
-- @labels@ — The object containing the target data (labels) and optionally, weights for the labels.
--
-- Returns: The MPSImage containing the gradient result.
--
-- ObjC selector: @- encodeToCommandBuffer:sourceImage:labels:@
encodeToCommandBuffer_sourceImage_labels :: (IsMPSCNNLoss mpscnnLoss, IsMPSImage sourceImage, IsMPSCNNLossLabels labels) => mpscnnLoss -> RawId -> sourceImage -> labels -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_labels mpscnnLoss commandBuffer sourceImage labels =
  sendMessage mpscnnLoss encodeToCommandBuffer_sourceImage_labelsSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNLossLabels labels)

-- | @- encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_destinationImages :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_labels_destinationImages mpscnnLoss commandBuffer sourceImage labels destinationImage =
  sendMessage mpscnnLoss encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector commandBuffer sourceImage labels destinationImage

-- | @- encodeBatchToCommandBuffer:sourceImages:labels:@
encodeBatchToCommandBuffer_sourceImages_labels :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages_labels mpscnnLoss commandBuffer sourceImage labels =
  sendMessage mpscnnLoss encodeBatchToCommandBuffer_sourceImages_labelsSelector commandBuffer sourceImage labels

-- | See MPSCNNLossDescriptor for information about the following properties.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO MPSCNNLossType
lossType mpscnnLoss =
  sendMessage mpscnnLoss lossTypeSelector

-- | @- reductionType@
reductionType :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO MPSCNNReductionType
reductionType mpscnnLoss =
  sendMessage mpscnnLoss reductionTypeSelector

-- | @- weight@
weight :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
weight mpscnnLoss =
  sendMessage mpscnnLoss weightSelector

-- | @- labelSmoothing@
labelSmoothing :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
labelSmoothing mpscnnLoss =
  sendMessage mpscnnLoss labelSmoothingSelector

-- | @- numberOfClasses@
numberOfClasses :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CULong
numberOfClasses mpscnnLoss =
  sendMessage mpscnnLoss numberOfClassesSelector

-- | @- epsilon@
epsilon :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
epsilon mpscnnLoss =
  sendMessage mpscnnLoss epsilonSelector

-- | @- delta@
delta :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
delta mpscnnLoss =
  sendMessage mpscnnLoss deltaSelector

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO Bool
reduceAcrossBatch mpscnnLoss =
  sendMessage mpscnnLoss reduceAcrossBatchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNLoss)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptorSelector :: Selector '[RawId, Id MPSCNNLossDescriptor] (Id MPSCNNLoss)
initWithDevice_lossDescriptorSelector = mkSelector "initWithDevice:lossDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNLoss)
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:labels:destinationImage:@
encodeToCommandBuffer_sourceImage_labels_destinationImageSelector :: Selector '[RawId, Id MPSImage, Id MPSCNNLossLabels, Id MPSImage] ()
encodeToCommandBuffer_sourceImage_labels_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:labels:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:labels:@
encodeToCommandBuffer_sourceImage_labelsSelector :: Selector '[RawId, Id MPSImage, Id MPSCNNLossLabels] (Id MPSImage)
encodeToCommandBuffer_sourceImage_labelsSelector = mkSelector "encodeToCommandBuffer:sourceImage:labels:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector :: Selector '[RawId, RawId, RawId, RawId] ()
encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:labels:@
encodeBatchToCommandBuffer_sourceImages_labelsSelector :: Selector '[RawId, RawId, RawId] RawId
encodeBatchToCommandBuffer_sourceImages_labelsSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:"

-- | @Selector@ for @lossType@
lossTypeSelector :: Selector '[] MPSCNNLossType
lossTypeSelector = mkSelector "lossType"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MPSCNNReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @weight@
weightSelector :: Selector '[] CFloat
weightSelector = mkSelector "weight"

-- | @Selector@ for @labelSmoothing@
labelSmoothingSelector :: Selector '[] CFloat
labelSmoothingSelector = mkSelector "labelSmoothing"

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector '[] CULong
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector '[] CFloat
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @delta@
deltaSelector :: Selector '[] CFloat
deltaSelector = mkSelector "delta"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector '[] Bool
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

