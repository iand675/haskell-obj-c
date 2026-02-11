{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_labels_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_labelsSelector
  , encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector
  , encodeBatchToCommandBuffer_sourceImages_labelsSelector
  , lossTypeSelector
  , reductionTypeSelector
  , weightSelector
  , labelSmoothingSelector
  , numberOfClassesSelector
  , epsilonSelector
  , deltaSelector
  , reduceAcrossBatchSelector

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

-- | @- initWithDevice:@
initWithDevice :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> RawId -> IO (Id MPSCNNLoss)
initWithDevice mpscnnLoss  device =
    sendMsg mpscnnLoss (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_lossDescriptor mpscnnLoss  device lossDescriptor =
  withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
      sendMsg mpscnnLoss (mkSelector "initWithDevice:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_lossDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNLoss mpscnnLoss, IsNSCoder aDecoder) => mpscnnLoss -> aDecoder -> RawId -> IO (Id MPSCNNLoss)
initWithCoder_device mpscnnLoss  aDecoder device =
  withObjCPtr aDecoder $ \raw_aDecoder ->
      sendMsg mpscnnLoss (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceImage_labels_destinationImage mpscnnLoss  commandBuffer sourceImage labels destinationImage =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr labels $ \raw_labels ->
      withObjCPtr destinationImage $ \raw_destinationImage ->
          sendMsg mpscnnLoss (mkSelector "encodeToCommandBuffer:sourceImage:labels:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

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
encodeToCommandBuffer_sourceImage_labels mpscnnLoss  commandBuffer sourceImage labels =
  withObjCPtr sourceImage $ \raw_sourceImage ->
    withObjCPtr labels $ \raw_labels ->
        sendMsg mpscnnLoss (mkSelector "encodeToCommandBuffer:sourceImage:labels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ())] >>= retainedObject . castPtr

-- | @- encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_destinationImages :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_labels_destinationImages mpscnnLoss  commandBuffer sourceImage labels destinationImage =
    sendMsg mpscnnLoss (mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr (unRawId labels) :: Ptr ()), argPtr (castPtr (unRawId destinationImage) :: Ptr ())]

-- | @- encodeBatchToCommandBuffer:sourceImages:labels:@
encodeBatchToCommandBuffer_sourceImages_labels :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages_labels mpscnnLoss  commandBuffer sourceImage labels =
    fmap (RawId . castPtr) $ sendMsg mpscnnLoss (mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr (unRawId sourceImage) :: Ptr ()), argPtr (castPtr (unRawId labels) :: Ptr ())]

-- | See MPSCNNLossDescriptor for information about the following properties.
--
-- ObjC selector: @- lossType@
lossType :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO MPSCNNLossType
lossType mpscnnLoss  =
    fmap (coerce :: CUInt -> MPSCNNLossType) $ sendMsg mpscnnLoss (mkSelector "lossType") retCUInt []

-- | @- reductionType@
reductionType :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO MPSCNNReductionType
reductionType mpscnnLoss  =
    fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpscnnLoss (mkSelector "reductionType") retCInt []

-- | @- weight@
weight :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
weight mpscnnLoss  =
    sendMsg mpscnnLoss (mkSelector "weight") retCFloat []

-- | @- labelSmoothing@
labelSmoothing :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
labelSmoothing mpscnnLoss  =
    sendMsg mpscnnLoss (mkSelector "labelSmoothing") retCFloat []

-- | @- numberOfClasses@
numberOfClasses :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CULong
numberOfClasses mpscnnLoss  =
    sendMsg mpscnnLoss (mkSelector "numberOfClasses") retCULong []

-- | @- epsilon@
epsilon :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
epsilon mpscnnLoss  =
    sendMsg mpscnnLoss (mkSelector "epsilon") retCFloat []

-- | @- delta@
delta :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO CFloat
delta mpscnnLoss  =
    sendMsg mpscnnLoss (mkSelector "delta") retCFloat []

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNLoss mpscnnLoss => mpscnnLoss -> IO Bool
reduceAcrossBatch mpscnnLoss  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnLoss (mkSelector "reduceAcrossBatch") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptorSelector :: Selector
initWithDevice_lossDescriptorSelector = mkSelector "initWithDevice:lossDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector
initWithCoder_deviceSelector = mkSelector "initWithCoder:device:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:labels:destinationImage:@
encodeToCommandBuffer_sourceImage_labels_destinationImageSelector :: Selector
encodeToCommandBuffer_sourceImage_labels_destinationImageSelector = mkSelector "encodeToCommandBuffer:sourceImage:labels:destinationImage:"

-- | @Selector@ for @encodeToCommandBuffer:sourceImage:labels:@
encodeToCommandBuffer_sourceImage_labelsSelector :: Selector
encodeToCommandBuffer_sourceImage_labelsSelector = mkSelector "encodeToCommandBuffer:sourceImage:labels:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:"

-- | @Selector@ for @encodeBatchToCommandBuffer:sourceImages:labels:@
encodeBatchToCommandBuffer_sourceImages_labelsSelector :: Selector
encodeBatchToCommandBuffer_sourceImages_labelsSelector = mkSelector "encodeBatchToCommandBuffer:sourceImages:labels:"

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

-- | @Selector@ for @numberOfClasses@
numberOfClassesSelector :: Selector
numberOfClassesSelector = mkSelector "numberOfClasses"

-- | @Selector@ for @epsilon@
epsilonSelector :: Selector
epsilonSelector = mkSelector "epsilon"

-- | @Selector@ for @delta@
deltaSelector :: Selector
deltaSelector = mkSelector "delta"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

