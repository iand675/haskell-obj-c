{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MPSCNNYOLOLoss@.
module ObjC.MetalPerformanceShaders.MPSCNNYOLOLoss
  ( MPSCNNYOLOLoss
  , IsMPSCNNYOLOLoss(..)
  , initWithDevice
  , initWithDevice_lossDescriptor
  , initWithCoder_device
  , encodeToCommandBuffer_sourceImage_labels_destinationImage
  , encodeToCommandBuffer_sourceImage_labels
  , encodeBatchToCommandBuffer_sourceImages_labels_destinationImages
  , encodeBatchToCommandBuffer_sourceImages_labels
  , lossXY
  , lossWH
  , lossConfidence
  , lossClasses
  , scaleXY
  , scaleWH
  , scaleNoObject
  , scaleObject
  , scaleClass
  , minIOUForObjectPresence
  , maxIOUForObjectAbsence
  , reductionType
  , numberOfAnchorBoxes
  , anchorBoxes
  , reduceAcrossBatch
  , anchorBoxesSelector
  , encodeBatchToCommandBuffer_sourceImages_labelsSelector
  , encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector
  , encodeToCommandBuffer_sourceImage_labelsSelector
  , encodeToCommandBuffer_sourceImage_labels_destinationImageSelector
  , initWithCoder_deviceSelector
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , lossClassesSelector
  , lossConfidenceSelector
  , lossWHSelector
  , lossXYSelector
  , maxIOUForObjectAbsenceSelector
  , minIOUForObjectPresenceSelector
  , numberOfAnchorBoxesSelector
  , reduceAcrossBatchSelector
  , reductionTypeSelector
  , scaleClassSelector
  , scaleNoObjectSelector
  , scaleObjectSelector
  , scaleWHSelector
  , scaleXYSelector

  -- * Enum types
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
initWithDevice :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> RawId -> IO (Id MPSCNNYOLOLoss)
initWithDevice mpscnnyoloLoss device =
  sendOwnedMessage mpscnnyoloLoss initWithDeviceSelector device

-- | Initialize the loss filter with a loss descriptor.
--
-- @device@ — The device the filter will run on.
--
-- @lossDescriptor@ — The loss descriptor.
--
-- Returns: A valid MPSCNNLoss object or nil, if failure.
--
-- ObjC selector: @- initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptor :: (IsMPSCNNYOLOLoss mpscnnyoloLoss, IsMPSCNNYOLOLossDescriptor lossDescriptor) => mpscnnyoloLoss -> RawId -> lossDescriptor -> IO (Id MPSCNNYOLOLoss)
initWithDevice_lossDescriptor mpscnnyoloLoss device lossDescriptor =
  sendOwnedMessage mpscnnyoloLoss initWithDevice_lossDescriptorSelector device (toMPSCNNYOLOLossDescriptor lossDescriptor)

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNYOLOLoss mpscnnyoloLoss, IsNSCoder aDecoder) => mpscnnyoloLoss -> aDecoder -> RawId -> IO (Id MPSCNNYOLOLoss)
initWithCoder_device mpscnnyoloLoss aDecoder device =
  sendOwnedMessage mpscnnyoloLoss initWithCoder_deviceSelector (toNSCoder aDecoder) device

-- | Encode a MPSCNNYOLOLoss filter and return a gradient in the destinationImage.
--
-- This filter consumes the output of a previous layer and the MPSCNNLossLabels object containing              the target data (labels) and optionally, weights for the labels.              The destinationImage contains the computed gradient for the loss layer.              It serves as a source gradient input image to the first gradient layer (in the backward direction).              For information on the data-layout see MPSCNNYOLOLossDescriptor.
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
encodeToCommandBuffer_sourceImage_labels_destinationImage :: (IsMPSCNNYOLOLoss mpscnnyoloLoss, IsMPSImage sourceImage, IsMPSCNNLossLabels labels, IsMPSImage destinationImage) => mpscnnyoloLoss -> RawId -> sourceImage -> labels -> destinationImage -> IO ()
encodeToCommandBuffer_sourceImage_labels_destinationImage mpscnnyoloLoss commandBuffer sourceImage labels destinationImage =
  sendMessage mpscnnyoloLoss encodeToCommandBuffer_sourceImage_labels_destinationImageSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNLossLabels labels) (toMPSImage destinationImage)

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
encodeToCommandBuffer_sourceImage_labels :: (IsMPSCNNYOLOLoss mpscnnyoloLoss, IsMPSImage sourceImage, IsMPSCNNLossLabels labels) => mpscnnyoloLoss -> RawId -> sourceImage -> labels -> IO (Id MPSImage)
encodeToCommandBuffer_sourceImage_labels mpscnnyoloLoss commandBuffer sourceImage labels =
  sendMessage mpscnnyoloLoss encodeToCommandBuffer_sourceImage_labelsSelector commandBuffer (toMPSImage sourceImage) (toMPSCNNLossLabels labels)

-- | @- encodeBatchToCommandBuffer:sourceImages:labels:destinationImages:@
encodeBatchToCommandBuffer_sourceImages_labels_destinationImages :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> RawId -> RawId -> RawId -> RawId -> IO ()
encodeBatchToCommandBuffer_sourceImages_labels_destinationImages mpscnnyoloLoss commandBuffer sourceImage labels destinationImage =
  sendMessage mpscnnyoloLoss encodeBatchToCommandBuffer_sourceImages_labels_destinationImagesSelector commandBuffer sourceImage labels destinationImage

-- | @- encodeBatchToCommandBuffer:sourceImages:labels:@
encodeBatchToCommandBuffer_sourceImages_labels :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> RawId -> RawId -> RawId -> IO RawId
encodeBatchToCommandBuffer_sourceImages_labels mpscnnyoloLoss commandBuffer sourceImage labels =
  sendMessage mpscnnyoloLoss encodeBatchToCommandBuffer_sourceImages_labelsSelector commandBuffer sourceImage labels

-- | lossXY
--
-- loss filter for prediction of bounding box position
--
-- ObjC selector: @- lossXY@
lossXY :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossXY mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss lossXYSelector

-- | lossWH
--
-- loss filter for prediction of bounding box size
--
-- ObjC selector: @- lossWH@
lossWH :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossWH mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss lossWHSelector

-- | lossConfidence
--
-- loss filter for prediction of bounding box probability of presence of object
--
-- ObjC selector: @- lossConfidence@
lossConfidence :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossConfidence mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss lossConfidenceSelector

-- | lossClasses
--
-- loss filter for prediction of bounding box predicted class of the detected object
--
-- ObjC selector: @- lossClasses@
lossClasses :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossClasses mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss lossClassesSelector

-- | See MPSCNNYOLOLossDescriptor for information about the following properties.
--
-- ObjC selector: @- scaleXY@
scaleXY :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleXY mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss scaleXYSelector

-- | @- scaleWH@
scaleWH :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleWH mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss scaleWHSelector

-- | @- scaleNoObject@
scaleNoObject :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleNoObject mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss scaleNoObjectSelector

-- | @- scaleObject@
scaleObject :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleObject mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss scaleObjectSelector

-- | @- scaleClass@
scaleClass :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleClass mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss scaleClassSelector

-- | @- minIOUForObjectPresence@
minIOUForObjectPresence :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
minIOUForObjectPresence mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss minIOUForObjectPresenceSelector

-- | @- maxIOUForObjectAbsence@
maxIOUForObjectAbsence :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
maxIOUForObjectAbsence mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss maxIOUForObjectAbsenceSelector

-- | @- reductionType@
reductionType :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO MPSCNNReductionType
reductionType mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss reductionTypeSelector

-- | @- numberOfAnchorBoxes@
numberOfAnchorBoxes :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CULong
numberOfAnchorBoxes mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss numberOfAnchorBoxesSelector

-- | @- anchorBoxes@
anchorBoxes :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id NSData)
anchorBoxes mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss anchorBoxesSelector

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO Bool
reduceAcrossBatch mpscnnyoloLoss =
  sendMessage mpscnnyoloLoss reduceAcrossBatchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDevice:@
initWithDeviceSelector :: Selector '[RawId] (Id MPSCNNYOLOLoss)
initWithDeviceSelector = mkSelector "initWithDevice:"

-- | @Selector@ for @initWithDevice:lossDescriptor:@
initWithDevice_lossDescriptorSelector :: Selector '[RawId, Id MPSCNNYOLOLossDescriptor] (Id MPSCNNYOLOLoss)
initWithDevice_lossDescriptorSelector = mkSelector "initWithDevice:lossDescriptor:"

-- | @Selector@ for @initWithCoder:device:@
initWithCoder_deviceSelector :: Selector '[Id NSCoder, RawId] (Id MPSCNNYOLOLoss)
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

-- | @Selector@ for @lossXY@
lossXYSelector :: Selector '[] (Id MPSCNNLoss)
lossXYSelector = mkSelector "lossXY"

-- | @Selector@ for @lossWH@
lossWHSelector :: Selector '[] (Id MPSCNNLoss)
lossWHSelector = mkSelector "lossWH"

-- | @Selector@ for @lossConfidence@
lossConfidenceSelector :: Selector '[] (Id MPSCNNLoss)
lossConfidenceSelector = mkSelector "lossConfidence"

-- | @Selector@ for @lossClasses@
lossClassesSelector :: Selector '[] (Id MPSCNNLoss)
lossClassesSelector = mkSelector "lossClasses"

-- | @Selector@ for @scaleXY@
scaleXYSelector :: Selector '[] CFloat
scaleXYSelector = mkSelector "scaleXY"

-- | @Selector@ for @scaleWH@
scaleWHSelector :: Selector '[] CFloat
scaleWHSelector = mkSelector "scaleWH"

-- | @Selector@ for @scaleNoObject@
scaleNoObjectSelector :: Selector '[] CFloat
scaleNoObjectSelector = mkSelector "scaleNoObject"

-- | @Selector@ for @scaleObject@
scaleObjectSelector :: Selector '[] CFloat
scaleObjectSelector = mkSelector "scaleObject"

-- | @Selector@ for @scaleClass@
scaleClassSelector :: Selector '[] CFloat
scaleClassSelector = mkSelector "scaleClass"

-- | @Selector@ for @minIOUForObjectPresence@
minIOUForObjectPresenceSelector :: Selector '[] CFloat
minIOUForObjectPresenceSelector = mkSelector "minIOUForObjectPresence"

-- | @Selector@ for @maxIOUForObjectAbsence@
maxIOUForObjectAbsenceSelector :: Selector '[] CFloat
maxIOUForObjectAbsenceSelector = mkSelector "maxIOUForObjectAbsence"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MPSCNNReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @numberOfAnchorBoxes@
numberOfAnchorBoxesSelector :: Selector '[] CULong
numberOfAnchorBoxesSelector = mkSelector "numberOfAnchorBoxes"

-- | @Selector@ for @anchorBoxes@
anchorBoxesSelector :: Selector '[] (Id NSData)
anchorBoxesSelector = mkSelector "anchorBoxes"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector '[] Bool
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

