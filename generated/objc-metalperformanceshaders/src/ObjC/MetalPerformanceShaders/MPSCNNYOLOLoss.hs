{-# LANGUAGE PatternSynonyms #-}
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
  , initWithDeviceSelector
  , initWithDevice_lossDescriptorSelector
  , initWithCoder_deviceSelector
  , encodeToCommandBuffer_sourceImage_labels_destinationImageSelector
  , encodeToCommandBuffer_sourceImage_labelsSelector
  , lossXYSelector
  , lossWHSelector
  , lossConfidenceSelector
  , lossClassesSelector
  , scaleXYSelector
  , scaleWHSelector
  , scaleNoObjectSelector
  , scaleObjectSelector
  , scaleClassSelector
  , minIOUForObjectPresenceSelector
  , maxIOUForObjectAbsenceSelector
  , reductionTypeSelector
  , numberOfAnchorBoxesSelector
  , anchorBoxesSelector
  , reduceAcrossBatchSelector

  -- * Enum types
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
initWithDevice :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> RawId -> IO (Id MPSCNNYOLOLoss)
initWithDevice mpscnnyoloLoss  device =
  sendMsg mpscnnyoloLoss (mkSelector "initWithDevice:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
initWithDevice_lossDescriptor mpscnnyoloLoss  device lossDescriptor =
withObjCPtr lossDescriptor $ \raw_lossDescriptor ->
    sendMsg mpscnnyoloLoss (mkSelector "initWithDevice:lossDescriptor:") (retPtr retVoid) [argPtr (castPtr (unRawId device) :: Ptr ()), argPtr (castPtr raw_lossDescriptor :: Ptr ())] >>= ownedObject . castPtr

-- | <NSSecureCoding> support
--
-- ObjC selector: @- initWithCoder:device:@
initWithCoder_device :: (IsMPSCNNYOLOLoss mpscnnyoloLoss, IsNSCoder aDecoder) => mpscnnyoloLoss -> aDecoder -> RawId -> IO (Id MPSCNNYOLOLoss)
initWithCoder_device mpscnnyoloLoss  aDecoder device =
withObjCPtr aDecoder $ \raw_aDecoder ->
    sendMsg mpscnnyoloLoss (mkSelector "initWithCoder:device:") (retPtr retVoid) [argPtr (castPtr raw_aDecoder :: Ptr ()), argPtr (castPtr (unRawId device) :: Ptr ())] >>= ownedObject . castPtr

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
encodeToCommandBuffer_sourceImage_labels_destinationImage mpscnnyoloLoss  commandBuffer sourceImage labels destinationImage =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr labels $ \raw_labels ->
    withObjCPtr destinationImage $ \raw_destinationImage ->
        sendMsg mpscnnyoloLoss (mkSelector "encodeToCommandBuffer:sourceImage:labels:destinationImage:") retVoid [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ()), argPtr (castPtr raw_destinationImage :: Ptr ())]

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
encodeToCommandBuffer_sourceImage_labels mpscnnyoloLoss  commandBuffer sourceImage labels =
withObjCPtr sourceImage $ \raw_sourceImage ->
  withObjCPtr labels $ \raw_labels ->
      sendMsg mpscnnyoloLoss (mkSelector "encodeToCommandBuffer:sourceImage:labels:") (retPtr retVoid) [argPtr (castPtr (unRawId commandBuffer) :: Ptr ()), argPtr (castPtr raw_sourceImage :: Ptr ()), argPtr (castPtr raw_labels :: Ptr ())] >>= retainedObject . castPtr

-- | lossXY
--
-- loss filter for prediction of bounding box position
--
-- ObjC selector: @- lossXY@
lossXY :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossXY mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "lossXY") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lossWH
--
-- loss filter for prediction of bounding box size
--
-- ObjC selector: @- lossWH@
lossWH :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossWH mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "lossWH") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lossConfidence
--
-- loss filter for prediction of bounding box probability of presence of object
--
-- ObjC selector: @- lossConfidence@
lossConfidence :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossConfidence mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "lossConfidence") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | lossClasses
--
-- loss filter for prediction of bounding box predicted class of the detected object
--
-- ObjC selector: @- lossClasses@
lossClasses :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id MPSCNNLoss)
lossClasses mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "lossClasses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | See MPSCNNYOLOLossDescriptor for information about the following properties.
--
-- ObjC selector: @- scaleXY@
scaleXY :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleXY mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "scaleXY") retCFloat []

-- | @- scaleWH@
scaleWH :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleWH mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "scaleWH") retCFloat []

-- | @- scaleNoObject@
scaleNoObject :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleNoObject mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "scaleNoObject") retCFloat []

-- | @- scaleObject@
scaleObject :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleObject mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "scaleObject") retCFloat []

-- | @- scaleClass@
scaleClass :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
scaleClass mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "scaleClass") retCFloat []

-- | @- minIOUForObjectPresence@
minIOUForObjectPresence :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
minIOUForObjectPresence mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "minIOUForObjectPresence") retCFloat []

-- | @- maxIOUForObjectAbsence@
maxIOUForObjectAbsence :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CFloat
maxIOUForObjectAbsence mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "maxIOUForObjectAbsence") retCFloat []

-- | @- reductionType@
reductionType :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO MPSCNNReductionType
reductionType mpscnnyoloLoss  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpscnnyoloLoss (mkSelector "reductionType") retCInt []

-- | @- numberOfAnchorBoxes@
numberOfAnchorBoxes :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO CULong
numberOfAnchorBoxes mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "numberOfAnchorBoxes") retCULong []

-- | @- anchorBoxes@
anchorBoxes :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO (Id NSData)
anchorBoxes mpscnnyoloLoss  =
  sendMsg mpscnnyoloLoss (mkSelector "anchorBoxes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNYOLOLoss mpscnnyoloLoss => mpscnnyoloLoss -> IO Bool
reduceAcrossBatch mpscnnyoloLoss  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnyoloLoss (mkSelector "reduceAcrossBatch") retCULong []

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

-- | @Selector@ for @lossXY@
lossXYSelector :: Selector
lossXYSelector = mkSelector "lossXY"

-- | @Selector@ for @lossWH@
lossWHSelector :: Selector
lossWHSelector = mkSelector "lossWH"

-- | @Selector@ for @lossConfidence@
lossConfidenceSelector :: Selector
lossConfidenceSelector = mkSelector "lossConfidence"

-- | @Selector@ for @lossClasses@
lossClassesSelector :: Selector
lossClassesSelector = mkSelector "lossClasses"

-- | @Selector@ for @scaleXY@
scaleXYSelector :: Selector
scaleXYSelector = mkSelector "scaleXY"

-- | @Selector@ for @scaleWH@
scaleWHSelector :: Selector
scaleWHSelector = mkSelector "scaleWH"

-- | @Selector@ for @scaleNoObject@
scaleNoObjectSelector :: Selector
scaleNoObjectSelector = mkSelector "scaleNoObject"

-- | @Selector@ for @scaleObject@
scaleObjectSelector :: Selector
scaleObjectSelector = mkSelector "scaleObject"

-- | @Selector@ for @scaleClass@
scaleClassSelector :: Selector
scaleClassSelector = mkSelector "scaleClass"

-- | @Selector@ for @minIOUForObjectPresence@
minIOUForObjectPresenceSelector :: Selector
minIOUForObjectPresenceSelector = mkSelector "minIOUForObjectPresence"

-- | @Selector@ for @maxIOUForObjectAbsence@
maxIOUForObjectAbsenceSelector :: Selector
maxIOUForObjectAbsenceSelector = mkSelector "maxIOUForObjectAbsence"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @numberOfAnchorBoxes@
numberOfAnchorBoxesSelector :: Selector
numberOfAnchorBoxesSelector = mkSelector "numberOfAnchorBoxes"

-- | @Selector@ for @anchorBoxes@
anchorBoxesSelector :: Selector
anchorBoxesSelector = mkSelector "anchorBoxes"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

