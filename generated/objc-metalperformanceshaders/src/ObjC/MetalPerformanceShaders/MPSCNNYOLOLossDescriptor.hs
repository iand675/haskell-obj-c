{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MPSCNNYOLOLossDescriptor
--
-- This depends on Metal.framework.
--
-- The MPSCNNYOLOLossDescriptor specifies a loss filter descriptor              that is used to create a MPSCNNLoss filter. The MPSCNNYOLOLoss is a filter that              has been specialized for object detection tasks and follows a specific layout              for the feature-channels of the input, output, weight and label data.
--
-- The layout of the data within the feature-channels is as follows:
--
-- Each anchorbox uses ( 2+2+1 + numberOfClasses = 5 + numberOfClasses ) feature channels.
--
-- Therefore the total number of feature channels used is: (5 + numberOfClasses) * numberOfAnchorBoxes.              The first feature channel for anchorbox index 'anchorIdx' is at fcIndex = (5 + numberOfClasses) * anchorIdx,              and the feature channels within each anchorbox are stored in the layout: 'XYWHCFFFFFF...', where (XY) are              the so-called raw x and y coordinates of the bounding box within each gridcell and (WH) are the corresponding              width and height. 'C' signifies a confidence for having an object in the cell and FFFFF... are the feature channel              values for each class of object to be classified in the object detector.
--
-- The YOLO-loss filter works by operating mostly independently on each anchorbox:                  *   The XY-channels of the inputs are first transformed to relative XY-values by applying the sigmoid-neuron on them,                      after which they are passed through the loss function defined by XYLossDescriptor, which is typically chosen                      to be the MPSCNNLossTypeMeanSquaredError type loss function.                  *   The WH-channels contain the raw width and height of the bounding box and they are operated with the                      loss function defined by WHLossDescriptor, which is typically of type MPSCNNLossTypeHuber.                  *   The C-channel contains the confidence value of having an object in the bounding box and it is operated                      by the loss function defined by confidenceLossDescriptor, which is typically chosen to be                      MPSCNNLossTypeSigmoidCrossEntropy.                  *   The FFFFF... (number of channels is number of classes) channels contains the raw feature channels for                      object classes, used to identify which objects are the most probable ones in the bounding box and                      these channels are passed through the loss function defined by classesLossDescriptor, which in                      typical cases is of the type MPSCNNLossTypeSoftMaxCrossEntropy.
--
-- For details on how to set up the label values and anchorboxes see https://arxiv.org/abs/1612.08242
--
-- Generated bindings for @MPSCNNYOLOLossDescriptor@.
module ObjC.MetalPerformanceShaders.MPSCNNYOLOLossDescriptor
  ( MPSCNNYOLOLossDescriptor
  , IsMPSCNNYOLOLossDescriptor(..)
  , init_
  , cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxes
  , xyLossDescriptor
  , setXYLossDescriptor
  , whLossDescriptor
  , setWHLossDescriptor
  , confidenceLossDescriptor
  , setConfidenceLossDescriptor
  , classesLossDescriptor
  , setClassesLossDescriptor
  , reductionType
  , setReductionType
  , reduceAcrossBatch
  , setReduceAcrossBatch
  , rescore
  , setRescore
  , scaleXY
  , setScaleXY
  , scaleWH
  , setScaleWH
  , scaleNoObject
  , setScaleNoObject
  , scaleObject
  , setScaleObject
  , scaleClass
  , setScaleClass
  , minIOUForObjectPresence
  , setMinIOUForObjectPresence
  , maxIOUForObjectAbsence
  , setMaxIOUForObjectAbsence
  , numberOfAnchorBoxes
  , setNumberOfAnchorBoxes
  , anchorBoxes
  , setAnchorBoxes
  , initSelector
  , cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector
  , xyLossDescriptorSelector
  , setXYLossDescriptorSelector
  , whLossDescriptorSelector
  , setWHLossDescriptorSelector
  , confidenceLossDescriptorSelector
  , setConfidenceLossDescriptorSelector
  , classesLossDescriptorSelector
  , setClassesLossDescriptorSelector
  , reductionTypeSelector
  , setReductionTypeSelector
  , reduceAcrossBatchSelector
  , setReduceAcrossBatchSelector
  , rescoreSelector
  , setRescoreSelector
  , scaleXYSelector
  , setScaleXYSelector
  , scaleWHSelector
  , setScaleWHSelector
  , scaleNoObjectSelector
  , setScaleNoObjectSelector
  , scaleObjectSelector
  , setScaleObjectSelector
  , scaleClassSelector
  , setScaleClassSelector
  , minIOUForObjectPresenceSelector
  , setMinIOUForObjectPresenceSelector
  , maxIOUForObjectAbsenceSelector
  , setMaxIOUForObjectAbsenceSelector
  , numberOfAnchorBoxesSelector
  , setNumberOfAnchorBoxesSelector
  , anchorBoxesSelector
  , setAnchorBoxesSelector

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

-- | @- init@
init_ :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNYOLOLossDescriptor)
init_ mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Make a descriptor for a MPSCNNYOLOLoss object.
--
-- @XYLossType@ — The type of spatial position loss filter.
--
-- @WHLossType@ — The type of spatial size loss filter.
--
-- @confidenceLossType@ — The type of confidence filter.
--
-- @classesLossType@ — The type of classes filter.
--
-- @reductionType@ — The type of a reduction operation to apply.
--
-- @anchorBoxes@ — This is an NSData which has an array of anchorBoxes defined as a struct{ float width; float height; };
--
-- Returns: A valid MPSCNNYOLOLossDescriptor object or nil, if failure.
--
-- ObjC selector: @+ cnnLossDescriptorWithXYLossType:WHLossType:confidenceLossType:classesLossType:reductionType:anchorBoxes:numberOfAnchorBoxes:@
cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxes :: IsNSData anchorBoxes => MPSCNNLossType -> MPSCNNLossType -> MPSCNNLossType -> MPSCNNLossType -> MPSCNNReductionType -> anchorBoxes -> CULong -> IO (Id MPSCNNYOLOLossDescriptor)
cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxes xyLossType whLossType confidenceLossType classesLossType reductionType anchorBoxes numberOfAnchorBoxes =
  do
    cls' <- getRequiredClass "MPSCNNYOLOLossDescriptor"
    withObjCPtr anchorBoxes $ \raw_anchorBoxes ->
      sendClassMsg cls' (mkSelector "cnnLossDescriptorWithXYLossType:WHLossType:confidenceLossType:classesLossType:reductionType:anchorBoxes:numberOfAnchorBoxes:") (retPtr retVoid) [argCUInt (coerce xyLossType), argCUInt (coerce whLossType), argCUInt (coerce confidenceLossType), argCUInt (coerce classesLossType), argCInt (coerce reductionType), argPtr (castPtr raw_anchorBoxes :: Ptr ()), argCULong (fromIntegral numberOfAnchorBoxes)] >>= retainedObject . castPtr

-- | XYLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- XYLossDescriptor@
xyLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
xyLossDescriptor mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "XYLossDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | XYLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setXYLossDescriptor:@
setXYLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setXYLossDescriptor mpscnnyoloLossDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpscnnyoloLossDescriptor (mkSelector "setXYLossDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | WHLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- WHLossDescriptor@
whLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
whLossDescriptor mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "WHLossDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | WHLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setWHLossDescriptor:@
setWHLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setWHLossDescriptor mpscnnyoloLossDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpscnnyoloLossDescriptor (mkSelector "setWHLossDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | confidenceLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- confidenceLossDescriptor@
confidenceLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
confidenceLossDescriptor mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "confidenceLossDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | confidenceLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setConfidenceLossDescriptor:@
setConfidenceLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setConfidenceLossDescriptor mpscnnyoloLossDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpscnnyoloLossDescriptor (mkSelector "setConfidenceLossDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | classesLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- classesLossDescriptor@
classesLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
classesLossDescriptor mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "classesLossDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | classesLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setClassesLossDescriptor:@
setClassesLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setClassesLossDescriptor mpscnnyoloLossDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpscnnyoloLossDescriptor (mkSelector "setClassesLossDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | reductionType
--
-- ReductionType shared accross all losses (so they may generate same sized output)
--
-- ObjC selector: @- reductionType@
reductionType :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO MPSCNNReductionType
reductionType mpscnnyoloLossDescriptor  =
  fmap (coerce :: CInt -> MPSCNNReductionType) $ sendMsg mpscnnyoloLossDescriptor (mkSelector "reductionType") retCInt []

-- | reductionType
--
-- ReductionType shared accross all losses (so they may generate same sized output)
--
-- ObjC selector: @- setReductionType:@
setReductionType :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> MPSCNNReductionType -> IO ()
setReductionType mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setReductionType:") retVoid [argCInt (coerce value)]

-- | reduceAcrossBatch
--
-- If set to YES then the reduction operation is applied also across the batch-index dimension,              ie. the loss value is summed over images in the batch and the result of the reduction is written              on the first loss image in the batch while the other loss images will be set to zero.              If set to NO, then no reductions are performed across the batch dimension and each image in the batch              will contain the loss value associated with that one particular image.              NOTE: If reductionType == MPSCNNReductionTypeNone, then this flag has no effect on results,              that is no reductions are done in this case.              NOTE: If reduceAcrossBatch is set to YES and reductionType == MPSCNNReductionTypeMean then              the final forward loss value is computed by first summing over the components and then by              dividing the result with: number of feature channels * width * height * number of images in the batch.              The default value is NO.
--
-- ObjC selector: @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO Bool
reduceAcrossBatch mpscnnyoloLossDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnyoloLossDescriptor (mkSelector "reduceAcrossBatch") retCULong []

-- | reduceAcrossBatch
--
-- If set to YES then the reduction operation is applied also across the batch-index dimension,              ie. the loss value is summed over images in the batch and the result of the reduction is written              on the first loss image in the batch while the other loss images will be set to zero.              If set to NO, then no reductions are performed across the batch dimension and each image in the batch              will contain the loss value associated with that one particular image.              NOTE: If reductionType == MPSCNNReductionTypeNone, then this flag has no effect on results,              that is no reductions are done in this case.              NOTE: If reduceAcrossBatch is set to YES and reductionType == MPSCNNReductionTypeMean then              the final forward loss value is computed by first summing over the components and then by              dividing the result with: number of feature channels * width * height * number of images in the batch.              The default value is NO.
--
-- ObjC selector: @- setReduceAcrossBatch:@
setReduceAcrossBatch :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> Bool -> IO ()
setReduceAcrossBatch mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setReduceAcrossBatch:") retVoid [argCULong (if value then 1 else 0)]

-- | rescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox. Default is YES
--
-- ObjC selector: @- rescore@
rescore :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO Bool
rescore mpscnnyoloLossDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mpscnnyoloLossDescriptor (mkSelector "rescore") retCULong []

-- | rescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox. Default is YES
--
-- ObjC selector: @- setRescore:@
setRescore :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> Bool -> IO ()
setRescore mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setRescore:") retVoid [argCULong (if value then 1 else 0)]

-- | scaleXY
--
-- scale factor for XY loss and loss gradient default is 10.0
--
-- ObjC selector: @- scaleXY@
scaleXY :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleXY mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "scaleXY") retCFloat []

-- | scaleXY
--
-- scale factor for XY loss and loss gradient default is 10.0
--
-- ObjC selector: @- setScaleXY:@
setScaleXY :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleXY mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setScaleXY:") retVoid [argCFloat (fromIntegral value)]

-- | scaleWH
--
-- scale factor for WH loss and loss gradient default is 10.0
--
-- ObjC selector: @- scaleWH@
scaleWH :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleWH mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "scaleWH") retCFloat []

-- | scaleWH
--
-- scale factor for WH loss and loss gradient default is 10.0
--
-- ObjC selector: @- setScaleWH:@
setScaleWH :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleWH mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setScaleWH:") retVoid [argCFloat (fromIntegral value)]

-- | scaleNoObject
--
-- scale factor for no object confidence loss and loss gradient default is 5.0
--
-- ObjC selector: @- scaleNoObject@
scaleNoObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleNoObject mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "scaleNoObject") retCFloat []

-- | scaleNoObject
--
-- scale factor for no object confidence loss and loss gradient default is 5.0
--
-- ObjC selector: @- setScaleNoObject:@
setScaleNoObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleNoObject mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setScaleNoObject:") retVoid [argCFloat (fromIntegral value)]

-- | scaleObject
--
-- scale factor for no object confidence loss and loss gradient default is 100.0
--
-- ObjC selector: @- scaleObject@
scaleObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleObject mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "scaleObject") retCFloat []

-- | scaleObject
--
-- scale factor for no object confidence loss and loss gradient default is 100.0
--
-- ObjC selector: @- setScaleObject:@
setScaleObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleObject mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setScaleObject:") retVoid [argCFloat (fromIntegral value)]

-- | scaleClass
--
-- scale factor for no object classes loss and loss gradient default is 2.0
--
-- ObjC selector: @- scaleClass@
scaleClass :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleClass mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "scaleClass") retCFloat []

-- | scaleClass
--
-- scale factor for no object classes loss and loss gradient default is 2.0
--
-- ObjC selector: @- setScaleClass:@
setScaleClass :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleClass mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setScaleClass:") retVoid [argCFloat (fromIntegral value)]

-- | pos_iou
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, default is 0.7
--
-- ObjC selector: @- minIOUForObjectPresence@
minIOUForObjectPresence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
minIOUForObjectPresence mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "minIOUForObjectPresence") retCFloat []

-- | pos_iou
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, default is 0.7
--
-- ObjC selector: @- setMinIOUForObjectPresence:@
setMinIOUForObjectPresence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setMinIOUForObjectPresence mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setMinIOUForObjectPresence:") retVoid [argCFloat (fromIntegral value)]

-- | neg_iou
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence, default is 0.3
--
-- ObjC selector: @- maxIOUForObjectAbsence@
maxIOUForObjectAbsence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
maxIOUForObjectAbsence mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "maxIOUForObjectAbsence") retCFloat []

-- | neg_iou
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence, default is 0.3
--
-- ObjC selector: @- setMaxIOUForObjectAbsence:@
setMaxIOUForObjectAbsence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setMaxIOUForObjectAbsence mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setMaxIOUForObjectAbsence:") retVoid [argCFloat (fromIntegral value)]

-- | numberOfAnchorBoxes
--
-- number of anchor boxes used to detect object per grid cell
--
-- ObjC selector: @- numberOfAnchorBoxes@
numberOfAnchorBoxes :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CULong
numberOfAnchorBoxes mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "numberOfAnchorBoxes") retCULong []

-- | numberOfAnchorBoxes
--
-- number of anchor boxes used to detect object per grid cell
--
-- ObjC selector: @- setNumberOfAnchorBoxes:@
setNumberOfAnchorBoxes :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CULong -> IO ()
setNumberOfAnchorBoxes mpscnnyoloLossDescriptor  value =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "setNumberOfAnchorBoxes:") retVoid [argCULong (fromIntegral value)]

-- | anchorBoxes
--
-- NSData containing the width and height for numberOfAnchorBoxes anchor boxes              This NSData should have 2 float values per anchor box which represent the width              and height of the anchor box.
--
-- typedef struct anchorBox{
-- float width;
-- float height;
-- }anchorBox;
--
-- anchorBox_t gAnchorBoxes[MAX_NUM_ANCHOR_BOXES] = {
-- {.width = 1.f, .height = 2.f},
-- {.width = 1.f, .height = 1.f},
-- {.width = 2.f, .height = 1.f},
-- };
-- NSData* labelsInputData = [NSData dataWithBytes: gAnchorBoxes length: MAX_NUM_ANCHOR_BOXES * sizeof(anchorBox)];
--
-- ObjC selector: @- anchorBoxes@
anchorBoxes :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id NSData)
anchorBoxes mpscnnyoloLossDescriptor  =
  sendMsg mpscnnyoloLossDescriptor (mkSelector "anchorBoxes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | anchorBoxes
--
-- NSData containing the width and height for numberOfAnchorBoxes anchor boxes              This NSData should have 2 float values per anchor box which represent the width              and height of the anchor box.
--
-- typedef struct anchorBox{
-- float width;
-- float height;
-- }anchorBox;
--
-- anchorBox_t gAnchorBoxes[MAX_NUM_ANCHOR_BOXES] = {
-- {.width = 1.f, .height = 2.f},
-- {.width = 1.f, .height = 1.f},
-- {.width = 2.f, .height = 1.f},
-- };
-- NSData* labelsInputData = [NSData dataWithBytes: gAnchorBoxes length: MAX_NUM_ANCHOR_BOXES * sizeof(anchorBox)];
--
-- ObjC selector: @- setAnchorBoxes:@
setAnchorBoxes :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsNSData value) => mpscnnyoloLossDescriptor -> value -> IO ()
setAnchorBoxes mpscnnyoloLossDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mpscnnyoloLossDescriptor (mkSelector "setAnchorBoxes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @cnnLossDescriptorWithXYLossType:WHLossType:confidenceLossType:classesLossType:reductionType:anchorBoxes:numberOfAnchorBoxes:@
cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector :: Selector
cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector = mkSelector "cnnLossDescriptorWithXYLossType:WHLossType:confidenceLossType:classesLossType:reductionType:anchorBoxes:numberOfAnchorBoxes:"

-- | @Selector@ for @XYLossDescriptor@
xyLossDescriptorSelector :: Selector
xyLossDescriptorSelector = mkSelector "XYLossDescriptor"

-- | @Selector@ for @setXYLossDescriptor:@
setXYLossDescriptorSelector :: Selector
setXYLossDescriptorSelector = mkSelector "setXYLossDescriptor:"

-- | @Selector@ for @WHLossDescriptor@
whLossDescriptorSelector :: Selector
whLossDescriptorSelector = mkSelector "WHLossDescriptor"

-- | @Selector@ for @setWHLossDescriptor:@
setWHLossDescriptorSelector :: Selector
setWHLossDescriptorSelector = mkSelector "setWHLossDescriptor:"

-- | @Selector@ for @confidenceLossDescriptor@
confidenceLossDescriptorSelector :: Selector
confidenceLossDescriptorSelector = mkSelector "confidenceLossDescriptor"

-- | @Selector@ for @setConfidenceLossDescriptor:@
setConfidenceLossDescriptorSelector :: Selector
setConfidenceLossDescriptorSelector = mkSelector "setConfidenceLossDescriptor:"

-- | @Selector@ for @classesLossDescriptor@
classesLossDescriptorSelector :: Selector
classesLossDescriptorSelector = mkSelector "classesLossDescriptor"

-- | @Selector@ for @setClassesLossDescriptor:@
setClassesLossDescriptorSelector :: Selector
setClassesLossDescriptorSelector = mkSelector "setClassesLossDescriptor:"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @setReductionType:@
setReductionTypeSelector :: Selector
setReductionTypeSelector = mkSelector "setReductionType:"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @setReduceAcrossBatch:@
setReduceAcrossBatchSelector :: Selector
setReduceAcrossBatchSelector = mkSelector "setReduceAcrossBatch:"

-- | @Selector@ for @rescore@
rescoreSelector :: Selector
rescoreSelector = mkSelector "rescore"

-- | @Selector@ for @setRescore:@
setRescoreSelector :: Selector
setRescoreSelector = mkSelector "setRescore:"

-- | @Selector@ for @scaleXY@
scaleXYSelector :: Selector
scaleXYSelector = mkSelector "scaleXY"

-- | @Selector@ for @setScaleXY:@
setScaleXYSelector :: Selector
setScaleXYSelector = mkSelector "setScaleXY:"

-- | @Selector@ for @scaleWH@
scaleWHSelector :: Selector
scaleWHSelector = mkSelector "scaleWH"

-- | @Selector@ for @setScaleWH:@
setScaleWHSelector :: Selector
setScaleWHSelector = mkSelector "setScaleWH:"

-- | @Selector@ for @scaleNoObject@
scaleNoObjectSelector :: Selector
scaleNoObjectSelector = mkSelector "scaleNoObject"

-- | @Selector@ for @setScaleNoObject:@
setScaleNoObjectSelector :: Selector
setScaleNoObjectSelector = mkSelector "setScaleNoObject:"

-- | @Selector@ for @scaleObject@
scaleObjectSelector :: Selector
scaleObjectSelector = mkSelector "scaleObject"

-- | @Selector@ for @setScaleObject:@
setScaleObjectSelector :: Selector
setScaleObjectSelector = mkSelector "setScaleObject:"

-- | @Selector@ for @scaleClass@
scaleClassSelector :: Selector
scaleClassSelector = mkSelector "scaleClass"

-- | @Selector@ for @setScaleClass:@
setScaleClassSelector :: Selector
setScaleClassSelector = mkSelector "setScaleClass:"

-- | @Selector@ for @minIOUForObjectPresence@
minIOUForObjectPresenceSelector :: Selector
minIOUForObjectPresenceSelector = mkSelector "minIOUForObjectPresence"

-- | @Selector@ for @setMinIOUForObjectPresence:@
setMinIOUForObjectPresenceSelector :: Selector
setMinIOUForObjectPresenceSelector = mkSelector "setMinIOUForObjectPresence:"

-- | @Selector@ for @maxIOUForObjectAbsence@
maxIOUForObjectAbsenceSelector :: Selector
maxIOUForObjectAbsenceSelector = mkSelector "maxIOUForObjectAbsence"

-- | @Selector@ for @setMaxIOUForObjectAbsence:@
setMaxIOUForObjectAbsenceSelector :: Selector
setMaxIOUForObjectAbsenceSelector = mkSelector "setMaxIOUForObjectAbsence:"

-- | @Selector@ for @numberOfAnchorBoxes@
numberOfAnchorBoxesSelector :: Selector
numberOfAnchorBoxesSelector = mkSelector "numberOfAnchorBoxes"

-- | @Selector@ for @setNumberOfAnchorBoxes:@
setNumberOfAnchorBoxesSelector :: Selector
setNumberOfAnchorBoxesSelector = mkSelector "setNumberOfAnchorBoxes:"

-- | @Selector@ for @anchorBoxes@
anchorBoxesSelector :: Selector
anchorBoxesSelector = mkSelector "anchorBoxes"

-- | @Selector@ for @setAnchorBoxes:@
setAnchorBoxesSelector :: Selector
setAnchorBoxesSelector = mkSelector "setAnchorBoxes:"

