{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , anchorBoxesSelector
  , classesLossDescriptorSelector
  , cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector
  , confidenceLossDescriptorSelector
  , initSelector
  , maxIOUForObjectAbsenceSelector
  , minIOUForObjectPresenceSelector
  , numberOfAnchorBoxesSelector
  , reduceAcrossBatchSelector
  , reductionTypeSelector
  , rescoreSelector
  , scaleClassSelector
  , scaleNoObjectSelector
  , scaleObjectSelector
  , scaleWHSelector
  , scaleXYSelector
  , setAnchorBoxesSelector
  , setClassesLossDescriptorSelector
  , setConfidenceLossDescriptorSelector
  , setMaxIOUForObjectAbsenceSelector
  , setMinIOUForObjectPresenceSelector
  , setNumberOfAnchorBoxesSelector
  , setReduceAcrossBatchSelector
  , setReductionTypeSelector
  , setRescoreSelector
  , setScaleClassSelector
  , setScaleNoObjectSelector
  , setScaleObjectSelector
  , setScaleWHSelector
  , setScaleXYSelector
  , setWHLossDescriptorSelector
  , setXYLossDescriptorSelector
  , whLossDescriptorSelector
  , xyLossDescriptorSelector

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

-- | @- init@
init_ :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNYOLOLossDescriptor)
init_ mpscnnyoloLossDescriptor =
  sendOwnedMessage mpscnnyoloLossDescriptor initSelector

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
    sendClassMessage cls' cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector xyLossType whLossType confidenceLossType classesLossType reductionType (toNSData anchorBoxes) numberOfAnchorBoxes

-- | XYLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- XYLossDescriptor@
xyLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
xyLossDescriptor mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor xyLossDescriptorSelector

-- | XYLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setXYLossDescriptor:@
setXYLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setXYLossDescriptor mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setXYLossDescriptorSelector (toMPSCNNLossDescriptor value)

-- | WHLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- WHLossDescriptor@
whLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
whLossDescriptor mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor whLossDescriptorSelector

-- | WHLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setWHLossDescriptor:@
setWHLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setWHLossDescriptor mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setWHLossDescriptorSelector (toMPSCNNLossDescriptor value)

-- | confidenceLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- confidenceLossDescriptor@
confidenceLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
confidenceLossDescriptor mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor confidenceLossDescriptorSelector

-- | confidenceLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setConfidenceLossDescriptor:@
setConfidenceLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setConfidenceLossDescriptor mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setConfidenceLossDescriptorSelector (toMPSCNNLossDescriptor value)

-- | classesLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- classesLossDescriptor@
classesLossDescriptor :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO (Id MPSCNNLossDescriptor)
classesLossDescriptor mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor classesLossDescriptorSelector

-- | classesLossDescriptor
--
-- The type of a loss filter.
--
-- This parameter specifies the type of a loss filter.
--
-- ObjC selector: @- setClassesLossDescriptor:@
setClassesLossDescriptor :: (IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor, IsMPSCNNLossDescriptor value) => mpscnnyoloLossDescriptor -> value -> IO ()
setClassesLossDescriptor mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setClassesLossDescriptorSelector (toMPSCNNLossDescriptor value)

-- | reductionType
--
-- ReductionType shared accross all losses (so they may generate same sized output)
--
-- ObjC selector: @- reductionType@
reductionType :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO MPSCNNReductionType
reductionType mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor reductionTypeSelector

-- | reductionType
--
-- ReductionType shared accross all losses (so they may generate same sized output)
--
-- ObjC selector: @- setReductionType:@
setReductionType :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> MPSCNNReductionType -> IO ()
setReductionType mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setReductionTypeSelector value

-- | reduceAcrossBatch
--
-- If set to YES then the reduction operation is applied also across the batch-index dimension,              ie. the loss value is summed over images in the batch and the result of the reduction is written              on the first loss image in the batch while the other loss images will be set to zero.              If set to NO, then no reductions are performed across the batch dimension and each image in the batch              will contain the loss value associated with that one particular image.              NOTE: If reductionType == MPSCNNReductionTypeNone, then this flag has no effect on results,              that is no reductions are done in this case.              NOTE: If reduceAcrossBatch is set to YES and reductionType == MPSCNNReductionTypeMean then              the final forward loss value is computed by first summing over the components and then by              dividing the result with: number of feature channels * width * height * number of images in the batch.              The default value is NO.
--
-- ObjC selector: @- reduceAcrossBatch@
reduceAcrossBatch :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO Bool
reduceAcrossBatch mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor reduceAcrossBatchSelector

-- | reduceAcrossBatch
--
-- If set to YES then the reduction operation is applied also across the batch-index dimension,              ie. the loss value is summed over images in the batch and the result of the reduction is written              on the first loss image in the batch while the other loss images will be set to zero.              If set to NO, then no reductions are performed across the batch dimension and each image in the batch              will contain the loss value associated with that one particular image.              NOTE: If reductionType == MPSCNNReductionTypeNone, then this flag has no effect on results,              that is no reductions are done in this case.              NOTE: If reduceAcrossBatch is set to YES and reductionType == MPSCNNReductionTypeMean then              the final forward loss value is computed by first summing over the components and then by              dividing the result with: number of feature channels * width * height * number of images in the batch.              The default value is NO.
--
-- ObjC selector: @- setReduceAcrossBatch:@
setReduceAcrossBatch :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> Bool -> IO ()
setReduceAcrossBatch mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setReduceAcrossBatchSelector value

-- | rescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox. Default is YES
--
-- ObjC selector: @- rescore@
rescore :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO Bool
rescore mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor rescoreSelector

-- | rescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox. Default is YES
--
-- ObjC selector: @- setRescore:@
setRescore :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> Bool -> IO ()
setRescore mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setRescoreSelector value

-- | scaleXY
--
-- scale factor for XY loss and loss gradient default is 10.0
--
-- ObjC selector: @- scaleXY@
scaleXY :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleXY mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor scaleXYSelector

-- | scaleXY
--
-- scale factor for XY loss and loss gradient default is 10.0
--
-- ObjC selector: @- setScaleXY:@
setScaleXY :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleXY mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setScaleXYSelector value

-- | scaleWH
--
-- scale factor for WH loss and loss gradient default is 10.0
--
-- ObjC selector: @- scaleWH@
scaleWH :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleWH mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor scaleWHSelector

-- | scaleWH
--
-- scale factor for WH loss and loss gradient default is 10.0
--
-- ObjC selector: @- setScaleWH:@
setScaleWH :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleWH mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setScaleWHSelector value

-- | scaleNoObject
--
-- scale factor for no object confidence loss and loss gradient default is 5.0
--
-- ObjC selector: @- scaleNoObject@
scaleNoObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleNoObject mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor scaleNoObjectSelector

-- | scaleNoObject
--
-- scale factor for no object confidence loss and loss gradient default is 5.0
--
-- ObjC selector: @- setScaleNoObject:@
setScaleNoObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleNoObject mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setScaleNoObjectSelector value

-- | scaleObject
--
-- scale factor for no object confidence loss and loss gradient default is 100.0
--
-- ObjC selector: @- scaleObject@
scaleObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleObject mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor scaleObjectSelector

-- | scaleObject
--
-- scale factor for no object confidence loss and loss gradient default is 100.0
--
-- ObjC selector: @- setScaleObject:@
setScaleObject :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleObject mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setScaleObjectSelector value

-- | scaleClass
--
-- scale factor for no object classes loss and loss gradient default is 2.0
--
-- ObjC selector: @- scaleClass@
scaleClass :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
scaleClass mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor scaleClassSelector

-- | scaleClass
--
-- scale factor for no object classes loss and loss gradient default is 2.0
--
-- ObjC selector: @- setScaleClass:@
setScaleClass :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setScaleClass mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setScaleClassSelector value

-- | pos_iou
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, default is 0.7
--
-- ObjC selector: @- minIOUForObjectPresence@
minIOUForObjectPresence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
minIOUForObjectPresence mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor minIOUForObjectPresenceSelector

-- | pos_iou
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, default is 0.7
--
-- ObjC selector: @- setMinIOUForObjectPresence:@
setMinIOUForObjectPresence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setMinIOUForObjectPresence mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setMinIOUForObjectPresenceSelector value

-- | neg_iou
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence, default is 0.3
--
-- ObjC selector: @- maxIOUForObjectAbsence@
maxIOUForObjectAbsence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CFloat
maxIOUForObjectAbsence mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor maxIOUForObjectAbsenceSelector

-- | neg_iou
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence, default is 0.3
--
-- ObjC selector: @- setMaxIOUForObjectAbsence:@
setMaxIOUForObjectAbsence :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CFloat -> IO ()
setMaxIOUForObjectAbsence mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setMaxIOUForObjectAbsenceSelector value

-- | numberOfAnchorBoxes
--
-- number of anchor boxes used to detect object per grid cell
--
-- ObjC selector: @- numberOfAnchorBoxes@
numberOfAnchorBoxes :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> IO CULong
numberOfAnchorBoxes mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor numberOfAnchorBoxesSelector

-- | numberOfAnchorBoxes
--
-- number of anchor boxes used to detect object per grid cell
--
-- ObjC selector: @- setNumberOfAnchorBoxes:@
setNumberOfAnchorBoxes :: IsMPSCNNYOLOLossDescriptor mpscnnyoloLossDescriptor => mpscnnyoloLossDescriptor -> CULong -> IO ()
setNumberOfAnchorBoxes mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setNumberOfAnchorBoxesSelector value

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
anchorBoxes mpscnnyoloLossDescriptor =
  sendMessage mpscnnyoloLossDescriptor anchorBoxesSelector

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
setAnchorBoxes mpscnnyoloLossDescriptor value =
  sendMessage mpscnnyoloLossDescriptor setAnchorBoxesSelector (toNSData value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MPSCNNYOLOLossDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @cnnLossDescriptorWithXYLossType:WHLossType:confidenceLossType:classesLossType:reductionType:anchorBoxes:numberOfAnchorBoxes:@
cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector :: Selector '[MPSCNNLossType, MPSCNNLossType, MPSCNNLossType, MPSCNNLossType, MPSCNNReductionType, Id NSData, CULong] (Id MPSCNNYOLOLossDescriptor)
cnnLossDescriptorWithXYLossType_WHLossType_confidenceLossType_classesLossType_reductionType_anchorBoxes_numberOfAnchorBoxesSelector = mkSelector "cnnLossDescriptorWithXYLossType:WHLossType:confidenceLossType:classesLossType:reductionType:anchorBoxes:numberOfAnchorBoxes:"

-- | @Selector@ for @XYLossDescriptor@
xyLossDescriptorSelector :: Selector '[] (Id MPSCNNLossDescriptor)
xyLossDescriptorSelector = mkSelector "XYLossDescriptor"

-- | @Selector@ for @setXYLossDescriptor:@
setXYLossDescriptorSelector :: Selector '[Id MPSCNNLossDescriptor] ()
setXYLossDescriptorSelector = mkSelector "setXYLossDescriptor:"

-- | @Selector@ for @WHLossDescriptor@
whLossDescriptorSelector :: Selector '[] (Id MPSCNNLossDescriptor)
whLossDescriptorSelector = mkSelector "WHLossDescriptor"

-- | @Selector@ for @setWHLossDescriptor:@
setWHLossDescriptorSelector :: Selector '[Id MPSCNNLossDescriptor] ()
setWHLossDescriptorSelector = mkSelector "setWHLossDescriptor:"

-- | @Selector@ for @confidenceLossDescriptor@
confidenceLossDescriptorSelector :: Selector '[] (Id MPSCNNLossDescriptor)
confidenceLossDescriptorSelector = mkSelector "confidenceLossDescriptor"

-- | @Selector@ for @setConfidenceLossDescriptor:@
setConfidenceLossDescriptorSelector :: Selector '[Id MPSCNNLossDescriptor] ()
setConfidenceLossDescriptorSelector = mkSelector "setConfidenceLossDescriptor:"

-- | @Selector@ for @classesLossDescriptor@
classesLossDescriptorSelector :: Selector '[] (Id MPSCNNLossDescriptor)
classesLossDescriptorSelector = mkSelector "classesLossDescriptor"

-- | @Selector@ for @setClassesLossDescriptor:@
setClassesLossDescriptorSelector :: Selector '[Id MPSCNNLossDescriptor] ()
setClassesLossDescriptorSelector = mkSelector "setClassesLossDescriptor:"

-- | @Selector@ for @reductionType@
reductionTypeSelector :: Selector '[] MPSCNNReductionType
reductionTypeSelector = mkSelector "reductionType"

-- | @Selector@ for @setReductionType:@
setReductionTypeSelector :: Selector '[MPSCNNReductionType] ()
setReductionTypeSelector = mkSelector "setReductionType:"

-- | @Selector@ for @reduceAcrossBatch@
reduceAcrossBatchSelector :: Selector '[] Bool
reduceAcrossBatchSelector = mkSelector "reduceAcrossBatch"

-- | @Selector@ for @setReduceAcrossBatch:@
setReduceAcrossBatchSelector :: Selector '[Bool] ()
setReduceAcrossBatchSelector = mkSelector "setReduceAcrossBatch:"

-- | @Selector@ for @rescore@
rescoreSelector :: Selector '[] Bool
rescoreSelector = mkSelector "rescore"

-- | @Selector@ for @setRescore:@
setRescoreSelector :: Selector '[Bool] ()
setRescoreSelector = mkSelector "setRescore:"

-- | @Selector@ for @scaleXY@
scaleXYSelector :: Selector '[] CFloat
scaleXYSelector = mkSelector "scaleXY"

-- | @Selector@ for @setScaleXY:@
setScaleXYSelector :: Selector '[CFloat] ()
setScaleXYSelector = mkSelector "setScaleXY:"

-- | @Selector@ for @scaleWH@
scaleWHSelector :: Selector '[] CFloat
scaleWHSelector = mkSelector "scaleWH"

-- | @Selector@ for @setScaleWH:@
setScaleWHSelector :: Selector '[CFloat] ()
setScaleWHSelector = mkSelector "setScaleWH:"

-- | @Selector@ for @scaleNoObject@
scaleNoObjectSelector :: Selector '[] CFloat
scaleNoObjectSelector = mkSelector "scaleNoObject"

-- | @Selector@ for @setScaleNoObject:@
setScaleNoObjectSelector :: Selector '[CFloat] ()
setScaleNoObjectSelector = mkSelector "setScaleNoObject:"

-- | @Selector@ for @scaleObject@
scaleObjectSelector :: Selector '[] CFloat
scaleObjectSelector = mkSelector "scaleObject"

-- | @Selector@ for @setScaleObject:@
setScaleObjectSelector :: Selector '[CFloat] ()
setScaleObjectSelector = mkSelector "setScaleObject:"

-- | @Selector@ for @scaleClass@
scaleClassSelector :: Selector '[] CFloat
scaleClassSelector = mkSelector "scaleClass"

-- | @Selector@ for @setScaleClass:@
setScaleClassSelector :: Selector '[CFloat] ()
setScaleClassSelector = mkSelector "setScaleClass:"

-- | @Selector@ for @minIOUForObjectPresence@
minIOUForObjectPresenceSelector :: Selector '[] CFloat
minIOUForObjectPresenceSelector = mkSelector "minIOUForObjectPresence"

-- | @Selector@ for @setMinIOUForObjectPresence:@
setMinIOUForObjectPresenceSelector :: Selector '[CFloat] ()
setMinIOUForObjectPresenceSelector = mkSelector "setMinIOUForObjectPresence:"

-- | @Selector@ for @maxIOUForObjectAbsence@
maxIOUForObjectAbsenceSelector :: Selector '[] CFloat
maxIOUForObjectAbsenceSelector = mkSelector "maxIOUForObjectAbsence"

-- | @Selector@ for @setMaxIOUForObjectAbsence:@
setMaxIOUForObjectAbsenceSelector :: Selector '[CFloat] ()
setMaxIOUForObjectAbsenceSelector = mkSelector "setMaxIOUForObjectAbsence:"

-- | @Selector@ for @numberOfAnchorBoxes@
numberOfAnchorBoxesSelector :: Selector '[] CULong
numberOfAnchorBoxesSelector = mkSelector "numberOfAnchorBoxes"

-- | @Selector@ for @setNumberOfAnchorBoxes:@
setNumberOfAnchorBoxesSelector :: Selector '[CULong] ()
setNumberOfAnchorBoxesSelector = mkSelector "setNumberOfAnchorBoxes:"

-- | @Selector@ for @anchorBoxes@
anchorBoxesSelector :: Selector '[] (Id NSData)
anchorBoxesSelector = mkSelector "anchorBoxes"

-- | @Selector@ for @setAnchorBoxes:@
setAnchorBoxesSelector :: Selector '[Id NSData] ()
setAnchorBoxesSelector = mkSelector "setAnchorBoxes:"

