{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MLCYOLOLossDescriptor
--
-- The MLCYOLOLossDescriptor specifies a YOLO loss filter descriptor.
--
-- Generated bindings for @MLCYOLOLossDescriptor@.
module ObjC.MLCompute.MLCYOLOLossDescriptor
  ( MLCYOLOLossDescriptor
  , IsMLCYOLOLossDescriptor(..)
  , new
  , init_
  , descriptorWithAnchorBoxes_anchorBoxCount
  , anchorBoxCount
  , anchorBoxes
  , shouldRescore
  , setShouldRescore
  , scaleSpatialPositionLoss
  , setScaleSpatialPositionLoss
  , scaleSpatialSizeLoss
  , setScaleSpatialSizeLoss
  , scaleNoObjectConfidenceLoss
  , setScaleNoObjectConfidenceLoss
  , scaleObjectConfidenceLoss
  , setScaleObjectConfidenceLoss
  , scaleClassLoss
  , setScaleClassLoss
  , minimumIOUForObjectPresence
  , setMinimumIOUForObjectPresence
  , maximumIOUForObjectAbsence
  , setMaximumIOUForObjectAbsence
  , anchorBoxCountSelector
  , anchorBoxesSelector
  , descriptorWithAnchorBoxes_anchorBoxCountSelector
  , initSelector
  , maximumIOUForObjectAbsenceSelector
  , minimumIOUForObjectPresenceSelector
  , newSelector
  , scaleClassLossSelector
  , scaleNoObjectConfidenceLossSelector
  , scaleObjectConfidenceLossSelector
  , scaleSpatialPositionLossSelector
  , scaleSpatialSizeLossSelector
  , setMaximumIOUForObjectAbsenceSelector
  , setMinimumIOUForObjectPresenceSelector
  , setScaleClassLossSelector
  , setScaleNoObjectConfidenceLossSelector
  , setScaleObjectConfidenceLossSelector
  , setScaleSpatialPositionLossSelector
  , setScaleSpatialSizeLossSelector
  , setShouldRescoreSelector
  , shouldRescoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MLCompute.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCYOLOLossDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCYOLOLossDescriptor"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO (Id MLCYOLOLossDescriptor)
init_ mlcyoloLossDescriptor =
  sendOwnedMessage mlcyoloLossDescriptor initSelector

-- | Create a YOLO loss descriptor object
--
-- @anchorBoxes@ — The anchor box data
--
-- @anchorBoxCount@ — The number of anchor boxes
--
-- Returns: A new MLCYOLOLossDescriptor object.
--
-- ObjC selector: @+ descriptorWithAnchorBoxes:anchorBoxCount:@
descriptorWithAnchorBoxes_anchorBoxCount :: IsNSData anchorBoxes => anchorBoxes -> CULong -> IO (Id MLCYOLOLossDescriptor)
descriptorWithAnchorBoxes_anchorBoxCount anchorBoxes anchorBoxCount =
  do
    cls' <- getRequiredClass "MLCYOLOLossDescriptor"
    sendClassMessage cls' descriptorWithAnchorBoxes_anchorBoxCountSelector (toNSData anchorBoxes) anchorBoxCount

-- | anchorBoxCount
--
-- number of anchor boxes used to detect object per grid cell
--
-- ObjC selector: @- anchorBoxCount@
anchorBoxCount :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CULong
anchorBoxCount mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor anchorBoxCountSelector

-- | anchorBoxes
--
-- @NSData@ containing the width and height for @anchorBoxCount@ anchor boxes              This @NSData@ should have 2 floating-point values per anchor box which represent the width              and height of the anchor box.
--
-- ObjC selector: @- anchorBoxes@
anchorBoxes :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO (Id NSData)
anchorBoxes mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor anchorBoxesSelector

-- | shouldRescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox.  The default is YES
--
-- ObjC selector: @- shouldRescore@
shouldRescore :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO Bool
shouldRescore mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor shouldRescoreSelector

-- | shouldRescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox.  The default is YES
--
-- ObjC selector: @- setShouldRescore:@
setShouldRescore :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> Bool -> IO ()
setShouldRescore mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setShouldRescoreSelector value

-- | scaleSpatialPositionLoss
--
-- The scale factor for spatial position loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- scaleSpatialPositionLoss@
scaleSpatialPositionLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleSpatialPositionLoss mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor scaleSpatialPositionLossSelector

-- | scaleSpatialPositionLoss
--
-- The scale factor for spatial position loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- setScaleSpatialPositionLoss:@
setScaleSpatialPositionLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleSpatialPositionLoss mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setScaleSpatialPositionLossSelector value

-- | scaleSpatialSizeLoss
--
-- The scale factor for spatial size loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- scaleSpatialSizeLoss@
scaleSpatialSizeLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleSpatialSizeLoss mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor scaleSpatialSizeLossSelector

-- | scaleSpatialSizeLoss
--
-- The scale factor for spatial size loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- setScaleSpatialSizeLoss:@
setScaleSpatialSizeLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleSpatialSizeLoss mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setScaleSpatialSizeLossSelector value

-- | scaleNoObject
--
-- The scale factor for no object confidence loss and loss gradient.  The default is 5.0
--
-- ObjC selector: @- scaleNoObjectConfidenceLoss@
scaleNoObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleNoObjectConfidenceLoss mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor scaleNoObjectConfidenceLossSelector

-- | scaleNoObject
--
-- The scale factor for no object confidence loss and loss gradient.  The default is 5.0
--
-- ObjC selector: @- setScaleNoObjectConfidenceLoss:@
setScaleNoObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleNoObjectConfidenceLoss mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setScaleNoObjectConfidenceLossSelector value

-- | scaleObject
--
-- The scale factor for object confidence loss and loss gradient.  The default is 100.0
--
-- ObjC selector: @- scaleObjectConfidenceLoss@
scaleObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleObjectConfidenceLoss mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor scaleObjectConfidenceLossSelector

-- | scaleObject
--
-- The scale factor for object confidence loss and loss gradient.  The default is 100.0
--
-- ObjC selector: @- setScaleObjectConfidenceLoss:@
setScaleObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleObjectConfidenceLoss mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setScaleObjectConfidenceLossSelector value

-- | scaleClass
--
-- The scale factor for no object classes loss and loss gradient.  The default is 2.0
--
-- ObjC selector: @- scaleClassLoss@
scaleClassLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleClassLoss mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor scaleClassLossSelector

-- | scaleClass
--
-- The scale factor for no object classes loss and loss gradient.  The default is 2.0
--
-- ObjC selector: @- setScaleClassLoss:@
setScaleClassLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleClassLoss mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setScaleClassLossSelector value

-- | positive IOU
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, The default is 0.7
--
-- ObjC selector: @- minimumIOUForObjectPresence@
minimumIOUForObjectPresence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
minimumIOUForObjectPresence mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor minimumIOUForObjectPresenceSelector

-- | positive IOU
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, The default is 0.7
--
-- ObjC selector: @- setMinimumIOUForObjectPresence:@
setMinimumIOUForObjectPresence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setMinimumIOUForObjectPresence mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setMinimumIOUForObjectPresenceSelector value

-- | negative IOU
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence.  The default is 0.3
--
-- ObjC selector: @- maximumIOUForObjectAbsence@
maximumIOUForObjectAbsence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
maximumIOUForObjectAbsence mlcyoloLossDescriptor =
  sendMessage mlcyoloLossDescriptor maximumIOUForObjectAbsenceSelector

-- | negative IOU
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence.  The default is 0.3
--
-- ObjC selector: @- setMaximumIOUForObjectAbsence:@
setMaximumIOUForObjectAbsence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setMaximumIOUForObjectAbsence mlcyoloLossDescriptor value =
  sendMessage mlcyoloLossDescriptor setMaximumIOUForObjectAbsenceSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id MLCYOLOLossDescriptor)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MLCYOLOLossDescriptor)
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithAnchorBoxes:anchorBoxCount:@
descriptorWithAnchorBoxes_anchorBoxCountSelector :: Selector '[Id NSData, CULong] (Id MLCYOLOLossDescriptor)
descriptorWithAnchorBoxes_anchorBoxCountSelector = mkSelector "descriptorWithAnchorBoxes:anchorBoxCount:"

-- | @Selector@ for @anchorBoxCount@
anchorBoxCountSelector :: Selector '[] CULong
anchorBoxCountSelector = mkSelector "anchorBoxCount"

-- | @Selector@ for @anchorBoxes@
anchorBoxesSelector :: Selector '[] (Id NSData)
anchorBoxesSelector = mkSelector "anchorBoxes"

-- | @Selector@ for @shouldRescore@
shouldRescoreSelector :: Selector '[] Bool
shouldRescoreSelector = mkSelector "shouldRescore"

-- | @Selector@ for @setShouldRescore:@
setShouldRescoreSelector :: Selector '[Bool] ()
setShouldRescoreSelector = mkSelector "setShouldRescore:"

-- | @Selector@ for @scaleSpatialPositionLoss@
scaleSpatialPositionLossSelector :: Selector '[] CFloat
scaleSpatialPositionLossSelector = mkSelector "scaleSpatialPositionLoss"

-- | @Selector@ for @setScaleSpatialPositionLoss:@
setScaleSpatialPositionLossSelector :: Selector '[CFloat] ()
setScaleSpatialPositionLossSelector = mkSelector "setScaleSpatialPositionLoss:"

-- | @Selector@ for @scaleSpatialSizeLoss@
scaleSpatialSizeLossSelector :: Selector '[] CFloat
scaleSpatialSizeLossSelector = mkSelector "scaleSpatialSizeLoss"

-- | @Selector@ for @setScaleSpatialSizeLoss:@
setScaleSpatialSizeLossSelector :: Selector '[CFloat] ()
setScaleSpatialSizeLossSelector = mkSelector "setScaleSpatialSizeLoss:"

-- | @Selector@ for @scaleNoObjectConfidenceLoss@
scaleNoObjectConfidenceLossSelector :: Selector '[] CFloat
scaleNoObjectConfidenceLossSelector = mkSelector "scaleNoObjectConfidenceLoss"

-- | @Selector@ for @setScaleNoObjectConfidenceLoss:@
setScaleNoObjectConfidenceLossSelector :: Selector '[CFloat] ()
setScaleNoObjectConfidenceLossSelector = mkSelector "setScaleNoObjectConfidenceLoss:"

-- | @Selector@ for @scaleObjectConfidenceLoss@
scaleObjectConfidenceLossSelector :: Selector '[] CFloat
scaleObjectConfidenceLossSelector = mkSelector "scaleObjectConfidenceLoss"

-- | @Selector@ for @setScaleObjectConfidenceLoss:@
setScaleObjectConfidenceLossSelector :: Selector '[CFloat] ()
setScaleObjectConfidenceLossSelector = mkSelector "setScaleObjectConfidenceLoss:"

-- | @Selector@ for @scaleClassLoss@
scaleClassLossSelector :: Selector '[] CFloat
scaleClassLossSelector = mkSelector "scaleClassLoss"

-- | @Selector@ for @setScaleClassLoss:@
setScaleClassLossSelector :: Selector '[CFloat] ()
setScaleClassLossSelector = mkSelector "setScaleClassLoss:"

-- | @Selector@ for @minimumIOUForObjectPresence@
minimumIOUForObjectPresenceSelector :: Selector '[] CFloat
minimumIOUForObjectPresenceSelector = mkSelector "minimumIOUForObjectPresence"

-- | @Selector@ for @setMinimumIOUForObjectPresence:@
setMinimumIOUForObjectPresenceSelector :: Selector '[CFloat] ()
setMinimumIOUForObjectPresenceSelector = mkSelector "setMinimumIOUForObjectPresence:"

-- | @Selector@ for @maximumIOUForObjectAbsence@
maximumIOUForObjectAbsenceSelector :: Selector '[] CFloat
maximumIOUForObjectAbsenceSelector = mkSelector "maximumIOUForObjectAbsence"

-- | @Selector@ for @setMaximumIOUForObjectAbsence:@
setMaximumIOUForObjectAbsenceSelector :: Selector '[CFloat] ()
setMaximumIOUForObjectAbsenceSelector = mkSelector "setMaximumIOUForObjectAbsence:"

