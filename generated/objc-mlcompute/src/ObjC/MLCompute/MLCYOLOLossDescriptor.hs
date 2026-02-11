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
  , newSelector
  , initSelector
  , descriptorWithAnchorBoxes_anchorBoxCountSelector
  , anchorBoxCountSelector
  , anchorBoxesSelector
  , shouldRescoreSelector
  , setShouldRescoreSelector
  , scaleSpatialPositionLossSelector
  , setScaleSpatialPositionLossSelector
  , scaleSpatialSizeLossSelector
  , setScaleSpatialSizeLossSelector
  , scaleNoObjectConfidenceLossSelector
  , setScaleNoObjectConfidenceLossSelector
  , scaleObjectConfidenceLossSelector
  , setScaleObjectConfidenceLossSelector
  , scaleClassLossSelector
  , setScaleClassLossSelector
  , minimumIOUForObjectPresenceSelector
  , setMinimumIOUForObjectPresenceSelector
  , maximumIOUForObjectAbsenceSelector
  , setMaximumIOUForObjectAbsenceSelector


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
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MLCYOLOLossDescriptor)
new  =
  do
    cls' <- getRequiredClass "MLCYOLOLossDescriptor"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO (Id MLCYOLOLossDescriptor)
init_ mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr anchorBoxes $ \raw_anchorBoxes ->
      sendClassMsg cls' (mkSelector "descriptorWithAnchorBoxes:anchorBoxCount:") (retPtr retVoid) [argPtr (castPtr raw_anchorBoxes :: Ptr ()), argCULong (fromIntegral anchorBoxCount)] >>= retainedObject . castPtr

-- | anchorBoxCount
--
-- number of anchor boxes used to detect object per grid cell
--
-- ObjC selector: @- anchorBoxCount@
anchorBoxCount :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CULong
anchorBoxCount mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "anchorBoxCount") retCULong []

-- | anchorBoxes
--
-- @NSData@ containing the width and height for @anchorBoxCount@ anchor boxes              This @NSData@ should have 2 floating-point values per anchor box which represent the width              and height of the anchor box.
--
-- ObjC selector: @- anchorBoxes@
anchorBoxes :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO (Id NSData)
anchorBoxes mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "anchorBoxes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | shouldRescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox.  The default is YES
--
-- ObjC selector: @- shouldRescore@
shouldRescore :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO Bool
shouldRescore mlcyoloLossDescriptor  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg mlcyoloLossDescriptor (mkSelector "shouldRescore") retCULong []

-- | shouldRescore
--
-- Rescore pertains to multiplying the confidence groundTruth with IOU (intersection over union)              of predicted bounding box and the groundTruth boundingBox.  The default is YES
--
-- ObjC selector: @- setShouldRescore:@
setShouldRescore :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> Bool -> IO ()
setShouldRescore mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setShouldRescore:") retVoid [argCULong (if value then 1 else 0)]

-- | scaleSpatialPositionLoss
--
-- The scale factor for spatial position loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- scaleSpatialPositionLoss@
scaleSpatialPositionLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleSpatialPositionLoss mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "scaleSpatialPositionLoss") retCFloat []

-- | scaleSpatialPositionLoss
--
-- The scale factor for spatial position loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- setScaleSpatialPositionLoss:@
setScaleSpatialPositionLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleSpatialPositionLoss mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setScaleSpatialPositionLoss:") retVoid [argCFloat (fromIntegral value)]

-- | scaleSpatialSizeLoss
--
-- The scale factor for spatial size loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- scaleSpatialSizeLoss@
scaleSpatialSizeLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleSpatialSizeLoss mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "scaleSpatialSizeLoss") retCFloat []

-- | scaleSpatialSizeLoss
--
-- The scale factor for spatial size loss and loss gradient.  The default is 10.0
--
-- ObjC selector: @- setScaleSpatialSizeLoss:@
setScaleSpatialSizeLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleSpatialSizeLoss mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setScaleSpatialSizeLoss:") retVoid [argCFloat (fromIntegral value)]

-- | scaleNoObject
--
-- The scale factor for no object confidence loss and loss gradient.  The default is 5.0
--
-- ObjC selector: @- scaleNoObjectConfidenceLoss@
scaleNoObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleNoObjectConfidenceLoss mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "scaleNoObjectConfidenceLoss") retCFloat []

-- | scaleNoObject
--
-- The scale factor for no object confidence loss and loss gradient.  The default is 5.0
--
-- ObjC selector: @- setScaleNoObjectConfidenceLoss:@
setScaleNoObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleNoObjectConfidenceLoss mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setScaleNoObjectConfidenceLoss:") retVoid [argCFloat (fromIntegral value)]

-- | scaleObject
--
-- The scale factor for object confidence loss and loss gradient.  The default is 100.0
--
-- ObjC selector: @- scaleObjectConfidenceLoss@
scaleObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleObjectConfidenceLoss mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "scaleObjectConfidenceLoss") retCFloat []

-- | scaleObject
--
-- The scale factor for object confidence loss and loss gradient.  The default is 100.0
--
-- ObjC selector: @- setScaleObjectConfidenceLoss:@
setScaleObjectConfidenceLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleObjectConfidenceLoss mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setScaleObjectConfidenceLoss:") retVoid [argCFloat (fromIntegral value)]

-- | scaleClass
--
-- The scale factor for no object classes loss and loss gradient.  The default is 2.0
--
-- ObjC selector: @- scaleClassLoss@
scaleClassLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
scaleClassLoss mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "scaleClassLoss") retCFloat []

-- | scaleClass
--
-- The scale factor for no object classes loss and loss gradient.  The default is 2.0
--
-- ObjC selector: @- setScaleClassLoss:@
setScaleClassLoss :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setScaleClassLoss mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setScaleClassLoss:") retVoid [argCFloat (fromIntegral value)]

-- | positive IOU
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, The default is 0.7
--
-- ObjC selector: @- minimumIOUForObjectPresence@
minimumIOUForObjectPresence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
minimumIOUForObjectPresence mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "minimumIOUForObjectPresence") retCFloat []

-- | positive IOU
--
-- If the prediction IOU with groundTruth is higher than this              value we consider it a confident object presence, The default is 0.7
--
-- ObjC selector: @- setMinimumIOUForObjectPresence:@
setMinimumIOUForObjectPresence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setMinimumIOUForObjectPresence mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setMinimumIOUForObjectPresence:") retVoid [argCFloat (fromIntegral value)]

-- | negative IOU
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence.  The default is 0.3
--
-- ObjC selector: @- maximumIOUForObjectAbsence@
maximumIOUForObjectAbsence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> IO CFloat
maximumIOUForObjectAbsence mlcyoloLossDescriptor  =
  sendMsg mlcyoloLossDescriptor (mkSelector "maximumIOUForObjectAbsence") retCFloat []

-- | negative IOU
--
-- If the prediction IOU with groundTruth is lower than this              value we consider it a confident object absence.  The default is 0.3
--
-- ObjC selector: @- setMaximumIOUForObjectAbsence:@
setMaximumIOUForObjectAbsence :: IsMLCYOLOLossDescriptor mlcyoloLossDescriptor => mlcyoloLossDescriptor -> CFloat -> IO ()
setMaximumIOUForObjectAbsence mlcyoloLossDescriptor  value =
  sendMsg mlcyoloLossDescriptor (mkSelector "setMaximumIOUForObjectAbsence:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @descriptorWithAnchorBoxes:anchorBoxCount:@
descriptorWithAnchorBoxes_anchorBoxCountSelector :: Selector
descriptorWithAnchorBoxes_anchorBoxCountSelector = mkSelector "descriptorWithAnchorBoxes:anchorBoxCount:"

-- | @Selector@ for @anchorBoxCount@
anchorBoxCountSelector :: Selector
anchorBoxCountSelector = mkSelector "anchorBoxCount"

-- | @Selector@ for @anchorBoxes@
anchorBoxesSelector :: Selector
anchorBoxesSelector = mkSelector "anchorBoxes"

-- | @Selector@ for @shouldRescore@
shouldRescoreSelector :: Selector
shouldRescoreSelector = mkSelector "shouldRescore"

-- | @Selector@ for @setShouldRescore:@
setShouldRescoreSelector :: Selector
setShouldRescoreSelector = mkSelector "setShouldRescore:"

-- | @Selector@ for @scaleSpatialPositionLoss@
scaleSpatialPositionLossSelector :: Selector
scaleSpatialPositionLossSelector = mkSelector "scaleSpatialPositionLoss"

-- | @Selector@ for @setScaleSpatialPositionLoss:@
setScaleSpatialPositionLossSelector :: Selector
setScaleSpatialPositionLossSelector = mkSelector "setScaleSpatialPositionLoss:"

-- | @Selector@ for @scaleSpatialSizeLoss@
scaleSpatialSizeLossSelector :: Selector
scaleSpatialSizeLossSelector = mkSelector "scaleSpatialSizeLoss"

-- | @Selector@ for @setScaleSpatialSizeLoss:@
setScaleSpatialSizeLossSelector :: Selector
setScaleSpatialSizeLossSelector = mkSelector "setScaleSpatialSizeLoss:"

-- | @Selector@ for @scaleNoObjectConfidenceLoss@
scaleNoObjectConfidenceLossSelector :: Selector
scaleNoObjectConfidenceLossSelector = mkSelector "scaleNoObjectConfidenceLoss"

-- | @Selector@ for @setScaleNoObjectConfidenceLoss:@
setScaleNoObjectConfidenceLossSelector :: Selector
setScaleNoObjectConfidenceLossSelector = mkSelector "setScaleNoObjectConfidenceLoss:"

-- | @Selector@ for @scaleObjectConfidenceLoss@
scaleObjectConfidenceLossSelector :: Selector
scaleObjectConfidenceLossSelector = mkSelector "scaleObjectConfidenceLoss"

-- | @Selector@ for @setScaleObjectConfidenceLoss:@
setScaleObjectConfidenceLossSelector :: Selector
setScaleObjectConfidenceLossSelector = mkSelector "setScaleObjectConfidenceLoss:"

-- | @Selector@ for @scaleClassLoss@
scaleClassLossSelector :: Selector
scaleClassLossSelector = mkSelector "scaleClassLoss"

-- | @Selector@ for @setScaleClassLoss:@
setScaleClassLossSelector :: Selector
setScaleClassLossSelector = mkSelector "setScaleClassLoss:"

-- | @Selector@ for @minimumIOUForObjectPresence@
minimumIOUForObjectPresenceSelector :: Selector
minimumIOUForObjectPresenceSelector = mkSelector "minimumIOUForObjectPresence"

-- | @Selector@ for @setMinimumIOUForObjectPresence:@
setMinimumIOUForObjectPresenceSelector :: Selector
setMinimumIOUForObjectPresenceSelector = mkSelector "setMinimumIOUForObjectPresence:"

-- | @Selector@ for @maximumIOUForObjectAbsence@
maximumIOUForObjectAbsenceSelector :: Selector
maximumIOUForObjectAbsenceSelector = mkSelector "maximumIOUForObjectAbsence"

-- | @Selector@ for @setMaximumIOUForObjectAbsence:@
setMaximumIOUForObjectAbsenceSelector :: Selector
setMaximumIOUForObjectAbsenceSelector = mkSelector "setMaximumIOUForObjectAbsence:"

