{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLStereoscopicCamera@.
module ObjC.ModelIO.MDLStereoscopicCamera
  ( MDLStereoscopicCamera
  , IsMDLStereoscopicCamera(..)
  , interPupillaryDistance
  , setInterPupillaryDistance
  , leftVergence
  , setLeftVergence
  , rightVergence
  , setRightVergence
  , overlap
  , setOverlap
  , interPupillaryDistanceSelector
  , setInterPupillaryDistanceSelector
  , leftVergenceSelector
  , setLeftVergenceSelector
  , rightVergenceSelector
  , setRightVergenceSelector
  , overlapSelector
  , setOverlapSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Inter-pupillary distance in mm. Default is 63mm.
--
-- ObjC selector: @- interPupillaryDistance@
interPupillaryDistance :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> IO CFloat
interPupillaryDistance mdlStereoscopicCamera  =
  sendMsg mdlStereoscopicCamera (mkSelector "interPupillaryDistance") retCFloat []

-- | Inter-pupillary distance in mm. Default is 63mm.
--
-- ObjC selector: @- setInterPupillaryDistance:@
setInterPupillaryDistance :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> CFloat -> IO ()
setInterPupillaryDistance mdlStereoscopicCamera  value =
  sendMsg mdlStereoscopicCamera (mkSelector "setInterPupillaryDistance:") retVoid [argCFloat (fromIntegral value)]

-- | Vergence in a stereoscopic camera can be controlled in two ways. A toed-in  binocular stereoscopic camera rotates the lens and sensor together such that a  ray projected from the center of either sensor and lens meets at a point. A  parallel binocular stereoscopic camera accomplishes the same thing by shifting  the relative centers of the sensor and lens.
--
-- If a parallel binocular stereoscopic camera is modeled, the sensor should be  considered to have shifted by an amount h. If left and right vergence are equal,   h = (focal length * interOcularDistance) / distance to vergence point.
--
-- Vergence is measured in degrees towards center and is usually positive.
--
-- ObjC selector: @- leftVergence@
leftVergence :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> IO CFloat
leftVergence mdlStereoscopicCamera  =
  sendMsg mdlStereoscopicCamera (mkSelector "leftVergence") retCFloat []

-- | Vergence in a stereoscopic camera can be controlled in two ways. A toed-in  binocular stereoscopic camera rotates the lens and sensor together such that a  ray projected from the center of either sensor and lens meets at a point. A  parallel binocular stereoscopic camera accomplishes the same thing by shifting  the relative centers of the sensor and lens.
--
-- If a parallel binocular stereoscopic camera is modeled, the sensor should be  considered to have shifted by an amount h. If left and right vergence are equal,   h = (focal length * interOcularDistance) / distance to vergence point.
--
-- Vergence is measured in degrees towards center and is usually positive.
--
-- ObjC selector: @- setLeftVergence:@
setLeftVergence :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> CFloat -> IO ()
setLeftVergence mdlStereoscopicCamera  value =
  sendMsg mdlStereoscopicCamera (mkSelector "setLeftVergence:") retVoid [argCFloat (fromIntegral value)]

-- | @- rightVergence@
rightVergence :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> IO CFloat
rightVergence mdlStereoscopicCamera  =
  sendMsg mdlStereoscopicCamera (mkSelector "rightVergence") retCFloat []

-- | @- setRightVergence:@
setRightVergence :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> CFloat -> IO ()
setRightVergence mdlStereoscopicCamera  value =
  sendMsg mdlStereoscopicCamera (mkSelector "setRightVergence:") retVoid [argCFloat (fromIntegral value)]

-- | The amount, as a percentage of image width to offset an image towards the other  camera. This value is used in a stereo grade to enhance or reduce the intensity  of the stereoscopic effect
--
-- ObjC selector: @- overlap@
overlap :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> IO CFloat
overlap mdlStereoscopicCamera  =
  sendMsg mdlStereoscopicCamera (mkSelector "overlap") retCFloat []

-- | The amount, as a percentage of image width to offset an image towards the other  camera. This value is used in a stereo grade to enhance or reduce the intensity  of the stereoscopic effect
--
-- ObjC selector: @- setOverlap:@
setOverlap :: IsMDLStereoscopicCamera mdlStereoscopicCamera => mdlStereoscopicCamera -> CFloat -> IO ()
setOverlap mdlStereoscopicCamera  value =
  sendMsg mdlStereoscopicCamera (mkSelector "setOverlap:") retVoid [argCFloat (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @interPupillaryDistance@
interPupillaryDistanceSelector :: Selector
interPupillaryDistanceSelector = mkSelector "interPupillaryDistance"

-- | @Selector@ for @setInterPupillaryDistance:@
setInterPupillaryDistanceSelector :: Selector
setInterPupillaryDistanceSelector = mkSelector "setInterPupillaryDistance:"

-- | @Selector@ for @leftVergence@
leftVergenceSelector :: Selector
leftVergenceSelector = mkSelector "leftVergence"

-- | @Selector@ for @setLeftVergence:@
setLeftVergenceSelector :: Selector
setLeftVergenceSelector = mkSelector "setLeftVergence:"

-- | @Selector@ for @rightVergence@
rightVergenceSelector :: Selector
rightVergenceSelector = mkSelector "rightVergence"

-- | @Selector@ for @setRightVergence:@
setRightVergenceSelector :: Selector
setRightVergenceSelector = mkSelector "setRightVergence:"

-- | @Selector@ for @overlap@
overlapSelector :: Selector
overlapSelector = mkSelector "overlap"

-- | @Selector@ for @setOverlap:@
setOverlapSelector :: Selector
setOverlapSelector = mkSelector "setOverlap:"

