{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNFaceLandmarks2D
--
-- VNFaceLandmarks2D is the result of a face landmarks 2D request, containing detected facial landmark points organized into VNFaceLandmarkRegion2D regions. The points are accessible as a full list, or as sub-gruops representing pre-defined facial regions.
--
-- Generated bindings for @VNFaceLandmarks2D@.
module ObjC.Vision.VNFaceLandmarks2D
  ( VNFaceLandmarks2D
  , IsVNFaceLandmarks2D(..)
  , allPoints
  , faceContour
  , leftEye
  , rightEye
  , leftEyebrow
  , rightEyebrow
  , nose
  , noseCrest
  , medianLine
  , outerLips
  , innerLips
  , leftPupil
  , rightPupil
  , allPointsSelector
  , faceContourSelector
  , leftEyeSelector
  , rightEyeSelector
  , leftEyebrowSelector
  , rightEyebrowSelector
  , noseSelector
  , noseCrestSelector
  , medianLineSelector
  , outerLipsSelector
  , innerLipsSelector
  , leftPupilSelector
  , rightPupilSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | allPoints the region containing all face landmark points.
--
-- ObjC selector: @- allPoints@
allPoints :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
allPoints vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "allPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | faceContour the region containing the points that describe the face contour from cheek over chin to cheek.
--
-- ObjC selector: @- faceContour@
faceContour :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
faceContour vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "faceContour") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leftEye the region containing the points describing the outline of the left eye.
--
-- ObjC selector: @- leftEye@
leftEye :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
leftEye vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "leftEye") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rightEye the region containing the points describing the outline of the right eye.
--
-- ObjC selector: @- rightEye@
rightEye :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
rightEye vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "rightEye") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leftEyebrow the region containing the points describing the trace of the left eyebrow.
--
-- ObjC selector: @- leftEyebrow@
leftEyebrow :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
leftEyebrow vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "leftEyebrow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rightEyebrow the region containing the points describing the trace of the right eyebrow.
--
-- ObjC selector: @- rightEyebrow@
rightEyebrow :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
rightEyebrow vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "rightEyebrow") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | nose the region containing the points describing the outline of the nose.
--
-- ObjC selector: @- nose@
nose :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
nose vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "nose") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | noseCrest the region containing the points describing the trace of the center crest of the nose.
--
-- ObjC selector: @- noseCrest@
noseCrest :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
noseCrest vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "noseCrest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | medianLine the region containing the points describing the trace of the center line of the face.
--
-- ObjC selector: @- medianLine@
medianLine :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
medianLine vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "medianLine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | outer lips the region containing the points describing the outline of the outside of the lips.
--
-- ObjC selector: @- outerLips@
outerLips :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
outerLips vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "outerLips") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | innerLips the region containing the points describing the outline of the space between the of the lips.
--
-- ObjC selector: @- innerLips@
innerLips :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
innerLips vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "innerLips") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | leftPupil the region containing the point where the left pupil is located.  This value may be inaccurate if the face isBlinking.
--
-- ObjC selector: @- leftPupil@
leftPupil :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
leftPupil vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "leftPupil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | rightPupil the region containing the point where the right pupil is located.  This value may be inaccurate if the face isBlinking.
--
-- ObjC selector: @- rightPupil@
rightPupil :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
rightPupil vnFaceLandmarks2D  =
  sendMsg vnFaceLandmarks2D (mkSelector "rightPupil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allPoints@
allPointsSelector :: Selector
allPointsSelector = mkSelector "allPoints"

-- | @Selector@ for @faceContour@
faceContourSelector :: Selector
faceContourSelector = mkSelector "faceContour"

-- | @Selector@ for @leftEye@
leftEyeSelector :: Selector
leftEyeSelector = mkSelector "leftEye"

-- | @Selector@ for @rightEye@
rightEyeSelector :: Selector
rightEyeSelector = mkSelector "rightEye"

-- | @Selector@ for @leftEyebrow@
leftEyebrowSelector :: Selector
leftEyebrowSelector = mkSelector "leftEyebrow"

-- | @Selector@ for @rightEyebrow@
rightEyebrowSelector :: Selector
rightEyebrowSelector = mkSelector "rightEyebrow"

-- | @Selector@ for @nose@
noseSelector :: Selector
noseSelector = mkSelector "nose"

-- | @Selector@ for @noseCrest@
noseCrestSelector :: Selector
noseCrestSelector = mkSelector "noseCrest"

-- | @Selector@ for @medianLine@
medianLineSelector :: Selector
medianLineSelector = mkSelector "medianLine"

-- | @Selector@ for @outerLips@
outerLipsSelector :: Selector
outerLipsSelector = mkSelector "outerLips"

-- | @Selector@ for @innerLips@
innerLipsSelector :: Selector
innerLipsSelector = mkSelector "innerLips"

-- | @Selector@ for @leftPupil@
leftPupilSelector :: Selector
leftPupilSelector = mkSelector "leftPupil"

-- | @Selector@ for @rightPupil@
rightPupilSelector :: Selector
rightPupilSelector = mkSelector "rightPupil"

