{-# LANGUAGE DataKinds #-}
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
  , innerLipsSelector
  , leftEyeSelector
  , leftEyebrowSelector
  , leftPupilSelector
  , medianLineSelector
  , noseCrestSelector
  , noseSelector
  , outerLipsSelector
  , rightEyeSelector
  , rightEyebrowSelector
  , rightPupilSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | allPoints the region containing all face landmark points.
--
-- ObjC selector: @- allPoints@
allPoints :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
allPoints vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D allPointsSelector

-- | faceContour the region containing the points that describe the face contour from cheek over chin to cheek.
--
-- ObjC selector: @- faceContour@
faceContour :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
faceContour vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D faceContourSelector

-- | leftEye the region containing the points describing the outline of the left eye.
--
-- ObjC selector: @- leftEye@
leftEye :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
leftEye vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D leftEyeSelector

-- | rightEye the region containing the points describing the outline of the right eye.
--
-- ObjC selector: @- rightEye@
rightEye :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
rightEye vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D rightEyeSelector

-- | leftEyebrow the region containing the points describing the trace of the left eyebrow.
--
-- ObjC selector: @- leftEyebrow@
leftEyebrow :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
leftEyebrow vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D leftEyebrowSelector

-- | rightEyebrow the region containing the points describing the trace of the right eyebrow.
--
-- ObjC selector: @- rightEyebrow@
rightEyebrow :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
rightEyebrow vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D rightEyebrowSelector

-- | nose the region containing the points describing the outline of the nose.
--
-- ObjC selector: @- nose@
nose :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
nose vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D noseSelector

-- | noseCrest the region containing the points describing the trace of the center crest of the nose.
--
-- ObjC selector: @- noseCrest@
noseCrest :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
noseCrest vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D noseCrestSelector

-- | medianLine the region containing the points describing the trace of the center line of the face.
--
-- ObjC selector: @- medianLine@
medianLine :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
medianLine vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D medianLineSelector

-- | outer lips the region containing the points describing the outline of the outside of the lips.
--
-- ObjC selector: @- outerLips@
outerLips :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
outerLips vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D outerLipsSelector

-- | innerLips the region containing the points describing the outline of the space between the of the lips.
--
-- ObjC selector: @- innerLips@
innerLips :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
innerLips vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D innerLipsSelector

-- | leftPupil the region containing the point where the left pupil is located.  This value may be inaccurate if the face isBlinking.
--
-- ObjC selector: @- leftPupil@
leftPupil :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
leftPupil vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D leftPupilSelector

-- | rightPupil the region containing the point where the right pupil is located.  This value may be inaccurate if the face isBlinking.
--
-- ObjC selector: @- rightPupil@
rightPupil :: IsVNFaceLandmarks2D vnFaceLandmarks2D => vnFaceLandmarks2D -> IO (Id VNFaceLandmarkRegion2D)
rightPupil vnFaceLandmarks2D =
  sendMessage vnFaceLandmarks2D rightPupilSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @allPoints@
allPointsSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
allPointsSelector = mkSelector "allPoints"

-- | @Selector@ for @faceContour@
faceContourSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
faceContourSelector = mkSelector "faceContour"

-- | @Selector@ for @leftEye@
leftEyeSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
leftEyeSelector = mkSelector "leftEye"

-- | @Selector@ for @rightEye@
rightEyeSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
rightEyeSelector = mkSelector "rightEye"

-- | @Selector@ for @leftEyebrow@
leftEyebrowSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
leftEyebrowSelector = mkSelector "leftEyebrow"

-- | @Selector@ for @rightEyebrow@
rightEyebrowSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
rightEyebrowSelector = mkSelector "rightEyebrow"

-- | @Selector@ for @nose@
noseSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
noseSelector = mkSelector "nose"

-- | @Selector@ for @noseCrest@
noseCrestSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
noseCrestSelector = mkSelector "noseCrest"

-- | @Selector@ for @medianLine@
medianLineSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
medianLineSelector = mkSelector "medianLine"

-- | @Selector@ for @outerLips@
outerLipsSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
outerLipsSelector = mkSelector "outerLips"

-- | @Selector@ for @innerLips@
innerLipsSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
innerLipsSelector = mkSelector "innerLips"

-- | @Selector@ for @leftPupil@
leftPupilSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
leftPupilSelector = mkSelector "leftPupil"

-- | @Selector@ for @rightPupil@
rightPupilSelector :: Selector '[] (Id VNFaceLandmarkRegion2D)
rightPupilSelector = mkSelector "rightPupil"

