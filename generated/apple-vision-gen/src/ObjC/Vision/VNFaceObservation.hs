{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VNFaceObservation
--
-- VNDetectedObjectObservation
--
-- VNFaceObservation is the result of a face detection request or derivatives like a face landmark request.
--
-- The properties filled in this obervation depend on the request being performed. For instance, if just a VNDetectFaceRectanglesRequest was performed the landmarks will not be populated. VNFaceObservation are also used as inputs to other request as defined by the VNFaceObservationAccepting protocol. An example would be the VNDetectFaceLandmarksRequest. This can be helpful for instance if the face rectangles in an image are not derived from a VNDetectFaceRectanglesRequest but instead come from other sources like EXIF or other face detectors. In that case the client of the API creates a VNFaceObservation with the boundingBox (in normalized coordinates) that were based on those detected faces.
--
-- Generated bindings for @VNFaceObservation@.
module ObjC.Vision.VNFaceObservation
  ( VNFaceObservation
  , IsVNFaceObservation(..)
  , landmarks
  , faceCaptureQuality
  , roll
  , yaw
  , pitch
  , faceCaptureQualitySelector
  , landmarksSelector
  , pitchSelector
  , rollSelector
  , yawSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The face landmarks populated by the VNDetectFaceLandmarksRequest. This is set to nil if only a VNDetectFaceRectanglesRequest was performed.
--
-- ObjC selector: @- landmarks@
landmarks :: IsVNFaceObservation vnFaceObservation => vnFaceObservation -> IO (Id VNFaceLandmarks2D)
landmarks vnFaceObservation =
  sendMessage vnFaceObservation landmarksSelector

-- | The capture quality of the face as a normalized value between 0.0 and 1.0 that can be used to compare the quality of the face in terms of it capture attributes (lighting, blur, position). This score can be used to compare the capture quality of a face against other captures of the same face in a given set.
--
-- ObjC selector: @- faceCaptureQuality@
faceCaptureQuality :: IsVNFaceObservation vnFaceObservation => vnFaceObservation -> IO (Id NSNumber)
faceCaptureQuality vnFaceObservation =
  sendMessage vnFaceObservation faceCaptureQualitySelector

-- | Face roll angle populated by VNDetectFaceRectanglesRequest. The roll is reported in radians, positive angle corresponds to counterclockwise direction, range [-Pi, Pi). nil value indicates that the roll angle hasn't been computed
--
-- ObjC selector: @- roll@
roll :: IsVNFaceObservation vnFaceObservation => vnFaceObservation -> IO (Id NSNumber)
roll vnFaceObservation =
  sendMessage vnFaceObservation rollSelector

-- | Face yaw angle populated by VNDetectFaceRectanglesRequest. The yaw is reported in radians, positive angle corresponds to counterclockwise direction, range [-Pi/2, Pi/2]. nil value indicates that the yaw angle hasn't been computed
--
-- ObjC selector: @- yaw@
yaw :: IsVNFaceObservation vnFaceObservation => vnFaceObservation -> IO (Id NSNumber)
yaw vnFaceObservation =
  sendMessage vnFaceObservation yawSelector

-- | Face pitch angle populated by VNDetectFaceRectanglesRequest. The pitch is reported in radians, positive angle corresponds to nodding head down direction, range [-Pi/2, Pi/2]. nil value indicates that the pitch angle hasn't been computed
--
-- ObjC selector: @- pitch@
pitch :: IsVNFaceObservation vnFaceObservation => vnFaceObservation -> IO (Id NSNumber)
pitch vnFaceObservation =
  sendMessage vnFaceObservation pitchSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @landmarks@
landmarksSelector :: Selector '[] (Id VNFaceLandmarks2D)
landmarksSelector = mkSelector "landmarks"

-- | @Selector@ for @faceCaptureQuality@
faceCaptureQualitySelector :: Selector '[] (Id NSNumber)
faceCaptureQualitySelector = mkSelector "faceCaptureQuality"

-- | @Selector@ for @roll@
rollSelector :: Selector '[] (Id NSNumber)
rollSelector = mkSelector "roll"

-- | @Selector@ for @yaw@
yawSelector :: Selector '[] (Id NSNumber)
yawSelector = mkSelector "yaw"

-- | @Selector@ for @pitch@
pitchSelector :: Selector '[] (Id NSNumber)
pitchSelector = mkSelector "pitch"

