{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information about a face detected in a still or video image.
--
-- > Note: In macOS 10.13, iOS 11, and tvOS 11 or later, the Vision framework replaces this  class for identifying and analyzing image features. See @VNDetectFaceRectanglesRequest@. See <doc://com.apple.documentation/documentation/vision/vndetectfacerectanglesrequest>)
--
-- The properties of a @CIFaceFeature@ object provide information about the face’s eyes and mouth.  A face object in a video can also have properties that track its location over time, tracking ID and frame count.
--
-- Generated bindings for @CIFaceFeature@.
module ObjC.CoreImage.CIFaceFeature
  ( CIFaceFeature
  , IsCIFaceFeature(..)
  , hasLeftEyePosition
  , hasRightEyePosition
  , hasMouthPosition
  , hasTrackingID
  , trackingID
  , hasTrackingFrameCount
  , trackingFrameCount
  , hasFaceAngle
  , faceAngle
  , hasSmile
  , leftEyeClosed
  , rightEyeClosed
  , faceAngleSelector
  , hasFaceAngleSelector
  , hasLeftEyePositionSelector
  , hasMouthPositionSelector
  , hasRightEyePositionSelector
  , hasSmileSelector
  , hasTrackingFrameCountSelector
  , hasTrackingIDSelector
  , leftEyeClosedSelector
  , rightEyeClosedSelector
  , trackingFrameCountSelector
  , trackingIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value that indicates whether the detector found the face’s left eye.
--
-- ObjC selector: @- hasLeftEyePosition@
hasLeftEyePosition :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasLeftEyePosition ciFaceFeature =
  sendMessage ciFaceFeature hasLeftEyePositionSelector

-- | A Boolean value that indicates whether the detector found the face’s right eye.
--
-- ObjC selector: @- hasRightEyePosition@
hasRightEyePosition :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasRightEyePosition ciFaceFeature =
  sendMessage ciFaceFeature hasRightEyePositionSelector

-- | A Boolean value that indicates whether the detector found the face’s mouth.
--
-- ObjC selector: @- hasMouthPosition@
hasMouthPosition :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasMouthPosition ciFaceFeature =
  sendMessage ciFaceFeature hasMouthPositionSelector

-- | A Boolean value that indicates whether the face object has a tracking ID.
--
-- ObjC selector: @- hasTrackingID@
hasTrackingID :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasTrackingID ciFaceFeature =
  sendMessage ciFaceFeature hasTrackingIDSelector

-- | The tracking identifier of the face object.
--
-- Core Image provides a tracking identifier for faces it detects in a video stream, which you can  use to identify when a CIFaceFeature objects detected in one video frame is the same face detected  in a previous video frame.
--
-- This identifier persists only as long as a face is in the frame and is not associated with a specific  face. In other words, if a face moves out of the video frame and comes back into the frame later,  another ID is assigned. (Core Image detects faces, but does not recognize specific faces.)
--
-- ObjC selector: @- trackingID@
trackingID :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO CInt
trackingID ciFaceFeature =
  sendMessage ciFaceFeature trackingIDSelector

-- | A Boolean value that indicates the face object has a tracking frame count.
--
-- ObjC selector: @- hasTrackingFrameCount@
hasTrackingFrameCount :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasTrackingFrameCount ciFaceFeature =
  sendMessage ciFaceFeature hasTrackingFrameCountSelector

-- | The tracking frame count of the face.
--
-- ObjC selector: @- trackingFrameCount@
trackingFrameCount :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO CInt
trackingFrameCount ciFaceFeature =
  sendMessage ciFaceFeature trackingFrameCountSelector

-- | A Boolean value that indicates whether information about face rotation is available.
--
-- ObjC selector: @- hasFaceAngle@
hasFaceAngle :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasFaceAngle ciFaceFeature =
  sendMessage ciFaceFeature hasFaceAngleSelector

-- | The rotation of the face.
--
-- Rotation is measured counterclockwise in degrees, with zero indicating that a line drawn between  the eyes is horizontal relative to the image orientation.
--
-- ObjC selector: @- faceAngle@
faceAngle :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO CFloat
faceAngle ciFaceFeature =
  sendMessage ciFaceFeature faceAngleSelector

-- | A Boolean value that indicates whether a smile is detected in the face.
--
-- To detect smiles, ``/CIDetector/featuresInImage:options:`` needs to be called with the ``CIDetectorSmile`` option set to true.
--
-- ObjC selector: @- hasSmile@
hasSmile :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasSmile ciFaceFeature =
  sendMessage ciFaceFeature hasSmileSelector

-- | A Boolean value that indicates whether a closed left eye is detected in the face.
--
-- To detect closed eyes, ``/CIDetector/featuresInImage:options:`` needs to be called with the ``CIDetectorEyeBlink`` option set to true.
--
-- ObjC selector: @- leftEyeClosed@
leftEyeClosed :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
leftEyeClosed ciFaceFeature =
  sendMessage ciFaceFeature leftEyeClosedSelector

-- | A Boolean value that indicates whether a closed right eye is detected in the face.
--
-- To detect closed eyes, ``/CIDetector/featuresInImage:options:`` needs to be called with the ``CIDetectorEyeBlink`` option set to true.
--
-- ObjC selector: @- rightEyeClosed@
rightEyeClosed :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
rightEyeClosed ciFaceFeature =
  sendMessage ciFaceFeature rightEyeClosedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasLeftEyePosition@
hasLeftEyePositionSelector :: Selector '[] Bool
hasLeftEyePositionSelector = mkSelector "hasLeftEyePosition"

-- | @Selector@ for @hasRightEyePosition@
hasRightEyePositionSelector :: Selector '[] Bool
hasRightEyePositionSelector = mkSelector "hasRightEyePosition"

-- | @Selector@ for @hasMouthPosition@
hasMouthPositionSelector :: Selector '[] Bool
hasMouthPositionSelector = mkSelector "hasMouthPosition"

-- | @Selector@ for @hasTrackingID@
hasTrackingIDSelector :: Selector '[] Bool
hasTrackingIDSelector = mkSelector "hasTrackingID"

-- | @Selector@ for @trackingID@
trackingIDSelector :: Selector '[] CInt
trackingIDSelector = mkSelector "trackingID"

-- | @Selector@ for @hasTrackingFrameCount@
hasTrackingFrameCountSelector :: Selector '[] Bool
hasTrackingFrameCountSelector = mkSelector "hasTrackingFrameCount"

-- | @Selector@ for @trackingFrameCount@
trackingFrameCountSelector :: Selector '[] CInt
trackingFrameCountSelector = mkSelector "trackingFrameCount"

-- | @Selector@ for @hasFaceAngle@
hasFaceAngleSelector :: Selector '[] Bool
hasFaceAngleSelector = mkSelector "hasFaceAngle"

-- | @Selector@ for @faceAngle@
faceAngleSelector :: Selector '[] CFloat
faceAngleSelector = mkSelector "faceAngle"

-- | @Selector@ for @hasSmile@
hasSmileSelector :: Selector '[] Bool
hasSmileSelector = mkSelector "hasSmile"

-- | @Selector@ for @leftEyeClosed@
leftEyeClosedSelector :: Selector '[] Bool
leftEyeClosedSelector = mkSelector "leftEyeClosed"

-- | @Selector@ for @rightEyeClosed@
rightEyeClosedSelector :: Selector '[] Bool
rightEyeClosedSelector = mkSelector "rightEyeClosed"

