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
  , hasLeftEyePositionSelector
  , hasRightEyePositionSelector
  , hasMouthPositionSelector
  , hasTrackingIDSelector
  , trackingIDSelector
  , hasTrackingFrameCountSelector
  , trackingFrameCountSelector
  , hasFaceAngleSelector
  , faceAngleSelector
  , hasSmileSelector
  , leftEyeClosedSelector
  , rightEyeClosedSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A Boolean value that indicates whether the detector found the face’s left eye.
--
-- ObjC selector: @- hasLeftEyePosition@
hasLeftEyePosition :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasLeftEyePosition ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasLeftEyePosition") retCULong []

-- | A Boolean value that indicates whether the detector found the face’s right eye.
--
-- ObjC selector: @- hasRightEyePosition@
hasRightEyePosition :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasRightEyePosition ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasRightEyePosition") retCULong []

-- | A Boolean value that indicates whether the detector found the face’s mouth.
--
-- ObjC selector: @- hasMouthPosition@
hasMouthPosition :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasMouthPosition ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasMouthPosition") retCULong []

-- | A Boolean value that indicates whether the face object has a tracking ID.
--
-- ObjC selector: @- hasTrackingID@
hasTrackingID :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasTrackingID ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasTrackingID") retCULong []

-- | The tracking identifier of the face object.
--
-- Core Image provides a tracking identifier for faces it detects in a video stream, which you can  use to identify when a CIFaceFeature objects detected in one video frame is the same face detected  in a previous video frame.
--
-- This identifier persists only as long as a face is in the frame and is not associated with a specific  face. In other words, if a face moves out of the video frame and comes back into the frame later,  another ID is assigned. (Core Image detects faces, but does not recognize specific faces.)
--
-- ObjC selector: @- trackingID@
trackingID :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO CInt
trackingID ciFaceFeature  =
  sendMsg ciFaceFeature (mkSelector "trackingID") retCInt []

-- | A Boolean value that indicates the face object has a tracking frame count.
--
-- ObjC selector: @- hasTrackingFrameCount@
hasTrackingFrameCount :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasTrackingFrameCount ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasTrackingFrameCount") retCULong []

-- | The tracking frame count of the face.
--
-- ObjC selector: @- trackingFrameCount@
trackingFrameCount :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO CInt
trackingFrameCount ciFaceFeature  =
  sendMsg ciFaceFeature (mkSelector "trackingFrameCount") retCInt []

-- | A Boolean value that indicates whether information about face rotation is available.
--
-- ObjC selector: @- hasFaceAngle@
hasFaceAngle :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasFaceAngle ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasFaceAngle") retCULong []

-- | The rotation of the face.
--
-- Rotation is measured counterclockwise in degrees, with zero indicating that a line drawn between  the eyes is horizontal relative to the image orientation.
--
-- ObjC selector: @- faceAngle@
faceAngle :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO CFloat
faceAngle ciFaceFeature  =
  sendMsg ciFaceFeature (mkSelector "faceAngle") retCFloat []

-- | A Boolean value that indicates whether a smile is detected in the face.
--
-- To detect smiles, ``/CIDetector/featuresInImage:options:`` needs to be called with the ``CIDetectorSmile`` option set to true.
--
-- ObjC selector: @- hasSmile@
hasSmile :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
hasSmile ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "hasSmile") retCULong []

-- | A Boolean value that indicates whether a closed left eye is detected in the face.
--
-- To detect closed eyes, ``/CIDetector/featuresInImage:options:`` needs to be called with the ``CIDetectorEyeBlink`` option set to true.
--
-- ObjC selector: @- leftEyeClosed@
leftEyeClosed :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
leftEyeClosed ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "leftEyeClosed") retCULong []

-- | A Boolean value that indicates whether a closed right eye is detected in the face.
--
-- To detect closed eyes, ``/CIDetector/featuresInImage:options:`` needs to be called with the ``CIDetectorEyeBlink`` option set to true.
--
-- ObjC selector: @- rightEyeClosed@
rightEyeClosed :: IsCIFaceFeature ciFaceFeature => ciFaceFeature -> IO Bool
rightEyeClosed ciFaceFeature  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ciFaceFeature (mkSelector "rightEyeClosed") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @hasLeftEyePosition@
hasLeftEyePositionSelector :: Selector
hasLeftEyePositionSelector = mkSelector "hasLeftEyePosition"

-- | @Selector@ for @hasRightEyePosition@
hasRightEyePositionSelector :: Selector
hasRightEyePositionSelector = mkSelector "hasRightEyePosition"

-- | @Selector@ for @hasMouthPosition@
hasMouthPositionSelector :: Selector
hasMouthPositionSelector = mkSelector "hasMouthPosition"

-- | @Selector@ for @hasTrackingID@
hasTrackingIDSelector :: Selector
hasTrackingIDSelector = mkSelector "hasTrackingID"

-- | @Selector@ for @trackingID@
trackingIDSelector :: Selector
trackingIDSelector = mkSelector "trackingID"

-- | @Selector@ for @hasTrackingFrameCount@
hasTrackingFrameCountSelector :: Selector
hasTrackingFrameCountSelector = mkSelector "hasTrackingFrameCount"

-- | @Selector@ for @trackingFrameCount@
trackingFrameCountSelector :: Selector
trackingFrameCountSelector = mkSelector "trackingFrameCount"

-- | @Selector@ for @hasFaceAngle@
hasFaceAngleSelector :: Selector
hasFaceAngleSelector = mkSelector "hasFaceAngle"

-- | @Selector@ for @faceAngle@
faceAngleSelector :: Selector
faceAngleSelector = mkSelector "faceAngle"

-- | @Selector@ for @hasSmile@
hasSmileSelector :: Selector
hasSmileSelector = mkSelector "hasSmile"

-- | @Selector@ for @leftEyeClosed@
leftEyeClosedSelector :: Selector
leftEyeClosedSelector = mkSelector "leftEyeClosed"

-- | @Selector@ for @rightEyeClosed@
rightEyeClosedSelector :: Selector
rightEyeClosedSelector = mkSelector "rightEyeClosed"

