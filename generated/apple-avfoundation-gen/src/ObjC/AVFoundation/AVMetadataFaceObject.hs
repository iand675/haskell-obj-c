{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataFaceObject
--
-- AVMetadataFaceObject is a concrete subclass of AVMetadataObject defining the features of a detected face.
--
-- AVMetadataFaceObject represents a single detected face in a picture. It is an immutable object describing the various features found in the face.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected face objects. See AVCaptureOutput.h.
--
-- Generated bindings for @AVMetadataFaceObject@.
module ObjC.AVFoundation.AVMetadataFaceObject
  ( AVMetadataFaceObject
  , IsAVMetadataFaceObject(..)
  , faceID
  , hasRollAngle
  , rollAngle
  , hasYawAngle
  , yawAngle
  , faceIDSelector
  , hasRollAngleSelector
  , hasYawAngleSelector
  , rollAngleSelector
  , yawAngleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | faceID
--
-- A unique number associated with the receiver.
--
-- The value of this property is an NSInteger indicating the unique identifier of this face in the picture. When a new face enters the picture, it is assigned a new unique identifier. faceIDs are not re-used as faces leave the picture and new ones enter. Faces that leave the picture then re-enter are assigned a new faceID.
--
-- ObjC selector: @- faceID@
faceID :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO CLong
faceID avMetadataFaceObject =
  sendMessage avMetadataFaceObject faceIDSelector

-- | hasRollAngle
--
-- A BOOL indicating whether the rollAngle property is valid for this receiver.
--
-- ObjC selector: @- hasRollAngle@
hasRollAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO Bool
hasRollAngle avMetadataFaceObject =
  sendMessage avMetadataFaceObject hasRollAngleSelector

-- | rollAngle
--
-- The roll angle of the face in degrees.
--
-- The value of this property is a CGFloat indicating the face's angle of roll (or tilt) in degrees. A value of 0.0 indicates that the face is level in the picture. If -hasRollAngle returns NO, then reading this property throws an NSGenericException.
--
-- ObjC selector: @- rollAngle@
rollAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO CDouble
rollAngle avMetadataFaceObject =
  sendMessage avMetadataFaceObject rollAngleSelector

-- | hasYawAngle
--
-- A BOOL indicating whether the yawAngle property is valid for this receiver.
--
-- ObjC selector: @- hasYawAngle@
hasYawAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO Bool
hasYawAngle avMetadataFaceObject =
  sendMessage avMetadataFaceObject hasYawAngleSelector

-- | yawAngle
--
-- The yaw angle of the face in degrees.
--
-- The value of this property is a CGFloat indicating the face's angle of yaw (or turn) in degrees. A value of 0.0 indicates that the face is straight on in the picture. If -hasYawAngle returns NO, then reading this property throws an NSGenericException.
--
-- ObjC selector: @- yawAngle@
yawAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO CDouble
yawAngle avMetadataFaceObject =
  sendMessage avMetadataFaceObject yawAngleSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @faceID@
faceIDSelector :: Selector '[] CLong
faceIDSelector = mkSelector "faceID"

-- | @Selector@ for @hasRollAngle@
hasRollAngleSelector :: Selector '[] Bool
hasRollAngleSelector = mkSelector "hasRollAngle"

-- | @Selector@ for @rollAngle@
rollAngleSelector :: Selector '[] CDouble
rollAngleSelector = mkSelector "rollAngle"

-- | @Selector@ for @hasYawAngle@
hasYawAngleSelector :: Selector '[] Bool
hasYawAngleSelector = mkSelector "hasYawAngle"

-- | @Selector@ for @yawAngle@
yawAngleSelector :: Selector '[] CDouble
yawAngleSelector = mkSelector "yawAngle"

