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
  , rollAngleSelector
  , hasYawAngleSelector
  , yawAngleSelector


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
faceID avMetadataFaceObject  =
  sendMsg avMetadataFaceObject (mkSelector "faceID") retCLong []

-- | hasRollAngle
--
-- A BOOL indicating whether the rollAngle property is valid for this receiver.
--
-- ObjC selector: @- hasRollAngle@
hasRollAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO Bool
hasRollAngle avMetadataFaceObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetadataFaceObject (mkSelector "hasRollAngle") retCULong []

-- | rollAngle
--
-- The roll angle of the face in degrees.
--
-- The value of this property is a CGFloat indicating the face's angle of roll (or tilt) in degrees. A value of 0.0 indicates that the face is level in the picture. If -hasRollAngle returns NO, then reading this property throws an NSGenericException.
--
-- ObjC selector: @- rollAngle@
rollAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO CDouble
rollAngle avMetadataFaceObject  =
  sendMsg avMetadataFaceObject (mkSelector "rollAngle") retCDouble []

-- | hasYawAngle
--
-- A BOOL indicating whether the yawAngle property is valid for this receiver.
--
-- ObjC selector: @- hasYawAngle@
hasYawAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO Bool
hasYawAngle avMetadataFaceObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avMetadataFaceObject (mkSelector "hasYawAngle") retCULong []

-- | yawAngle
--
-- The yaw angle of the face in degrees.
--
-- The value of this property is a CGFloat indicating the face's angle of yaw (or turn) in degrees. A value of 0.0 indicates that the face is straight on in the picture. If -hasYawAngle returns NO, then reading this property throws an NSGenericException.
--
-- ObjC selector: @- yawAngle@
yawAngle :: IsAVMetadataFaceObject avMetadataFaceObject => avMetadataFaceObject -> IO CDouble
yawAngle avMetadataFaceObject  =
  sendMsg avMetadataFaceObject (mkSelector "yawAngle") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @faceID@
faceIDSelector :: Selector
faceIDSelector = mkSelector "faceID"

-- | @Selector@ for @hasRollAngle@
hasRollAngleSelector :: Selector
hasRollAngleSelector = mkSelector "hasRollAngle"

-- | @Selector@ for @rollAngle@
rollAngleSelector :: Selector
rollAngleSelector = mkSelector "rollAngle"

-- | @Selector@ for @hasYawAngle@
hasYawAngleSelector :: Selector
hasYawAngleSelector = mkSelector "hasYawAngle"

-- | @Selector@ for @yawAngle@
yawAngleSelector :: Selector
yawAngleSelector = mkSelector "yawAngle"

