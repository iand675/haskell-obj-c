{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataObject
--
-- AVMetadataObject is an abstract class that defines an interface for a metadata object used by AVFoundation.
--
-- AVMetadataObject provides an abstract interface for metadata associated with a piece of media. One example is face metadata that might be detected in a picture. All metadata objects have a time, duration, bounds, and type.
--
-- The concrete AVMetadataFaceObject is used by AVCaptureMetadataOutput for face detection.
--
-- Generated bindings for @AVMetadataObject@.
module ObjC.AVFoundation.AVMetadataObject
  ( AVMetadataObject
  , IsAVMetadataObject(..)
  , init_
  , new
  , type_
  , groupID
  , objectID
  , cinematicVideoFocusMode
  , fixedFocus
  , cinematicVideoFocusModeSelector
  , fixedFocusSelector
  , groupIDSelector
  , initSelector
  , newSelector
  , objectIDSelector
  , typeSelector

  -- * Enum types
  , AVCaptureCinematicVideoFocusMode(AVCaptureCinematicVideoFocusMode)
  , pattern AVCaptureCinematicVideoFocusModeNone
  , pattern AVCaptureCinematicVideoFocusModeStrong
  , pattern AVCaptureCinematicVideoFocusModeWeak

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVMetadataObject avMetadataObject => avMetadataObject -> IO (Id AVMetadataObject)
init_ avMetadataObject =
  sendOwnedMessage avMetadataObject initSelector

-- | @+ new@
new :: IO (Id AVMetadataObject)
new  =
  do
    cls' <- getRequiredClass "AVMetadataObject"
    sendOwnedClassMessage cls' newSelector

-- | type
--
-- An identifier for the metadata object.
--
-- The value of this property is an AVMetadataObjectType representing the type of the metadata object. Clients inspecting a collection of metadata objects can use this property to filter objects with a matching type.
--
-- ObjC selector: @- type@
type_ :: IsAVMetadataObject avMetadataObject => avMetadataObject -> IO (Id NSString)
type_ avMetadataObject =
  sendMessage avMetadataObject typeSelector

-- | An identifier associated with a metadata object used to group it with other metadata objects belonging to a common parent.
--
-- When presented with a collection of ``AVMetadataObject`` instances of different types, you may use the objects' ``groupID`` to combine them into groups. For example, a human body and face belonging to the same person have the same ``groupID``.  If an object's ``groupID`` property is set to -1, it is invalid. When set to a value of >=0, it is unique across all object groups.
--
-- ObjC selector: @- groupID@
groupID :: IsAVMetadataObject avMetadataObject => avMetadataObject -> IO CLong
groupID avMetadataObject =
  sendMessage avMetadataObject groupIDSelector

-- | A unique identifier for each detected object type (face, body, hands, heads and salient objects) in a collection.
--
-- Defaults to a value of -1 when invalid or not available. When used in conjunction with an ``AVCaptureMetadataOutput``, each newly detected object that enters the scene is assigned a unique identifier. ``objectID``s are never re-used as objects leave the picture and new ones enter. Objects that leave the picture and then re-enter are assigned a new ``objectID``.
--
-- ObjC selector: @- objectID@
objectID :: IsAVMetadataObject avMetadataObject => avMetadataObject -> IO CLong
objectID avMetadataObject =
  sendMessage avMetadataObject objectIDSelector

-- | The current focus mode when an object is detected during a Cinematic Video recording.
--
-- Default is ``AVCaptureCinematicVideoFocusMode/AVCaptureCinematicVideoFocusModeNone``.
--
-- ObjC selector: @- cinematicVideoFocusMode@
cinematicVideoFocusMode :: IsAVMetadataObject avMetadataObject => avMetadataObject -> IO AVCaptureCinematicVideoFocusMode
cinematicVideoFocusMode avMetadataObject =
  sendMessage avMetadataObject cinematicVideoFocusModeSelector

-- | A BOOL indicating whether this metadata object represents a fixed focus.
--
-- ObjC selector: @- fixedFocus@
fixedFocus :: IsAVMetadataObject avMetadataObject => avMetadataObject -> IO Bool
fixedFocus avMetadataObject =
  sendMessage avMetadataObject fixedFocusSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMetadataObject)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMetadataObject)
newSelector = mkSelector "new"

-- | @Selector@ for @type@
typeSelector :: Selector '[] (Id NSString)
typeSelector = mkSelector "type"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] CLong
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector '[] CLong
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @cinematicVideoFocusMode@
cinematicVideoFocusModeSelector :: Selector '[] AVCaptureCinematicVideoFocusMode
cinematicVideoFocusModeSelector = mkSelector "cinematicVideoFocusMode"

-- | @Selector@ for @fixedFocus@
fixedFocusSelector :: Selector '[] Bool
fixedFocusSelector = mkSelector "fixedFocus"

