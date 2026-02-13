{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataCatBodyObject
--
-- AVMetadataCatBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected cat body.
--
-- AVMetadataCatBodyObject represents a single detected cat body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected cat body objects. See AVCaptureOutput.h.
--
-- Generated bindings for @AVMetadataCatBodyObject@.
module ObjC.AVFoundation.AVMetadataCatBodyObject
  ( AVMetadataCatBodyObject
  , IsAVMetadataCatBodyObject(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

