{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataHumanBodyObject
--
-- AVMetadataHumanBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected human body.
--
-- AVMetadataHumanBodyObject represents a single detected human body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected human body objects. See AVCaptureOutput.h.
--
-- Generated bindings for @AVMetadataHumanBodyObject@.
module ObjC.AVFoundation.AVMetadataHumanBodyObject
  ( AVMetadataHumanBodyObject
  , IsAVMetadataHumanBodyObject(..)


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

