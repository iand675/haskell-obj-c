{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataDogBodyObject
--
-- AVMetadataDogBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected dog body.
--
-- AVMetadataDogBodyObject represents a single detected dog body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected dog body objects. See AVCaptureOutput.h.
--
-- Generated bindings for @AVMetadataDogBodyObject@.
module ObjC.AVFoundation.AVMetadataDogBodyObject
  ( AVMetadataDogBodyObject
  , IsAVMetadataDogBodyObject(..)


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

