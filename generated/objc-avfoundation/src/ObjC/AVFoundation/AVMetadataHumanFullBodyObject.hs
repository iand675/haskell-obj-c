{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataHumanFullBodyObject
--
-- AVMetadataHumanFullBodyObject is a concrete subclass of AVMetadataBodyObject defining a detected human full body.
--
-- AVMetadataHumanFullBodyObject represents a single detected human full body in a picture. It is an immutable object describing the various features found in the body.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected human full body objects. See AVCaptureOutput.h.
--
-- Generated bindings for @AVMetadataHumanFullBodyObject@.
module ObjC.AVFoundation.AVMetadataHumanFullBodyObject
  ( AVMetadataHumanFullBodyObject
  , IsAVMetadataHumanFullBodyObject(..)


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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

