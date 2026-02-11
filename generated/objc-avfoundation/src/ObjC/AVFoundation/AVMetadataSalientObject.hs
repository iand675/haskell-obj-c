{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataSalientObject
--
-- AVMetadataSalientObject is a concrete subclass of AVMetadataObject defining the features of a salient object.
--
-- AVMetadataSalientObject represents a single detected salient area in a picture. It is an immutable object describing the salient object.
--
-- On supported platforms, AVCaptureMetadataOutput outputs arrays of detected salient objects. See AVCaptureOutput.h.
--
-- Generated bindings for @AVMetadataSalientObject@.
module ObjC.AVFoundation.AVMetadataSalientObject
  ( AVMetadataSalientObject
  , IsAVMetadataSalientObject(..)
  , objectID
  , objectIDSelector


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

-- | objectID
--
-- A unique number associated with the receiver.
--
-- The value of this property is an NSInteger indicating the unique identifier of this object in the picture. When a new object enters the picture, it is assigned a new unique identifier. objectIDs are not re-used as object leave the picture and new ones enter. Objects that leave the picture then re-enter are assigned a new objectID.
--
-- ObjC selector: @- objectID@
objectID :: IsAVMetadataSalientObject avMetadataSalientObject => avMetadataSalientObject -> IO CLong
objectID avMetadataSalientObject  =
  sendMsg avMetadataSalientObject (mkSelector "objectID") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @objectID@
objectIDSelector :: Selector
objectIDSelector = mkSelector "objectID"

