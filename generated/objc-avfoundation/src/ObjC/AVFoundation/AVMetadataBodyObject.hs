{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMetadataBodyObject
--
-- AVMetadataBodyObject is an abstract class that defines an interface for a body metadata object used by AVFoundation.
--
-- AVMetadataBodyObject represents a single detected body in a picture. It is the base object used to represent bodies, for example AVMetadataHumanBodyObject, AVMetadataCatBodyObject, AVMetadataDogBodyObject.
--
-- Generated bindings for @AVMetadataBodyObject@.
module ObjC.AVFoundation.AVMetadataBodyObject
  ( AVMetadataBodyObject
  , IsAVMetadataBodyObject(..)
  , bodyID
  , bodyIDSelector


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

-- | bodyID
--
-- A unique number associated with the receiver.
--
-- The value of this property is an NSInteger indicating the unique identifier of this body type (Human, Dog, Cat) in the picture. When a new body enters the picture, it is assigned a new unique identifier. bodyIDs are not re-used as bodies leave the picture and new ones enter. Bodies that leave the picture then re-enter are assigned a new bodyID.
--
-- ObjC selector: @- bodyID@
bodyID :: IsAVMetadataBodyObject avMetadataBodyObject => avMetadataBodyObject -> IO CLong
bodyID avMetadataBodyObject  =
  sendMsg avMetadataBodyObject (mkSelector "bodyID") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bodyID@
bodyIDSelector :: Selector
bodyIDSelector = mkSelector "bodyID"

