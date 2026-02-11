{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a timecode source that a timecode generator can synchronize to.
--
-- @AVCaptureTimecodeSource@ provides information about a specific timecode source available for synchronization in @AVCaptureTimecodeGenerator@. It includes metadata such as the source’s name, type, and unique identifier.
--
-- Generated bindings for @AVCaptureTimecodeSource@.
module ObjC.AVFoundation.AVCaptureTimecodeSource
  ( AVCaptureTimecodeSource
  , IsAVCaptureTimecodeSource(..)
  , displayName
  , type_
  , uuid
  , displayNameSelector
  , typeSelector
  , uuidSelector

  -- * Enum types
  , AVCaptureTimecodeSourceType(AVCaptureTimecodeSourceType)
  , pattern AVCaptureTimecodeSourceTypeFrameCount
  , pattern AVCaptureTimecodeSourceTypeRealTimeClock
  , pattern AVCaptureTimecodeSourceTypeExternal

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
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The name of the timecode source.
--
-- This property provides a descriptive name of the timecode source, useful for display in user interfaces or logging.
--
-- ObjC selector: @- displayName@
displayName :: IsAVCaptureTimecodeSource avCaptureTimecodeSource => avCaptureTimecodeSource -> IO (Id NSString)
displayName avCaptureTimecodeSource  =
  sendMsg avCaptureTimecodeSource (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The type of timecode source.
--
-- Indicates the type of timecode source, represented as a value from the ``AVCaptureTimecodeSynchronizationSourceType`` enum. This helps you identify the source for specific synchronization use cases, such as frame counter, real-time clock, MIDI, or HID.
--
-- ObjC selector: @- type@
type_ :: IsAVCaptureTimecodeSource avCaptureTimecodeSource => avCaptureTimecodeSource -> IO AVCaptureTimecodeSourceType
type_ avCaptureTimecodeSource  =
  fmap (coerce :: CLong -> AVCaptureTimecodeSourceType) $ sendMsg avCaptureTimecodeSource (mkSelector "type") retCLong []

-- | A unique identifier for the timecode source.
--
-- The UUID uniquely identifies this timecode source. It is particularly useful when multiple sources of the same type are available, allowing your application to distinguish between them.
--
-- - Note: This value does not persist across application sessions.
--
-- ObjC selector: @- uuid@
uuid :: IsAVCaptureTimecodeSource avCaptureTimecodeSource => avCaptureTimecodeSource -> IO (Id NSUUID)
uuid avCaptureTimecodeSource  =
  sendMsg avCaptureTimecodeSource (mkSelector "uuid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @uuid@
uuidSelector :: Selector
uuidSelector = mkSelector "uuid"

