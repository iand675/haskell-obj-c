{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIMetaEvent
--
-- The event class representing MIDI Meta-Event messages.
--
-- The size and contents of an AVMIDIMetaEvent cannot be modified once created.
--
-- Events with AVMIDIMetaEventType AVMIDIMetaEventTypeTempo, AVMIDIMetaEventTypeSmpteOffset,		or AVMIDIMetaEventTypeTimeSignature can only be added to a sequence's tempo track.
--
-- The class does not verify that the content matches the MIDI specification.
--
-- Generated bindings for @AVMIDIMetaEvent@.
module ObjC.AVFAudio.AVMIDIMetaEvent
  ( AVMIDIMetaEvent
  , IsAVMIDIMetaEvent(..)
  , initWithType_data
  , type_
  , initWithType_dataSelector
  , typeSelector

  -- * Enum types
  , AVMIDIMetaEventType(AVMIDIMetaEventType)
  , pattern AVMIDIMetaEventTypeSequenceNumber
  , pattern AVMIDIMetaEventTypeText
  , pattern AVMIDIMetaEventTypeCopyright
  , pattern AVMIDIMetaEventTypeTrackName
  , pattern AVMIDIMetaEventTypeInstrument
  , pattern AVMIDIMetaEventTypeLyric
  , pattern AVMIDIMetaEventTypeMarker
  , pattern AVMIDIMetaEventTypeCuePoint
  , pattern AVMIDIMetaEventTypeMidiChannel
  , pattern AVMIDIMetaEventTypeMidiPort
  , pattern AVMIDIMetaEventTypeEndOfTrack
  , pattern AVMIDIMetaEventTypeTempo
  , pattern AVMIDIMetaEventTypeSmpteOffset
  , pattern AVMIDIMetaEventTypeTimeSignature
  , pattern AVMIDIMetaEventTypeKeySignature
  , pattern AVMIDIMetaEventTypeProprietaryEvent

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

import ObjC.AVFAudio.Internal.Classes
import ObjC.AVFAudio.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | initWithType:data
--
-- Initialize the event with a MIDI Meta-Event type and an NSData.
--
-- @type@ — A AVMIDIMetaEventType indicating which type of Meta-Event.
--
-- @data@ — An NSData object containing the raw contents of the Meta-Event.
--
-- ObjC selector: @- initWithType:data:@
initWithType_data :: (IsAVMIDIMetaEvent avmidiMetaEvent, IsNSData data_) => avmidiMetaEvent -> AVMIDIMetaEventType -> data_ -> IO (Id AVMIDIMetaEvent)
initWithType_data avmidiMetaEvent  type_ data_ =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg avmidiMetaEvent (mkSelector "initWithType:data:") (retPtr retVoid) [argCLong (coerce type_), argPtr (castPtr raw_data_ :: Ptr ())] >>= ownedObject . castPtr

-- | type
--
-- The type of Meta-Event, specified as an AVMIDIMetaEventType.
--
-- ObjC selector: @- type@
type_ :: IsAVMIDIMetaEvent avmidiMetaEvent => avmidiMetaEvent -> IO AVMIDIMetaEventType
type_ avmidiMetaEvent  =
  fmap (coerce :: CLong -> AVMIDIMetaEventType) $ sendMsg avmidiMetaEvent (mkSelector "type") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:data:@
initWithType_dataSelector :: Selector
initWithType_dataSelector = mkSelector "initWithType:data:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

