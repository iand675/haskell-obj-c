{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
initWithType_data avmidiMetaEvent type_ data_ =
  sendOwnedMessage avmidiMetaEvent initWithType_dataSelector type_ (toNSData data_)

-- | type
--
-- The type of Meta-Event, specified as an AVMIDIMetaEventType.
--
-- ObjC selector: @- type@
type_ :: IsAVMIDIMetaEvent avmidiMetaEvent => avmidiMetaEvent -> IO AVMIDIMetaEventType
type_ avmidiMetaEvent =
  sendMessage avmidiMetaEvent typeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithType:data:@
initWithType_dataSelector :: Selector '[AVMIDIMetaEventType, Id NSData] (Id AVMIDIMetaEvent)
initWithType_dataSelector = mkSelector "initWithType:data:"

-- | @Selector@ for @type@
typeSelector :: Selector '[] AVMIDIMetaEventType
typeSelector = mkSelector "type"

