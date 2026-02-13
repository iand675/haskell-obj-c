{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVExtendedNoteOnEvent
--
-- The event class representing a custom extension of a MIDI note-on.
--
-- Using an AVExtendedNoteOnEvent allows an application to trigger a specialized note-on event on one of several		Apple audio units which support it.  The floating point note and velocity numbers allow optional fractional control		of the note's run-time properties which are modulated by those inputs.  In addition, it supports the possibility		of an audio unit with more than the standard 16 MIDI channels.
--
-- Generated bindings for @AVExtendedNoteOnEvent@.
module ObjC.AVFAudio.AVExtendedNoteOnEvent
  ( AVExtendedNoteOnEvent
  , IsAVExtendedNoteOnEvent(..)
  , initWithMIDINote_velocity_groupID_duration
  , initWithMIDINote_velocity_instrumentID_groupID_duration
  , midiNote
  , setMidiNote
  , velocity
  , setVelocity
  , instrumentID
  , setInstrumentID
  , groupID
  , setGroupID
  , duration
  , setDuration
  , durationSelector
  , groupIDSelector
  , initWithMIDINote_velocity_groupID_durationSelector
  , initWithMIDINote_velocity_instrumentID_groupID_durationSelector
  , instrumentIDSelector
  , midiNoteSelector
  , setDurationSelector
  , setGroupIDSelector
  , setInstrumentIDSelector
  , setMidiNoteSelector
  , setVelocitySelector
  , velocitySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithMIDINote:velocity:groupID:duration
--
-- Initialize the event with a midi note, velocity, instrument and group ID, and a duration.
--
-- @midiNote@ — The MIDI velocity represented as a floating point.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- @velocity@ — The MIDI velocity represented as a floating point.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- @groupID@ — An index indicating the AudioUnitElement within the Group Scope which should handle this event (see AudioUnitElement).		This normally maps to a channel within the audio unit.		Range: normally between 0 and 15, but may be higher if the AVMusicTrack's destinationAudioUnit supports more channels.
--
-- @duration@ — The duration of this event in AVMusicTimeStamp beats.  Range:  Any nonnegative number.
--
-- ObjC selector: @- initWithMIDINote:velocity:groupID:duration:@
initWithMIDINote_velocity_groupID_duration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> CFloat -> CUInt -> CDouble -> IO (Id AVExtendedNoteOnEvent)
initWithMIDINote_velocity_groupID_duration avExtendedNoteOnEvent midiNote velocity groupID duration =
  sendOwnedMessage avExtendedNoteOnEvent initWithMIDINote_velocity_groupID_durationSelector midiNote velocity groupID duration

-- | initWithMIDINote:velocity:instrumentID:groupID:duration
--
-- Initialize the event with a midi note, velocity, instrument and group ID, and a duration.
--
-- This initializer is identical to initWithMIDINote:velocity:groupID:duration with the addition of		an instrumentID parameter which will allow for the possibility of an externally-created custom instrument.		If this initializer is used, instrumentID should be set to AVExtendedNoteOnEventDefaultInstrument for now.
--
-- ObjC selector: @- initWithMIDINote:velocity:instrumentID:groupID:duration:@
initWithMIDINote_velocity_instrumentID_groupID_duration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> CFloat -> CUInt -> CUInt -> CDouble -> IO (Id AVExtendedNoteOnEvent)
initWithMIDINote_velocity_instrumentID_groupID_duration avExtendedNoteOnEvent midiNote velocity instrumentID groupID duration =
  sendOwnedMessage avExtendedNoteOnEvent initWithMIDINote_velocity_instrumentID_groupID_durationSelector midiNote velocity instrumentID groupID duration

-- | midiNote
--
-- The MIDI note number represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate arbitrary		macro- and micro-tunings.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- midiNote@
midiNote :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CFloat
midiNote avExtendedNoteOnEvent =
  sendMessage avExtendedNoteOnEvent midiNoteSelector

-- | midiNote
--
-- The MIDI note number represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate arbitrary		macro- and micro-tunings.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- setMidiNote:@
setMidiNote :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> IO ()
setMidiNote avExtendedNoteOnEvent value =
  sendMessage avExtendedNoteOnEvent setMidiNoteSelector value

-- | velocity
--
-- The MIDI velocity represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate very precise changes		in gain, etc.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- velocity@
velocity :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CFloat
velocity avExtendedNoteOnEvent =
  sendMessage avExtendedNoteOnEvent velocitySelector

-- | velocity
--
-- The MIDI velocity represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate very precise changes		in gain, etc.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- setVelocity:@
setVelocity :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> IO ()
setVelocity avExtendedNoteOnEvent value =
  sendMessage avExtendedNoteOnEvent setVelocitySelector value

-- | instrumentID
--
-- This should be set to AVExtendedNoteOnEventDefaultInstrument.
--
-- ObjC selector: @- instrumentID@
instrumentID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CUInt
instrumentID avExtendedNoteOnEvent =
  sendMessage avExtendedNoteOnEvent instrumentIDSelector

-- | instrumentID
--
-- This should be set to AVExtendedNoteOnEventDefaultInstrument.
--
-- ObjC selector: @- setInstrumentID:@
setInstrumentID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CUInt -> IO ()
setInstrumentID avExtendedNoteOnEvent value =
  sendMessage avExtendedNoteOnEvent setInstrumentIDSelector value

-- | groupID
--
-- This represents the audio unit channel (i.e., Group Scope) which should handle this event.		Range: normally between 0 and 15, but may be higher if the AVMusicTrack's destinationAudioUnit		supports more channels.
--
-- ObjC selector: @- groupID@
groupID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CUInt
groupID avExtendedNoteOnEvent =
  sendMessage avExtendedNoteOnEvent groupIDSelector

-- | groupID
--
-- This represents the audio unit channel (i.e., Group Scope) which should handle this event.		Range: normally between 0 and 15, but may be higher if the AVMusicTrack's destinationAudioUnit		supports more channels.
--
-- ObjC selector: @- setGroupID:@
setGroupID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CUInt -> IO ()
setGroupID avExtendedNoteOnEvent value =
  sendMessage avExtendedNoteOnEvent setGroupIDSelector value

-- | duration
--
-- The duration of this event in AVMusicTimeStamp beats.  Range:  Any nonnegative number.
--
-- ObjC selector: @- duration@
duration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CDouble
duration avExtendedNoteOnEvent =
  sendMessage avExtendedNoteOnEvent durationSelector

-- | duration
--
-- The duration of this event in AVMusicTimeStamp beats.  Range:  Any nonnegative number.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CDouble -> IO ()
setDuration avExtendedNoteOnEvent value =
  sendMessage avExtendedNoteOnEvent setDurationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMIDINote:velocity:groupID:duration:@
initWithMIDINote_velocity_groupID_durationSelector :: Selector '[CFloat, CFloat, CUInt, CDouble] (Id AVExtendedNoteOnEvent)
initWithMIDINote_velocity_groupID_durationSelector = mkSelector "initWithMIDINote:velocity:groupID:duration:"

-- | @Selector@ for @initWithMIDINote:velocity:instrumentID:groupID:duration:@
initWithMIDINote_velocity_instrumentID_groupID_durationSelector :: Selector '[CFloat, CFloat, CUInt, CUInt, CDouble] (Id AVExtendedNoteOnEvent)
initWithMIDINote_velocity_instrumentID_groupID_durationSelector = mkSelector "initWithMIDINote:velocity:instrumentID:groupID:duration:"

-- | @Selector@ for @midiNote@
midiNoteSelector :: Selector '[] CFloat
midiNoteSelector = mkSelector "midiNote"

-- | @Selector@ for @setMidiNote:@
setMidiNoteSelector :: Selector '[CFloat] ()
setMidiNoteSelector = mkSelector "setMidiNote:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector '[] CFloat
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector '[CFloat] ()
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @instrumentID@
instrumentIDSelector :: Selector '[] CUInt
instrumentIDSelector = mkSelector "instrumentID"

-- | @Selector@ for @setInstrumentID:@
setInstrumentIDSelector :: Selector '[CUInt] ()
setInstrumentIDSelector = mkSelector "setInstrumentID:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector '[] CUInt
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector '[CUInt] ()
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

