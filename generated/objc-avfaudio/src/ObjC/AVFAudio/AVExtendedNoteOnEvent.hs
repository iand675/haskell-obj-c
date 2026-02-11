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
  , initWithMIDINote_velocity_groupID_durationSelector
  , initWithMIDINote_velocity_instrumentID_groupID_durationSelector
  , midiNoteSelector
  , setMidiNoteSelector
  , velocitySelector
  , setVelocitySelector
  , instrumentIDSelector
  , setInstrumentIDSelector
  , groupIDSelector
  , setGroupIDSelector
  , durationSelector
  , setDurationSelector


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
initWithMIDINote_velocity_groupID_duration avExtendedNoteOnEvent  midiNote velocity groupID duration =
  sendMsg avExtendedNoteOnEvent (mkSelector "initWithMIDINote:velocity:groupID:duration:") (retPtr retVoid) [argCFloat (fromIntegral midiNote), argCFloat (fromIntegral velocity), argCUInt (fromIntegral groupID), argCDouble (fromIntegral duration)] >>= ownedObject . castPtr

-- | initWithMIDINote:velocity:instrumentID:groupID:duration
--
-- Initialize the event with a midi note, velocity, instrument and group ID, and a duration.
--
-- This initializer is identical to initWithMIDINote:velocity:groupID:duration with the addition of		an instrumentID parameter which will allow for the possibility of an externally-created custom instrument.		If this initializer is used, instrumentID should be set to AVExtendedNoteOnEventDefaultInstrument for now.
--
-- ObjC selector: @- initWithMIDINote:velocity:instrumentID:groupID:duration:@
initWithMIDINote_velocity_instrumentID_groupID_duration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> CFloat -> CUInt -> CUInt -> CDouble -> IO (Id AVExtendedNoteOnEvent)
initWithMIDINote_velocity_instrumentID_groupID_duration avExtendedNoteOnEvent  midiNote velocity instrumentID groupID duration =
  sendMsg avExtendedNoteOnEvent (mkSelector "initWithMIDINote:velocity:instrumentID:groupID:duration:") (retPtr retVoid) [argCFloat (fromIntegral midiNote), argCFloat (fromIntegral velocity), argCUInt (fromIntegral instrumentID), argCUInt (fromIntegral groupID), argCDouble (fromIntegral duration)] >>= ownedObject . castPtr

-- | midiNote
--
-- The MIDI note number represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate arbitrary		macro- and micro-tunings.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- midiNote@
midiNote :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CFloat
midiNote avExtendedNoteOnEvent  =
  sendMsg avExtendedNoteOnEvent (mkSelector "midiNote") retCFloat []

-- | midiNote
--
-- The MIDI note number represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate arbitrary		macro- and micro-tunings.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- setMidiNote:@
setMidiNote :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> IO ()
setMidiNote avExtendedNoteOnEvent  value =
  sendMsg avExtendedNoteOnEvent (mkSelector "setMidiNote:") retVoid [argCFloat (fromIntegral value)]

-- | velocity
--
-- The MIDI velocity represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate very precise changes		in gain, etc.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- velocity@
velocity :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CFloat
velocity avExtendedNoteOnEvent  =
  sendMsg avExtendedNoteOnEvent (mkSelector "velocity") retCFloat []

-- | velocity
--
-- The MIDI velocity represented as a floating point.  If the instrument within the AVMusicTrack's		destinationAudioUnit supports fractional values, this may be used to generate very precise changes		in gain, etc.  Range: Destination-dependent, usually 0.0 - 127.0.
--
-- ObjC selector: @- setVelocity:@
setVelocity :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CFloat -> IO ()
setVelocity avExtendedNoteOnEvent  value =
  sendMsg avExtendedNoteOnEvent (mkSelector "setVelocity:") retVoid [argCFloat (fromIntegral value)]

-- | instrumentID
--
-- This should be set to AVExtendedNoteOnEventDefaultInstrument.
--
-- ObjC selector: @- instrumentID@
instrumentID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CUInt
instrumentID avExtendedNoteOnEvent  =
  sendMsg avExtendedNoteOnEvent (mkSelector "instrumentID") retCUInt []

-- | instrumentID
--
-- This should be set to AVExtendedNoteOnEventDefaultInstrument.
--
-- ObjC selector: @- setInstrumentID:@
setInstrumentID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CUInt -> IO ()
setInstrumentID avExtendedNoteOnEvent  value =
  sendMsg avExtendedNoteOnEvent (mkSelector "setInstrumentID:") retVoid [argCUInt (fromIntegral value)]

-- | groupID
--
-- This represents the audio unit channel (i.e., Group Scope) which should handle this event.		Range: normally between 0 and 15, but may be higher if the AVMusicTrack's destinationAudioUnit		supports more channels.
--
-- ObjC selector: @- groupID@
groupID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CUInt
groupID avExtendedNoteOnEvent  =
  sendMsg avExtendedNoteOnEvent (mkSelector "groupID") retCUInt []

-- | groupID
--
-- This represents the audio unit channel (i.e., Group Scope) which should handle this event.		Range: normally between 0 and 15, but may be higher if the AVMusicTrack's destinationAudioUnit		supports more channels.
--
-- ObjC selector: @- setGroupID:@
setGroupID :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CUInt -> IO ()
setGroupID avExtendedNoteOnEvent  value =
  sendMsg avExtendedNoteOnEvent (mkSelector "setGroupID:") retVoid [argCUInt (fromIntegral value)]

-- | duration
--
-- The duration of this event in AVMusicTimeStamp beats.  Range:  Any nonnegative number.
--
-- ObjC selector: @- duration@
duration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> IO CDouble
duration avExtendedNoteOnEvent  =
  sendMsg avExtendedNoteOnEvent (mkSelector "duration") retCDouble []

-- | duration
--
-- The duration of this event in AVMusicTimeStamp beats.  Range:  Any nonnegative number.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsAVExtendedNoteOnEvent avExtendedNoteOnEvent => avExtendedNoteOnEvent -> CDouble -> IO ()
setDuration avExtendedNoteOnEvent  value =
  sendMsg avExtendedNoteOnEvent (mkSelector "setDuration:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMIDINote:velocity:groupID:duration:@
initWithMIDINote_velocity_groupID_durationSelector :: Selector
initWithMIDINote_velocity_groupID_durationSelector = mkSelector "initWithMIDINote:velocity:groupID:duration:"

-- | @Selector@ for @initWithMIDINote:velocity:instrumentID:groupID:duration:@
initWithMIDINote_velocity_instrumentID_groupID_durationSelector :: Selector
initWithMIDINote_velocity_instrumentID_groupID_durationSelector = mkSelector "initWithMIDINote:velocity:instrumentID:groupID:duration:"

-- | @Selector@ for @midiNote@
midiNoteSelector :: Selector
midiNoteSelector = mkSelector "midiNote"

-- | @Selector@ for @setMidiNote:@
setMidiNoteSelector :: Selector
setMidiNoteSelector = mkSelector "setMidiNote:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @instrumentID@
instrumentIDSelector :: Selector
instrumentIDSelector = mkSelector "instrumentID"

-- | @Selector@ for @setInstrumentID:@
setInstrumentIDSelector :: Selector
setInstrumentIDSelector = mkSelector "setInstrumentID:"

-- | @Selector@ for @groupID@
groupIDSelector :: Selector
groupIDSelector = mkSelector "groupID"

-- | @Selector@ for @setGroupID:@
setGroupIDSelector :: Selector
setGroupIDSelector = mkSelector "setGroupID:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

