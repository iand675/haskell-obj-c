{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDINoteEvent
--
-- The event class representing MIDI note-on/off messages.
--
-- @channel@ — The MIDI channel for the note.  Range: 0-15.
--
-- @key@ — The MIDI key number for the note.  Range: 0-127.
--
-- @velocity@ — The MIDI velocity for the note.  Range: 0-127 (see discussion).
--
-- @duration@ — The duration of this note event in AVMusicTimeStamp beats.  Range: Any non-negative number.
--
-- The AVAudioSequencer will automatically send a MIDI note-off after the note duration has passed.		To send an explicit note-off event, create an AVMIDINoteEvent with its velocity set to zero.
--
-- Generated bindings for @AVMIDINoteEvent@.
module ObjC.AVFAudio.AVMIDINoteEvent
  ( AVMIDINoteEvent
  , IsAVMIDINoteEvent(..)
  , initWithChannel_key_velocity_duration
  , channel
  , setChannel
  , key
  , setKey
  , velocity
  , setVelocity
  , duration
  , setDuration
  , channelSelector
  , durationSelector
  , initWithChannel_key_velocity_durationSelector
  , keySelector
  , setChannelSelector
  , setDurationSelector
  , setKeySelector
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

-- | initWithChannel:key:velocity:duration
--
-- Initialize the event with a MIDI channel, key number, velocity and duration.
--
-- @channel@ — The MIDI channel.  Range: 0-15.
--
-- @key@ — The MIDI key number.  Range: 0-127.
--
-- @velocity@ — The MIDI velocity.  Range: 0-127 with zero indicating a note-off event.
--
-- @duration@ — The duration in beats for this note.  Range: Any non-negative number.
--
-- ObjC selector: @- initWithChannel:key:velocity:duration:@
initWithChannel_key_velocity_duration :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> CUInt -> CUInt -> CDouble -> IO (Id AVMIDINoteEvent)
initWithChannel_key_velocity_duration avmidiNoteEvent channel keyNum velocity duration =
  sendOwnedMessage avmidiNoteEvent initWithChannel_key_velocity_durationSelector channel keyNum velocity duration

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- channel@
channel :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CUInt
channel avmidiNoteEvent =
  sendMessage avmidiNoteEvent channelSelector

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- setChannel:@
setChannel :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> IO ()
setChannel avmidiNoteEvent value =
  sendMessage avmidiNoteEvent setChannelSelector value

-- | key
--
-- The MIDI key number for the event.  Range: 0-127.
--
-- ObjC selector: @- key@
key :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CUInt
key avmidiNoteEvent =
  sendMessage avmidiNoteEvent keySelector

-- | key
--
-- The MIDI key number for the event.  Range: 0-127.
--
-- ObjC selector: @- setKey:@
setKey :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> IO ()
setKey avmidiNoteEvent value =
  sendMessage avmidiNoteEvent setKeySelector value

-- | velocity
--
-- The MIDI velocity for the event.  Range: 0-127.
--
-- ObjC selector: @- velocity@
velocity :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CUInt
velocity avmidiNoteEvent =
  sendMessage avmidiNoteEvent velocitySelector

-- | velocity
--
-- The MIDI velocity for the event.  Range: 0-127.
--
-- ObjC selector: @- setVelocity:@
setVelocity :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> IO ()
setVelocity avmidiNoteEvent value =
  sendMessage avmidiNoteEvent setVelocitySelector value

-- | duration
--
-- The duration of the event in AVMusicTimeStamp beats.  Range: Any non-negative number.
--
-- ObjC selector: @- duration@
duration :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CDouble
duration avmidiNoteEvent =
  sendMessage avmidiNoteEvent durationSelector

-- | duration
--
-- The duration of the event in AVMusicTimeStamp beats.  Range: Any non-negative number.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CDouble -> IO ()
setDuration avmidiNoteEvent value =
  sendMessage avmidiNoteEvent setDurationSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:key:velocity:duration:@
initWithChannel_key_velocity_durationSelector :: Selector '[CUInt, CUInt, CUInt, CDouble] (Id AVMIDINoteEvent)
initWithChannel_key_velocity_durationSelector = mkSelector "initWithChannel:key:velocity:duration:"

-- | @Selector@ for @channel@
channelSelector :: Selector '[] CUInt
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[CUInt] ()
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @key@
keySelector :: Selector '[] CUInt
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[CUInt] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector '[] CUInt
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector '[CUInt] ()
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @duration@
durationSelector :: Selector '[] CDouble
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector '[CDouble] ()
setDurationSelector = mkSelector "setDuration:"

