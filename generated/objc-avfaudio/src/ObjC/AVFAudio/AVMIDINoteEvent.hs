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
  , initWithChannel_key_velocity_durationSelector
  , channelSelector
  , setChannelSelector
  , keySelector
  , setKeySelector
  , velocitySelector
  , setVelocitySelector
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
initWithChannel_key_velocity_duration avmidiNoteEvent  channel keyNum velocity duration =
  sendMsg avmidiNoteEvent (mkSelector "initWithChannel:key:velocity:duration:") (retPtr retVoid) [argCUInt (fromIntegral channel), argCUInt (fromIntegral keyNum), argCUInt (fromIntegral velocity), argCDouble (fromIntegral duration)] >>= ownedObject . castPtr

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- channel@
channel :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CUInt
channel avmidiNoteEvent  =
  sendMsg avmidiNoteEvent (mkSelector "channel") retCUInt []

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- setChannel:@
setChannel :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> IO ()
setChannel avmidiNoteEvent  value =
  sendMsg avmidiNoteEvent (mkSelector "setChannel:") retVoid [argCUInt (fromIntegral value)]

-- | key
--
-- The MIDI key number for the event.  Range: 0-127.
--
-- ObjC selector: @- key@
key :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CUInt
key avmidiNoteEvent  =
  sendMsg avmidiNoteEvent (mkSelector "key") retCUInt []

-- | key
--
-- The MIDI key number for the event.  Range: 0-127.
--
-- ObjC selector: @- setKey:@
setKey :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> IO ()
setKey avmidiNoteEvent  value =
  sendMsg avmidiNoteEvent (mkSelector "setKey:") retVoid [argCUInt (fromIntegral value)]

-- | velocity
--
-- The MIDI velocity for the event.  Range: 0-127.
--
-- ObjC selector: @- velocity@
velocity :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CUInt
velocity avmidiNoteEvent  =
  sendMsg avmidiNoteEvent (mkSelector "velocity") retCUInt []

-- | velocity
--
-- The MIDI velocity for the event.  Range: 0-127.
--
-- ObjC selector: @- setVelocity:@
setVelocity :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CUInt -> IO ()
setVelocity avmidiNoteEvent  value =
  sendMsg avmidiNoteEvent (mkSelector "setVelocity:") retVoid [argCUInt (fromIntegral value)]

-- | duration
--
-- The duration of the event in AVMusicTimeStamp beats.  Range: Any non-negative number.
--
-- ObjC selector: @- duration@
duration :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> IO CDouble
duration avmidiNoteEvent  =
  sendMsg avmidiNoteEvent (mkSelector "duration") retCDouble []

-- | duration
--
-- The duration of the event in AVMusicTimeStamp beats.  Range: Any non-negative number.
--
-- ObjC selector: @- setDuration:@
setDuration :: IsAVMIDINoteEvent avmidiNoteEvent => avmidiNoteEvent -> CDouble -> IO ()
setDuration avmidiNoteEvent  value =
  sendMsg avmidiNoteEvent (mkSelector "setDuration:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:key:velocity:duration:@
initWithChannel_key_velocity_durationSelector :: Selector
initWithChannel_key_velocity_durationSelector = mkSelector "initWithChannel:key:velocity:duration:"

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @velocity@
velocitySelector :: Selector
velocitySelector = mkSelector "velocity"

-- | @Selector@ for @setVelocity:@
setVelocitySelector :: Selector
setVelocitySelector = mkSelector "setVelocity:"

-- | @Selector@ for @duration@
durationSelector :: Selector
durationSelector = mkSelector "duration"

-- | @Selector@ for @setDuration:@
setDurationSelector :: Selector
setDurationSelector = mkSelector "setDuration:"

