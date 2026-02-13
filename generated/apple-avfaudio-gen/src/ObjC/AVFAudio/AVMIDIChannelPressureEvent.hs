{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIChannelPressureEvent
--
-- The event class representing MIDI channel pressure messages.
--
-- The effect of these messages will depend on the containing AVMusicTrack's destinationAudioUnit		and the capabilities of the destination's currently-loaded instrument.
--
-- Generated bindings for @AVMIDIChannelPressureEvent@.
module ObjC.AVFAudio.AVMIDIChannelPressureEvent
  ( AVMIDIChannelPressureEvent
  , IsAVMIDIChannelPressureEvent(..)
  , initWithChannel_pressure
  , pressure
  , setPressure
  , initWithChannel_pressureSelector
  , pressureSelector
  , setPressureSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithChannel:pressure:
--
-- Initialize the event with a channel and a pressure value.
--
-- @channel@ — The MIDI channel for the message.  Range: 0-15.
--
-- @pressure@ — The MIDI channel pressure.  Range: 0-127.
--
-- ObjC selector: @- initWithChannel:pressure:@
initWithChannel_pressure :: IsAVMIDIChannelPressureEvent avmidiChannelPressureEvent => avmidiChannelPressureEvent -> CUInt -> CUInt -> IO (Id AVMIDIChannelPressureEvent)
initWithChannel_pressure avmidiChannelPressureEvent channel pressure =
  sendOwnedMessage avmidiChannelPressureEvent initWithChannel_pressureSelector channel pressure

-- | pressure
--
-- The MIDI channel pressure.
--
-- ObjC selector: @- pressure@
pressure :: IsAVMIDIChannelPressureEvent avmidiChannelPressureEvent => avmidiChannelPressureEvent -> IO CUInt
pressure avmidiChannelPressureEvent =
  sendMessage avmidiChannelPressureEvent pressureSelector

-- | pressure
--
-- The MIDI channel pressure.
--
-- ObjC selector: @- setPressure:@
setPressure :: IsAVMIDIChannelPressureEvent avmidiChannelPressureEvent => avmidiChannelPressureEvent -> CUInt -> IO ()
setPressure avmidiChannelPressureEvent value =
  sendMessage avmidiChannelPressureEvent setPressureSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:pressure:@
initWithChannel_pressureSelector :: Selector '[CUInt, CUInt] (Id AVMIDIChannelPressureEvent)
initWithChannel_pressureSelector = mkSelector "initWithChannel:pressure:"

-- | @Selector@ for @pressure@
pressureSelector :: Selector '[] CUInt
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @setPressure:@
setPressureSelector :: Selector '[CUInt] ()
setPressureSelector = mkSelector "setPressure:"

