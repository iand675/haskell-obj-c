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
initWithChannel_pressure avmidiChannelPressureEvent  channel pressure =
  sendMsg avmidiChannelPressureEvent (mkSelector "initWithChannel:pressure:") (retPtr retVoid) [argCUInt (fromIntegral channel), argCUInt (fromIntegral pressure)] >>= ownedObject . castPtr

-- | pressure
--
-- The MIDI channel pressure.
--
-- ObjC selector: @- pressure@
pressure :: IsAVMIDIChannelPressureEvent avmidiChannelPressureEvent => avmidiChannelPressureEvent -> IO CUInt
pressure avmidiChannelPressureEvent  =
  sendMsg avmidiChannelPressureEvent (mkSelector "pressure") retCUInt []

-- | pressure
--
-- The MIDI channel pressure.
--
-- ObjC selector: @- setPressure:@
setPressure :: IsAVMIDIChannelPressureEvent avmidiChannelPressureEvent => avmidiChannelPressureEvent -> CUInt -> IO ()
setPressure avmidiChannelPressureEvent  value =
  sendMsg avmidiChannelPressureEvent (mkSelector "setPressure:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:pressure:@
initWithChannel_pressureSelector :: Selector
initWithChannel_pressureSelector = mkSelector "initWithChannel:pressure:"

-- | @Selector@ for @pressure@
pressureSelector :: Selector
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @setPressure:@
setPressureSelector :: Selector
setPressureSelector = mkSelector "setPressure:"

