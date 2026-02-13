{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIPolyPressureEvent
--
-- The event class representing MIDI "poly" or "key" pressure messages.
--
-- Generated bindings for @AVMIDIPolyPressureEvent@.
module ObjC.AVFAudio.AVMIDIPolyPressureEvent
  ( AVMIDIPolyPressureEvent
  , IsAVMIDIPolyPressureEvent(..)
  , initWithChannel_key_pressure
  , key
  , setKey
  , pressure
  , setPressure
  , initWithChannel_key_pressureSelector
  , keySelector
  , pressureSelector
  , setKeySelector
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

-- | initWithChannel:key:pressure
--
-- Initialize the event with a channel, a MIDI key number, and a key pressure value.
--
-- @channel@ — The MIDI channel for the message.  Range: 0-15.
--
-- @key@ — The MIDI key number to which the pressure should be applied.
--
-- @pressure@ — The poly pressure value.
--
-- ObjC selector: @- initWithChannel:key:pressure:@
initWithChannel_key_pressure :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> CUInt -> CUInt -> CUInt -> IO (Id AVMIDIPolyPressureEvent)
initWithChannel_key_pressure avmidiPolyPressureEvent channel key pressure =
  sendOwnedMessage avmidiPolyPressureEvent initWithChannel_key_pressureSelector channel key pressure

-- | key
--
-- The MIDI key number.
--
-- ObjC selector: @- key@
key :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> IO CUInt
key avmidiPolyPressureEvent =
  sendMessage avmidiPolyPressureEvent keySelector

-- | key
--
-- The MIDI key number.
--
-- ObjC selector: @- setKey:@
setKey :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> CUInt -> IO ()
setKey avmidiPolyPressureEvent value =
  sendMessage avmidiPolyPressureEvent setKeySelector value

-- | pressure
--
-- The poly pressure value for the requested key.
--
-- ObjC selector: @- pressure@
pressure :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> IO CUInt
pressure avmidiPolyPressureEvent =
  sendMessage avmidiPolyPressureEvent pressureSelector

-- | pressure
--
-- The poly pressure value for the requested key.
--
-- ObjC selector: @- setPressure:@
setPressure :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> CUInt -> IO ()
setPressure avmidiPolyPressureEvent value =
  sendMessage avmidiPolyPressureEvent setPressureSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:key:pressure:@
initWithChannel_key_pressureSelector :: Selector '[CUInt, CUInt, CUInt] (Id AVMIDIPolyPressureEvent)
initWithChannel_key_pressureSelector = mkSelector "initWithChannel:key:pressure:"

-- | @Selector@ for @key@
keySelector :: Selector '[] CUInt
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[CUInt] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @pressure@
pressureSelector :: Selector '[] CUInt
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @setPressure:@
setPressureSelector :: Selector '[CUInt] ()
setPressureSelector = mkSelector "setPressure:"

