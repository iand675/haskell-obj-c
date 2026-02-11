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
  , setKeySelector
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
initWithChannel_key_pressure avmidiPolyPressureEvent  channel key pressure =
  sendMsg avmidiPolyPressureEvent (mkSelector "initWithChannel:key:pressure:") (retPtr retVoid) [argCUInt (fromIntegral channel), argCUInt (fromIntegral key), argCUInt (fromIntegral pressure)] >>= ownedObject . castPtr

-- | key
--
-- The MIDI key number.
--
-- ObjC selector: @- key@
key :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> IO CUInt
key avmidiPolyPressureEvent  =
  sendMsg avmidiPolyPressureEvent (mkSelector "key") retCUInt []

-- | key
--
-- The MIDI key number.
--
-- ObjC selector: @- setKey:@
setKey :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> CUInt -> IO ()
setKey avmidiPolyPressureEvent  value =
  sendMsg avmidiPolyPressureEvent (mkSelector "setKey:") retVoid [argCUInt (fromIntegral value)]

-- | pressure
--
-- The poly pressure value for the requested key.
--
-- ObjC selector: @- pressure@
pressure :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> IO CUInt
pressure avmidiPolyPressureEvent  =
  sendMsg avmidiPolyPressureEvent (mkSelector "pressure") retCUInt []

-- | pressure
--
-- The poly pressure value for the requested key.
--
-- ObjC selector: @- setPressure:@
setPressure :: IsAVMIDIPolyPressureEvent avmidiPolyPressureEvent => avmidiPolyPressureEvent -> CUInt -> IO ()
setPressure avmidiPolyPressureEvent  value =
  sendMsg avmidiPolyPressureEvent (mkSelector "setPressure:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:key:pressure:@
initWithChannel_key_pressureSelector :: Selector
initWithChannel_key_pressureSelector = mkSelector "initWithChannel:key:pressure:"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @pressure@
pressureSelector :: Selector
pressureSelector = mkSelector "pressure"

-- | @Selector@ for @setPressure:@
setPressureSelector :: Selector
setPressureSelector = mkSelector "setPressure:"

