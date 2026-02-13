{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIPitchBendEvent
--
-- The event class representing MIDI pitch bend messages.
--
-- The effect of these messages will depend on the AVMusicTrack's destinationAudioUnit		and the capabilities of the destination's currently-loaded instrument.
--
-- Generated bindings for @AVMIDIPitchBendEvent@.
module ObjC.AVFAudio.AVMIDIPitchBendEvent
  ( AVMIDIPitchBendEvent
  , IsAVMIDIPitchBendEvent(..)
  , initWithChannel_value
  , value
  , setValue
  , initWithChannel_valueSelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithChannel:value:
--
-- Initialize the event with a channel and a pitch bend value.
--
-- @channel@ — The MIDI channel for the message.  Range: 0-15.
--
-- @value@ — The pitch bend value.  Range: 0-16383 (midpoint 8192).
--
-- ObjC selector: @- initWithChannel:value:@
initWithChannel_value :: IsAVMIDIPitchBendEvent avmidiPitchBendEvent => avmidiPitchBendEvent -> CUInt -> CUInt -> IO (Id AVMIDIPitchBendEvent)
initWithChannel_value avmidiPitchBendEvent channel value =
  sendOwnedMessage avmidiPitchBendEvent initWithChannel_valueSelector channel value

-- | value
--
-- The value of the pitch bend event.  Range: 0-16383 (midpoint 8192).
--
-- ObjC selector: @- value@
value :: IsAVMIDIPitchBendEvent avmidiPitchBendEvent => avmidiPitchBendEvent -> IO CUInt
value avmidiPitchBendEvent =
  sendMessage avmidiPitchBendEvent valueSelector

-- | value
--
-- The value of the pitch bend event.  Range: 0-16383 (midpoint 8192).
--
-- ObjC selector: @- setValue:@
setValue :: IsAVMIDIPitchBendEvent avmidiPitchBendEvent => avmidiPitchBendEvent -> CUInt -> IO ()
setValue avmidiPitchBendEvent value =
  sendMessage avmidiPitchBendEvent setValueSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:value:@
initWithChannel_valueSelector :: Selector '[CUInt, CUInt] (Id AVMIDIPitchBendEvent)
initWithChannel_valueSelector = mkSelector "initWithChannel:value:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] CUInt
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[CUInt] ()
setValueSelector = mkSelector "setValue:"

