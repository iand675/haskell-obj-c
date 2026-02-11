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
  , valueSelector
  , setValueSelector


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
initWithChannel_value avmidiPitchBendEvent  channel value =
  sendMsg avmidiPitchBendEvent (mkSelector "initWithChannel:value:") (retPtr retVoid) [argCUInt (fromIntegral channel), argCUInt (fromIntegral value)] >>= ownedObject . castPtr

-- | value
--
-- The value of the pitch bend event.  Range: 0-16383 (midpoint 8192).
--
-- ObjC selector: @- value@
value :: IsAVMIDIPitchBendEvent avmidiPitchBendEvent => avmidiPitchBendEvent -> IO CUInt
value avmidiPitchBendEvent  =
  sendMsg avmidiPitchBendEvent (mkSelector "value") retCUInt []

-- | value
--
-- The value of the pitch bend event.  Range: 0-16383 (midpoint 8192).
--
-- ObjC selector: @- setValue:@
setValue :: IsAVMIDIPitchBendEvent avmidiPitchBendEvent => avmidiPitchBendEvent -> CUInt -> IO ()
setValue avmidiPitchBendEvent  value =
  sendMsg avmidiPitchBendEvent (mkSelector "setValue:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:value:@
initWithChannel_valueSelector :: Selector
initWithChannel_valueSelector = mkSelector "initWithChannel:value:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

