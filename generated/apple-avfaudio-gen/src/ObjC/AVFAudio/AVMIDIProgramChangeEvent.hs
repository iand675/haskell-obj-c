{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIProgramChangeEvent
--
-- The event class representing MIDI program or patch change messages.
--
-- The effect of these messages will depend on the containing AVMusicTrack's destinationAudioUnit.
--
-- Generated bindings for @AVMIDIProgramChangeEvent@.
module ObjC.AVFAudio.AVMIDIProgramChangeEvent
  ( AVMIDIProgramChangeEvent
  , IsAVMIDIProgramChangeEvent(..)
  , initWithChannel_programNumber
  , programNumber
  , setProgramNumber
  , initWithChannel_programNumberSelector
  , programNumberSelector
  , setProgramNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | initWithChannel:programNumber:
--
-- Initialize the event with a channel and a program number.
--
-- @channel@ — The MIDI channel for the message.  Range: 0-15.
--
-- @programNumber@ — The program number to be sent.  Range: 0-127.
--
-- Per the General MIDI specification, the actual instrument that is chosen will depend on optional		AVMIDIControlChangeMessageTypeBankSelect events sent prior to this program change.
--
-- ObjC selector: @- initWithChannel:programNumber:@
initWithChannel_programNumber :: IsAVMIDIProgramChangeEvent avmidiProgramChangeEvent => avmidiProgramChangeEvent -> CUInt -> CUInt -> IO (Id AVMIDIProgramChangeEvent)
initWithChannel_programNumber avmidiProgramChangeEvent channel programNumber =
  sendOwnedMessage avmidiProgramChangeEvent initWithChannel_programNumberSelector channel programNumber

-- | programNumber
--
-- The MIDI program number.  Range: 0-127.
--
-- ObjC selector: @- programNumber@
programNumber :: IsAVMIDIProgramChangeEvent avmidiProgramChangeEvent => avmidiProgramChangeEvent -> IO CUInt
programNumber avmidiProgramChangeEvent =
  sendMessage avmidiProgramChangeEvent programNumberSelector

-- | programNumber
--
-- The MIDI program number.  Range: 0-127.
--
-- ObjC selector: @- setProgramNumber:@
setProgramNumber :: IsAVMIDIProgramChangeEvent avmidiProgramChangeEvent => avmidiProgramChangeEvent -> CUInt -> IO ()
setProgramNumber avmidiProgramChangeEvent value =
  sendMessage avmidiProgramChangeEvent setProgramNumberSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithChannel:programNumber:@
initWithChannel_programNumberSelector :: Selector '[CUInt, CUInt] (Id AVMIDIProgramChangeEvent)
initWithChannel_programNumberSelector = mkSelector "initWithChannel:programNumber:"

-- | @Selector@ for @programNumber@
programNumberSelector :: Selector '[] CUInt
programNumberSelector = mkSelector "programNumber"

-- | @Selector@ for @setProgramNumber:@
setProgramNumberSelector :: Selector '[CUInt] ()
setProgramNumberSelector = mkSelector "setProgramNumber:"

