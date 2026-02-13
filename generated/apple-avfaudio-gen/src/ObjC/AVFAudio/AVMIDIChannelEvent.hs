{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVMIDIChannelEvent
--
-- The event base class for all MIDI messages which operate on a single MIDI channel.
--
-- Generated bindings for @AVMIDIChannelEvent@.
module ObjC.AVFAudio.AVMIDIChannelEvent
  ( AVMIDIChannelEvent
  , IsAVMIDIChannelEvent(..)
  , channel
  , setChannel
  , channelSelector
  , setChannelSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- channel@
channel :: IsAVMIDIChannelEvent avmidiChannelEvent => avmidiChannelEvent -> IO CUInt
channel avmidiChannelEvent =
  sendMessage avmidiChannelEvent channelSelector

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- setChannel:@
setChannel :: IsAVMIDIChannelEvent avmidiChannelEvent => avmidiChannelEvent -> CUInt -> IO ()
setChannel avmidiChannelEvent value =
  sendMessage avmidiChannelEvent setChannelSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channel@
channelSelector :: Selector '[] CUInt
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector '[CUInt] ()
setChannelSelector = mkSelector "setChannel:"

