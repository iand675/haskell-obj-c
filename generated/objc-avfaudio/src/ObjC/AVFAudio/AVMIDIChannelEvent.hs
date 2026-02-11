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

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- channel@
channel :: IsAVMIDIChannelEvent avmidiChannelEvent => avmidiChannelEvent -> IO CUInt
channel avmidiChannelEvent  =
  sendMsg avmidiChannelEvent (mkSelector "channel") retCUInt []

-- | channel
--
-- The MIDI channel for the event.  Range: 0-15.
--
-- ObjC selector: @- setChannel:@
setChannel :: IsAVMIDIChannelEvent avmidiChannelEvent => avmidiChannelEvent -> CUInt -> IO ()
setChannel avmidiChannelEvent  value =
  sendMsg avmidiChannelEvent (mkSelector "setChannel:") retVoid [argCUInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channel@
channelSelector :: Selector
channelSelector = mkSelector "channel"

-- | @Selector@ for @setChannel:@
setChannelSelector :: Selector
setChannelSelector = mkSelector "setChannel:"

