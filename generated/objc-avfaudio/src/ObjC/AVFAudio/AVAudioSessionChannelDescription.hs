{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVAudioSessionChannelDescription
--
-- Information about a port's audio channels.
--
-- AudioQueue, AURemoteIO and AUVoiceIO instances can be assigned to communicate with specific	hardware channels by setting an array of <port UID, channel index> pairs.
--
-- Generated bindings for @AVAudioSessionChannelDescription@.
module ObjC.AVFAudio.AVAudioSessionChannelDescription
  ( AVAudioSessionChannelDescription
  , IsAVAudioSessionChannelDescription(..)
  , channelNumber
  , channelLabel
  , channelNumberSelector
  , channelLabelSelector


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

-- | The index of this channel in its owning port's array of channels.
--
-- ObjC selector: @- channelNumber@
channelNumber :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO CULong
channelNumber avAudioSessionChannelDescription  =
  sendMsg avAudioSessionChannelDescription (mkSelector "channelNumber") retCULong []

-- | Description of the physical location of this channel.
--
-- ObjC selector: @- channelLabel@
channelLabel :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO CUInt
channelLabel avAudioSessionChannelDescription  =
  sendMsg avAudioSessionChannelDescription (mkSelector "channelLabel") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @channelLabel@
channelLabelSelector :: Selector
channelLabelSelector = mkSelector "channelLabel"

