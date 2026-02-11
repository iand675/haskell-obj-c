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
  , channelName
  , owningPortUID
  , channelNumber
  , channelLabel
  , channelNameSelector
  , owningPortUIDSelector
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

-- | A human-readable name for the channel.
--
-- ObjC selector: @- channelName@
channelName :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO (Id NSString)
channelName avAudioSessionChannelDescription  =
    sendMsg avAudioSessionChannelDescription (mkSelector "channelName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The UID (unique identifier) of the port owning the channel.
--
-- ObjC selector: @- owningPortUID@
owningPortUID :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO (Id NSString)
owningPortUID avAudioSessionChannelDescription  =
    sendMsg avAudioSessionChannelDescription (mkSelector "owningPortUID") (retPtr retVoid) [] >>= retainedObject . castPtr

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

-- | @Selector@ for @channelName@
channelNameSelector :: Selector
channelNameSelector = mkSelector "channelName"

-- | @Selector@ for @owningPortUID@
owningPortUIDSelector :: Selector
owningPortUIDSelector = mkSelector "owningPortUID"

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @channelLabel@
channelLabelSelector :: Selector
channelLabelSelector = mkSelector "channelLabel"

