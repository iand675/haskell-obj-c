{-# LANGUAGE DataKinds #-}
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
  , channelLabelSelector
  , channelNameSelector
  , channelNumberSelector
  , owningPortUIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFAudio.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A human-readable name for the channel.
--
-- ObjC selector: @- channelName@
channelName :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO (Id NSString)
channelName avAudioSessionChannelDescription =
  sendMessage avAudioSessionChannelDescription channelNameSelector

-- | The UID (unique identifier) of the port owning the channel.
--
-- ObjC selector: @- owningPortUID@
owningPortUID :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO (Id NSString)
owningPortUID avAudioSessionChannelDescription =
  sendMessage avAudioSessionChannelDescription owningPortUIDSelector

-- | The index of this channel in its owning port's array of channels.
--
-- ObjC selector: @- channelNumber@
channelNumber :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO CULong
channelNumber avAudioSessionChannelDescription =
  sendMessage avAudioSessionChannelDescription channelNumberSelector

-- | Description of the physical location of this channel.
--
-- ObjC selector: @- channelLabel@
channelLabel :: IsAVAudioSessionChannelDescription avAudioSessionChannelDescription => avAudioSessionChannelDescription -> IO CUInt
channelLabel avAudioSessionChannelDescription =
  sendMessage avAudioSessionChannelDescription channelLabelSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @channelName@
channelNameSelector :: Selector '[] (Id NSString)
channelNameSelector = mkSelector "channelName"

-- | @Selector@ for @owningPortUID@
owningPortUIDSelector :: Selector '[] (Id NSString)
owningPortUIDSelector = mkSelector "owningPortUID"

-- | @Selector@ for @channelNumber@
channelNumberSelector :: Selector '[] CULong
channelNumberSelector = mkSelector "channelNumber"

-- | @Selector@ for @channelLabel@
channelLabelSelector :: Selector '[] CUInt
channelLabelSelector = mkSelector "channelLabel"

