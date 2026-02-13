{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Sound Device Output Stream Configuration.
--
-- A PCM stream of output audio data, such as to a speaker.
--
-- Generated bindings for @VZVirtioSoundDeviceOutputStreamConfiguration@.
module ObjC.Virtualization.VZVirtioSoundDeviceOutputStreamConfiguration
  ( VZVirtioSoundDeviceOutputStreamConfiguration
  , IsVZVirtioSoundDeviceOutputStreamConfiguration(..)
  , init_
  , sink
  , setSink
  , initSelector
  , setSinkSelector
  , sinkSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the output stream configuration.
--
-- ObjC selector: @- init@
init_ :: IsVZVirtioSoundDeviceOutputStreamConfiguration vzVirtioSoundDeviceOutputStreamConfiguration => vzVirtioSoundDeviceOutputStreamConfiguration -> IO (Id VZVirtioSoundDeviceOutputStreamConfiguration)
init_ vzVirtioSoundDeviceOutputStreamConfiguration =
  sendOwnedMessage vzVirtioSoundDeviceOutputStreamConfiguration initSelector

-- | Audio Stream Sink. Defines how the audio data produced by the guest is handled on the host. The default is nil.
--
-- Not specifying a Sink will have a default handler that swallows the audio.
--
-- See: VZAudioOutputStreamSink
--
-- ObjC selector: @- sink@
sink :: IsVZVirtioSoundDeviceOutputStreamConfiguration vzVirtioSoundDeviceOutputStreamConfiguration => vzVirtioSoundDeviceOutputStreamConfiguration -> IO (Id VZAudioOutputStreamSink)
sink vzVirtioSoundDeviceOutputStreamConfiguration =
  sendMessage vzVirtioSoundDeviceOutputStreamConfiguration sinkSelector

-- | Audio Stream Sink. Defines how the audio data produced by the guest is handled on the host. The default is nil.
--
-- Not specifying a Sink will have a default handler that swallows the audio.
--
-- See: VZAudioOutputStreamSink
--
-- ObjC selector: @- setSink:@
setSink :: (IsVZVirtioSoundDeviceOutputStreamConfiguration vzVirtioSoundDeviceOutputStreamConfiguration, IsVZAudioOutputStreamSink value) => vzVirtioSoundDeviceOutputStreamConfiguration -> value -> IO ()
setSink vzVirtioSoundDeviceOutputStreamConfiguration value =
  sendMessage vzVirtioSoundDeviceOutputStreamConfiguration setSinkSelector (toVZAudioOutputStreamSink value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSoundDeviceOutputStreamConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @sink@
sinkSelector :: Selector '[] (Id VZAudioOutputStreamSink)
sinkSelector = mkSelector "sink"

-- | @Selector@ for @setSink:@
setSinkSelector :: Selector '[Id VZAudioOutputStreamSink] ()
setSinkSelector = mkSelector "setSink:"

