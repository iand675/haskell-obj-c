{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Sound Device Input Stream Configuration.
--
-- A PCM stream of input audio data, such as from a microphone.
--
-- Generated bindings for @VZVirtioSoundDeviceInputStreamConfiguration@.
module ObjC.Virtualization.VZVirtioSoundDeviceInputStreamConfiguration
  ( VZVirtioSoundDeviceInputStreamConfiguration
  , IsVZVirtioSoundDeviceInputStreamConfiguration(..)
  , init_
  , source
  , setSource
  , initSelector
  , setSourceSelector
  , sourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the input stream configuration.
--
-- ObjC selector: @- init@
init_ :: IsVZVirtioSoundDeviceInputStreamConfiguration vzVirtioSoundDeviceInputStreamConfiguration => vzVirtioSoundDeviceInputStreamConfiguration -> IO (Id VZVirtioSoundDeviceInputStreamConfiguration)
init_ vzVirtioSoundDeviceInputStreamConfiguration =
  sendOwnedMessage vzVirtioSoundDeviceInputStreamConfiguration initSelector

-- | Audio Stream Source. Defines how the audio data is supplied on the host for the guest. The default is nil.
--
-- Not specifying a Source will have a default handler that produces audio silence.
--
-- See: VZAudioInputStreamSource
--
-- ObjC selector: @- source@
source :: IsVZVirtioSoundDeviceInputStreamConfiguration vzVirtioSoundDeviceInputStreamConfiguration => vzVirtioSoundDeviceInputStreamConfiguration -> IO (Id VZAudioInputStreamSource)
source vzVirtioSoundDeviceInputStreamConfiguration =
  sendMessage vzVirtioSoundDeviceInputStreamConfiguration sourceSelector

-- | Audio Stream Source. Defines how the audio data is supplied on the host for the guest. The default is nil.
--
-- Not specifying a Source will have a default handler that produces audio silence.
--
-- See: VZAudioInputStreamSource
--
-- ObjC selector: @- setSource:@
setSource :: (IsVZVirtioSoundDeviceInputStreamConfiguration vzVirtioSoundDeviceInputStreamConfiguration, IsVZAudioInputStreamSource value) => vzVirtioSoundDeviceInputStreamConfiguration -> value -> IO ()
setSource vzVirtioSoundDeviceInputStreamConfiguration value =
  sendMessage vzVirtioSoundDeviceInputStreamConfiguration setSourceSelector (toVZAudioInputStreamSource value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSoundDeviceInputStreamConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id VZAudioInputStreamSource)
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id VZAudioInputStreamSource] ()
setSourceSelector = mkSelector "setSource:"

