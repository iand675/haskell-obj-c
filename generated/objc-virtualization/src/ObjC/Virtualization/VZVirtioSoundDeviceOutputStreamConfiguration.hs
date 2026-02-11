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
  , sinkSelector
  , setSinkSelector


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

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initialize the output stream configuration.
--
-- ObjC selector: @- init@
init_ :: IsVZVirtioSoundDeviceOutputStreamConfiguration vzVirtioSoundDeviceOutputStreamConfiguration => vzVirtioSoundDeviceOutputStreamConfiguration -> IO (Id VZVirtioSoundDeviceOutputStreamConfiguration)
init_ vzVirtioSoundDeviceOutputStreamConfiguration  =
  sendMsg vzVirtioSoundDeviceOutputStreamConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Audio Stream Sink. Defines how the audio data produced by the guest is handled on the host. The default is nil.
--
-- Not specifying a Sink will have a default handler that swallows the audio.
--
-- See: VZAudioOutputStreamSink
--
-- ObjC selector: @- sink@
sink :: IsVZVirtioSoundDeviceOutputStreamConfiguration vzVirtioSoundDeviceOutputStreamConfiguration => vzVirtioSoundDeviceOutputStreamConfiguration -> IO (Id VZAudioOutputStreamSink)
sink vzVirtioSoundDeviceOutputStreamConfiguration  =
  sendMsg vzVirtioSoundDeviceOutputStreamConfiguration (mkSelector "sink") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Audio Stream Sink. Defines how the audio data produced by the guest is handled on the host. The default is nil.
--
-- Not specifying a Sink will have a default handler that swallows the audio.
--
-- See: VZAudioOutputStreamSink
--
-- ObjC selector: @- setSink:@
setSink :: (IsVZVirtioSoundDeviceOutputStreamConfiguration vzVirtioSoundDeviceOutputStreamConfiguration, IsVZAudioOutputStreamSink value) => vzVirtioSoundDeviceOutputStreamConfiguration -> value -> IO ()
setSink vzVirtioSoundDeviceOutputStreamConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioSoundDeviceOutputStreamConfiguration (mkSelector "setSink:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @sink@
sinkSelector :: Selector
sinkSelector = mkSelector "sink"

-- | @Selector@ for @setSink:@
setSinkSelector :: Selector
setSinkSelector = mkSelector "setSink:"

