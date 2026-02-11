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
  , sourceSelector
  , setSourceSelector


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

-- | Initialize the input stream configuration.
--
-- ObjC selector: @- init@
init_ :: IsVZVirtioSoundDeviceInputStreamConfiguration vzVirtioSoundDeviceInputStreamConfiguration => vzVirtioSoundDeviceInputStreamConfiguration -> IO (Id VZVirtioSoundDeviceInputStreamConfiguration)
init_ vzVirtioSoundDeviceInputStreamConfiguration  =
  sendMsg vzVirtioSoundDeviceInputStreamConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Audio Stream Source. Defines how the audio data is supplied on the host for the guest. The default is nil.
--
-- Not specifying a Source will have a default handler that produces audio silence.
--
-- See: VZAudioInputStreamSource
--
-- ObjC selector: @- source@
source :: IsVZVirtioSoundDeviceInputStreamConfiguration vzVirtioSoundDeviceInputStreamConfiguration => vzVirtioSoundDeviceInputStreamConfiguration -> IO (Id VZAudioInputStreamSource)
source vzVirtioSoundDeviceInputStreamConfiguration  =
  sendMsg vzVirtioSoundDeviceInputStreamConfiguration (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Audio Stream Source. Defines how the audio data is supplied on the host for the guest. The default is nil.
--
-- Not specifying a Source will have a default handler that produces audio silence.
--
-- See: VZAudioInputStreamSource
--
-- ObjC selector: @- setSource:@
setSource :: (IsVZVirtioSoundDeviceInputStreamConfiguration vzVirtioSoundDeviceInputStreamConfiguration, IsVZAudioInputStreamSource value) => vzVirtioSoundDeviceInputStreamConfiguration -> value -> IO ()
setSource vzVirtioSoundDeviceInputStreamConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioSoundDeviceInputStreamConfiguration (mkSelector "setSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

