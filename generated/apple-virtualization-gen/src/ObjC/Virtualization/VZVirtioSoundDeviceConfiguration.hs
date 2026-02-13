{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Sound Device Configuration.
--
-- The device exposes a source or destination of sound.
--
-- Generated bindings for @VZVirtioSoundDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioSoundDeviceConfiguration
  ( VZVirtioSoundDeviceConfiguration
  , IsVZVirtioSoundDeviceConfiguration(..)
  , init_
  , streams
  , setStreams
  , initSelector
  , setStreamsSelector
  , streamsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZVirtioSoundDeviceConfiguration vzVirtioSoundDeviceConfiguration => vzVirtioSoundDeviceConfiguration -> IO (Id VZVirtioSoundDeviceConfiguration)
init_ vzVirtioSoundDeviceConfiguration =
  sendOwnedMessage vzVirtioSoundDeviceConfiguration initSelector

-- | List of audio streams exposed by this device. Empty by default.
--
-- ObjC selector: @- streams@
streams :: IsVZVirtioSoundDeviceConfiguration vzVirtioSoundDeviceConfiguration => vzVirtioSoundDeviceConfiguration -> IO (Id NSArray)
streams vzVirtioSoundDeviceConfiguration =
  sendMessage vzVirtioSoundDeviceConfiguration streamsSelector

-- | List of audio streams exposed by this device. Empty by default.
--
-- ObjC selector: @- setStreams:@
setStreams :: (IsVZVirtioSoundDeviceConfiguration vzVirtioSoundDeviceConfiguration, IsNSArray value) => vzVirtioSoundDeviceConfiguration -> value -> IO ()
setStreams vzVirtioSoundDeviceConfiguration value =
  sendMessage vzVirtioSoundDeviceConfiguration setStreamsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSoundDeviceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @streams@
streamsSelector :: Selector '[] (Id NSArray)
streamsSelector = mkSelector "streams"

-- | @Selector@ for @setStreams:@
setStreamsSelector :: Selector '[Id NSArray] ()
setStreamsSelector = mkSelector "setStreams:"

