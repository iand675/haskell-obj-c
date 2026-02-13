{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Sound Device Stream Configuration.
--
-- A PCM stream of audio data.    VZVirtioSoundDeviceStreamConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioSoundDeviceInputStreamConfiguration or VZVirtioSoundDeviceOutputStreamConfiguration should be used instead.
--
-- See: VZVirtioSoundDeviceInputStreamConfiguration
--
-- See: VZVirtioSoundDeviceOutputStreamConfiguration
--
-- Generated bindings for @VZVirtioSoundDeviceStreamConfiguration@.
module ObjC.Virtualization.VZVirtioSoundDeviceStreamConfiguration
  ( VZVirtioSoundDeviceStreamConfiguration
  , IsVZVirtioSoundDeviceStreamConfiguration(..)
  , new
  , init_
  , initSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZVirtioSoundDeviceStreamConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZVirtioSoundDeviceStreamConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZVirtioSoundDeviceStreamConfiguration vzVirtioSoundDeviceStreamConfiguration => vzVirtioSoundDeviceStreamConfiguration -> IO (Id VZVirtioSoundDeviceStreamConfiguration)
init_ vzVirtioSoundDeviceStreamConfiguration =
  sendOwnedMessage vzVirtioSoundDeviceStreamConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZVirtioSoundDeviceStreamConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioSoundDeviceStreamConfiguration)
initSelector = mkSelector "init"

