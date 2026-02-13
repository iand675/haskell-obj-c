{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for an audio device configuration.
--
-- VZAudioDeviceConfiguration should not be instantiated directly.    The subclass VZVirtioSoundDeviceConfiguration should be used instead.
--
-- See: VZVirtioSoundDeviceConfiguration
--
-- Generated bindings for @VZAudioDeviceConfiguration@.
module ObjC.Virtualization.VZAudioDeviceConfiguration
  ( VZAudioDeviceConfiguration
  , IsVZAudioDeviceConfiguration(..)
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
new :: IO (Id VZAudioDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZAudioDeviceConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZAudioDeviceConfiguration vzAudioDeviceConfiguration => vzAudioDeviceConfiguration -> IO (Id VZAudioDeviceConfiguration)
init_ vzAudioDeviceConfiguration =
  sendOwnedMessage vzAudioDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZAudioDeviceConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZAudioDeviceConfiguration)
initSelector = mkSelector "init"

