{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Host audio input stream source provides audio from the host system's default input device.
--
-- Host input data comes from the same device that AudioQueueNewInput uses.
--
-- See: VZVirtioSoundDeviceInputStreamConfiguration
--
-- Generated bindings for @VZHostAudioInputStreamSource@.
module ObjC.Virtualization.VZHostAudioInputStreamSource
  ( VZHostAudioInputStreamSource
  , IsVZHostAudioInputStreamSource(..)
  , init_
  , initSelector


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
init_ :: IsVZHostAudioInputStreamSource vzHostAudioInputStreamSource => vzHostAudioInputStreamSource -> IO (Id VZHostAudioInputStreamSource)
init_ vzHostAudioInputStreamSource =
  sendOwnedMessage vzHostAudioInputStreamSource initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZHostAudioInputStreamSource)
initSelector = mkSelector "init"

