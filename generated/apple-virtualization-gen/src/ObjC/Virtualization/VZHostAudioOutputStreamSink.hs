{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Host audio output stream sink plays audio to the host system's default output device.
--
-- Host output data goes to the same device that AudioQueueNewOutput uses.
--
-- See: VZVirtioSoundDeviceOutputStreamConfiguration
--
-- Generated bindings for @VZHostAudioOutputStreamSink@.
module ObjC.Virtualization.VZHostAudioOutputStreamSink
  ( VZHostAudioOutputStreamSink
  , IsVZHostAudioOutputStreamSink(..)
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
init_ :: IsVZHostAudioOutputStreamSink vzHostAudioOutputStreamSink => vzHostAudioOutputStreamSink -> IO (Id VZHostAudioOutputStreamSink)
init_ vzHostAudioOutputStreamSink =
  sendOwnedMessage vzHostAudioOutputStreamSink initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZHostAudioOutputStreamSink)
initSelector = mkSelector "init"

