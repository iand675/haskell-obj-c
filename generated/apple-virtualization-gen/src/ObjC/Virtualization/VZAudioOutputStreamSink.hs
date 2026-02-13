{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for an audio output stream sink.
--
-- An audio output stream sink defines how audio data from a guest is consumed on the host system.
--
-- VZAudioOutputStreamSink should not be instantiated directly. One of its subclasses should be used instead.
--
-- See: VZHostAudioOutputStreamSink
--
-- Generated bindings for @VZAudioOutputStreamSink@.
module ObjC.Virtualization.VZAudioOutputStreamSink
  ( VZAudioOutputStreamSink
  , IsVZAudioOutputStreamSink(..)
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
new :: IO (Id VZAudioOutputStreamSink)
new  =
  do
    cls' <- getRequiredClass "VZAudioOutputStreamSink"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZAudioOutputStreamSink vzAudioOutputStreamSink => vzAudioOutputStreamSink -> IO (Id VZAudioOutputStreamSink)
init_ vzAudioOutputStreamSink =
  sendOwnedMessage vzAudioOutputStreamSink initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZAudioOutputStreamSink)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZAudioOutputStreamSink)
initSelector = mkSelector "init"

