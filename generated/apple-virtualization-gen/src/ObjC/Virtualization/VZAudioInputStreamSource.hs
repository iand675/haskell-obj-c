{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Base class for an audio input stream source.
--
-- An audio input stream source defines how audio input data for a guest is produced on the host system.
--
-- VZAudioInputStreamSource should not be instantiated directly. One of its subclasses should be used instead.
--
-- See: VZHostAudioInputStreamSource
--
-- Generated bindings for @VZAudioInputStreamSource@.
module ObjC.Virtualization.VZAudioInputStreamSource
  ( VZAudioInputStreamSource
  , IsVZAudioInputStreamSource(..)
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
new :: IO (Id VZAudioInputStreamSource)
new  =
  do
    cls' <- getRequiredClass "VZAudioInputStreamSource"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZAudioInputStreamSource vzAudioInputStreamSource => vzAudioInputStreamSource -> IO (Id VZAudioInputStreamSource)
init_ vzAudioInputStreamSource =
  sendOwnedMessage vzAudioInputStreamSource initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZAudioInputStreamSource)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZAudioInputStreamSource)
initSelector = mkSelector "init"

