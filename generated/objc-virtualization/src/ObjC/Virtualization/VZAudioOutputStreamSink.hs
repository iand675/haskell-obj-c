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
  , newSelector
  , initSelector


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

-- | @+ new@
new :: IO (Id VZAudioOutputStreamSink)
new  =
  do
    cls' <- getRequiredClass "VZAudioOutputStreamSink"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZAudioOutputStreamSink vzAudioOutputStreamSink => vzAudioOutputStreamSink -> IO (Id VZAudioOutputStreamSink)
init_ vzAudioOutputStreamSink  =
  sendMsg vzAudioOutputStreamSink (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

