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

-- | @- init@
init_ :: IsVZHostAudioOutputStreamSink vzHostAudioOutputStreamSink => vzHostAudioOutputStreamSink -> IO (Id VZHostAudioOutputStreamSink)
init_ vzHostAudioOutputStreamSink  =
  sendMsg vzHostAudioOutputStreamSink (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

