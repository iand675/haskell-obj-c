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
init_ :: IsVZHostAudioInputStreamSource vzHostAudioInputStreamSource => vzHostAudioInputStreamSource -> IO (Id VZHostAudioInputStreamSource)
init_ vzHostAudioInputStreamSource  =
  sendMsg vzHostAudioInputStreamSource (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

