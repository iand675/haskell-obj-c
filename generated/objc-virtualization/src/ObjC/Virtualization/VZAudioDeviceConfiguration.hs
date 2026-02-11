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
new :: IO (Id VZAudioDeviceConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZAudioDeviceConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZAudioDeviceConfiguration vzAudioDeviceConfiguration => vzAudioDeviceConfiguration -> IO (Id VZAudioDeviceConfiguration)
init_ vzAudioDeviceConfiguration  =
  sendMsg vzAudioDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

