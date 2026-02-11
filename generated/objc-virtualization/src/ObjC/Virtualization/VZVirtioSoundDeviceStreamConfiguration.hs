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
new :: IO (Id VZVirtioSoundDeviceStreamConfiguration)
new  =
  do
    cls' <- getRequiredClass "VZVirtioSoundDeviceStreamConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZVirtioSoundDeviceStreamConfiguration vzVirtioSoundDeviceStreamConfiguration => vzVirtioSoundDeviceStreamConfiguration -> IO (Id VZVirtioSoundDeviceStreamConfiguration)
init_ vzVirtioSoundDeviceStreamConfiguration  =
  sendMsg vzVirtioSoundDeviceStreamConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

