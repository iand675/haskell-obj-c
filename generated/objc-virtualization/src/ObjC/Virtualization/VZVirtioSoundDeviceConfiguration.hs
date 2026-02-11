{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtio Sound Device Configuration.
--
-- The device exposes a source or destination of sound.
--
-- Generated bindings for @VZVirtioSoundDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioSoundDeviceConfiguration
  ( VZVirtioSoundDeviceConfiguration
  , IsVZVirtioSoundDeviceConfiguration(..)
  , init_
  , streams
  , setStreams
  , initSelector
  , streamsSelector
  , setStreamsSelector


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
init_ :: IsVZVirtioSoundDeviceConfiguration vzVirtioSoundDeviceConfiguration => vzVirtioSoundDeviceConfiguration -> IO (Id VZVirtioSoundDeviceConfiguration)
init_ vzVirtioSoundDeviceConfiguration  =
  sendMsg vzVirtioSoundDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | List of audio streams exposed by this device. Empty by default.
--
-- ObjC selector: @- streams@
streams :: IsVZVirtioSoundDeviceConfiguration vzVirtioSoundDeviceConfiguration => vzVirtioSoundDeviceConfiguration -> IO (Id NSArray)
streams vzVirtioSoundDeviceConfiguration  =
  sendMsg vzVirtioSoundDeviceConfiguration (mkSelector "streams") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of audio streams exposed by this device. Empty by default.
--
-- ObjC selector: @- setStreams:@
setStreams :: (IsVZVirtioSoundDeviceConfiguration vzVirtioSoundDeviceConfiguration, IsNSArray value) => vzVirtioSoundDeviceConfiguration -> value -> IO ()
setStreams vzVirtioSoundDeviceConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioSoundDeviceConfiguration (mkSelector "setStreams:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @streams@
streamsSelector :: Selector
streamsSelector = mkSelector "streams"

-- | @Selector@ for @setStreams:@
setStreamsSelector :: Selector
setStreamsSelector = mkSelector "setStreams:"

