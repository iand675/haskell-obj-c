{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration of the Virtio traditional memory balloon device.
--
-- This configuration creates a Virtio traditional memory balloon device which allows for managing guest memory.    Only one Virtio traditional memory balloon device can be used per virtual machine.
--
-- See: VZVirtioTraditionalMemoryBalloonDevice
--
-- Generated bindings for @VZVirtioTraditionalMemoryBalloonDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioTraditionalMemoryBalloonDeviceConfiguration
  ( VZVirtioTraditionalMemoryBalloonDeviceConfiguration
  , IsVZVirtioTraditionalMemoryBalloonDeviceConfiguration(..)
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
init_ :: IsVZVirtioTraditionalMemoryBalloonDeviceConfiguration vzVirtioTraditionalMemoryBalloonDeviceConfiguration => vzVirtioTraditionalMemoryBalloonDeviceConfiguration -> IO (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration)
init_ vzVirtioTraditionalMemoryBalloonDeviceConfiguration  =
  sendMsg vzVirtioTraditionalMemoryBalloonDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

