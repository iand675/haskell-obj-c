{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a Virtio graphics device.
--
-- This device configuration creates a graphics device using paravirtualization.    The emulated device follows the Virtio GPU Device specification.
--
-- This device can be used to attach a display to be shown in a VZVirtualMachineView.
--
-- Generated bindings for @VZVirtioGraphicsDeviceConfiguration@.
module ObjC.Virtualization.VZVirtioGraphicsDeviceConfiguration
  ( VZVirtioGraphicsDeviceConfiguration
  , IsVZVirtioGraphicsDeviceConfiguration(..)
  , init_
  , scanouts
  , setScanouts
  , initSelector
  , scanoutsSelector
  , setScanoutsSelector


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
init_ :: IsVZVirtioGraphicsDeviceConfiguration vzVirtioGraphicsDeviceConfiguration => vzVirtioGraphicsDeviceConfiguration -> IO (Id VZVirtioGraphicsDeviceConfiguration)
init_ vzVirtioGraphicsDeviceConfiguration  =
  sendMsg vzVirtioGraphicsDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The scanouts to be attached to this graphics device.
--
-- Maximum of one scanout is supported.
--
-- ObjC selector: @- scanouts@
scanouts :: IsVZVirtioGraphicsDeviceConfiguration vzVirtioGraphicsDeviceConfiguration => vzVirtioGraphicsDeviceConfiguration -> IO (Id NSArray)
scanouts vzVirtioGraphicsDeviceConfiguration  =
  sendMsg vzVirtioGraphicsDeviceConfiguration (mkSelector "scanouts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The scanouts to be attached to this graphics device.
--
-- Maximum of one scanout is supported.
--
-- ObjC selector: @- setScanouts:@
setScanouts :: (IsVZVirtioGraphicsDeviceConfiguration vzVirtioGraphicsDeviceConfiguration, IsNSArray value) => vzVirtioGraphicsDeviceConfiguration -> value -> IO ()
setScanouts vzVirtioGraphicsDeviceConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtioGraphicsDeviceConfiguration (mkSelector "setScanouts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @scanouts@
scanoutsSelector :: Selector
scanoutsSelector = mkSelector "scanouts"

-- | @Selector@ for @setScanouts:@
setScanoutsSelector :: Selector
setScanoutsSelector = mkSelector "setScanouts:"

