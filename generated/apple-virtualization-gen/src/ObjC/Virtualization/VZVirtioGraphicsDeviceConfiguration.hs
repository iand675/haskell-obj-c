{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsVZVirtioGraphicsDeviceConfiguration vzVirtioGraphicsDeviceConfiguration => vzVirtioGraphicsDeviceConfiguration -> IO (Id VZVirtioGraphicsDeviceConfiguration)
init_ vzVirtioGraphicsDeviceConfiguration =
  sendOwnedMessage vzVirtioGraphicsDeviceConfiguration initSelector

-- | The scanouts to be attached to this graphics device.
--
-- Maximum of one scanout is supported.
--
-- ObjC selector: @- scanouts@
scanouts :: IsVZVirtioGraphicsDeviceConfiguration vzVirtioGraphicsDeviceConfiguration => vzVirtioGraphicsDeviceConfiguration -> IO (Id NSArray)
scanouts vzVirtioGraphicsDeviceConfiguration =
  sendMessage vzVirtioGraphicsDeviceConfiguration scanoutsSelector

-- | The scanouts to be attached to this graphics device.
--
-- Maximum of one scanout is supported.
--
-- ObjC selector: @- setScanouts:@
setScanouts :: (IsVZVirtioGraphicsDeviceConfiguration vzVirtioGraphicsDeviceConfiguration, IsNSArray value) => vzVirtioGraphicsDeviceConfiguration -> value -> IO ()
setScanouts vzVirtioGraphicsDeviceConfiguration value =
  sendMessage vzVirtioGraphicsDeviceConfiguration setScanoutsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZVirtioGraphicsDeviceConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @scanouts@
scanoutsSelector :: Selector '[] (Id NSArray)
scanoutsSelector = mkSelector "scanouts"

-- | @Selector@ for @setScanouts:@
setScanoutsSelector :: Selector '[Id NSArray] ()
setScanoutsSelector = mkSelector "setScanouts:"

