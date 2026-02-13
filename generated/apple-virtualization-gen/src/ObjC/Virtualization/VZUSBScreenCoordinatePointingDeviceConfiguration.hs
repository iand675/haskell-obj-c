{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a USB pointing device that reports absolute coordinates.
--
-- This device can be used by VZVirtualMachineView to send pointer events to the virtual machine.
--
-- Generated bindings for @VZUSBScreenCoordinatePointingDeviceConfiguration@.
module ObjC.Virtualization.VZUSBScreenCoordinatePointingDeviceConfiguration
  ( VZUSBScreenCoordinatePointingDeviceConfiguration
  , IsVZUSBScreenCoordinatePointingDeviceConfiguration(..)
  , init_
  , initSelector


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
init_ :: IsVZUSBScreenCoordinatePointingDeviceConfiguration vzusbScreenCoordinatePointingDeviceConfiguration => vzusbScreenCoordinatePointingDeviceConfiguration -> IO (Id VZUSBScreenCoordinatePointingDeviceConfiguration)
init_ vzusbScreenCoordinatePointingDeviceConfiguration =
  sendOwnedMessage vzusbScreenCoordinatePointingDeviceConfiguration initSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZUSBScreenCoordinatePointingDeviceConfiguration)
initSelector = mkSelector "init"

