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
init_ :: IsVZUSBScreenCoordinatePointingDeviceConfiguration vzusbScreenCoordinatePointingDeviceConfiguration => vzusbScreenCoordinatePointingDeviceConfiguration -> IO (Id VZUSBScreenCoordinatePointingDeviceConfiguration)
init_ vzusbScreenCoordinatePointingDeviceConfiguration  =
  sendMsg vzusbScreenCoordinatePointingDeviceConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

