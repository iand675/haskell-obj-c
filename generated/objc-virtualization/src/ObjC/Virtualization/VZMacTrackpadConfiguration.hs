{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a Mac trackpad.
--
-- This device can be used by VZVirtualMachineView to send pointer events and multi-touch trackpad gestures to the virtual machine.    Note: this device is only recognized by virtual machines running macOS 13.0 and later. In order to support both macOS 13.0 and earlier    guests, VZVirtualMachineConfiguration.pointingDevices can be set to an array containing both a VZMacTrackpadConfiguration and    a VZUSBScreenCoordinatePointingDeviceConfiguration object. macOS 13.0 and later guests will use the multi-touch trackpad device,    while earlier versions of macOS will use the USB pointing device.
--
-- Generated bindings for @VZMacTrackpadConfiguration@.
module ObjC.Virtualization.VZMacTrackpadConfiguration
  ( VZMacTrackpadConfiguration
  , IsVZMacTrackpadConfiguration(..)
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
init_ :: IsVZMacTrackpadConfiguration vzMacTrackpadConfiguration => vzMacTrackpadConfiguration -> IO (Id VZMacTrackpadConfiguration)
init_ vzMacTrackpadConfiguration  =
  sendMsg vzMacTrackpadConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

