{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Configuration for a Mac keyboard.
--
-- This device can be used by VZVirtualMachineView to send keyboard events to the virtual machine.    This keyboard supports Apple-specific features such as the globe key.    Note: this device is only recognized by virtual machines running macOS 13.0 and later. In order to support both macOS 13.0 and earlier    guests, VZVirtualMachineConfiguration.keyboards can be set to an array containing both a VZMacKeyboardConfiguration and    a VZUSBKeyboardConfiguration object. macOS 13.0 and later guests will use the Mac keyboard device,    while earlier versions of macOS will use the USB keyboard device.
--
-- Generated bindings for @VZMacKeyboardConfiguration@.
module ObjC.Virtualization.VZMacKeyboardConfiguration
  ( VZMacKeyboardConfiguration
  , IsVZMacKeyboardConfiguration(..)
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
init_ :: IsVZMacKeyboardConfiguration vzMacKeyboardConfiguration => vzMacKeyboardConfiguration -> IO (Id VZMacKeyboardConfiguration)
init_ vzMacKeyboardConfiguration  =
  sendMsg vzMacKeyboardConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

