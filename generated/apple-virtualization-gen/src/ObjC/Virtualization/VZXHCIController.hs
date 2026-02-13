{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Class representing a USB XHCI controller in a virtual machine.
--
-- VZXHCIController should not be instantiated directly.    In order to create a runtime VZXHCIController object, the usbControllers property of    VZVirtualMachineConfiguration object needs to be populated with VZXHCIControllerConfiguration object.
--
-- See: VZUSBController
--
-- See: VZXHCIControllerConfiguration
--
-- Generated bindings for @VZXHCIController@.
module ObjC.Virtualization.VZXHCIController
  ( VZXHCIController
  , IsVZXHCIController(..)


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

