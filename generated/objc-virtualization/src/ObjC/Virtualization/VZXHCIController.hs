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

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

