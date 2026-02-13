{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Options controlling startup behavior of a virtual machine using VZMacOSBootLoader.
--
-- Generated bindings for @VZMacOSVirtualMachineStartOptions@.
module ObjC.Virtualization.VZMacOSVirtualMachineStartOptions
  ( VZMacOSVirtualMachineStartOptions
  , IsVZMacOSVirtualMachineStartOptions(..)
  , startUpFromMacOSRecovery
  , setStartUpFromMacOSRecovery
  , setStartUpFromMacOSRecoverySelector
  , startUpFromMacOSRecoverySelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Whether to start up from macOS Recovery.
--
-- ObjC selector: @- startUpFromMacOSRecovery@
startUpFromMacOSRecovery :: IsVZMacOSVirtualMachineStartOptions vzMacOSVirtualMachineStartOptions => vzMacOSVirtualMachineStartOptions -> IO Bool
startUpFromMacOSRecovery vzMacOSVirtualMachineStartOptions =
  sendMessage vzMacOSVirtualMachineStartOptions startUpFromMacOSRecoverySelector

-- | Whether to start up from macOS Recovery.
--
-- ObjC selector: @- setStartUpFromMacOSRecovery:@
setStartUpFromMacOSRecovery :: IsVZMacOSVirtualMachineStartOptions vzMacOSVirtualMachineStartOptions => vzMacOSVirtualMachineStartOptions -> Bool -> IO ()
setStartUpFromMacOSRecovery vzMacOSVirtualMachineStartOptions value =
  sendMessage vzMacOSVirtualMachineStartOptions setStartUpFromMacOSRecoverySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startUpFromMacOSRecovery@
startUpFromMacOSRecoverySelector :: Selector '[] Bool
startUpFromMacOSRecoverySelector = mkSelector "startUpFromMacOSRecovery"

-- | @Selector@ for @setStartUpFromMacOSRecovery:@
setStartUpFromMacOSRecoverySelector :: Selector '[Bool] ()
setStartUpFromMacOSRecoverySelector = mkSelector "setStartUpFromMacOSRecovery:"

