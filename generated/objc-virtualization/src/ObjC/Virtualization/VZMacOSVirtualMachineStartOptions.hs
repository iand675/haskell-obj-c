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
  , startUpFromMacOSRecoverySelector
  , setStartUpFromMacOSRecoverySelector


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

-- | Whether to start up from macOS Recovery.
--
-- ObjC selector: @- startUpFromMacOSRecovery@
startUpFromMacOSRecovery :: IsVZMacOSVirtualMachineStartOptions vzMacOSVirtualMachineStartOptions => vzMacOSVirtualMachineStartOptions -> IO Bool
startUpFromMacOSRecovery vzMacOSVirtualMachineStartOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzMacOSVirtualMachineStartOptions (mkSelector "startUpFromMacOSRecovery") retCULong []

-- | Whether to start up from macOS Recovery.
--
-- ObjC selector: @- setStartUpFromMacOSRecovery:@
setStartUpFromMacOSRecovery :: IsVZMacOSVirtualMachineStartOptions vzMacOSVirtualMachineStartOptions => vzMacOSVirtualMachineStartOptions -> Bool -> IO ()
setStartUpFromMacOSRecovery vzMacOSVirtualMachineStartOptions  value =
  sendMsg vzMacOSVirtualMachineStartOptions (mkSelector "setStartUpFromMacOSRecovery:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startUpFromMacOSRecovery@
startUpFromMacOSRecoverySelector :: Selector
startUpFromMacOSRecoverySelector = mkSelector "startUpFromMacOSRecovery"

-- | @Selector@ for @setStartUpFromMacOSRecovery:@
setStartUpFromMacOSRecoverySelector :: Selector
setStartUpFromMacOSRecoverySelector = mkSelector "setStartUpFromMacOSRecovery:"

