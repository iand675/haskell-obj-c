{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VZMacOSConfigurationRequirements describes the parameter constraints required by a specific configuration of macOS.
--
-- When a VZMacOSRestoreImage is loaded, it can be inspected to determine the configurations supported by that restore image.
--
-- VZMacHardwareModel
--
-- VZMacOSRestoreImage
--
-- Generated bindings for @VZMacOSConfigurationRequirements@.
module ObjC.Virtualization.VZMacOSConfigurationRequirements
  ( VZMacOSConfigurationRequirements
  , IsVZMacOSConfigurationRequirements(..)
  , new
  , init_
  , hardwareModel
  , minimumSupportedCPUCount
  , minimumSupportedMemorySize
  , newSelector
  , initSelector
  , hardwareModelSelector
  , minimumSupportedCPUCountSelector
  , minimumSupportedMemorySizeSelector


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

-- | @+ new@
new :: IO (Id VZMacOSConfigurationRequirements)
new  =
  do
    cls' <- getRequiredClass "VZMacOSConfigurationRequirements"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO (Id VZMacOSConfigurationRequirements)
init_ vzMacOSConfigurationRequirements  =
  sendMsg vzMacOSConfigurationRequirements (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The hardware model for this configuration.
--
-- The hardware model can be used to configure a new virtual machine that meets the requirements.    Use VZMacPlatformConfiguration.hardwareModel to configure the Mac platform, and -[VZMacAuxiliaryStorage initCreatingStorageAtURL:hardwareModel:options:error:] to create its auxiliary storage.
--
-- VZMacPlatformConfiguration
--
-- VZMacAuxiliaryStorage
--
-- ObjC selector: @- hardwareModel@
hardwareModel :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO (Id VZMacHardwareModel)
hardwareModel vzMacOSConfigurationRequirements  =
  sendMsg vzMacOSConfigurationRequirements (mkSelector "hardwareModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The minimum supported number of CPUs for this configuration.
--
-- A VZMacOSConfigurationRequirements object is associated with a specific VZMacOSRestoreImage object, and thus a specific macOS configuration.    This property specifies the minimum number of CPUs required by the associated macOS configuration.    Installing or running the associated configuration of macOS on a virtual machine with fewer than this number of CPUs will result in undefined behavior.
--
-- ObjC selector: @- minimumSupportedCPUCount@
minimumSupportedCPUCount :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO CULong
minimumSupportedCPUCount vzMacOSConfigurationRequirements  =
  sendMsg vzMacOSConfigurationRequirements (mkSelector "minimumSupportedCPUCount") retCULong []

-- | The minimum supported memory size for this configuration.
--
-- A VZMacOSConfigurationRequirements object is associated with a specific VZMacOSRestoreImage object, and thus a specific macOS configuration.    This property specifies the minimum amount of memory required by the associated macOS configuration.    Installing or running the associated configuration of macOS on a virtual machine with less than this amount of memory will result in undefined behavior.
--
-- ObjC selector: @- minimumSupportedMemorySize@
minimumSupportedMemorySize :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO CULong
minimumSupportedMemorySize vzMacOSConfigurationRequirements  =
  sendMsg vzMacOSConfigurationRequirements (mkSelector "minimumSupportedMemorySize") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @hardwareModel@
hardwareModelSelector :: Selector
hardwareModelSelector = mkSelector "hardwareModel"

-- | @Selector@ for @minimumSupportedCPUCount@
minimumSupportedCPUCountSelector :: Selector
minimumSupportedCPUCountSelector = mkSelector "minimumSupportedCPUCount"

-- | @Selector@ for @minimumSupportedMemorySize@
minimumSupportedMemorySizeSelector :: Selector
minimumSupportedMemorySizeSelector = mkSelector "minimumSupportedMemorySize"

