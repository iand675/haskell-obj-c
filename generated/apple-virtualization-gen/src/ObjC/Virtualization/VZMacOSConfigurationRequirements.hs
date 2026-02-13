{-# LANGUAGE DataKinds #-}
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
  , hardwareModelSelector
  , initSelector
  , minimumSupportedCPUCountSelector
  , minimumSupportedMemorySizeSelector
  , newSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZMacOSConfigurationRequirements)
new  =
  do
    cls' <- getRequiredClass "VZMacOSConfigurationRequirements"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO (Id VZMacOSConfigurationRequirements)
init_ vzMacOSConfigurationRequirements =
  sendOwnedMessage vzMacOSConfigurationRequirements initSelector

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
hardwareModel vzMacOSConfigurationRequirements =
  sendMessage vzMacOSConfigurationRequirements hardwareModelSelector

-- | The minimum supported number of CPUs for this configuration.
--
-- A VZMacOSConfigurationRequirements object is associated with a specific VZMacOSRestoreImage object, and thus a specific macOS configuration.    This property specifies the minimum number of CPUs required by the associated macOS configuration.    Installing or running the associated configuration of macOS on a virtual machine with fewer than this number of CPUs will result in undefined behavior.
--
-- ObjC selector: @- minimumSupportedCPUCount@
minimumSupportedCPUCount :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO CULong
minimumSupportedCPUCount vzMacOSConfigurationRequirements =
  sendMessage vzMacOSConfigurationRequirements minimumSupportedCPUCountSelector

-- | The minimum supported memory size for this configuration.
--
-- A VZMacOSConfigurationRequirements object is associated with a specific VZMacOSRestoreImage object, and thus a specific macOS configuration.    This property specifies the minimum amount of memory required by the associated macOS configuration.    Installing or running the associated configuration of macOS on a virtual machine with less than this amount of memory will result in undefined behavior.
--
-- ObjC selector: @- minimumSupportedMemorySize@
minimumSupportedMemorySize :: IsVZMacOSConfigurationRequirements vzMacOSConfigurationRequirements => vzMacOSConfigurationRequirements -> IO CULong
minimumSupportedMemorySize vzMacOSConfigurationRequirements =
  sendMessage vzMacOSConfigurationRequirements minimumSupportedMemorySizeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZMacOSConfigurationRequirements)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacOSConfigurationRequirements)
initSelector = mkSelector "init"

-- | @Selector@ for @hardwareModel@
hardwareModelSelector :: Selector '[] (Id VZMacHardwareModel)
hardwareModelSelector = mkSelector "hardwareModel"

-- | @Selector@ for @minimumSupportedCPUCount@
minimumSupportedCPUCountSelector :: Selector '[] CULong
minimumSupportedCPUCountSelector = mkSelector "minimumSupportedCPUCount"

-- | @Selector@ for @minimumSupportedMemorySize@
minimumSupportedMemorySizeSelector :: Selector '[] CULong
minimumSupportedMemorySizeSelector = mkSelector "minimumSupportedMemorySize"

