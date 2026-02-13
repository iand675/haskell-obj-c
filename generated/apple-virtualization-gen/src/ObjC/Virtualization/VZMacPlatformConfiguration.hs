{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The platform configuration for booting macOS on Apple Silicon.
--
-- When creating a virtual machine from scratch, the @hardwareModel@ and @auxiliaryStorage@ depend on the restore image    that will be used to install macOS.
--
-- To choose the hardware model, start from VZMacOSRestoreImage.mostFeaturefulSupportedConfiguration to get a supported configuration, then    use its VZMacOSConfigurationRequirements.hardwareModel property to get the hardware model.    Use the hardware model to set up VZMacPlatformConfiguration and to initialize a new auxiliary storage with    -[VZMacAuxiliaryStorage initCreatingStorageAtURL:hardwareModel:options:error:].
--
-- When a virtual machine is saved to disk then loaded again, the @hardwareModel@, @machineIdentifier@ and @auxiliaryStorage@    must be restored to their original values.
--
-- If multiple virtual machines are created from the same configuration, each should have a unique  @auxiliaryStorage@ and @machineIdentifier@.
--
-- VZMacOSRestoreImage
--
-- VZMacOSConfigurationRequirements
--
-- Generated bindings for @VZMacPlatformConfiguration@.
module ObjC.Virtualization.VZMacPlatformConfiguration
  ( VZMacPlatformConfiguration
  , IsVZMacPlatformConfiguration(..)
  , init_
  , hardwareModel
  , setHardwareModel
  , machineIdentifier
  , setMachineIdentifier
  , auxiliaryStorage
  , setAuxiliaryStorage
  , auxiliaryStorageSelector
  , hardwareModelSelector
  , initSelector
  , machineIdentifierSelector
  , setAuxiliaryStorageSelector
  , setHardwareModelSelector
  , setMachineIdentifierSelector


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
init_ :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacPlatformConfiguration)
init_ vzMacPlatformConfiguration =
  sendOwnedMessage vzMacPlatformConfiguration initSelector

-- | The Mac hardware model.
--
-- ObjC selector: @- hardwareModel@
hardwareModel :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacHardwareModel)
hardwareModel vzMacPlatformConfiguration =
  sendMessage vzMacPlatformConfiguration hardwareModelSelector

-- | The Mac hardware model.
--
-- ObjC selector: @- setHardwareModel:@
setHardwareModel :: (IsVZMacPlatformConfiguration vzMacPlatformConfiguration, IsVZMacHardwareModel value) => vzMacPlatformConfiguration -> value -> IO ()
setHardwareModel vzMacPlatformConfiguration value =
  sendMessage vzMacPlatformConfiguration setHardwareModelSelector (toVZMacHardwareModel value)

-- | The unique Mac machine identifier.
--
-- Running two virtual machines concurrently with the same identifier results in undefined behavior in the guest operating system.
--
-- ObjC selector: @- machineIdentifier@
machineIdentifier :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacMachineIdentifier)
machineIdentifier vzMacPlatformConfiguration =
  sendMessage vzMacPlatformConfiguration machineIdentifierSelector

-- | The unique Mac machine identifier.
--
-- Running two virtual machines concurrently with the same identifier results in undefined behavior in the guest operating system.
--
-- ObjC selector: @- setMachineIdentifier:@
setMachineIdentifier :: (IsVZMacPlatformConfiguration vzMacPlatformConfiguration, IsVZMacMachineIdentifier value) => vzMacPlatformConfiguration -> value -> IO ()
setMachineIdentifier vzMacPlatformConfiguration value =
  sendMessage vzMacPlatformConfiguration setMachineIdentifierSelector (toVZMacMachineIdentifier value)

-- | The Mac auxiliary storage.
--
-- When creating a virtual machine from scratch, the hardware model of the @auxiliaryStorage@ must match the hardware model of    the @hardwareModel@ property.
--
-- ObjC selector: @- auxiliaryStorage@
auxiliaryStorage :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacAuxiliaryStorage)
auxiliaryStorage vzMacPlatformConfiguration =
  sendMessage vzMacPlatformConfiguration auxiliaryStorageSelector

-- | The Mac auxiliary storage.
--
-- When creating a virtual machine from scratch, the hardware model of the @auxiliaryStorage@ must match the hardware model of    the @hardwareModel@ property.
--
-- ObjC selector: @- setAuxiliaryStorage:@
setAuxiliaryStorage :: (IsVZMacPlatformConfiguration vzMacPlatformConfiguration, IsVZMacAuxiliaryStorage value) => vzMacPlatformConfiguration -> value -> IO ()
setAuxiliaryStorage vzMacPlatformConfiguration value =
  sendMessage vzMacPlatformConfiguration setAuxiliaryStorageSelector (toVZMacAuxiliaryStorage value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacPlatformConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @hardwareModel@
hardwareModelSelector :: Selector '[] (Id VZMacHardwareModel)
hardwareModelSelector = mkSelector "hardwareModel"

-- | @Selector@ for @setHardwareModel:@
setHardwareModelSelector :: Selector '[Id VZMacHardwareModel] ()
setHardwareModelSelector = mkSelector "setHardwareModel:"

-- | @Selector@ for @machineIdentifier@
machineIdentifierSelector :: Selector '[] (Id VZMacMachineIdentifier)
machineIdentifierSelector = mkSelector "machineIdentifier"

-- | @Selector@ for @setMachineIdentifier:@
setMachineIdentifierSelector :: Selector '[Id VZMacMachineIdentifier] ()
setMachineIdentifierSelector = mkSelector "setMachineIdentifier:"

-- | @Selector@ for @auxiliaryStorage@
auxiliaryStorageSelector :: Selector '[] (Id VZMacAuxiliaryStorage)
auxiliaryStorageSelector = mkSelector "auxiliaryStorage"

-- | @Selector@ for @setAuxiliaryStorage:@
setAuxiliaryStorageSelector :: Selector '[Id VZMacAuxiliaryStorage] ()
setAuxiliaryStorageSelector = mkSelector "setAuxiliaryStorage:"

