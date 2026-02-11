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
  , initSelector
  , hardwareModelSelector
  , setHardwareModelSelector
  , machineIdentifierSelector
  , setMachineIdentifierSelector
  , auxiliaryStorageSelector
  , setAuxiliaryStorageSelector


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
init_ :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacPlatformConfiguration)
init_ vzMacPlatformConfiguration  =
  sendMsg vzMacPlatformConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The Mac hardware model.
--
-- ObjC selector: @- hardwareModel@
hardwareModel :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacHardwareModel)
hardwareModel vzMacPlatformConfiguration  =
  sendMsg vzMacPlatformConfiguration (mkSelector "hardwareModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Mac hardware model.
--
-- ObjC selector: @- setHardwareModel:@
setHardwareModel :: (IsVZMacPlatformConfiguration vzMacPlatformConfiguration, IsVZMacHardwareModel value) => vzMacPlatformConfiguration -> value -> IO ()
setHardwareModel vzMacPlatformConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzMacPlatformConfiguration (mkSelector "setHardwareModel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The unique Mac machine identifier.
--
-- Running two virtual machines concurrently with the same identifier results in undefined behavior in the guest operating system.
--
-- ObjC selector: @- machineIdentifier@
machineIdentifier :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacMachineIdentifier)
machineIdentifier vzMacPlatformConfiguration  =
  sendMsg vzMacPlatformConfiguration (mkSelector "machineIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The unique Mac machine identifier.
--
-- Running two virtual machines concurrently with the same identifier results in undefined behavior in the guest operating system.
--
-- ObjC selector: @- setMachineIdentifier:@
setMachineIdentifier :: (IsVZMacPlatformConfiguration vzMacPlatformConfiguration, IsVZMacMachineIdentifier value) => vzMacPlatformConfiguration -> value -> IO ()
setMachineIdentifier vzMacPlatformConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzMacPlatformConfiguration (mkSelector "setMachineIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The Mac auxiliary storage.
--
-- When creating a virtual machine from scratch, the hardware model of the @auxiliaryStorage@ must match the hardware model of    the @hardwareModel@ property.
--
-- ObjC selector: @- auxiliaryStorage@
auxiliaryStorage :: IsVZMacPlatformConfiguration vzMacPlatformConfiguration => vzMacPlatformConfiguration -> IO (Id VZMacAuxiliaryStorage)
auxiliaryStorage vzMacPlatformConfiguration  =
  sendMsg vzMacPlatformConfiguration (mkSelector "auxiliaryStorage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The Mac auxiliary storage.
--
-- When creating a virtual machine from scratch, the hardware model of the @auxiliaryStorage@ must match the hardware model of    the @hardwareModel@ property.
--
-- ObjC selector: @- setAuxiliaryStorage:@
setAuxiliaryStorage :: (IsVZMacPlatformConfiguration vzMacPlatformConfiguration, IsVZMacAuxiliaryStorage value) => vzMacPlatformConfiguration -> value -> IO ()
setAuxiliaryStorage vzMacPlatformConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzMacPlatformConfiguration (mkSelector "setAuxiliaryStorage:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @hardwareModel@
hardwareModelSelector :: Selector
hardwareModelSelector = mkSelector "hardwareModel"

-- | @Selector@ for @setHardwareModel:@
setHardwareModelSelector :: Selector
setHardwareModelSelector = mkSelector "setHardwareModel:"

-- | @Selector@ for @machineIdentifier@
machineIdentifierSelector :: Selector
machineIdentifierSelector = mkSelector "machineIdentifier"

-- | @Selector@ for @setMachineIdentifier:@
setMachineIdentifierSelector :: Selector
setMachineIdentifierSelector = mkSelector "setMachineIdentifier:"

-- | @Selector@ for @auxiliaryStorage@
auxiliaryStorageSelector :: Selector
auxiliaryStorageSelector = mkSelector "auxiliaryStorage"

-- | @Selector@ for @setAuxiliaryStorage:@
setAuxiliaryStorageSelector :: Selector
setAuxiliaryStorageSelector = mkSelector "setAuxiliaryStorage:"

