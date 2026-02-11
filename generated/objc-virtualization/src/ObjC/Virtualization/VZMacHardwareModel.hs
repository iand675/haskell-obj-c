{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a specific virtual Mac hardware model.
--
-- The Mac hardware model abstracts a set of virtualized hardware elements and configurations.    A version of macOS may only run on certain hardware models. The host may also only provide certain hardware models    based on the version of macOS and the underlying hardware.    Use VZMacHardwareModel.supported to know if a hardware model is supported on the current host.
--
-- Choosing the hardware model starts from a restore image with VZMacOSRestoreImage.    A restore image describes its supported configuration requirements through VZMacOSRestoreImage.mostFeaturefulSupportedConfiguration.    A configuration requirements object has a corresponding hardware model that can be used to configure a virtual machine    that meets the requirements.
--
-- Once the hardware model is obtained, use VZMacPlatformConfiguration.hardwareModel to configure the Mac platform,    and -[VZMacAuxiliaryStorage initCreatingStorageAtURL:hardwareModel:options:error:] to create its auxiliary storage.    Once the virtual machine is created, use VZMacOSInstaller to install macOS on it.
--
-- If the virtual machine is preserved on disk, the hardware model used for installation should be preserved for subsequent boots.    The VZMacHardwareModel.dataRepresentation property provides a unique binary representation that can be serialized.    The hardware model then can be recreated from the binary representation with -[VZMacHardwareModel initWithDataRepresentation:].
--
-- VZMacOSInstaller
--
-- VZMacOSRestoreImage
--
-- Generated bindings for @VZMacHardwareModel@.
module ObjC.Virtualization.VZMacHardwareModel
  ( VZMacHardwareModel
  , IsVZMacHardwareModel(..)
  , new
  , init_
  , initWithDataRepresentation
  , dataRepresentation
  , supported
  , newSelector
  , initSelector
  , initWithDataRepresentationSelector
  , dataRepresentationSelector
  , supportedSelector


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
new :: IO (Id VZMacHardwareModel)
new  =
  do
    cls' <- getRequiredClass "VZMacHardwareModel"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVZMacHardwareModel vzMacHardwareModel => vzMacHardwareModel -> IO (Id VZMacHardwareModel)
init_ vzMacHardwareModel  =
  sendMsg vzMacHardwareModel (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Get the hardware model described by the specified data representation.
--
-- @dataRepresentation@ — The opaque data representation of the hardware model to be obtained.
--
-- ObjC selector: @- initWithDataRepresentation:@
initWithDataRepresentation :: (IsVZMacHardwareModel vzMacHardwareModel, IsNSData dataRepresentation) => vzMacHardwareModel -> dataRepresentation -> IO (Id VZMacHardwareModel)
initWithDataRepresentation vzMacHardwareModel  dataRepresentation =
withObjCPtr dataRepresentation $ \raw_dataRepresentation ->
    sendMsg vzMacHardwareModel (mkSelector "initWithDataRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_dataRepresentation :: Ptr ())] >>= ownedObject . castPtr

-- | Opaque data representation of the hardware model.
--
-- This can be used to recreate the same hardware model with -[VZMacHardwareModel initWithDataRepresentation:].
--
-- ObjC selector: @- dataRepresentation@
dataRepresentation :: IsVZMacHardwareModel vzMacHardwareModel => vzMacHardwareModel -> IO (Id NSData)
dataRepresentation vzMacHardwareModel  =
  sendMsg vzMacHardwareModel (mkSelector "dataRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicate whether this hardware model is supported by the host.
--
-- If this hardware model is not supported by the host, no VZVirtualMachineConfiguration using it will validate.    The validation error of the VZVirtualMachineConfiguration provides more information about why the hardware model is unsupported.
--
-- ObjC selector: @- supported@
supported :: IsVZMacHardwareModel vzMacHardwareModel => vzMacHardwareModel -> IO Bool
supported vzMacHardwareModel  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzMacHardwareModel (mkSelector "supported") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDataRepresentation:@
initWithDataRepresentationSelector :: Selector
initWithDataRepresentationSelector = mkSelector "initWithDataRepresentation:"

-- | @Selector@ for @dataRepresentation@
dataRepresentationSelector :: Selector
dataRepresentationSelector = mkSelector "dataRepresentation"

-- | @Selector@ for @supported@
supportedSelector :: Selector
supportedSelector = mkSelector "supported"

