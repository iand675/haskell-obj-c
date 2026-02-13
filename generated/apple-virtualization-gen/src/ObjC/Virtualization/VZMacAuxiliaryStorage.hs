{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Mac auxiliary storage.
--
-- The Mac auxiliary storage contains data used by the boot loader and the guest operating system. It is necessary to boot a macOS guest OS.
--
-- When creating a new virtual machine from scratch, VZMacOSInstaller can use a default initialized auxiliary storage.    Use -[VZMacAuxiliaryStorage initCreatingStorageAtURL:hardwareModel:options:error:] to create an empty auxiliary storage.
--
-- The hardware model used when creating the new auxiliary storage depends on the restore image that will be used for installation.    From the restore image, use VZMacOSRestoreImage.mostFeaturefulSupportedConfiguration to get a supported configuration.    A configuration has a VZMacHardwareModel associated with it.
--
-- After initializing the new auxiliary storage, set it on VZMacPlatformConfiguration.auxiliaryStorage to use it.    The hardware model in VZMacPlatformConfiguration.hardwareModel must be identical to the one used to create the empty    auxiliary storage. The behavior is undefined otherwise.
--
-- When installing macOS, the VZMacOSInstaller lays out data on the auxiliary storage.    After installation, the macOS guest uses the auxiliary storage for every subsequent boot.
--
-- When moving or doing a backup of a virtual machine, the file containing the auxiliary storage must also be moved    or copied along with the main disk image.
--
-- To boot a virtual machine that has already been installed with VZMacOSInstaller, use -[VZMacAuxiliaryStorage initWithContentsOfURL:]    to set up the auxiliary storage from the existing file used at installation.    When using an existing file, the hardware model of the VZMacPlatformConfiguration must match the hardware model used when    the file was created.
--
-- VZMacPlatformConfiguration
--
-- VZMacOSRestoreImage
--
-- VZMacOSConfigurationRequirements
--
-- VZMacOSInstaller
--
-- Generated bindings for @VZMacAuxiliaryStorage@.
module ObjC.Virtualization.VZMacAuxiliaryStorage
  ( VZMacAuxiliaryStorage
  , IsVZMacAuxiliaryStorage(..)
  , new
  , init_
  , initWithURL
  , initCreatingStorageAtURL_hardwareModel_options_error
  , initWithContentsOfURL
  , url
  , initCreatingStorageAtURL_hardwareModel_options_errorSelector
  , initSelector
  , initWithContentsOfURLSelector
  , initWithURLSelector
  , newSelector
  , urlSelector

  -- * Enum types
  , VZMacAuxiliaryStorageInitializationOptions(VZMacAuxiliaryStorageInitializationOptions)
  , pattern VZMacAuxiliaryStorageInitializationOptionAllowOverwrite

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Virtualization.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VZMacAuxiliaryStorage)
new  =
  do
    cls' <- getRequiredClass "VZMacAuxiliaryStorage"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZMacAuxiliaryStorage vzMacAuxiliaryStorage => vzMacAuxiliaryStorage -> IO (Id VZMacAuxiliaryStorage)
init_ vzMacAuxiliaryStorage =
  sendOwnedMessage vzMacAuxiliaryStorage initSelector

-- | Initialize the auxiliary storage from the URL of an existing file.
--
-- @URL@ — The URL of the auxiliary storage on the local file system.
--
-- To create a new auxiliary storage, use -[VZMacAuxiliaryStorage initCreatingStorageAtURL:hardwareModel:options:error].
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsVZMacAuxiliaryStorage vzMacAuxiliaryStorage, IsNSURL url) => vzMacAuxiliaryStorage -> url -> IO (Id VZMacAuxiliaryStorage)
initWithURL vzMacAuxiliaryStorage url =
  sendOwnedMessage vzMacAuxiliaryStorage initWithURLSelector (toNSURL url)

-- | Write an initialized VZMacAuxiliaryStorage to a URL on a file system.
--
-- @URL@ — The URL to write the auxiliary storage to on the local file system.
--
-- @hardwareModel@ — The hardware model to use. The auxiliary storage can be laid out differently for different hardware models.
--
-- @options@ — Initialization options.
--
-- @error@ — If not nil, used to report errors if creation fails.
--
-- Returns: A newly initialized VZMacAuxiliaryStorage on success. If an error was encountered returns @nil,@ and @error@ contains the error.
--
-- ObjC selector: @- initCreatingStorageAtURL:hardwareModel:options:error:@
initCreatingStorageAtURL_hardwareModel_options_error :: (IsVZMacAuxiliaryStorage vzMacAuxiliaryStorage, IsNSURL url, IsVZMacHardwareModel hardwareModel, IsNSError error_) => vzMacAuxiliaryStorage -> url -> hardwareModel -> VZMacAuxiliaryStorageInitializationOptions -> error_ -> IO (Id VZMacAuxiliaryStorage)
initCreatingStorageAtURL_hardwareModel_options_error vzMacAuxiliaryStorage url hardwareModel options error_ =
  sendOwnedMessage vzMacAuxiliaryStorage initCreatingStorageAtURL_hardwareModel_options_errorSelector (toNSURL url) (toVZMacHardwareModel hardwareModel) options (toNSError error_)

-- | @- initWithContentsOfURL:@
initWithContentsOfURL :: (IsVZMacAuxiliaryStorage vzMacAuxiliaryStorage, IsNSURL url) => vzMacAuxiliaryStorage -> url -> IO (Id VZMacAuxiliaryStorage)
initWithContentsOfURL vzMacAuxiliaryStorage url =
  sendOwnedMessage vzMacAuxiliaryStorage initWithContentsOfURLSelector (toNSURL url)

-- | The URL of the auxiliary storage on the local file system.
--
-- ObjC selector: @- URL@
url :: IsVZMacAuxiliaryStorage vzMacAuxiliaryStorage => vzMacAuxiliaryStorage -> IO (Id NSURL)
url vzMacAuxiliaryStorage =
  sendMessage vzMacAuxiliaryStorage urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZMacAuxiliaryStorage)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacAuxiliaryStorage)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id VZMacAuxiliaryStorage)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @initCreatingStorageAtURL:hardwareModel:options:error:@
initCreatingStorageAtURL_hardwareModel_options_errorSelector :: Selector '[Id NSURL, Id VZMacHardwareModel, VZMacAuxiliaryStorageInitializationOptions, Id NSError] (Id VZMacAuxiliaryStorage)
initCreatingStorageAtURL_hardwareModel_options_errorSelector = mkSelector "initCreatingStorageAtURL:hardwareModel:options:error:"

-- | @Selector@ for @initWithContentsOfURL:@
initWithContentsOfURLSelector :: Selector '[Id NSURL] (Id VZMacAuxiliaryStorage)
initWithContentsOfURLSelector = mkSelector "initWithContentsOfURL:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

