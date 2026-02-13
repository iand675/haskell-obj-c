{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | VZMacOSInstaller is used to install macOS on the specified virtual machine.
--
-- A VZMacOSInstaller object must be initialized with a VZVirtualMachine and a file URL referring to a macOS restore image.    The following code example shows how VZMacOSInstaller is used.
--
-- // VZMacOSInstaller must be called with a URL corresponding to a local file. We need a place to store the restore image we download.
-- NSURL *localRestoreImageURL = ...;
--
-- // Load the latest restore image.
-- [VZMacOSRestoreImage fetchLatestSupportedWithCompletionHandler:^(VZMacOSRestoreImage *restoreImage, NSError *error) {
-- if (error) {
-- // Handle the error.
-- abort();
-- }
--
-- // VZMacOSInstaller must be called with a URL corresponding to a local file. Since restoreImage came from
-- // fetchLatestSupportedWithCompletionHandler, its URL property refers to a restore image on the network.
-- // Download the restore image to the local filesystem.
-- [[NSURLSession sharedSession] downloadTaskWithURL:restoreImage.URL completionHandler:^(NSURL *location, NSURLResponse *response, NSError *error) {
-- if (error) {
-- // Handle the error.
-- abort();
-- }
-- if (![[NSFileManager defaultManager] moveItemAtURL:location toURL:localRestoreImageURL error:&error]) {
-- // Handle the error.
-- abort();
-- }
-- dispatch_async(dispatch_get_main_queue(), ^{
-- // Since this restore image came from -[VZMacOSRestoreImage fetchLatestSupportedWithCompletionHandler:], mostFeaturefulSupportedConfiguration should not be nil.
-- VZMacOSConfigurationRequirements *configurationRequirements = restoreImage.mostFeaturefulSupportedConfiguration;
--
-- // Construct a VZVirtualMachineConfiguration that satisfies the configuration requirements.
-- VZVirtualMachineConfiguration *configuration = [[VZVirtualMachineConfiguration alloc] init];
-- configuration.bootLoader = [[VZMacOSBootLoader alloc] init];
-- configuration.platform = [[VZMacPlatformConfiguration alloc] init];
--
-- // The following are minimum values; you can use larger values if desired.
-- configuration.CPUCount = configurationRequirements.minimumSupportedCPUCount;
-- configuration.memorySize = configurationRequirements.minimumSupportedMemorySize;
--
-- // Set other configuration properties as necessary.
-- // ...
--
-- assert([configuration validateWithError:nil]);
--
-- VZVirtualMachine *virtualMachine = [[VZVirtualMachine alloc] initWithConfiguration:configuration];
-- VZMacOSInstaller *installer = [[VZMacOSInstaller alloc] initWithVirtualMachine:virtualMachine restoreImageURL:localRestoreImageURL];
-- [installer installWithCompletionHandler:^(NSError *error) {
-- if (error) {
-- // Handle the error.
-- abort();
-- } else {
-- // Installation was successful.
-- }
-- }];
--
-- // Observe progress using installer.progress object.
-- });
-- }];
-- }];
--
-- VZVirtualMachine
--
-- VZMacOSRestoreImage
--
-- Generated bindings for @VZMacOSInstaller@.
module ObjC.Virtualization.VZMacOSInstaller
  ( VZMacOSInstaller
  , IsVZMacOSInstaller(..)
  , new
  , init_
  , initWithVirtualMachine_restoreImageURL
  , installWithCompletionHandler
  , progress
  , virtualMachine
  , restoreImageURL
  , initSelector
  , initWithVirtualMachine_restoreImageURLSelector
  , installWithCompletionHandlerSelector
  , newSelector
  , progressSelector
  , restoreImageURLSelector
  , virtualMachineSelector


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
new :: IO (Id VZMacOSInstaller)
new  =
  do
    cls' <- getRequiredClass "VZMacOSInstaller"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVZMacOSInstaller vzMacOSInstaller => vzMacOSInstaller -> IO (Id VZMacOSInstaller)
init_ vzMacOSInstaller =
  sendOwnedMessage vzMacOSInstaller initSelector

-- | Initialize a VZMacOSInstaller object.
--
-- @virtualMachine@ — The virtual machine that the operating system will be installed onto.
--
-- @restoreImageFileURL@ — A file URL indicating the macOS restore image to install.
--
-- The virtual machine platform must be macOS and the restore image URL must be a file URL referring to a file on disk or an exception will be raised.    This method must be called on the virtual machine's queue.
--
-- ObjC selector: @- initWithVirtualMachine:restoreImageURL:@
initWithVirtualMachine_restoreImageURL :: (IsVZMacOSInstaller vzMacOSInstaller, IsVZVirtualMachine virtualMachine, IsNSURL restoreImageFileURL) => vzMacOSInstaller -> virtualMachine -> restoreImageFileURL -> IO (Id VZMacOSInstaller)
initWithVirtualMachine_restoreImageURL vzMacOSInstaller virtualMachine restoreImageFileURL =
  sendOwnedMessage vzMacOSInstaller initWithVirtualMachine_restoreImageURLSelector (toVZVirtualMachine virtualMachine) (toNSURL restoreImageFileURL)

-- | Start installing macOS.
--
-- @completionHandler@ — Block called after installation has successfully completed or has failed.    The error parameter passed to the block is nil if installation was successful. The block will be invoked on the virtual machine's queue.
--
-- This method starts the installation process. The virtual machine must be in a stopped state. During the installation operation, pausing or stopping    the virtual machine will result in undefined behavior.    If installation is started on the same VZMacOSInstaller object more than once, an exception will be raised.    This method must be called on the virtual machine's queue.
--
-- ObjC selector: @- installWithCompletionHandler:@
installWithCompletionHandler :: IsVZMacOSInstaller vzMacOSInstaller => vzMacOSInstaller -> Ptr () -> IO ()
installWithCompletionHandler vzMacOSInstaller completionHandler =
  sendMessage vzMacOSInstaller installWithCompletionHandlerSelector completionHandler

-- | An NSProgress object that can be used to observe or cancel installation.
--
-- If the progress object is cancelled before installation is started, an exception will be raised.
--
-- ObjC selector: @- progress@
progress :: IsVZMacOSInstaller vzMacOSInstaller => vzMacOSInstaller -> IO (Id NSProgress)
progress vzMacOSInstaller =
  sendMessage vzMacOSInstaller progressSelector

-- | The virtual machine that this installer was initialized with.
--
-- ObjC selector: @- virtualMachine@
virtualMachine :: IsVZMacOSInstaller vzMacOSInstaller => vzMacOSInstaller -> IO (Id VZVirtualMachine)
virtualMachine vzMacOSInstaller =
  sendMessage vzMacOSInstaller virtualMachineSelector

-- | The restore image URL that this installer was initialized with.
--
-- ObjC selector: @- restoreImageURL@
restoreImageURL :: IsVZMacOSInstaller vzMacOSInstaller => vzMacOSInstaller -> IO (Id NSURL)
restoreImageURL vzMacOSInstaller =
  sendMessage vzMacOSInstaller restoreImageURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VZMacOSInstaller)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VZMacOSInstaller)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithVirtualMachine:restoreImageURL:@
initWithVirtualMachine_restoreImageURLSelector :: Selector '[Id VZVirtualMachine, Id NSURL] (Id VZMacOSInstaller)
initWithVirtualMachine_restoreImageURLSelector = mkSelector "initWithVirtualMachine:restoreImageURL:"

-- | @Selector@ for @installWithCompletionHandler:@
installWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
installWithCompletionHandlerSelector = mkSelector "installWithCompletionHandler:"

-- | @Selector@ for @progress@
progressSelector :: Selector '[] (Id NSProgress)
progressSelector = mkSelector "progress"

-- | @Selector@ for @virtualMachine@
virtualMachineSelector :: Selector '[] (Id VZVirtualMachine)
virtualMachineSelector = mkSelector "virtualMachine"

-- | @Selector@ for @restoreImageURL@
restoreImageURLSelector :: Selector '[] (Id NSURL)
restoreImageURLSelector = mkSelector "restoreImageURL"

