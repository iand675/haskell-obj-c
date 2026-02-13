{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Boot loader configuration for a Linux kernel.
--
-- You must use a VZGenericPlatformConfiguration in conjunction with the Linux boot loader.    It is invalid to use it with any other platform configuration.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZVirtualMachineConfiguration.platform.
--
-- Generated bindings for @VZLinuxBootLoader@.
module ObjC.Virtualization.VZLinuxBootLoader
  ( VZLinuxBootLoader
  , IsVZLinuxBootLoader(..)
  , initWithKernelURL
  , kernelURL
  , setKernelURL
  , commandLine
  , setCommandLine
  , initialRamdiskURL
  , setInitialRamdiskURL
  , commandLineSelector
  , initWithKernelURLSelector
  , initialRamdiskURLSelector
  , kernelURLSelector
  , setCommandLineSelector
  , setInitialRamdiskURLSelector
  , setKernelURLSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Create a VZLinuxBootLoader with the Linux kernel passed as URL.
--
-- @kernelURL@ — The URL of Linux kernel on the local file system.
--
-- ObjC selector: @- initWithKernelURL:@
initWithKernelURL :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSURL kernelURL) => vzLinuxBootLoader -> kernelURL -> IO (Id VZLinuxBootLoader)
initWithKernelURL vzLinuxBootLoader kernelURL =
  sendOwnedMessage vzLinuxBootLoader initWithKernelURLSelector (toNSURL kernelURL)

-- | URL of the Linux kernel.
--
-- ObjC selector: @- kernelURL@
kernelURL :: IsVZLinuxBootLoader vzLinuxBootLoader => vzLinuxBootLoader -> IO (Id NSURL)
kernelURL vzLinuxBootLoader =
  sendMessage vzLinuxBootLoader kernelURLSelector

-- | URL of the Linux kernel.
--
-- ObjC selector: @- setKernelURL:@
setKernelURL :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSURL value) => vzLinuxBootLoader -> value -> IO ()
setKernelURL vzLinuxBootLoader value =
  sendMessage vzLinuxBootLoader setKernelURLSelector (toNSURL value)

-- | Define the command-line parameters passed to the kernel on boot.
--
-- https://www.kernel.org/doc/html/latest/admin-guide/kernel-parameters.html
--
-- ObjC selector: @- commandLine@
commandLine :: IsVZLinuxBootLoader vzLinuxBootLoader => vzLinuxBootLoader -> IO (Id NSString)
commandLine vzLinuxBootLoader =
  sendMessage vzLinuxBootLoader commandLineSelector

-- | Define the command-line parameters passed to the kernel on boot.
--
-- https://www.kernel.org/doc/html/latest/admin-guide/kernel-parameters.html
--
-- ObjC selector: @- setCommandLine:@
setCommandLine :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSString value) => vzLinuxBootLoader -> value -> IO ()
setCommandLine vzLinuxBootLoader value =
  sendMessage vzLinuxBootLoader setCommandLineSelector (toNSString value)

-- | Set the optional initial RAM disk. The RAM disk is mapped into memory before booting the kernel.
--
-- ObjC selector: @- initialRamdiskURL@
initialRamdiskURL :: IsVZLinuxBootLoader vzLinuxBootLoader => vzLinuxBootLoader -> IO (Id NSURL)
initialRamdiskURL vzLinuxBootLoader =
  sendOwnedMessage vzLinuxBootLoader initialRamdiskURLSelector

-- | Set the optional initial RAM disk. The RAM disk is mapped into memory before booting the kernel.
--
-- ObjC selector: @- setInitialRamdiskURL:@
setInitialRamdiskURL :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSURL value) => vzLinuxBootLoader -> value -> IO ()
setInitialRamdiskURL vzLinuxBootLoader value =
  sendMessage vzLinuxBootLoader setInitialRamdiskURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKernelURL:@
initWithKernelURLSelector :: Selector '[Id NSURL] (Id VZLinuxBootLoader)
initWithKernelURLSelector = mkSelector "initWithKernelURL:"

-- | @Selector@ for @kernelURL@
kernelURLSelector :: Selector '[] (Id NSURL)
kernelURLSelector = mkSelector "kernelURL"

-- | @Selector@ for @setKernelURL:@
setKernelURLSelector :: Selector '[Id NSURL] ()
setKernelURLSelector = mkSelector "setKernelURL:"

-- | @Selector@ for @commandLine@
commandLineSelector :: Selector '[] (Id NSString)
commandLineSelector = mkSelector "commandLine"

-- | @Selector@ for @setCommandLine:@
setCommandLineSelector :: Selector '[Id NSString] ()
setCommandLineSelector = mkSelector "setCommandLine:"

-- | @Selector@ for @initialRamdiskURL@
initialRamdiskURLSelector :: Selector '[] (Id NSURL)
initialRamdiskURLSelector = mkSelector "initialRamdiskURL"

-- | @Selector@ for @setInitialRamdiskURL:@
setInitialRamdiskURLSelector :: Selector '[Id NSURL] ()
setInitialRamdiskURLSelector = mkSelector "setInitialRamdiskURL:"

