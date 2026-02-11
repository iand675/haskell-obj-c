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
  , initWithKernelURLSelector
  , kernelURLSelector
  , setKernelURLSelector
  , commandLineSelector
  , setCommandLineSelector
  , initialRamdiskURLSelector
  , setInitialRamdiskURLSelector


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

-- | Create a VZLinuxBootLoader with the Linux kernel passed as URL.
--
-- @kernelURL@ — The URL of Linux kernel on the local file system.
--
-- ObjC selector: @- initWithKernelURL:@
initWithKernelURL :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSURL kernelURL) => vzLinuxBootLoader -> kernelURL -> IO (Id VZLinuxBootLoader)
initWithKernelURL vzLinuxBootLoader  kernelURL =
withObjCPtr kernelURL $ \raw_kernelURL ->
    sendMsg vzLinuxBootLoader (mkSelector "initWithKernelURL:") (retPtr retVoid) [argPtr (castPtr raw_kernelURL :: Ptr ())] >>= ownedObject . castPtr

-- | URL of the Linux kernel.
--
-- ObjC selector: @- kernelURL@
kernelURL :: IsVZLinuxBootLoader vzLinuxBootLoader => vzLinuxBootLoader -> IO (Id NSURL)
kernelURL vzLinuxBootLoader  =
  sendMsg vzLinuxBootLoader (mkSelector "kernelURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL of the Linux kernel.
--
-- ObjC selector: @- setKernelURL:@
setKernelURL :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSURL value) => vzLinuxBootLoader -> value -> IO ()
setKernelURL vzLinuxBootLoader  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzLinuxBootLoader (mkSelector "setKernelURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Define the command-line parameters passed to the kernel on boot.
--
-- https://www.kernel.org/doc/html/latest/admin-guide/kernel-parameters.html
--
-- ObjC selector: @- commandLine@
commandLine :: IsVZLinuxBootLoader vzLinuxBootLoader => vzLinuxBootLoader -> IO (Id NSString)
commandLine vzLinuxBootLoader  =
  sendMsg vzLinuxBootLoader (mkSelector "commandLine") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Define the command-line parameters passed to the kernel on boot.
--
-- https://www.kernel.org/doc/html/latest/admin-guide/kernel-parameters.html
--
-- ObjC selector: @- setCommandLine:@
setCommandLine :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSString value) => vzLinuxBootLoader -> value -> IO ()
setCommandLine vzLinuxBootLoader  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzLinuxBootLoader (mkSelector "setCommandLine:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Set the optional initial RAM disk. The RAM disk is mapped into memory before booting the kernel.
--
-- ObjC selector: @- initialRamdiskURL@
initialRamdiskURL :: IsVZLinuxBootLoader vzLinuxBootLoader => vzLinuxBootLoader -> IO (Id NSURL)
initialRamdiskURL vzLinuxBootLoader  =
  sendMsg vzLinuxBootLoader (mkSelector "initialRamdiskURL") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Set the optional initial RAM disk. The RAM disk is mapped into memory before booting the kernel.
--
-- ObjC selector: @- setInitialRamdiskURL:@
setInitialRamdiskURL :: (IsVZLinuxBootLoader vzLinuxBootLoader, IsNSURL value) => vzLinuxBootLoader -> value -> IO ()
setInitialRamdiskURL vzLinuxBootLoader  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzLinuxBootLoader (mkSelector "setInitialRamdiskURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithKernelURL:@
initWithKernelURLSelector :: Selector
initWithKernelURLSelector = mkSelector "initWithKernelURL:"

-- | @Selector@ for @kernelURL@
kernelURLSelector :: Selector
kernelURLSelector = mkSelector "kernelURL"

-- | @Selector@ for @setKernelURL:@
setKernelURLSelector :: Selector
setKernelURLSelector = mkSelector "setKernelURL:"

-- | @Selector@ for @commandLine@
commandLineSelector :: Selector
commandLineSelector = mkSelector "commandLine"

-- | @Selector@ for @setCommandLine:@
setCommandLineSelector :: Selector
setCommandLineSelector = mkSelector "setCommandLine:"

-- | @Selector@ for @initialRamdiskURL@
initialRamdiskURLSelector :: Selector
initialRamdiskURLSelector = mkSelector "initialRamdiskURL"

-- | @Selector@ for @setInitialRamdiskURL:@
setInitialRamdiskURLSelector :: Selector
setInitialRamdiskURLSelector = mkSelector "setInitialRamdiskURL:"

