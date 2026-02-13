{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Virtual machine configuration.
--
-- VZVirtualMachineConfiguration defines the configuration of a VZVirtualMachine.
--
-- The configuration of devices is often done in two parts:    - Device configuration    - Device attachment
--
-- The device configuration defines the characteristics of the emulated hardware device.    For example, for a network device, the device configuration defines the type of network adapter present    in the virtual machine and its MAC address.
--
-- The device attachment defines the host machine's resources that are exposed by the virtual device.    For example, for a network device, the device attachment can be virtual network interface with a NAT    to the real network.
--
-- Creating a virtual machine using the Virtualization framework requires the app to have the "com.apple.security.virtualization" entitlement.    A VZVirtualMachineConfiguration is considered invalid if the application does not have the entitlement.
--
-- ## Configuring a virtual machine to run macOS
--
-- To configure a virtual machine running macOS:    - Set up a platform configuration of type VZMacPlatformConfiguration and set it on the @platform@ property.    - Set up a VZMacOSBootLoader on the @bootLoader@ property.    - Set the CPUCount and memorySize based on the guest's VZMacOSConfigurationRequirements.    - Set up the main storage device as first device on @storageDevices@. Additional storage devices can be set up after the main storage.    - Set up the @keyboards@, @pointingDevices@ and @graphicsDevices@ devices.    - Set up any additional device as needed.
--
-- ## Configuring a virtual machine to run Linux
--
-- To configure a virtual machine running Linux:    - Set up a VZLinuxBootLoader on the @bootLoader@ property.    - Set the CPUCount and memorySize.    - Set up any additional device as needed.
--
-- See: VZVirtualMachine
--
-- Generated bindings for @VZVirtualMachineConfiguration@.
module ObjC.Virtualization.VZVirtualMachineConfiguration
  ( VZVirtualMachineConfiguration
  , IsVZVirtualMachineConfiguration(..)
  , validateWithError
  , validateSaveRestoreSupportWithError
  , bootLoader
  , setBootLoader
  , memorySize
  , setMemorySize
  , cpuCount
  , setCPUCount
  , platform
  , setPlatform
  , audioDevices
  , setAudioDevices
  , consoleDevices
  , setConsoleDevices
  , directorySharingDevices
  , setDirectorySharingDevices
  , entropyDevices
  , setEntropyDevices
  , memoryBalloonDevices
  , setMemoryBalloonDevices
  , networkDevices
  , setNetworkDevices
  , serialPorts
  , setSerialPorts
  , socketDevices
  , setSocketDevices
  , storageDevices
  , setStorageDevices
  , keyboards
  , setKeyboards
  , pointingDevices
  , setPointingDevices
  , graphicsDevices
  , setGraphicsDevices
  , usbControllers
  , setUsbControllers
  , minimumAllowedMemorySize
  , maximumAllowedMemorySize
  , minimumAllowedCPUCount
  , maximumAllowedCPUCount
  , audioDevicesSelector
  , bootLoaderSelector
  , consoleDevicesSelector
  , cpuCountSelector
  , directorySharingDevicesSelector
  , entropyDevicesSelector
  , graphicsDevicesSelector
  , keyboardsSelector
  , maximumAllowedCPUCountSelector
  , maximumAllowedMemorySizeSelector
  , memoryBalloonDevicesSelector
  , memorySizeSelector
  , minimumAllowedCPUCountSelector
  , minimumAllowedMemorySizeSelector
  , networkDevicesSelector
  , platformSelector
  , pointingDevicesSelector
  , serialPortsSelector
  , setAudioDevicesSelector
  , setBootLoaderSelector
  , setCPUCountSelector
  , setConsoleDevicesSelector
  , setDirectorySharingDevicesSelector
  , setEntropyDevicesSelector
  , setGraphicsDevicesSelector
  , setKeyboardsSelector
  , setMemoryBalloonDevicesSelector
  , setMemorySizeSelector
  , setNetworkDevicesSelector
  , setPlatformSelector
  , setPointingDevicesSelector
  , setSerialPortsSelector
  , setSocketDevicesSelector
  , setStorageDevicesSelector
  , setUsbControllersSelector
  , socketDevicesSelector
  , storageDevicesSelector
  , usbControllersSelector
  , validateSaveRestoreSupportWithErrorSelector
  , validateWithErrorSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Virtualization.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Validate the configuration.
--
-- @error@ — If not nil, assigned with the validation error if the validation failed.
--
-- Returns: YES if the configuration is valid.
--
-- ObjC selector: @- validateWithError:@
validateWithError :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSError error_) => vzVirtualMachineConfiguration -> error_ -> IO Bool
validateWithError vzVirtualMachineConfiguration error_ =
  sendMessage vzVirtualMachineConfiguration validateWithErrorSelector (toNSError error_)

-- | Validate the configuration is savable.
--
-- Verify that a virtual machine with this configuration is savable.    Not all configuration options can be safely saved and restored from file.    If this evaluates to NO, the caller should expect future calls to saveMachineStateToURL:completionHandler: to fail.
--
-- @error@ — If not nil, assigned with an error describing the unsupported configuration option.
--
-- Returns: YES if the configuration is savable.
--
-- ObjC selector: @- validateSaveRestoreSupportWithError:@
validateSaveRestoreSupportWithError :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSError error_) => vzVirtualMachineConfiguration -> error_ -> IO Bool
validateSaveRestoreSupportWithError vzVirtualMachineConfiguration error_ =
  sendMessage vzVirtualMachineConfiguration validateSaveRestoreSupportWithErrorSelector (toNSError error_)

-- | Boot loader used when the virtual machine starts.
--
-- See: VZLinuxBootLoader
--
-- See: VZMacOSBootLoader
--
-- ObjC selector: @- bootLoader@
bootLoader :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id VZBootLoader)
bootLoader vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration bootLoaderSelector

-- | Boot loader used when the virtual machine starts.
--
-- See: VZLinuxBootLoader
--
-- See: VZMacOSBootLoader
--
-- ObjC selector: @- setBootLoader:@
setBootLoader :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsVZBootLoader value) => vzVirtualMachineConfiguration -> value -> IO ()
setBootLoader vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setBootLoaderSelector (toVZBootLoader value)

-- | Virtual machine memory size in bytes.
--
-- The memory size must be a multiple of a 1 megabyte (1024 * 1024 bytes) between VZVirtualMachineConfiguration.minimumAllowedMemorySize    and VZVirtualMachineConfiguration.maximumAllowedMemorySize.
--
-- The memorySize represents the total physical memory seen by a guest OS running in the virtual machine.    Not all memory is allocated on start, the virtual machine allocates memory on demand.
--
-- See: VZVirtualMachineConfiguration.minimumAllowedMemorySize
--
-- See: VZVirtualMachineConfiguration.maximumAllowedMemorySize
--
-- ObjC selector: @- memorySize@
memorySize :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO CULong
memorySize vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration memorySizeSelector

-- | Virtual machine memory size in bytes.
--
-- The memory size must be a multiple of a 1 megabyte (1024 * 1024 bytes) between VZVirtualMachineConfiguration.minimumAllowedMemorySize    and VZVirtualMachineConfiguration.maximumAllowedMemorySize.
--
-- The memorySize represents the total physical memory seen by a guest OS running in the virtual machine.    Not all memory is allocated on start, the virtual machine allocates memory on demand.
--
-- See: VZVirtualMachineConfiguration.minimumAllowedMemorySize
--
-- See: VZVirtualMachineConfiguration.maximumAllowedMemorySize
--
-- ObjC selector: @- setMemorySize:@
setMemorySize :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> CULong -> IO ()
setMemorySize vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setMemorySizeSelector value

-- | Number of CPUs.
--
-- The number of CPUs must be a value between VZVirtualMachineConfiguration.minimumAllowedCPUCount    and VZVirtualMachineConfiguration.maximumAllowedCPUCount.
--
-- See: VZVirtualMachineConfiguration.minimumAllowedCPUCount
--
-- See: VZVirtualMachineConfiguration.maximumAllowedCPUCount
--
-- ObjC selector: @- CPUCount@
cpuCount :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO CULong
cpuCount vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration cpuCountSelector

-- | Number of CPUs.
--
-- The number of CPUs must be a value between VZVirtualMachineConfiguration.minimumAllowedCPUCount    and VZVirtualMachineConfiguration.maximumAllowedCPUCount.
--
-- See: VZVirtualMachineConfiguration.minimumAllowedCPUCount
--
-- See: VZVirtualMachineConfiguration.maximumAllowedCPUCount
--
-- ObjC selector: @- setCPUCount:@
setCPUCount :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> CULong -> IO ()
setCPUCount vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setCPUCountSelector value

-- | The hardware platform to use.
--
-- Can be an instance of a VZGenericPlatformConfiguration or VZMacPlatformConfiguration. Defaults to VZGenericPlatformConfiguration.    When restoring from saved state you must ensure your configuration matches that of the saved virtual machine.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZMacPlatformConfiguration
--
-- ObjC selector: @- platform@
platform :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id VZPlatformConfiguration)
platform vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration platformSelector

-- | The hardware platform to use.
--
-- Can be an instance of a VZGenericPlatformConfiguration or VZMacPlatformConfiguration. Defaults to VZGenericPlatformConfiguration.    When restoring from saved state you must ensure your configuration matches that of the saved virtual machine.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZMacPlatformConfiguration
--
-- ObjC selector: @- setPlatform:@
setPlatform :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsVZPlatformConfiguration value) => vzVirtualMachineConfiguration -> value -> IO ()
setPlatform vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setPlatformSelector (toVZPlatformConfiguration value)

-- | List of audio devices. Empty by default.
--
-- See: VZVirtioSoundDeviceConfiguration
--
-- ObjC selector: @- audioDevices@
audioDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
audioDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration audioDevicesSelector

-- | List of audio devices. Empty by default.
--
-- See: VZVirtioSoundDeviceConfiguration
--
-- ObjC selector: @- setAudioDevices:@
setAudioDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setAudioDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setAudioDevicesSelector (toNSArray value)

-- | List of console devices. Empty by default.
--
-- See: VZVirtioConsoleDeviceConfiguration
--
-- ObjC selector: @- consoleDevices@
consoleDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
consoleDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration consoleDevicesSelector

-- | List of console devices. Empty by default.
--
-- See: VZVirtioConsoleDeviceConfiguration
--
-- ObjC selector: @- setConsoleDevices:@
setConsoleDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setConsoleDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setConsoleDevicesSelector (toNSArray value)

-- | List of directory sharing devices. Empty by default.
--
-- See: VZVirtioFileSystemDeviceConfiguration
--
-- ObjC selector: @- directorySharingDevices@
directorySharingDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
directorySharingDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration directorySharingDevicesSelector

-- | List of directory sharing devices. Empty by default.
--
-- See: VZVirtioFileSystemDeviceConfiguration
--
-- ObjC selector: @- setDirectorySharingDevices:@
setDirectorySharingDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setDirectorySharingDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setDirectorySharingDevicesSelector (toNSArray value)

-- | List of entropy devices. Empty by default.
--
-- See: VZVirtioEntropyDeviceConfiguration
--
-- ObjC selector: @- entropyDevices@
entropyDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
entropyDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration entropyDevicesSelector

-- | List of entropy devices. Empty by default.
--
-- See: VZVirtioEntropyDeviceConfiguration
--
-- ObjC selector: @- setEntropyDevices:@
setEntropyDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setEntropyDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setEntropyDevicesSelector (toNSArray value)

-- | List of memory balloon devices. Empty by default.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- ObjC selector: @- memoryBalloonDevices@
memoryBalloonDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
memoryBalloonDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration memoryBalloonDevicesSelector

-- | List of memory balloon devices. Empty by default.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- ObjC selector: @- setMemoryBalloonDevices:@
setMemoryBalloonDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setMemoryBalloonDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setMemoryBalloonDevicesSelector (toNSArray value)

-- | List of network adapters. Empty by default.
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- ObjC selector: @- networkDevices@
networkDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
networkDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration networkDevicesSelector

-- | List of network adapters. Empty by default.
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- ObjC selector: @- setNetworkDevices:@
setNetworkDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setNetworkDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setNetworkDevicesSelector (toNSArray value)

-- | List of serial ports. Empty by default.
--
-- See: VZVirtioConsoleDeviceSerialPortConfiguration
--
-- ObjC selector: @- serialPorts@
serialPorts :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
serialPorts vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration serialPortsSelector

-- | List of serial ports. Empty by default.
--
-- See: VZVirtioConsoleDeviceSerialPortConfiguration
--
-- ObjC selector: @- setSerialPorts:@
setSerialPorts :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setSerialPorts vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setSerialPortsSelector (toNSArray value)

-- | List of socket devices. Empty by default.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- ObjC selector: @- socketDevices@
socketDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
socketDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration socketDevicesSelector

-- | List of socket devices. Empty by default.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- ObjC selector: @- setSocketDevices:@
setSocketDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setSocketDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setSocketDevicesSelector (toNSArray value)

-- | List of disk devices. Empty by default.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
--
-- ObjC selector: @- storageDevices@
storageDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
storageDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration storageDevicesSelector

-- | List of disk devices. Empty by default.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
--
-- ObjC selector: @- setStorageDevices:@
setStorageDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setStorageDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setStorageDevicesSelector (toNSArray value)

-- | List of keyboards. Empty by default.
--
-- See: VZUSBKeyboardConfiguration
--
-- See: VZMacKeyboardConfiguration
--
-- ObjC selector: @- keyboards@
keyboards :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
keyboards vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration keyboardsSelector

-- | List of keyboards. Empty by default.
--
-- See: VZUSBKeyboardConfiguration
--
-- See: VZMacKeyboardConfiguration
--
-- ObjC selector: @- setKeyboards:@
setKeyboards :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setKeyboards vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setKeyboardsSelector (toNSArray value)

-- | List of pointing devices. Empty by default.
--
-- See: VZUSBScreenCoordinatePointingDeviceConfiguration
--
-- See: VZMacTrackpadConfiguration
--
-- ObjC selector: @- pointingDevices@
pointingDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
pointingDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration pointingDevicesSelector

-- | List of pointing devices. Empty by default.
--
-- See: VZUSBScreenCoordinatePointingDeviceConfiguration
--
-- See: VZMacTrackpadConfiguration
--
-- ObjC selector: @- setPointingDevices:@
setPointingDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setPointingDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setPointingDevicesSelector (toNSArray value)

-- | List of graphics devices. Empty by default.
--
-- See: VZMacGraphicsDeviceConfiguration
--
-- ObjC selector: @- graphicsDevices@
graphicsDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
graphicsDevices vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration graphicsDevicesSelector

-- | List of graphics devices. Empty by default.
--
-- See: VZMacGraphicsDeviceConfiguration
--
-- ObjC selector: @- setGraphicsDevices:@
setGraphicsDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setGraphicsDevices vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setGraphicsDevicesSelector (toNSArray value)

-- | List of USB Controllers. Empty by default.
--
-- This list represents a set of USB controllers that the virtual machine will start with.    For each entry in this list, there will be a corresponding runtime object created in VZVirtualMachine.usbControllers property.
--
-- See: VZUSBControllerConfiguration
--
-- ObjC selector: @- usbControllers@
usbControllers :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
usbControllers vzVirtualMachineConfiguration =
  sendMessage vzVirtualMachineConfiguration usbControllersSelector

-- | List of USB Controllers. Empty by default.
--
-- This list represents a set of USB controllers that the virtual machine will start with.    For each entry in this list, there will be a corresponding runtime object created in VZVirtualMachine.usbControllers property.
--
-- See: VZUSBControllerConfiguration
--
-- ObjC selector: @- setUsbControllers:@
setUsbControllers :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setUsbControllers vzVirtualMachineConfiguration value =
  sendMessage vzVirtualMachineConfiguration setUsbControllersSelector (toNSArray value)

-- | Minimum amount of memory required by virtual machines.
--
-- See: VZVirtualMachineConfiguration.memorySize
--
-- ObjC selector: @+ minimumAllowedMemorySize@
minimumAllowedMemorySize :: IO CULong
minimumAllowedMemorySize  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMessage cls' minimumAllowedMemorySizeSelector

-- | Maximum amount of memory allowed for a virtual machine.
--
-- See: VZVirtualMachineConfiguration.memorySize
--
-- ObjC selector: @+ maximumAllowedMemorySize@
maximumAllowedMemorySize :: IO CULong
maximumAllowedMemorySize  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMessage cls' maximumAllowedMemorySizeSelector

-- | Minimum number of CPUs for a virtual machine.
--
-- See: VZVirtualMachineConfiguration.CPUCount
--
-- ObjC selector: @+ minimumAllowedCPUCount@
minimumAllowedCPUCount :: IO CULong
minimumAllowedCPUCount  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMessage cls' minimumAllowedCPUCountSelector

-- | Maximum number of CPUs for a virtual machine.
--
-- See: VZVirtualMachineConfiguration.CPUCount
--
-- ObjC selector: @+ maximumAllowedCPUCount@
maximumAllowedCPUCount :: IO CULong
maximumAllowedCPUCount  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMessage cls' maximumAllowedCPUCountSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @validateWithError:@
validateWithErrorSelector :: Selector '[Id NSError] Bool
validateWithErrorSelector = mkSelector "validateWithError:"

-- | @Selector@ for @validateSaveRestoreSupportWithError:@
validateSaveRestoreSupportWithErrorSelector :: Selector '[Id NSError] Bool
validateSaveRestoreSupportWithErrorSelector = mkSelector "validateSaveRestoreSupportWithError:"

-- | @Selector@ for @bootLoader@
bootLoaderSelector :: Selector '[] (Id VZBootLoader)
bootLoaderSelector = mkSelector "bootLoader"

-- | @Selector@ for @setBootLoader:@
setBootLoaderSelector :: Selector '[Id VZBootLoader] ()
setBootLoaderSelector = mkSelector "setBootLoader:"

-- | @Selector@ for @memorySize@
memorySizeSelector :: Selector '[] CULong
memorySizeSelector = mkSelector "memorySize"

-- | @Selector@ for @setMemorySize:@
setMemorySizeSelector :: Selector '[CULong] ()
setMemorySizeSelector = mkSelector "setMemorySize:"

-- | @Selector@ for @CPUCount@
cpuCountSelector :: Selector '[] CULong
cpuCountSelector = mkSelector "CPUCount"

-- | @Selector@ for @setCPUCount:@
setCPUCountSelector :: Selector '[CULong] ()
setCPUCountSelector = mkSelector "setCPUCount:"

-- | @Selector@ for @platform@
platformSelector :: Selector '[] (Id VZPlatformConfiguration)
platformSelector = mkSelector "platform"

-- | @Selector@ for @setPlatform:@
setPlatformSelector :: Selector '[Id VZPlatformConfiguration] ()
setPlatformSelector = mkSelector "setPlatform:"

-- | @Selector@ for @audioDevices@
audioDevicesSelector :: Selector '[] (Id NSArray)
audioDevicesSelector = mkSelector "audioDevices"

-- | @Selector@ for @setAudioDevices:@
setAudioDevicesSelector :: Selector '[Id NSArray] ()
setAudioDevicesSelector = mkSelector "setAudioDevices:"

-- | @Selector@ for @consoleDevices@
consoleDevicesSelector :: Selector '[] (Id NSArray)
consoleDevicesSelector = mkSelector "consoleDevices"

-- | @Selector@ for @setConsoleDevices:@
setConsoleDevicesSelector :: Selector '[Id NSArray] ()
setConsoleDevicesSelector = mkSelector "setConsoleDevices:"

-- | @Selector@ for @directorySharingDevices@
directorySharingDevicesSelector :: Selector '[] (Id NSArray)
directorySharingDevicesSelector = mkSelector "directorySharingDevices"

-- | @Selector@ for @setDirectorySharingDevices:@
setDirectorySharingDevicesSelector :: Selector '[Id NSArray] ()
setDirectorySharingDevicesSelector = mkSelector "setDirectorySharingDevices:"

-- | @Selector@ for @entropyDevices@
entropyDevicesSelector :: Selector '[] (Id NSArray)
entropyDevicesSelector = mkSelector "entropyDevices"

-- | @Selector@ for @setEntropyDevices:@
setEntropyDevicesSelector :: Selector '[Id NSArray] ()
setEntropyDevicesSelector = mkSelector "setEntropyDevices:"

-- | @Selector@ for @memoryBalloonDevices@
memoryBalloonDevicesSelector :: Selector '[] (Id NSArray)
memoryBalloonDevicesSelector = mkSelector "memoryBalloonDevices"

-- | @Selector@ for @setMemoryBalloonDevices:@
setMemoryBalloonDevicesSelector :: Selector '[Id NSArray] ()
setMemoryBalloonDevicesSelector = mkSelector "setMemoryBalloonDevices:"

-- | @Selector@ for @networkDevices@
networkDevicesSelector :: Selector '[] (Id NSArray)
networkDevicesSelector = mkSelector "networkDevices"

-- | @Selector@ for @setNetworkDevices:@
setNetworkDevicesSelector :: Selector '[Id NSArray] ()
setNetworkDevicesSelector = mkSelector "setNetworkDevices:"

-- | @Selector@ for @serialPorts@
serialPortsSelector :: Selector '[] (Id NSArray)
serialPortsSelector = mkSelector "serialPorts"

-- | @Selector@ for @setSerialPorts:@
setSerialPortsSelector :: Selector '[Id NSArray] ()
setSerialPortsSelector = mkSelector "setSerialPorts:"

-- | @Selector@ for @socketDevices@
socketDevicesSelector :: Selector '[] (Id NSArray)
socketDevicesSelector = mkSelector "socketDevices"

-- | @Selector@ for @setSocketDevices:@
setSocketDevicesSelector :: Selector '[Id NSArray] ()
setSocketDevicesSelector = mkSelector "setSocketDevices:"

-- | @Selector@ for @storageDevices@
storageDevicesSelector :: Selector '[] (Id NSArray)
storageDevicesSelector = mkSelector "storageDevices"

-- | @Selector@ for @setStorageDevices:@
setStorageDevicesSelector :: Selector '[Id NSArray] ()
setStorageDevicesSelector = mkSelector "setStorageDevices:"

-- | @Selector@ for @keyboards@
keyboardsSelector :: Selector '[] (Id NSArray)
keyboardsSelector = mkSelector "keyboards"

-- | @Selector@ for @setKeyboards:@
setKeyboardsSelector :: Selector '[Id NSArray] ()
setKeyboardsSelector = mkSelector "setKeyboards:"

-- | @Selector@ for @pointingDevices@
pointingDevicesSelector :: Selector '[] (Id NSArray)
pointingDevicesSelector = mkSelector "pointingDevices"

-- | @Selector@ for @setPointingDevices:@
setPointingDevicesSelector :: Selector '[Id NSArray] ()
setPointingDevicesSelector = mkSelector "setPointingDevices:"

-- | @Selector@ for @graphicsDevices@
graphicsDevicesSelector :: Selector '[] (Id NSArray)
graphicsDevicesSelector = mkSelector "graphicsDevices"

-- | @Selector@ for @setGraphicsDevices:@
setGraphicsDevicesSelector :: Selector '[Id NSArray] ()
setGraphicsDevicesSelector = mkSelector "setGraphicsDevices:"

-- | @Selector@ for @usbControllers@
usbControllersSelector :: Selector '[] (Id NSArray)
usbControllersSelector = mkSelector "usbControllers"

-- | @Selector@ for @setUsbControllers:@
setUsbControllersSelector :: Selector '[Id NSArray] ()
setUsbControllersSelector = mkSelector "setUsbControllers:"

-- | @Selector@ for @minimumAllowedMemorySize@
minimumAllowedMemorySizeSelector :: Selector '[] CULong
minimumAllowedMemorySizeSelector = mkSelector "minimumAllowedMemorySize"

-- | @Selector@ for @maximumAllowedMemorySize@
maximumAllowedMemorySizeSelector :: Selector '[] CULong
maximumAllowedMemorySizeSelector = mkSelector "maximumAllowedMemorySize"

-- | @Selector@ for @minimumAllowedCPUCount@
minimumAllowedCPUCountSelector :: Selector '[] CULong
minimumAllowedCPUCountSelector = mkSelector "minimumAllowedCPUCount"

-- | @Selector@ for @maximumAllowedCPUCount@
maximumAllowedCPUCountSelector :: Selector '[] CULong
maximumAllowedCPUCountSelector = mkSelector "maximumAllowedCPUCount"

