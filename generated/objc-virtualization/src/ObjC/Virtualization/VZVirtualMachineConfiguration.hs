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
  , minimumAllowedMemorySize
  , maximumAllowedMemorySize
  , minimumAllowedCPUCount
  , maximumAllowedCPUCount
  , validateWithErrorSelector
  , validateSaveRestoreSupportWithErrorSelector
  , bootLoaderSelector
  , setBootLoaderSelector
  , memorySizeSelector
  , setMemorySizeSelector
  , cpuCountSelector
  , setCPUCountSelector
  , entropyDevicesSelector
  , setEntropyDevicesSelector
  , memoryBalloonDevicesSelector
  , setMemoryBalloonDevicesSelector
  , networkDevicesSelector
  , setNetworkDevicesSelector
  , serialPortsSelector
  , setSerialPortsSelector
  , socketDevicesSelector
  , setSocketDevicesSelector
  , storageDevicesSelector
  , setStorageDevicesSelector
  , minimumAllowedMemorySizeSelector
  , maximumAllowedMemorySizeSelector
  , minimumAllowedCPUCountSelector
  , maximumAllowedCPUCountSelector


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

-- | Validate the configuration.
--
-- @error@ — If not nil, assigned with the validation error if the validation failed.
--
-- Returns: YES if the configuration is valid.
--
-- ObjC selector: @- validateWithError:@
validateWithError :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSError error_) => vzVirtualMachineConfiguration -> error_ -> IO Bool
validateWithError vzVirtualMachineConfiguration  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachineConfiguration (mkSelector "validateWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

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
validateSaveRestoreSupportWithError vzVirtualMachineConfiguration  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg vzVirtualMachineConfiguration (mkSelector "validateSaveRestoreSupportWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Boot loader used when the virtual machine starts.
--
-- See: VZLinuxBootLoader
--
-- See: VZMacOSBootLoader
--
-- ObjC selector: @- bootLoader@
bootLoader :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id VZBootLoader)
bootLoader vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "bootLoader") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Boot loader used when the virtual machine starts.
--
-- See: VZLinuxBootLoader
--
-- See: VZMacOSBootLoader
--
-- ObjC selector: @- setBootLoader:@
setBootLoader :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsVZBootLoader value) => vzVirtualMachineConfiguration -> value -> IO ()
setBootLoader vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setBootLoader:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
memorySize vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "memorySize") retCULong []

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
setMemorySize vzVirtualMachineConfiguration  value =
  sendMsg vzVirtualMachineConfiguration (mkSelector "setMemorySize:") retVoid [argCULong (fromIntegral value)]

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
cpuCount vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "CPUCount") retCULong []

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
setCPUCount vzVirtualMachineConfiguration  value =
  sendMsg vzVirtualMachineConfiguration (mkSelector "setCPUCount:") retVoid [argCULong (fromIntegral value)]

-- | List of entropy devices. Empty by default.
--
-- See: VZVirtioEntropyDeviceConfiguration
--
-- ObjC selector: @- entropyDevices@
entropyDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
entropyDevices vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "entropyDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of entropy devices. Empty by default.
--
-- See: VZVirtioEntropyDeviceConfiguration
--
-- ObjC selector: @- setEntropyDevices:@
setEntropyDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setEntropyDevices vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setEntropyDevices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | List of memory balloon devices. Empty by default.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- ObjC selector: @- memoryBalloonDevices@
memoryBalloonDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
memoryBalloonDevices vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "memoryBalloonDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of memory balloon devices. Empty by default.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
--
-- ObjC selector: @- setMemoryBalloonDevices:@
setMemoryBalloonDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setMemoryBalloonDevices vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setMemoryBalloonDevices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | List of network adapters. Empty by default.
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- ObjC selector: @- networkDevices@
networkDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
networkDevices vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "networkDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of network adapters. Empty by default.
--
-- See: VZVirtioNetworkDeviceConfiguration
--
-- ObjC selector: @- setNetworkDevices:@
setNetworkDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setNetworkDevices vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setNetworkDevices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | List of serial ports. Empty by default.
--
-- See: VZVirtioConsoleDeviceSerialPortConfiguration
--
-- ObjC selector: @- serialPorts@
serialPorts :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
serialPorts vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "serialPorts") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of serial ports. Empty by default.
--
-- See: VZVirtioConsoleDeviceSerialPortConfiguration
--
-- ObjC selector: @- setSerialPorts:@
setSerialPorts :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setSerialPorts vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setSerialPorts:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | List of socket devices. Empty by default.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- ObjC selector: @- socketDevices@
socketDevices :: IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration => vzVirtualMachineConfiguration -> IO (Id NSArray)
socketDevices vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "socketDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | List of socket devices. Empty by default.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- ObjC selector: @- setSocketDevices:@
setSocketDevices :: (IsVZVirtualMachineConfiguration vzVirtualMachineConfiguration, IsNSArray value) => vzVirtualMachineConfiguration -> value -> IO ()
setSocketDevices vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setSocketDevices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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
storageDevices vzVirtualMachineConfiguration  =
  sendMsg vzVirtualMachineConfiguration (mkSelector "storageDevices") (retPtr retVoid) [] >>= retainedObject . castPtr

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
setStorageDevices vzVirtualMachineConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg vzVirtualMachineConfiguration (mkSelector "setStorageDevices:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Minimum amount of memory required by virtual machines.
--
-- See: VZVirtualMachineConfiguration.memorySize
--
-- ObjC selector: @+ minimumAllowedMemorySize@
minimumAllowedMemorySize :: IO CULong
minimumAllowedMemorySize  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMsg cls' (mkSelector "minimumAllowedMemorySize") retCULong []

-- | Maximum amount of memory allowed for a virtual machine.
--
-- See: VZVirtualMachineConfiguration.memorySize
--
-- ObjC selector: @+ maximumAllowedMemorySize@
maximumAllowedMemorySize :: IO CULong
maximumAllowedMemorySize  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMsg cls' (mkSelector "maximumAllowedMemorySize") retCULong []

-- | Minimum number of CPUs for a virtual machine.
--
-- See: VZVirtualMachineConfiguration.CPUCount
--
-- ObjC selector: @+ minimumAllowedCPUCount@
minimumAllowedCPUCount :: IO CULong
minimumAllowedCPUCount  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMsg cls' (mkSelector "minimumAllowedCPUCount") retCULong []

-- | Maximum number of CPUs for a virtual machine.
--
-- See: VZVirtualMachineConfiguration.CPUCount
--
-- ObjC selector: @+ maximumAllowedCPUCount@
maximumAllowedCPUCount :: IO CULong
maximumAllowedCPUCount  =
  do
    cls' <- getRequiredClass "VZVirtualMachineConfiguration"
    sendClassMsg cls' (mkSelector "maximumAllowedCPUCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @validateWithError:@
validateWithErrorSelector :: Selector
validateWithErrorSelector = mkSelector "validateWithError:"

-- | @Selector@ for @validateSaveRestoreSupportWithError:@
validateSaveRestoreSupportWithErrorSelector :: Selector
validateSaveRestoreSupportWithErrorSelector = mkSelector "validateSaveRestoreSupportWithError:"

-- | @Selector@ for @bootLoader@
bootLoaderSelector :: Selector
bootLoaderSelector = mkSelector "bootLoader"

-- | @Selector@ for @setBootLoader:@
setBootLoaderSelector :: Selector
setBootLoaderSelector = mkSelector "setBootLoader:"

-- | @Selector@ for @memorySize@
memorySizeSelector :: Selector
memorySizeSelector = mkSelector "memorySize"

-- | @Selector@ for @setMemorySize:@
setMemorySizeSelector :: Selector
setMemorySizeSelector = mkSelector "setMemorySize:"

-- | @Selector@ for @CPUCount@
cpuCountSelector :: Selector
cpuCountSelector = mkSelector "CPUCount"

-- | @Selector@ for @setCPUCount:@
setCPUCountSelector :: Selector
setCPUCountSelector = mkSelector "setCPUCount:"

-- | @Selector@ for @entropyDevices@
entropyDevicesSelector :: Selector
entropyDevicesSelector = mkSelector "entropyDevices"

-- | @Selector@ for @setEntropyDevices:@
setEntropyDevicesSelector :: Selector
setEntropyDevicesSelector = mkSelector "setEntropyDevices:"

-- | @Selector@ for @memoryBalloonDevices@
memoryBalloonDevicesSelector :: Selector
memoryBalloonDevicesSelector = mkSelector "memoryBalloonDevices"

-- | @Selector@ for @setMemoryBalloonDevices:@
setMemoryBalloonDevicesSelector :: Selector
setMemoryBalloonDevicesSelector = mkSelector "setMemoryBalloonDevices:"

-- | @Selector@ for @networkDevices@
networkDevicesSelector :: Selector
networkDevicesSelector = mkSelector "networkDevices"

-- | @Selector@ for @setNetworkDevices:@
setNetworkDevicesSelector :: Selector
setNetworkDevicesSelector = mkSelector "setNetworkDevices:"

-- | @Selector@ for @serialPorts@
serialPortsSelector :: Selector
serialPortsSelector = mkSelector "serialPorts"

-- | @Selector@ for @setSerialPorts:@
setSerialPortsSelector :: Selector
setSerialPortsSelector = mkSelector "setSerialPorts:"

-- | @Selector@ for @socketDevices@
socketDevicesSelector :: Selector
socketDevicesSelector = mkSelector "socketDevices"

-- | @Selector@ for @setSocketDevices:@
setSocketDevicesSelector :: Selector
setSocketDevicesSelector = mkSelector "setSocketDevices:"

-- | @Selector@ for @storageDevices@
storageDevicesSelector :: Selector
storageDevicesSelector = mkSelector "storageDevices"

-- | @Selector@ for @setStorageDevices:@
setStorageDevicesSelector :: Selector
setStorageDevicesSelector = mkSelector "setStorageDevices:"

-- | @Selector@ for @minimumAllowedMemorySize@
minimumAllowedMemorySizeSelector :: Selector
minimumAllowedMemorySizeSelector = mkSelector "minimumAllowedMemorySize"

-- | @Selector@ for @maximumAllowedMemorySize@
maximumAllowedMemorySizeSelector :: Selector
maximumAllowedMemorySizeSelector = mkSelector "maximumAllowedMemorySize"

-- | @Selector@ for @minimumAllowedCPUCount@
minimumAllowedCPUCountSelector :: Selector
minimumAllowedCPUCountSelector = mkSelector "minimumAllowedCPUCount"

-- | @Selector@ for @maximumAllowedCPUCount@
maximumAllowedCPUCountSelector :: Selector
maximumAllowedCPUCountSelector = mkSelector "maximumAllowedCPUCount"

