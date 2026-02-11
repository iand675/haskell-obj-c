{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.Virtualization.Internal.Classes (
    module ObjC.Virtualization.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- VZAudioDeviceConfiguration ----------

-- | Base class for an audio device configuration.
--
-- VZAudioDeviceConfiguration should not be instantiated directly.    The subclass VZVirtioSoundDeviceConfiguration should be used instead.
--
-- See: VZVirtioSoundDeviceConfiguration
-- 
-- Phantom type for @VZAudioDeviceConfiguration@.
data VZAudioDeviceConfiguration

instance IsObjCObject (Id VZAudioDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZAudioDeviceConfiguration"

class IsNSObject a => IsVZAudioDeviceConfiguration a where
  toVZAudioDeviceConfiguration :: a -> Id VZAudioDeviceConfiguration

instance IsVZAudioDeviceConfiguration (Id VZAudioDeviceConfiguration) where
  toVZAudioDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZAudioDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZAudioInputStreamSource ----------

-- | Base class for an audio input stream source.
--
-- An audio input stream source defines how audio input data for a guest is produced on the host system.
--
-- VZAudioInputStreamSource should not be instantiated directly. One of its subclasses should be used instead.
--
-- See: VZHostAudioInputStreamSource
-- 
-- Phantom type for @VZAudioInputStreamSource@.
data VZAudioInputStreamSource

instance IsObjCObject (Id VZAudioInputStreamSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZAudioInputStreamSource"

class IsNSObject a => IsVZAudioInputStreamSource a where
  toVZAudioInputStreamSource :: a -> Id VZAudioInputStreamSource

instance IsVZAudioInputStreamSource (Id VZAudioInputStreamSource) where
  toVZAudioInputStreamSource = unsafeCastId

instance IsNSObject (Id VZAudioInputStreamSource) where
  toNSObject = unsafeCastId

-- ---------- VZAudioOutputStreamSink ----------

-- | Base class for an audio output stream sink.
--
-- An audio output stream sink defines how audio data from a guest is consumed on the host system.
--
-- VZAudioOutputStreamSink should not be instantiated directly. One of its subclasses should be used instead.
--
-- See: VZHostAudioOutputStreamSink
-- 
-- Phantom type for @VZAudioOutputStreamSink@.
data VZAudioOutputStreamSink

instance IsObjCObject (Id VZAudioOutputStreamSink) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZAudioOutputStreamSink"

class IsNSObject a => IsVZAudioOutputStreamSink a where
  toVZAudioOutputStreamSink :: a -> Id VZAudioOutputStreamSink

instance IsVZAudioOutputStreamSink (Id VZAudioOutputStreamSink) where
  toVZAudioOutputStreamSink = unsafeCastId

instance IsNSObject (Id VZAudioOutputStreamSink) where
  toNSObject = unsafeCastId

-- ---------- VZBootLoader ----------

-- | Base class of boot loader configuration.
--
-- VZVirtualMachineConfiguration requires a boot loader defining how to start the virtual machine.    VZBootLoader is the abstract base class of boot loader definitions.
--
-- Don't instantiate VZBootLoader directly, instead use its subclass VZEFIBootLoader, VZLinuxBootLoader, or VZMacOSBootLoader.
--
-- See: VZEFIBootLoader
--
-- See: VZLinuxBootLoader
--
-- See: VZMacOSBootLoader
-- 
-- Phantom type for @VZBootLoader@.
data VZBootLoader

instance IsObjCObject (Id VZBootLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZBootLoader"

class IsNSObject a => IsVZBootLoader a where
  toVZBootLoader :: a -> Id VZBootLoader

instance IsVZBootLoader (Id VZBootLoader) where
  toVZBootLoader = unsafeCastId

instance IsNSObject (Id VZBootLoader) where
  toNSObject = unsafeCastId

-- ---------- VZBridgedNetworkInterface ----------

-- | Define a network interface that bridges a physical interface with a virtual machine.
--
-- A bridged interface is shared between the virtual machine and the host system. Both host and virtual machine send and receive packets on the same physical interface    but have distinct network layers.
--
-- VZBridgedNetworkInterface cannot be instantiated directly. A list of supported network interfaces can be obtained using +[VZBridgedNetworkInterface networkInterfaces].
--
-- The VZBridgedNetworkInterface can be used with a VZBridgedNetworkDeviceAttachment to set up a network device VZNetworkDeviceConfiguration.
--
-- VZBridgedNetworkDeviceAttachment
--
-- VZNATNetworkDeviceAttachment
--
-- VZNetworkDeviceConfiguration
-- 
-- Phantom type for @VZBridgedNetworkInterface@.
data VZBridgedNetworkInterface

instance IsObjCObject (Id VZBridgedNetworkInterface) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZBridgedNetworkInterface"

class IsNSObject a => IsVZBridgedNetworkInterface a where
  toVZBridgedNetworkInterface :: a -> Id VZBridgedNetworkInterface

instance IsVZBridgedNetworkInterface (Id VZBridgedNetworkInterface) where
  toVZBridgedNetworkInterface = unsafeCastId

instance IsNSObject (Id VZBridgedNetworkInterface) where
  toNSObject = unsafeCastId

-- ---------- VZConsoleDevice ----------

-- | Class representing a console device in a virtual machine.
--
-- VZConsoleDevice should not be instantiated directly.
--
-- Console devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZConsoleDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the console devices are available through the VZVirtualMachine.consoleDevices property.
--
-- The real type of VZConsoleDevice corresponds to the type used by the configuration.    For example, a VZVirtioConsoleDeviceConfiguration leads to a device of type VZVirtioConsoleDevice.
--
-- See: VZConsoleDeviceConfiguration
-- 
-- Phantom type for @VZConsoleDevice@.
data VZConsoleDevice

instance IsObjCObject (Id VZConsoleDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZConsoleDevice"

class IsNSObject a => IsVZConsoleDevice a where
  toVZConsoleDevice :: a -> Id VZConsoleDevice

instance IsVZConsoleDevice (Id VZConsoleDevice) where
  toVZConsoleDevice = unsafeCastId

instance IsNSObject (Id VZConsoleDevice) where
  toNSObject = unsafeCastId

-- ---------- VZConsoleDeviceConfiguration ----------

-- | Base class for a console device configuration.
--
-- VZConsoleDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioConsoleDeviceConfiguration should be used instead.
--
-- See: VZVirtioConsoleDeviceConfiguration
-- 
-- Phantom type for @VZConsoleDeviceConfiguration@.
data VZConsoleDeviceConfiguration

instance IsObjCObject (Id VZConsoleDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZConsoleDeviceConfiguration"

class IsNSObject a => IsVZConsoleDeviceConfiguration a where
  toVZConsoleDeviceConfiguration :: a -> Id VZConsoleDeviceConfiguration

instance IsVZConsoleDeviceConfiguration (Id VZConsoleDeviceConfiguration) where
  toVZConsoleDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZConsoleDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZConsolePortConfiguration ----------

-- | Base class for a console port configuration.
--
-- VZConsolePortConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioConsolePortConfiguration should be used instead.
--
-- See: VZVirtioConsolePortConfiguration
-- 
-- Phantom type for @VZConsolePortConfiguration@.
data VZConsolePortConfiguration

instance IsObjCObject (Id VZConsolePortConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZConsolePortConfiguration"

class IsNSObject a => IsVZConsolePortConfiguration a where
  toVZConsolePortConfiguration :: a -> Id VZConsolePortConfiguration

instance IsVZConsolePortConfiguration (Id VZConsolePortConfiguration) where
  toVZConsolePortConfiguration = unsafeCastId

instance IsNSObject (Id VZConsolePortConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZDirectoryShare ----------

-- | Base class for a directory share.
--
-- A directory share defines how host directories get exposed to a virtual machine guest.
--
-- VZDirectoryShare should not be instantiated directly.    One of its subclasses like VZSingleDirectoryShare or VZMultipleDirectoryShare should be used instead.
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
-- 
-- Phantom type for @VZDirectoryShare@.
data VZDirectoryShare

instance IsObjCObject (Id VZDirectoryShare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZDirectoryShare"

class IsNSObject a => IsVZDirectoryShare a where
  toVZDirectoryShare :: a -> Id VZDirectoryShare

instance IsVZDirectoryShare (Id VZDirectoryShare) where
  toVZDirectoryShare = unsafeCastId

instance IsNSObject (Id VZDirectoryShare) where
  toNSObject = unsafeCastId

-- ---------- VZDirectorySharingDevice ----------

-- | Base class representing a directory sharing device in a virtual machine.
--
-- VZDirectorySharingDevice should not be instantiated directly.
--
-- Directory sharing devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZDirectorySharingDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the directory sharing devices are available through the VZVirtualMachine.directorySharingDevices property.
--
-- The real type of VZDirectorySharingDevice corresponds to the type used by the configuration.    For example, a VZVirtioFileSystemDeviceConfiguration leads to a device of type VZVirtioFileSystemDevice.
--
-- See: VZVirtioFileSystemDevice
--
-- See: VZVirtioFileSystemDeviceConfiguration
-- 
-- Phantom type for @VZDirectorySharingDevice@.
data VZDirectorySharingDevice

instance IsObjCObject (Id VZDirectorySharingDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZDirectorySharingDevice"

class IsNSObject a => IsVZDirectorySharingDevice a where
  toVZDirectorySharingDevice :: a -> Id VZDirectorySharingDevice

instance IsVZDirectorySharingDevice (Id VZDirectorySharingDevice) where
  toVZDirectorySharingDevice = unsafeCastId

instance IsNSObject (Id VZDirectorySharingDevice) where
  toNSObject = unsafeCastId

-- ---------- VZDirectorySharingDeviceConfiguration ----------

-- | Base class for a directory sharing device configuration.
--
-- VZDirectorySharingDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioFileSystemDeviceConfiguration should be used instead.
--
-- See: VZVirtioFileSystemDeviceConfiguration
-- 
-- Phantom type for @VZDirectorySharingDeviceConfiguration@.
data VZDirectorySharingDeviceConfiguration

instance IsObjCObject (Id VZDirectorySharingDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZDirectorySharingDeviceConfiguration"

class IsNSObject a => IsVZDirectorySharingDeviceConfiguration a where
  toVZDirectorySharingDeviceConfiguration :: a -> Id VZDirectorySharingDeviceConfiguration

instance IsVZDirectorySharingDeviceConfiguration (Id VZDirectorySharingDeviceConfiguration) where
  toVZDirectorySharingDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZDirectorySharingDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZEFIVariableStore ----------

-- | EFI variable store
--
-- The EFI variable store contains NVRAM variables exposed by the EFI ROM.
--
-- VZEFIBootLoader
-- 
-- Phantom type for @VZEFIVariableStore@.
data VZEFIVariableStore

instance IsObjCObject (Id VZEFIVariableStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZEFIVariableStore"

class IsNSObject a => IsVZEFIVariableStore a where
  toVZEFIVariableStore :: a -> Id VZEFIVariableStore

instance IsVZEFIVariableStore (Id VZEFIVariableStore) where
  toVZEFIVariableStore = unsafeCastId

instance IsNSObject (Id VZEFIVariableStore) where
  toNSObject = unsafeCastId

-- ---------- VZEntropyDeviceConfiguration ----------

-- | Base class for an entropy device configuration.
--
-- VZEntropyDeviceConfiguration should not be instantiated directly.    The subclass VZVirtioEntropyDeviceConfiguration should be used instead.
--
-- See: VZVirtioEntropyDeviceConfiguration
-- 
-- Phantom type for @VZEntropyDeviceConfiguration@.
data VZEntropyDeviceConfiguration

instance IsObjCObject (Id VZEntropyDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZEntropyDeviceConfiguration"

class IsNSObject a => IsVZEntropyDeviceConfiguration a where
  toVZEntropyDeviceConfiguration :: a -> Id VZEntropyDeviceConfiguration

instance IsVZEntropyDeviceConfiguration (Id VZEntropyDeviceConfiguration) where
  toVZEntropyDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZEntropyDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZGenericMachineIdentifier ----------

-- | An identifier to make a virtual machine unique.
--
-- The generic machine identifier is used by guests to uniquely identify the virtual hardware.
--
-- If the virtual machine is serialized to disk, the identifier can be preserved in a binary representation through VZGenericMachineIdentifier.dataRepresentation.    The identifier can then be recreated with -[VZGenericMachineIdentifier initWithDataRepresentation:] from the binary representation.
--
-- The contents of two identifiers can be compared with -[VZGenericMachineIdentifier isEqual:].
--
-- VZGenericPlatformConfiguration
-- 
-- Phantom type for @VZGenericMachineIdentifier@.
data VZGenericMachineIdentifier

instance IsObjCObject (Id VZGenericMachineIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZGenericMachineIdentifier"

class IsNSObject a => IsVZGenericMachineIdentifier a where
  toVZGenericMachineIdentifier :: a -> Id VZGenericMachineIdentifier

instance IsVZGenericMachineIdentifier (Id VZGenericMachineIdentifier) where
  toVZGenericMachineIdentifier = unsafeCastId

instance IsNSObject (Id VZGenericMachineIdentifier) where
  toNSObject = unsafeCastId

-- ---------- VZGraphicsDevice ----------

-- | Class representing a graphics device in a virtual machine.
--
-- VZGraphicsDevice should not be instantiated directly.
--
-- Graphics devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZGraphicsDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the graphics devices are available through the VZVirtualMachine.graphicsDevices property.
--
-- The real type of VZGraphicsDevice corresponds to the type used by the configuration.    For example, a VZVirtioGraphicsDeviceConfiguration leads to a device of type VZVirtioGraphicsDevice.    And a VZMacGraphicsDeviceConfiguration leads to a device of type VZMacGraphicsDevice.
--
-- See: VZGraphicsDeviceConfiguration
-- 
-- Phantom type for @VZGraphicsDevice@.
data VZGraphicsDevice

instance IsObjCObject (Id VZGraphicsDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZGraphicsDevice"

class IsNSObject a => IsVZGraphicsDevice a where
  toVZGraphicsDevice :: a -> Id VZGraphicsDevice

instance IsVZGraphicsDevice (Id VZGraphicsDevice) where
  toVZGraphicsDevice = unsafeCastId

instance IsNSObject (Id VZGraphicsDevice) where
  toNSObject = unsafeCastId

-- ---------- VZGraphicsDeviceConfiguration ----------

-- | Phantom type for @VZGraphicsDeviceConfiguration@.
data VZGraphicsDeviceConfiguration

instance IsObjCObject (Id VZGraphicsDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZGraphicsDeviceConfiguration"

class IsNSObject a => IsVZGraphicsDeviceConfiguration a where
  toVZGraphicsDeviceConfiguration :: a -> Id VZGraphicsDeviceConfiguration

instance IsVZGraphicsDeviceConfiguration (Id VZGraphicsDeviceConfiguration) where
  toVZGraphicsDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZGraphicsDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZGraphicsDisplay ----------

-- | Class representing a graphics display in a virtual machine.
--
-- VZGraphicsDisplay should not be instantiated directly.
--
-- Graphics displays are first configured on a VZGraphicsDeviceConfiguration subclass.    When a VZVirtualMachine is created from the configuration, the displays    are available through the VZGraphicsDevice's @displays@ property.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZVirtioGraphicsScanoutConfiguration
-- 
-- Phantom type for @VZGraphicsDisplay@.
data VZGraphicsDisplay

instance IsObjCObject (Id VZGraphicsDisplay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZGraphicsDisplay"

class IsNSObject a => IsVZGraphicsDisplay a where
  toVZGraphicsDisplay :: a -> Id VZGraphicsDisplay

instance IsVZGraphicsDisplay (Id VZGraphicsDisplay) where
  toVZGraphicsDisplay = unsafeCastId

instance IsNSObject (Id VZGraphicsDisplay) where
  toNSObject = unsafeCastId

-- ---------- VZGraphicsDisplayConfiguration ----------

-- | Base class for a graphics display configuration.
--
-- VZGraphicsDisplayConfiguration should not be instantiated directly.    One of its subclasses should be used instead.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZVirtioGraphicsScanoutConfiguration
-- 
-- Phantom type for @VZGraphicsDisplayConfiguration@.
data VZGraphicsDisplayConfiguration

instance IsObjCObject (Id VZGraphicsDisplayConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZGraphicsDisplayConfiguration"

class IsNSObject a => IsVZGraphicsDisplayConfiguration a where
  toVZGraphicsDisplayConfiguration :: a -> Id VZGraphicsDisplayConfiguration

instance IsVZGraphicsDisplayConfiguration (Id VZGraphicsDisplayConfiguration) where
  toVZGraphicsDisplayConfiguration = unsafeCastId

instance IsNSObject (Id VZGraphicsDisplayConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZKeyboardConfiguration ----------

-- | Base class for a keyboard configuration.
--
-- VZKeyboardConfiguration should not be instantiated directly.    One of its subclasses like VZUSBKeyboardConfiguration or VZMacKeyboardConfiguration should be used instead.
--
-- See: VZUSBKeyboardConfiguration
--
-- See: VZMacKeyboardConfiguration
-- 
-- Phantom type for @VZKeyboardConfiguration@.
data VZKeyboardConfiguration

instance IsObjCObject (Id VZKeyboardConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZKeyboardConfiguration"

class IsNSObject a => IsVZKeyboardConfiguration a where
  toVZKeyboardConfiguration :: a -> Id VZKeyboardConfiguration

instance IsVZKeyboardConfiguration (Id VZKeyboardConfiguration) where
  toVZKeyboardConfiguration = unsafeCastId

instance IsNSObject (Id VZKeyboardConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZLinuxRosettaCachingOptions ----------

-- | Base class for a VZLinuxRosettaCachingOptions.
--
-- VZLinuxRosettaCachingOptions define the communication mechanism between the Rosetta daemon and the Rosetta runtime.
--
-- VZLinuxRosettaCachingOptions should not be instantiated directly.    One of its subclasses like VZLinuxRosettaUnixSocketCachingOptions or VZLinuxRosettaAbstractCachingOptions should be used instead.
--
-- See: VZLinuxRosettaUnixSocketCachingOptions
--
-- See: VZLinuxRosettaAbstractCachingOptions
-- 
-- Phantom type for @VZLinuxRosettaCachingOptions@.
data VZLinuxRosettaCachingOptions

instance IsObjCObject (Id VZLinuxRosettaCachingOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZLinuxRosettaCachingOptions"

class IsNSObject a => IsVZLinuxRosettaCachingOptions a where
  toVZLinuxRosettaCachingOptions :: a -> Id VZLinuxRosettaCachingOptions

instance IsVZLinuxRosettaCachingOptions (Id VZLinuxRosettaCachingOptions) where
  toVZLinuxRosettaCachingOptions = unsafeCastId

instance IsNSObject (Id VZLinuxRosettaCachingOptions) where
  toNSObject = unsafeCastId

-- ---------- VZMACAddress ----------

-- | VZMACAddress represents a media access control address (MAC address), the 48-bit ethernet address.
--
-- The easiest way to obtain a MAC address is with +[VZMACAddress randomLocallyAdministeredAddress]. The method    returns a valid local MAC address typically used with network interfaces.
--
-- See: VZNetworkDeviceConfiguration
-- 
-- Phantom type for @VZMACAddress@.
data VZMACAddress

instance IsObjCObject (Id VZMACAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMACAddress"

class IsNSObject a => IsVZMACAddress a where
  toVZMACAddress :: a -> Id VZMACAddress

instance IsVZMACAddress (Id VZMACAddress) where
  toVZMACAddress = unsafeCastId

instance IsNSObject (Id VZMACAddress) where
  toNSObject = unsafeCastId

-- ---------- VZMacAuxiliaryStorage ----------

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
-- Phantom type for @VZMacAuxiliaryStorage@.
data VZMacAuxiliaryStorage

instance IsObjCObject (Id VZMacAuxiliaryStorage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacAuxiliaryStorage"

class IsNSObject a => IsVZMacAuxiliaryStorage a where
  toVZMacAuxiliaryStorage :: a -> Id VZMacAuxiliaryStorage

instance IsVZMacAuxiliaryStorage (Id VZMacAuxiliaryStorage) where
  toVZMacAuxiliaryStorage = unsafeCastId

instance IsNSObject (Id VZMacAuxiliaryStorage) where
  toNSObject = unsafeCastId

-- ---------- VZMacHardwareModel ----------

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
-- Phantom type for @VZMacHardwareModel@.
data VZMacHardwareModel

instance IsObjCObject (Id VZMacHardwareModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacHardwareModel"

class IsNSObject a => IsVZMacHardwareModel a where
  toVZMacHardwareModel :: a -> Id VZMacHardwareModel

instance IsVZMacHardwareModel (Id VZMacHardwareModel) where
  toVZMacHardwareModel = unsafeCastId

instance IsNSObject (Id VZMacHardwareModel) where
  toNSObject = unsafeCastId

-- ---------- VZMacMachineIdentifier ----------

-- | An identifier to make a virtual machine unique.
--
-- The Mac machine identifier is used by macOS guests to uniquely identify the virtual hardware.
--
-- Two virtual machines running concurrently should not use the same identifier.
--
-- If the virtual machine is serialized to disk, the identifier can be preserved in a binary representation through VZMacMachineIdentifier.dataRepresentation.    The identifier can then be recreated with -[VZMacMachineIdentifier initWithDataRepresentation:] from the binary representation.
--
-- The contents of two identifiers can be compared with -[VZMacMachineIdentifier isEqual:].
--
-- VZMacPlatformConfiguration
-- 
-- Phantom type for @VZMacMachineIdentifier@.
data VZMacMachineIdentifier

instance IsObjCObject (Id VZMacMachineIdentifier) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacMachineIdentifier"

class IsNSObject a => IsVZMacMachineIdentifier a where
  toVZMacMachineIdentifier :: a -> Id VZMacMachineIdentifier

instance IsVZMacMachineIdentifier (Id VZMacMachineIdentifier) where
  toVZMacMachineIdentifier = unsafeCastId

instance IsNSObject (Id VZMacMachineIdentifier) where
  toNSObject = unsafeCastId

-- ---------- VZMacOSConfigurationRequirements ----------

-- | VZMacOSConfigurationRequirements describes the parameter constraints required by a specific configuration of macOS.
--
-- When a VZMacOSRestoreImage is loaded, it can be inspected to determine the configurations supported by that restore image.
--
-- VZMacHardwareModel
--
-- VZMacOSRestoreImage
-- 
-- Phantom type for @VZMacOSConfigurationRequirements@.
data VZMacOSConfigurationRequirements

instance IsObjCObject (Id VZMacOSConfigurationRequirements) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacOSConfigurationRequirements"

class IsNSObject a => IsVZMacOSConfigurationRequirements a where
  toVZMacOSConfigurationRequirements :: a -> Id VZMacOSConfigurationRequirements

instance IsVZMacOSConfigurationRequirements (Id VZMacOSConfigurationRequirements) where
  toVZMacOSConfigurationRequirements = unsafeCastId

instance IsNSObject (Id VZMacOSConfigurationRequirements) where
  toNSObject = unsafeCastId

-- ---------- VZMacOSInstaller ----------

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
-- Phantom type for @VZMacOSInstaller@.
data VZMacOSInstaller

instance IsObjCObject (Id VZMacOSInstaller) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacOSInstaller"

class IsNSObject a => IsVZMacOSInstaller a where
  toVZMacOSInstaller :: a -> Id VZMacOSInstaller

instance IsVZMacOSInstaller (Id VZMacOSInstaller) where
  toVZMacOSInstaller = unsafeCastId

instance IsNSObject (Id VZMacOSInstaller) where
  toNSObject = unsafeCastId

-- ---------- VZMacOSRestoreImage ----------

-- | VZMacOSRestoreImage describes a version of macOS to be installed to a virtual machine.
--
-- A VZMacOSRestoreImage object can be created by loading an installation media file. A VZMacOSInstaller    object must be initialized with this VZMacOSRestoreImage object in order to install the operating    system onto a virtual machine.
--
-- Loading a restore image requires the app to have the "com.apple.security.virtualization" entitlement.
--
-- VZMacHardwareModel
--
-- VZMacOSInstaller
--
-- VZMacOSConfigurationRequirements
-- 
-- Phantom type for @VZMacOSRestoreImage@.
data VZMacOSRestoreImage

instance IsObjCObject (Id VZMacOSRestoreImage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacOSRestoreImage"

class IsNSObject a => IsVZMacOSRestoreImage a where
  toVZMacOSRestoreImage :: a -> Id VZMacOSRestoreImage

instance IsVZMacOSRestoreImage (Id VZMacOSRestoreImage) where
  toVZMacOSRestoreImage = unsafeCastId

instance IsNSObject (Id VZMacOSRestoreImage) where
  toNSObject = unsafeCastId

-- ---------- VZMemoryBalloonDevice ----------

-- | Base class representing a memory balloon device in a virtual machine.
--
-- VZMemoryBalloonDevice should not be instantiated directly.
--
-- Memory balloon devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZMemoryBalloonDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the memory balloon devices are available through the VZVirtualMachine.memoryBalloonDevices property.
--
-- The real type of VZMemoryBalloonDevice corresponds to the type used by the configuration.    For example, a VZVirtioTraditionalMemoryBalloonDeviceConfiguration leads to a device of type VZVirtioTraditionalMemoryBalloonDevice.
--
-- See: VZVirtioTraditionalMemoryBalloonDevice
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
-- 
-- Phantom type for @VZMemoryBalloonDevice@.
data VZMemoryBalloonDevice

instance IsObjCObject (Id VZMemoryBalloonDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMemoryBalloonDevice"

class IsNSObject a => IsVZMemoryBalloonDevice a where
  toVZMemoryBalloonDevice :: a -> Id VZMemoryBalloonDevice

instance IsVZMemoryBalloonDevice (Id VZMemoryBalloonDevice) where
  toVZMemoryBalloonDevice = unsafeCastId

instance IsNSObject (Id VZMemoryBalloonDevice) where
  toNSObject = unsafeCastId

-- ---------- VZMemoryBalloonDeviceConfiguration ----------

-- | Base class for a memory balloon device configuration.
--
-- VZMemoryBalloonDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioTraditionalMemoryBalloonDeviceConfiguration should be used instead.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
-- 
-- Phantom type for @VZMemoryBalloonDeviceConfiguration@.
data VZMemoryBalloonDeviceConfiguration

instance IsObjCObject (Id VZMemoryBalloonDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMemoryBalloonDeviceConfiguration"

class IsNSObject a => IsVZMemoryBalloonDeviceConfiguration a where
  toVZMemoryBalloonDeviceConfiguration :: a -> Id VZMemoryBalloonDeviceConfiguration

instance IsVZMemoryBalloonDeviceConfiguration (Id VZMemoryBalloonDeviceConfiguration) where
  toVZMemoryBalloonDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZMemoryBalloonDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZNetworkDevice ----------

-- | Class representing a network device in a virtual machine.
--
-- VZNetworkDevice should not be instantiated directly.
--
-- Network devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZNetworkDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the network devices are available through the VZVirtualMachine.networkDevices property.
--
-- See: VZNetworkDeviceConfiguration
-- 
-- Phantom type for @VZNetworkDevice@.
data VZNetworkDevice

instance IsObjCObject (Id VZNetworkDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZNetworkDevice"

class IsNSObject a => IsVZNetworkDevice a where
  toVZNetworkDevice :: a -> Id VZNetworkDevice

instance IsVZNetworkDevice (Id VZNetworkDevice) where
  toVZNetworkDevice = unsafeCastId

instance IsNSObject (Id VZNetworkDevice) where
  toNSObject = unsafeCastId

-- ---------- VZNetworkDeviceAttachment ----------

-- | Base class for a network device attachment.
--
-- A network device attachment defines how a virtual network device interfaces with the host system.
--
-- VZNetworkDeviceAttachment should not be instantiated directly. One of its subclasses should be used instead.
--
-- Common attachment types include:    - VZNATNetworkDeviceAttachment    - VZFileHandleNetworkDeviceAttachment
--
-- See: VZBridgedNetworkDeviceAttachment
--
-- See: VZFileHandleNetworkDeviceAttachment
--
-- See: VZNATNetworkDeviceAttachment
-- 
-- Phantom type for @VZNetworkDeviceAttachment@.
data VZNetworkDeviceAttachment

instance IsObjCObject (Id VZNetworkDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZNetworkDeviceAttachment"

class IsNSObject a => IsVZNetworkDeviceAttachment a where
  toVZNetworkDeviceAttachment :: a -> Id VZNetworkDeviceAttachment

instance IsVZNetworkDeviceAttachment (Id VZNetworkDeviceAttachment) where
  toVZNetworkDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZNetworkDeviceAttachment) where
  toNSObject = unsafeCastId

-- ---------- VZNetworkDeviceConfiguration ----------

-- | Base class for a network adapter configuration.
--
-- VZNetworkDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioNetworkDeviceConfiguration should be used instead.
--
-- See: VZVirtioNetworkDeviceConfiguration
-- 
-- Phantom type for @VZNetworkDeviceConfiguration@.
data VZNetworkDeviceConfiguration

instance IsObjCObject (Id VZNetworkDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZNetworkDeviceConfiguration"

class IsNSObject a => IsVZNetworkDeviceConfiguration a where
  toVZNetworkDeviceConfiguration :: a -> Id VZNetworkDeviceConfiguration

instance IsVZNetworkDeviceConfiguration (Id VZNetworkDeviceConfiguration) where
  toVZNetworkDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZNetworkDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZPlatformConfiguration ----------

-- | Base class for a platform configuration.
--
-- VZPlatformConfiguration should not be instantiated directly.    One of its subclasses should be used instead.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZMacPlatformConfiguration.
-- 
-- Phantom type for @VZPlatformConfiguration@.
data VZPlatformConfiguration

instance IsObjCObject (Id VZPlatformConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZPlatformConfiguration"

class IsNSObject a => IsVZPlatformConfiguration a where
  toVZPlatformConfiguration :: a -> Id VZPlatformConfiguration

instance IsVZPlatformConfiguration (Id VZPlatformConfiguration) where
  toVZPlatformConfiguration = unsafeCastId

instance IsNSObject (Id VZPlatformConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZPointingDeviceConfiguration ----------

-- | Base class for a pointing device configuration.
--
-- VZPointingDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZUSBScreenCoordinatePointingDeviceConfiguration or VZMacTrackpadConfiguration should be used instead.
--
-- See: VZUSBScreenCoordinatePointingDeviceConfiguration
--
-- See: VZMacTrackpadConfiguration
-- 
-- Phantom type for @VZPointingDeviceConfiguration@.
data VZPointingDeviceConfiguration

instance IsObjCObject (Id VZPointingDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZPointingDeviceConfiguration"

class IsNSObject a => IsVZPointingDeviceConfiguration a where
  toVZPointingDeviceConfiguration :: a -> Id VZPointingDeviceConfiguration

instance IsVZPointingDeviceConfiguration (Id VZPointingDeviceConfiguration) where
  toVZPointingDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZPointingDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZSerialPortAttachment ----------

-- | Base class for a serial port attachment.
--
-- A serial port attachment defines how the virtual machine's serial port interfaces with the host system.    VZSerialPortAttachment should not be instantiated directly.    One of its subclasses like VZFileHandleSerialPortAttachment should be used instead.
--
-- See: VZFileHandleSerialPortAttachment
--
-- See: VZFileSerialPortAttachment
-- 
-- Phantom type for @VZSerialPortAttachment@.
data VZSerialPortAttachment

instance IsObjCObject (Id VZSerialPortAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSerialPortAttachment"

class IsNSObject a => IsVZSerialPortAttachment a where
  toVZSerialPortAttachment :: a -> Id VZSerialPortAttachment

instance IsVZSerialPortAttachment (Id VZSerialPortAttachment) where
  toVZSerialPortAttachment = unsafeCastId

instance IsNSObject (Id VZSerialPortAttachment) where
  toNSObject = unsafeCastId

-- ---------- VZSerialPortConfiguration ----------

-- | Base class for a serial port configuration.
--
-- VZSerialPortConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioConsoleDeviceSerialPortConfiguration should be used instead.
--
-- See: VZVirtioConsoleDeviceSerialPortConfiguration
-- 
-- Phantom type for @VZSerialPortConfiguration@.
data VZSerialPortConfiguration

instance IsObjCObject (Id VZSerialPortConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSerialPortConfiguration"

class IsNSObject a => IsVZSerialPortConfiguration a where
  toVZSerialPortConfiguration :: a -> Id VZSerialPortConfiguration

instance IsVZSerialPortConfiguration (Id VZSerialPortConfiguration) where
  toVZSerialPortConfiguration = unsafeCastId

instance IsNSObject (Id VZSerialPortConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZSharedDirectory ----------

-- | A directory on the host that can be exposed to a guest.
-- 
-- Phantom type for @VZSharedDirectory@.
data VZSharedDirectory

instance IsObjCObject (Id VZSharedDirectory) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSharedDirectory"

class IsNSObject a => IsVZSharedDirectory a where
  toVZSharedDirectory :: a -> Id VZSharedDirectory

instance IsVZSharedDirectory (Id VZSharedDirectory) where
  toVZSharedDirectory = unsafeCastId

instance IsNSObject (Id VZSharedDirectory) where
  toNSObject = unsafeCastId

-- ---------- VZSocketDevice ----------

-- | Base class representing a socket device in a virtual machine.
--
-- VZSocketDevice should not be instantiated directly.
--
-- Socket devices are first configured on the VZVirtualMachineConfiguration through a subclass of VZSocketDeviceConfiguration.    When a VZVirtualMachine is created from the configuration, the socket devices are available through the VZVirtualMachine.socketDevices property.
--
-- The real type of VZSocketDevice corresponds to the type used by the configuration.    For example, a VZVirtioSocketDeviceConfiguration leads to a device of type VZVirtioSocketDevice.
--
-- See: VZVirtioSocketDevice
--
-- See: VZVirtioSocketDeviceConfiguration
-- 
-- Phantom type for @VZSocketDevice@.
data VZSocketDevice

instance IsObjCObject (Id VZSocketDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSocketDevice"

class IsNSObject a => IsVZSocketDevice a where
  toVZSocketDevice :: a -> Id VZSocketDevice

instance IsVZSocketDevice (Id VZSocketDevice) where
  toVZSocketDevice = unsafeCastId

instance IsNSObject (Id VZSocketDevice) where
  toNSObject = unsafeCastId

-- ---------- VZSocketDeviceConfiguration ----------

-- | Base class for a socket device configuration.
--
-- VZSocketDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioSocketDeviceConfiguration should be used instead.
--
-- See: VZVirtioSocketDeviceConfiguration
-- 
-- Phantom type for @VZSocketDeviceConfiguration@.
data VZSocketDeviceConfiguration

instance IsObjCObject (Id VZSocketDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSocketDeviceConfiguration"

class IsNSObject a => IsVZSocketDeviceConfiguration a where
  toVZSocketDeviceConfiguration :: a -> Id VZSocketDeviceConfiguration

instance IsVZSocketDeviceConfiguration (Id VZSocketDeviceConfiguration) where
  toVZSocketDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZSocketDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZStorageDevice ----------

-- | Class representing a storage device in a virtual machine.
--
-- VZStorageDevice should not be instantiated directly.    One of its subclasses like VZUSBMassStorageDevice should be used instead.
--
-- See: VZUSBMassStorageDevice
-- 
-- Phantom type for @VZStorageDevice@.
data VZStorageDevice

instance IsObjCObject (Id VZStorageDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZStorageDevice"

class IsNSObject a => IsVZStorageDevice a where
  toVZStorageDevice :: a -> Id VZStorageDevice

instance IsVZStorageDevice (Id VZStorageDevice) where
  toVZStorageDevice = unsafeCastId

instance IsNSObject (Id VZStorageDevice) where
  toNSObject = unsafeCastId

-- ---------- VZStorageDeviceAttachment ----------

-- | Base class for a storage device attachment.
--
-- A storage device attachment defines how a virtual machine storage device interfaces with the host system.
--
-- VZStorageDeviceAttachment should not be instantiated directly.    One of its subclasses like VZDiskImageStorageDeviceAttachment should be used instead.
--
-- See: VZDiskImageStorageDeviceAttachment
-- 
-- Phantom type for @VZStorageDeviceAttachment@.
data VZStorageDeviceAttachment

instance IsObjCObject (Id VZStorageDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZStorageDeviceAttachment"

class IsNSObject a => IsVZStorageDeviceAttachment a where
  toVZStorageDeviceAttachment :: a -> Id VZStorageDeviceAttachment

instance IsVZStorageDeviceAttachment (Id VZStorageDeviceAttachment) where
  toVZStorageDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZStorageDeviceAttachment) where
  toNSObject = unsafeCastId

-- ---------- VZStorageDeviceConfiguration ----------

-- | Base class for a storage device configuration.
--
-- VZStorageDeviceConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioBlockDeviceConfiguration should be used instead.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
-- 
-- Phantom type for @VZStorageDeviceConfiguration@.
data VZStorageDeviceConfiguration

instance IsObjCObject (Id VZStorageDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZStorageDeviceConfiguration"

class IsNSObject a => IsVZStorageDeviceConfiguration a where
  toVZStorageDeviceConfiguration :: a -> Id VZStorageDeviceConfiguration

instance IsVZStorageDeviceConfiguration (Id VZStorageDeviceConfiguration) where
  toVZStorageDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZStorageDeviceConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZUSBController ----------

-- | Class representing a USB controller in a virtual machine.
--
-- VZUSBController should not be instantiated directly.    USB controllers are first configured on the VZVirtualMachineConfiguration through a subclass of VZUSBControllerConfiguration.    When a VZVirtualMachine is created from the configuration, the USB controllers are available through the VZVirtualMachine.usbControllers property.    The real type of VZUSBController corresponds to the type used by the configuration.    For example, a VZXHCIControllerConfiguration leads to a device of type VZXHCIController.
--
-- See: VZUSBControllerConfiguration
-- 
-- Phantom type for @VZUSBController@.
data VZUSBController

instance IsObjCObject (Id VZUSBController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZUSBController"

class IsNSObject a => IsVZUSBController a where
  toVZUSBController :: a -> Id VZUSBController

instance IsVZUSBController (Id VZUSBController) where
  toVZUSBController = unsafeCastId

instance IsNSObject (Id VZUSBController) where
  toNSObject = unsafeCastId

-- ---------- VZUSBControllerConfiguration ----------

-- | Base class for a USB Controller configuration.
--
-- VZUSBControllerConfiguration should not be instantiated directly.    One of its subclasses like VZXHCIControllerConfiguration should be used instead.
--
-- See: VZXHCIControllerConfiguration
-- 
-- Phantom type for @VZUSBControllerConfiguration@.
data VZUSBControllerConfiguration

instance IsObjCObject (Id VZUSBControllerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZUSBControllerConfiguration"

class IsNSObject a => IsVZUSBControllerConfiguration a where
  toVZUSBControllerConfiguration :: a -> Id VZUSBControllerConfiguration

instance IsVZUSBControllerConfiguration (Id VZUSBControllerConfiguration) where
  toVZUSBControllerConfiguration = unsafeCastId

instance IsNSObject (Id VZUSBControllerConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioConsolePort ----------

-- | Class representing a Virtio console port in a virtual machine.
--
-- VZVirtioConsolePort should not be instantiated directly. This object can be retrieved from the VZVirtioConsoleDevice ports property.
-- 
-- Phantom type for @VZVirtioConsolePort@.
data VZVirtioConsolePort

instance IsObjCObject (Id VZVirtioConsolePort) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsolePort"

class IsNSObject a => IsVZVirtioConsolePort a where
  toVZVirtioConsolePort :: a -> Id VZVirtioConsolePort

instance IsVZVirtioConsolePort (Id VZVirtioConsolePort) where
  toVZVirtioConsolePort = unsafeCastId

instance IsNSObject (Id VZVirtioConsolePort) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioConsolePortArray ----------

-- | Virtio Console Port Array
--
-- This array stores a collection of ports configured for use by a VZVirtioConsoleDevice. VZVirtioConsolePort objects may be retrieved by index.
--
-- See: VZVirtioConsoleDevice
--
-- See: VZVirtioConsolePort
-- 
-- Phantom type for @VZVirtioConsolePortArray@.
data VZVirtioConsolePortArray

instance IsObjCObject (Id VZVirtioConsolePortArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsolePortArray"

class IsNSObject a => IsVZVirtioConsolePortArray a where
  toVZVirtioConsolePortArray :: a -> Id VZVirtioConsolePortArray

instance IsVZVirtioConsolePortArray (Id VZVirtioConsolePortArray) where
  toVZVirtioConsolePortArray = unsafeCastId

instance IsNSObject (Id VZVirtioConsolePortArray) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioConsolePortConfigurationArray ----------

-- | Virtio Console Port Configuration Array
--
-- This array stores a collection of port configurations for a VZVirtioConsoleConfiguration. The index in the array corresponds to the port index used in the virtual machine.
--
-- A maximumPortCount value may be set but must be larger than the highest indexed port. If no maximumPortCount value is set, the highest indexed port will be used.
--
-- See: VZVirtioConsoleConfiguration
--
-- See: VZVirtioConsolePortConfiguration
-- 
-- Phantom type for @VZVirtioConsolePortConfigurationArray@.
data VZVirtioConsolePortConfigurationArray

instance IsObjCObject (Id VZVirtioConsolePortConfigurationArray) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsolePortConfigurationArray"

class IsNSObject a => IsVZVirtioConsolePortConfigurationArray a where
  toVZVirtioConsolePortConfigurationArray :: a -> Id VZVirtioConsolePortConfigurationArray

instance IsVZVirtioConsolePortConfigurationArray (Id VZVirtioConsolePortConfigurationArray) where
  toVZVirtioConsolePortConfigurationArray = unsafeCastId

instance IsNSObject (Id VZVirtioConsolePortConfigurationArray) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioSocketConnection ----------

-- | The VZVirtioSocketConnection object represents a Virtio socket device's connection.
--
-- The connection encompasses a source port, destination port, and an associated file descriptor.
--
-- See: VZVirtioSocketDevice
-- 
-- Phantom type for @VZVirtioSocketConnection@.
data VZVirtioSocketConnection

instance IsObjCObject (Id VZVirtioSocketConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSocketConnection"

class IsNSObject a => IsVZVirtioSocketConnection a where
  toVZVirtioSocketConnection :: a -> Id VZVirtioSocketConnection

instance IsVZVirtioSocketConnection (Id VZVirtioSocketConnection) where
  toVZVirtioSocketConnection = unsafeCastId

instance IsNSObject (Id VZVirtioSocketConnection) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioSocketListener ----------

-- | The VZVirtioSocketListener object represents a listener for the Virtio socket device.
--
-- The listener encompasses a VZVirtioSocketListenerDelegate object.    VZVirtioSocketListener is used with VZVirtioSocketDevice to listen to a particular port.    The delegate is used when a guest connects to a port associated with the listener.
--
-- See: VZVirtioSocketDevice
--
-- See: VZVirtioSocketListenerDelegate
-- 
-- Phantom type for @VZVirtioSocketListener@.
data VZVirtioSocketListener

instance IsObjCObject (Id VZVirtioSocketListener) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSocketListener"

class IsNSObject a => IsVZVirtioSocketListener a where
  toVZVirtioSocketListener :: a -> Id VZVirtioSocketListener

instance IsVZVirtioSocketListener (Id VZVirtioSocketListener) where
  toVZVirtioSocketListener = unsafeCastId

instance IsNSObject (Id VZVirtioSocketListener) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioSoundDeviceStreamConfiguration ----------

-- | Virtio Sound Device Stream Configuration.
--
-- A PCM stream of audio data.    VZVirtioSoundDeviceStreamConfiguration should not be instantiated directly.    One of its subclasses like VZVirtioSoundDeviceInputStreamConfiguration or VZVirtioSoundDeviceOutputStreamConfiguration should be used instead.
--
-- See: VZVirtioSoundDeviceInputStreamConfiguration
--
-- See: VZVirtioSoundDeviceOutputStreamConfiguration
-- 
-- Phantom type for @VZVirtioSoundDeviceStreamConfiguration@.
data VZVirtioSoundDeviceStreamConfiguration

instance IsObjCObject (Id VZVirtioSoundDeviceStreamConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSoundDeviceStreamConfiguration"

class IsNSObject a => IsVZVirtioSoundDeviceStreamConfiguration a where
  toVZVirtioSoundDeviceStreamConfiguration :: a -> Id VZVirtioSoundDeviceStreamConfiguration

instance IsVZVirtioSoundDeviceStreamConfiguration (Id VZVirtioSoundDeviceStreamConfiguration) where
  toVZVirtioSoundDeviceStreamConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioSoundDeviceStreamConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZVirtualMachine ----------

-- | VZVirtualMachine represents the entire state of a single virtual machine.
--
-- A Virtual Machine is the emulation of a complete hardware machine of the same architecture as the real hardware machine.    When executing the Virtual Machine, the Virtualization framework uses certain hardware resources and emulates others to provide isolation    and great performance.
--
-- The definition of a virtual machine starts with its configuration. This is done by setting up a VZVirtualMachineConfiguration object.    Once configured, the virtual machine can be started with [VZVirtualMachine startWithCompletionHandler:].
--
-- To install macOS on a virtual machine, configure a new virtual machine with a suitable VZMacPlatformConfiguration, then use a VZMacOSInstaller    to install the restore image on it.
--
-- Creating a virtual machine using the Virtualization framework requires the app to have the "com.apple.security.virtualization" entitlement.
--
-- VZVirtualMachineConfiguration
--
-- VZMacOSInstaller
-- 
-- Phantom type for @VZVirtualMachine@.
data VZVirtualMachine

instance IsObjCObject (Id VZVirtualMachine) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtualMachine"

class IsNSObject a => IsVZVirtualMachine a where
  toVZVirtualMachine :: a -> Id VZVirtualMachine

instance IsVZVirtualMachine (Id VZVirtualMachine) where
  toVZVirtualMachine = unsafeCastId

instance IsNSObject (Id VZVirtualMachine) where
  toNSObject = unsafeCastId

-- ---------- VZVirtualMachineConfiguration ----------

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
-- Phantom type for @VZVirtualMachineConfiguration@.
data VZVirtualMachineConfiguration

instance IsObjCObject (Id VZVirtualMachineConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtualMachineConfiguration"

class IsNSObject a => IsVZVirtualMachineConfiguration a where
  toVZVirtualMachineConfiguration :: a -> Id VZVirtualMachineConfiguration

instance IsVZVirtualMachineConfiguration (Id VZVirtualMachineConfiguration) where
  toVZVirtualMachineConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtualMachineConfiguration) where
  toNSObject = unsafeCastId

-- ---------- VZVirtualMachineStartOptions ----------

-- | Base class for virtual machine start options.
--
-- See: VZMacOSVirtualMachineStartOptions
-- 
-- Phantom type for @VZVirtualMachineStartOptions@.
data VZVirtualMachineStartOptions

instance IsObjCObject (Id VZVirtualMachineStartOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtualMachineStartOptions"

class IsNSObject a => IsVZVirtualMachineStartOptions a where
  toVZVirtualMachineStartOptions :: a -> Id VZVirtualMachineStartOptions

instance IsVZVirtualMachineStartOptions (Id VZVirtualMachineStartOptions) where
  toVZVirtualMachineStartOptions = unsafeCastId

instance IsNSObject (Id VZVirtualMachineStartOptions) where
  toNSObject = unsafeCastId

-- ---------- VZVirtioSoundDeviceConfiguration ----------

-- | Virtio Sound Device Configuration.
--
-- The device exposes a source or destination of sound.
-- 
-- Phantom type for @VZVirtioSoundDeviceConfiguration@.
data VZVirtioSoundDeviceConfiguration

instance IsObjCObject (Id VZVirtioSoundDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSoundDeviceConfiguration"

class IsVZAudioDeviceConfiguration a => IsVZVirtioSoundDeviceConfiguration a where
  toVZVirtioSoundDeviceConfiguration :: a -> Id VZVirtioSoundDeviceConfiguration

instance IsVZVirtioSoundDeviceConfiguration (Id VZVirtioSoundDeviceConfiguration) where
  toVZVirtioSoundDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioSoundDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZAudioDeviceConfiguration (Id VZVirtioSoundDeviceConfiguration) where
  toVZAudioDeviceConfiguration = unsafeCastId

-- ---------- VZHostAudioInputStreamSource ----------

-- | Host audio input stream source provides audio from the host system's default input device.
--
-- Host input data comes from the same device that AudioQueueNewInput uses.
--
-- See: VZVirtioSoundDeviceInputStreamConfiguration
-- 
-- Phantom type for @VZHostAudioInputStreamSource@.
data VZHostAudioInputStreamSource

instance IsObjCObject (Id VZHostAudioInputStreamSource) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZHostAudioInputStreamSource"

class IsVZAudioInputStreamSource a => IsVZHostAudioInputStreamSource a where
  toVZHostAudioInputStreamSource :: a -> Id VZHostAudioInputStreamSource

instance IsVZHostAudioInputStreamSource (Id VZHostAudioInputStreamSource) where
  toVZHostAudioInputStreamSource = unsafeCastId

instance IsNSObject (Id VZHostAudioInputStreamSource) where
  toNSObject = unsafeCastId

instance IsVZAudioInputStreamSource (Id VZHostAudioInputStreamSource) where
  toVZAudioInputStreamSource = unsafeCastId

-- ---------- VZHostAudioOutputStreamSink ----------

-- | Host audio output stream sink plays audio to the host system's default output device.
--
-- Host output data goes to the same device that AudioQueueNewOutput uses.
--
-- See: VZVirtioSoundDeviceOutputStreamConfiguration
-- 
-- Phantom type for @VZHostAudioOutputStreamSink@.
data VZHostAudioOutputStreamSink

instance IsObjCObject (Id VZHostAudioOutputStreamSink) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZHostAudioOutputStreamSink"

class IsVZAudioOutputStreamSink a => IsVZHostAudioOutputStreamSink a where
  toVZHostAudioOutputStreamSink :: a -> Id VZHostAudioOutputStreamSink

instance IsVZHostAudioOutputStreamSink (Id VZHostAudioOutputStreamSink) where
  toVZHostAudioOutputStreamSink = unsafeCastId

instance IsNSObject (Id VZHostAudioOutputStreamSink) where
  toNSObject = unsafeCastId

instance IsVZAudioOutputStreamSink (Id VZHostAudioOutputStreamSink) where
  toVZAudioOutputStreamSink = unsafeCastId

-- ---------- VZEFIBootLoader ----------

-- | Boot loader configuration for booting guest operating systems expecting an EFI ROM.
--
-- You must use a VZGenericPlatformConfiguration in conjunction with the EFI boot loader.    It is invalid to use it with any other platform configuration.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZVirtualMachineConfiguration.platform.
-- 
-- Phantom type for @VZEFIBootLoader@.
data VZEFIBootLoader

instance IsObjCObject (Id VZEFIBootLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZEFIBootLoader"

class IsVZBootLoader a => IsVZEFIBootLoader a where
  toVZEFIBootLoader :: a -> Id VZEFIBootLoader

instance IsVZEFIBootLoader (Id VZEFIBootLoader) where
  toVZEFIBootLoader = unsafeCastId

instance IsNSObject (Id VZEFIBootLoader) where
  toNSObject = unsafeCastId

instance IsVZBootLoader (Id VZEFIBootLoader) where
  toVZBootLoader = unsafeCastId

-- ---------- VZLinuxBootLoader ----------

-- | Boot loader configuration for a Linux kernel.
--
-- You must use a VZGenericPlatformConfiguration in conjunction with the Linux boot loader.    It is invalid to use it with any other platform configuration.
--
-- See: VZGenericPlatformConfiguration
--
-- See: VZVirtualMachineConfiguration.platform.
-- 
-- Phantom type for @VZLinuxBootLoader@.
data VZLinuxBootLoader

instance IsObjCObject (Id VZLinuxBootLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZLinuxBootLoader"

class IsVZBootLoader a => IsVZLinuxBootLoader a where
  toVZLinuxBootLoader :: a -> Id VZLinuxBootLoader

instance IsVZLinuxBootLoader (Id VZLinuxBootLoader) where
  toVZLinuxBootLoader = unsafeCastId

instance IsNSObject (Id VZLinuxBootLoader) where
  toNSObject = unsafeCastId

instance IsVZBootLoader (Id VZLinuxBootLoader) where
  toVZBootLoader = unsafeCastId

-- ---------- VZMacOSBootLoader ----------

-- | Boot loader configuration for booting macOS on Apple Silicon.
--
-- You must use a VZMacPlatformConfiguration in conjunction with the macOS boot loader.    It is invalid to use it with any other platform configuration.
--
-- See: VZMacPlatformConfiguration
--
-- See: VZVirtualMachineConfiguration.platform.
-- 
-- Phantom type for @VZMacOSBootLoader@.
data VZMacOSBootLoader

instance IsObjCObject (Id VZMacOSBootLoader) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacOSBootLoader"

class IsVZBootLoader a => IsVZMacOSBootLoader a where
  toVZMacOSBootLoader :: a -> Id VZMacOSBootLoader

instance IsVZMacOSBootLoader (Id VZMacOSBootLoader) where
  toVZMacOSBootLoader = unsafeCastId

instance IsNSObject (Id VZMacOSBootLoader) where
  toNSObject = unsafeCastId

instance IsVZBootLoader (Id VZMacOSBootLoader) where
  toVZBootLoader = unsafeCastId

-- ---------- VZVirtioConsoleDevice ----------

-- | Class representing a Virtio console device in a virtual machine.
--
-- VZVirtioConsoleDevice should not be instantiated directly.
--
-- See: VZConsoleDeviceConfiguration
-- 
-- Phantom type for @VZVirtioConsoleDevice@.
data VZVirtioConsoleDevice

instance IsObjCObject (Id VZVirtioConsoleDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsoleDevice"

class IsVZConsoleDevice a => IsVZVirtioConsoleDevice a where
  toVZVirtioConsoleDevice :: a -> Id VZVirtioConsoleDevice

instance IsVZVirtioConsoleDevice (Id VZVirtioConsoleDevice) where
  toVZVirtioConsoleDevice = unsafeCastId

instance IsNSObject (Id VZVirtioConsoleDevice) where
  toNSObject = unsafeCastId

instance IsVZConsoleDevice (Id VZVirtioConsoleDevice) where
  toVZConsoleDevice = unsafeCastId

-- ---------- VZVirtioConsoleDeviceConfiguration ----------

-- | Virtio Console Device
--
-- This console device enables communication between the host and the guest using console ports through the Virtio interface.
--
-- The device sets up one or more ports via VZVirtioConsolePortConfiguration on the Virtio console device.
--
-- See: VZVirtioConsolePortConfiguration
--
-- See: VZVirtualMachineConfiguration.consoleDevices
-- 
-- Phantom type for @VZVirtioConsoleDeviceConfiguration@.
data VZVirtioConsoleDeviceConfiguration

instance IsObjCObject (Id VZVirtioConsoleDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsoleDeviceConfiguration"

class IsVZConsoleDeviceConfiguration a => IsVZVirtioConsoleDeviceConfiguration a where
  toVZVirtioConsoleDeviceConfiguration :: a -> Id VZVirtioConsoleDeviceConfiguration

instance IsVZVirtioConsoleDeviceConfiguration (Id VZVirtioConsoleDeviceConfiguration) where
  toVZVirtioConsoleDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioConsoleDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZConsoleDeviceConfiguration (Id VZVirtioConsoleDeviceConfiguration) where
  toVZConsoleDeviceConfiguration = unsafeCastId

-- ---------- VZVirtioConsolePortConfiguration ----------

-- | Virtio Console Port
--
-- A console port is a two-way communication channel between a host VZSerialPortAttachment and a virtual machine console port. One or more console ports are attached to a Virtio console device.
--
-- An optional name may be set for a console port. A console port may also be configured for use as the system console.
--
-- See: VZConsolePortConfiguration
--
-- See: VZVirtualMachineConfiguration.consoleDevices
-- 
-- Phantom type for @VZVirtioConsolePortConfiguration@.
data VZVirtioConsolePortConfiguration

instance IsObjCObject (Id VZVirtioConsolePortConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsolePortConfiguration"

class IsVZConsolePortConfiguration a => IsVZVirtioConsolePortConfiguration a where
  toVZVirtioConsolePortConfiguration :: a -> Id VZVirtioConsolePortConfiguration

instance IsVZVirtioConsolePortConfiguration (Id VZVirtioConsolePortConfiguration) where
  toVZVirtioConsolePortConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioConsolePortConfiguration) where
  toNSObject = unsafeCastId

instance IsVZConsolePortConfiguration (Id VZVirtioConsolePortConfiguration) where
  toVZConsolePortConfiguration = unsafeCastId

-- ---------- VZLinuxRosettaDirectoryShare ----------

-- | Directory share to enable Rosetta support for Linux binaries.
--
-- This directory share exposes Rosetta within a shared directory in the guest. Linux can use it to translate x86_64 binaries.
--
-- See: VZDirectorySharingDeviceConfiguration
--
-- See: VZSharedDirectory
-- 
-- Phantom type for @VZLinuxRosettaDirectoryShare@.
data VZLinuxRosettaDirectoryShare

instance IsObjCObject (Id VZLinuxRosettaDirectoryShare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZLinuxRosettaDirectoryShare"

class IsVZDirectoryShare a => IsVZLinuxRosettaDirectoryShare a where
  toVZLinuxRosettaDirectoryShare :: a -> Id VZLinuxRosettaDirectoryShare

instance IsVZLinuxRosettaDirectoryShare (Id VZLinuxRosettaDirectoryShare) where
  toVZLinuxRosettaDirectoryShare = unsafeCastId

instance IsNSObject (Id VZLinuxRosettaDirectoryShare) where
  toNSObject = unsafeCastId

instance IsVZDirectoryShare (Id VZLinuxRosettaDirectoryShare) where
  toVZDirectoryShare = unsafeCastId

-- ---------- VZMultipleDirectoryShare ----------

-- | Directory share for multiple directories.
--
-- This directory share exposes multiple directories from the host file system to the guest.
--
-- See: VZDirectorySharingDeviceConfiguration
--
-- See: VZSharedDirectory
-- 
-- Phantom type for @VZMultipleDirectoryShare@.
data VZMultipleDirectoryShare

instance IsObjCObject (Id VZMultipleDirectoryShare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMultipleDirectoryShare"

class IsVZDirectoryShare a => IsVZMultipleDirectoryShare a where
  toVZMultipleDirectoryShare :: a -> Id VZMultipleDirectoryShare

instance IsVZMultipleDirectoryShare (Id VZMultipleDirectoryShare) where
  toVZMultipleDirectoryShare = unsafeCastId

instance IsNSObject (Id VZMultipleDirectoryShare) where
  toNSObject = unsafeCastId

instance IsVZDirectoryShare (Id VZMultipleDirectoryShare) where
  toVZDirectoryShare = unsafeCastId

-- ---------- VZSingleDirectoryShare ----------

-- | Directory share for a single directory.
--
-- This directory share exposes a single directory from the host file system to the guest.
--
-- See: VZDirectorySharingDeviceConfiguration
--
-- See: VZSharedDirectory
-- 
-- Phantom type for @VZSingleDirectoryShare@.
data VZSingleDirectoryShare

instance IsObjCObject (Id VZSingleDirectoryShare) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSingleDirectoryShare"

class IsVZDirectoryShare a => IsVZSingleDirectoryShare a where
  toVZSingleDirectoryShare :: a -> Id VZSingleDirectoryShare

instance IsVZSingleDirectoryShare (Id VZSingleDirectoryShare) where
  toVZSingleDirectoryShare = unsafeCastId

instance IsNSObject (Id VZSingleDirectoryShare) where
  toNSObject = unsafeCastId

instance IsVZDirectoryShare (Id VZSingleDirectoryShare) where
  toVZDirectoryShare = unsafeCastId

-- ---------- VZVirtioFileSystemDevice ----------

-- | Virtio File System Device
--
-- This is a device that exposes host resources to the guest as a file system mount.    The directory share defines which host resources are exposed to the guest.
--
-- This device is created through instantiating a VZVirtioFileSystemDeviceConfiguration in a VZVirtualMachineConfiguration and is available in the    VZVirtualMachine.directorySharingDevices property.
--
-- See: VZVirtioFileSystemDeviceConfiguration
--
-- See: VZSingleDirectoryShare
--
-- See: VZMultipleDirectoryShare
-- 
-- Phantom type for @VZVirtioFileSystemDevice@.
data VZVirtioFileSystemDevice

instance IsObjCObject (Id VZVirtioFileSystemDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioFileSystemDevice"

class IsVZDirectorySharingDevice a => IsVZVirtioFileSystemDevice a where
  toVZVirtioFileSystemDevice :: a -> Id VZVirtioFileSystemDevice

instance IsVZVirtioFileSystemDevice (Id VZVirtioFileSystemDevice) where
  toVZVirtioFileSystemDevice = unsafeCastId

instance IsNSObject (Id VZVirtioFileSystemDevice) where
  toNSObject = unsafeCastId

instance IsVZDirectorySharingDevice (Id VZVirtioFileSystemDevice) where
  toVZDirectorySharingDevice = unsafeCastId

-- ---------- VZVirtioFileSystemDeviceConfiguration ----------

-- | Configuration of a Virtio file system device.
--
-- This configuration creates a Virtio file system device which allows for exposing    directories on the host to a guest via a tag label.
-- 
-- Phantom type for @VZVirtioFileSystemDeviceConfiguration@.
data VZVirtioFileSystemDeviceConfiguration

instance IsObjCObject (Id VZVirtioFileSystemDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioFileSystemDeviceConfiguration"

class IsVZDirectorySharingDeviceConfiguration a => IsVZVirtioFileSystemDeviceConfiguration a where
  toVZVirtioFileSystemDeviceConfiguration :: a -> Id VZVirtioFileSystemDeviceConfiguration

instance IsVZVirtioFileSystemDeviceConfiguration (Id VZVirtioFileSystemDeviceConfiguration) where
  toVZVirtioFileSystemDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioFileSystemDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZDirectorySharingDeviceConfiguration (Id VZVirtioFileSystemDeviceConfiguration) where
  toVZDirectorySharingDeviceConfiguration = unsafeCastId

-- ---------- VZVirtioEntropyDeviceConfiguration ----------

-- | Virtio Entropy Device
--
-- The device exposes a source of entropy for the guest's random number generator.
-- 
-- Phantom type for @VZVirtioEntropyDeviceConfiguration@.
data VZVirtioEntropyDeviceConfiguration

instance IsObjCObject (Id VZVirtioEntropyDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioEntropyDeviceConfiguration"

class IsVZEntropyDeviceConfiguration a => IsVZVirtioEntropyDeviceConfiguration a where
  toVZVirtioEntropyDeviceConfiguration :: a -> Id VZVirtioEntropyDeviceConfiguration

instance IsVZVirtioEntropyDeviceConfiguration (Id VZVirtioEntropyDeviceConfiguration) where
  toVZVirtioEntropyDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioEntropyDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZEntropyDeviceConfiguration (Id VZVirtioEntropyDeviceConfiguration) where
  toVZEntropyDeviceConfiguration = unsafeCastId

-- ---------- VZMacGraphicsDevice ----------

-- | A Mac graphics device.
--
-- The VZMacGraphicsDevice is the runtime counterpart of VZMacGraphicsDeviceConfiguration.
--
-- For example, if a @VZVirtualMachineConfiguration.graphicsDevices[0]@ is an instance of @VZMacGraphicsDeviceConfiguration@, when creating the virtual Mac, the @VZVirtualMachine.graphicsDevices[0]@ is the corresponding @VZMacGraphicsDevice@.
--
-- An important property is the @displays@ inherited from @VZGraphicsDevice@. It provides the list of displays on the graphics device,    each corresponding to the display configuration set on @VZMacGraphicsDeviceConfiguration@.
--
-- See: VZMacGraphicsDeviceConfiguration
--
-- See: VZGraphicsDevice.displays
-- 
-- Phantom type for @VZMacGraphicsDevice@.
data VZMacGraphicsDevice

instance IsObjCObject (Id VZMacGraphicsDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacGraphicsDevice"

class IsVZGraphicsDevice a => IsVZMacGraphicsDevice a where
  toVZMacGraphicsDevice :: a -> Id VZMacGraphicsDevice

instance IsVZMacGraphicsDevice (Id VZMacGraphicsDevice) where
  toVZMacGraphicsDevice = unsafeCastId

instance IsNSObject (Id VZMacGraphicsDevice) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDevice (Id VZMacGraphicsDevice) where
  toVZGraphicsDevice = unsafeCastId

-- ---------- VZVirtioGraphicsDevice ----------

-- | A Virtio graphics device.
--
-- The VZVirtioGraphicsDevice is the runtime counterpart of VZVirtioGraphicsDeviceConfiguration.
--
-- For example, if a @VZVirtualMachineConfiguration.graphicsDevices[0]@ is an instance of @VZVirtioGraphicsDeviceConfiguration@, when creating the virtual machine, the @VZVirtualMachine.graphicsDevices[0]@ is the corresponding @VZVirtioGraphicsDevice@.
--
-- An important property is the @displays@ inherited from @VZGraphicsDevice@. It provides the list of scanouts on the graphics device,    each corresponding to the scanout configuration set on @VZVirtioGraphicsDeviceConfiguration@.
--
-- See: VZVirtioGraphicsDeviceConfiguration
--
-- See: VZGraphicsDevice.displays
-- 
-- Phantom type for @VZVirtioGraphicsDevice@.
data VZVirtioGraphicsDevice

instance IsObjCObject (Id VZVirtioGraphicsDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioGraphicsDevice"

class IsVZGraphicsDevice a => IsVZVirtioGraphicsDevice a where
  toVZVirtioGraphicsDevice :: a -> Id VZVirtioGraphicsDevice

instance IsVZVirtioGraphicsDevice (Id VZVirtioGraphicsDevice) where
  toVZVirtioGraphicsDevice = unsafeCastId

instance IsNSObject (Id VZVirtioGraphicsDevice) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDevice (Id VZVirtioGraphicsDevice) where
  toVZGraphicsDevice = unsafeCastId

-- ---------- VZMacGraphicsDeviceConfiguration ----------

-- | Configuration for a Mac graphics device.
--
-- This device can be used to attach a display to be shown in a VZVirtualMachineView.
-- 
-- Phantom type for @VZMacGraphicsDeviceConfiguration@.
data VZMacGraphicsDeviceConfiguration

instance IsObjCObject (Id VZMacGraphicsDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacGraphicsDeviceConfiguration"

class IsVZGraphicsDeviceConfiguration a => IsVZMacGraphicsDeviceConfiguration a where
  toVZMacGraphicsDeviceConfiguration :: a -> Id VZMacGraphicsDeviceConfiguration

instance IsVZMacGraphicsDeviceConfiguration (Id VZMacGraphicsDeviceConfiguration) where
  toVZMacGraphicsDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZMacGraphicsDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDeviceConfiguration (Id VZMacGraphicsDeviceConfiguration) where
  toVZGraphicsDeviceConfiguration = unsafeCastId

-- ---------- VZVirtioGraphicsDeviceConfiguration ----------

-- | Configuration for a Virtio graphics device.
--
-- This device configuration creates a graphics device using paravirtualization.    The emulated device follows the Virtio GPU Device specification.
--
-- This device can be used to attach a display to be shown in a VZVirtualMachineView.
-- 
-- Phantom type for @VZVirtioGraphicsDeviceConfiguration@.
data VZVirtioGraphicsDeviceConfiguration

instance IsObjCObject (Id VZVirtioGraphicsDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioGraphicsDeviceConfiguration"

class IsVZGraphicsDeviceConfiguration a => IsVZVirtioGraphicsDeviceConfiguration a where
  toVZVirtioGraphicsDeviceConfiguration :: a -> Id VZVirtioGraphicsDeviceConfiguration

instance IsVZVirtioGraphicsDeviceConfiguration (Id VZVirtioGraphicsDeviceConfiguration) where
  toVZVirtioGraphicsDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioGraphicsDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDeviceConfiguration (Id VZVirtioGraphicsDeviceConfiguration) where
  toVZGraphicsDeviceConfiguration = unsafeCastId

-- ---------- VZMacGraphicsDisplay ----------

-- | Class representing a virtual Mac graphics display.
--
-- The VZMacGraphicsDisplay is the runtime counterpart of VZMacGraphicsDisplayConfiguration.
--
-- When a graphics device is configured with class VZMacGraphicsDisplayConfiguration,    the VZGraphicsDevice's displays are in the same order as their configuration objects and they have the type VZMacGraphicsDisplay.
--
-- For example, if when setting up a virtual Mac, @VZMacGraphicsDeviceConfiguration.displays[0]@ is a @VZMacGraphicsDisplayConfiguration@,    then after creating a virtual machine from the configuration, the @VZVirtualMachine.graphicsDevices@ is a @VZMacGraphicsDevice@.    The @VZMacGraphicsDevice.displays[0]@ is a @VZMacGraphicsDisplay@ corresponding to the @VZMacGraphicsDeviceConfiguration@ in the configuration.
--
-- See: VZMacGraphicsDisplayConfiguration
--
-- See: VZGraphicsDevice
-- 
-- Phantom type for @VZMacGraphicsDisplay@.
data VZMacGraphicsDisplay

instance IsObjCObject (Id VZMacGraphicsDisplay) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacGraphicsDisplay"

class IsVZGraphicsDisplay a => IsVZMacGraphicsDisplay a where
  toVZMacGraphicsDisplay :: a -> Id VZMacGraphicsDisplay

instance IsVZMacGraphicsDisplay (Id VZMacGraphicsDisplay) where
  toVZMacGraphicsDisplay = unsafeCastId

instance IsNSObject (Id VZMacGraphicsDisplay) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDisplay (Id VZMacGraphicsDisplay) where
  toVZGraphicsDisplay = unsafeCastId

-- ---------- VZVirtioGraphicsScanout ----------

-- | Class representing a Virtio graphics device scanout.
--
-- The VZVirtioGraphicsScanout is the runtime counterpart of VZVirtioGraphicsScanoutConfiguration.
--
-- When a graphics device is configured with class VZVirtioGraphicsScanoutConfiguration,    the VZGraphicsDevice's displays are in the same order as their configuration objects and they have the type VZVirtioGraphicsScanout.
--
-- For example, if when setting up a virtual machine, its @VZVirtioGraphicsDeviceConfiguration.scanouts[0]@ is a @VZVirtioGraphicsScanoutConfiguration@,    then after creating a virtual machine from the configuration, the @VZVirtualMachine.graphicsDevices@ is a @VZVirtioGraphicsDevice@.    The @VZVirtioGraphicsDevice.displays[0]@ is a @VZVirtioGraphicsScanout@ corresponding to the @VZVirtioGraphicsScanoutConfiguration@ in the configuration.
--
-- See: VZVirtioGraphicsScanoutConfiguration
--
-- See: VZGraphicsDisplay
--
-- See: VZGraphicsDevice
-- 
-- Phantom type for @VZVirtioGraphicsScanout@.
data VZVirtioGraphicsScanout

instance IsObjCObject (Id VZVirtioGraphicsScanout) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioGraphicsScanout"

class IsVZGraphicsDisplay a => IsVZVirtioGraphicsScanout a where
  toVZVirtioGraphicsScanout :: a -> Id VZVirtioGraphicsScanout

instance IsVZVirtioGraphicsScanout (Id VZVirtioGraphicsScanout) where
  toVZVirtioGraphicsScanout = unsafeCastId

instance IsNSObject (Id VZVirtioGraphicsScanout) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDisplay (Id VZVirtioGraphicsScanout) where
  toVZGraphicsDisplay = unsafeCastId

-- ---------- VZMacGraphicsDisplayConfiguration ----------

-- | Configuration for a display attached to a Mac graphics device.
--
-- This display can be shown in a VZVirtualMachineView.
-- 
-- Phantom type for @VZMacGraphicsDisplayConfiguration@.
data VZMacGraphicsDisplayConfiguration

instance IsObjCObject (Id VZMacGraphicsDisplayConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacGraphicsDisplayConfiguration"

class IsVZGraphicsDisplayConfiguration a => IsVZMacGraphicsDisplayConfiguration a where
  toVZMacGraphicsDisplayConfiguration :: a -> Id VZMacGraphicsDisplayConfiguration

instance IsVZMacGraphicsDisplayConfiguration (Id VZMacGraphicsDisplayConfiguration) where
  toVZMacGraphicsDisplayConfiguration = unsafeCastId

instance IsNSObject (Id VZMacGraphicsDisplayConfiguration) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDisplayConfiguration (Id VZMacGraphicsDisplayConfiguration) where
  toVZGraphicsDisplayConfiguration = unsafeCastId

-- ---------- VZVirtioGraphicsScanoutConfiguration ----------

-- | Configuration for a scanout attached to a Virtio graphics device.
--
-- This scanout can be shown in a VZVirtualMachineView.
--
-- VZVirtioGraphicsDeviceConfiguration
-- 
-- Phantom type for @VZVirtioGraphicsScanoutConfiguration@.
data VZVirtioGraphicsScanoutConfiguration

instance IsObjCObject (Id VZVirtioGraphicsScanoutConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioGraphicsScanoutConfiguration"

class IsVZGraphicsDisplayConfiguration a => IsVZVirtioGraphicsScanoutConfiguration a where
  toVZVirtioGraphicsScanoutConfiguration :: a -> Id VZVirtioGraphicsScanoutConfiguration

instance IsVZVirtioGraphicsScanoutConfiguration (Id VZVirtioGraphicsScanoutConfiguration) where
  toVZVirtioGraphicsScanoutConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioGraphicsScanoutConfiguration) where
  toNSObject = unsafeCastId

instance IsVZGraphicsDisplayConfiguration (Id VZVirtioGraphicsScanoutConfiguration) where
  toVZGraphicsDisplayConfiguration = unsafeCastId

-- ---------- VZMacKeyboardConfiguration ----------

-- | Configuration for a Mac keyboard.
--
-- This device can be used by VZVirtualMachineView to send keyboard events to the virtual machine.    This keyboard supports Apple-specific features such as the globe key.    Note: this device is only recognized by virtual machines running macOS 13.0 and later. In order to support both macOS 13.0 and earlier    guests, VZVirtualMachineConfiguration.keyboards can be set to an array containing both a VZMacKeyboardConfiguration and    a VZUSBKeyboardConfiguration object. macOS 13.0 and later guests will use the Mac keyboard device,    while earlier versions of macOS will use the USB keyboard device.
-- 
-- Phantom type for @VZMacKeyboardConfiguration@.
data VZMacKeyboardConfiguration

instance IsObjCObject (Id VZMacKeyboardConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacKeyboardConfiguration"

class IsVZKeyboardConfiguration a => IsVZMacKeyboardConfiguration a where
  toVZMacKeyboardConfiguration :: a -> Id VZMacKeyboardConfiguration

instance IsVZMacKeyboardConfiguration (Id VZMacKeyboardConfiguration) where
  toVZMacKeyboardConfiguration = unsafeCastId

instance IsNSObject (Id VZMacKeyboardConfiguration) where
  toNSObject = unsafeCastId

instance IsVZKeyboardConfiguration (Id VZMacKeyboardConfiguration) where
  toVZKeyboardConfiguration = unsafeCastId

-- ---------- VZUSBKeyboardConfiguration ----------

-- | Configuration for a USB keyboard.
--
-- This device can be used by VZVirtualMachineView to send key events to the virtual machine.
-- 
-- Phantom type for @VZUSBKeyboardConfiguration@.
data VZUSBKeyboardConfiguration

instance IsObjCObject (Id VZUSBKeyboardConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZUSBKeyboardConfiguration"

class IsVZKeyboardConfiguration a => IsVZUSBKeyboardConfiguration a where
  toVZUSBKeyboardConfiguration :: a -> Id VZUSBKeyboardConfiguration

instance IsVZUSBKeyboardConfiguration (Id VZUSBKeyboardConfiguration) where
  toVZUSBKeyboardConfiguration = unsafeCastId

instance IsNSObject (Id VZUSBKeyboardConfiguration) where
  toNSObject = unsafeCastId

instance IsVZKeyboardConfiguration (Id VZUSBKeyboardConfiguration) where
  toVZKeyboardConfiguration = unsafeCastId

-- ---------- VZLinuxRosettaAbstractSocketCachingOptions ----------

-- | Caching options for an Abstract Socket.
--
-- This object configures Rosetta to communicate with the Rosetta daemon using an Abstract Socket.
--
-- See: VZLinuxRosettaCachingOptions
-- 
-- Phantom type for @VZLinuxRosettaAbstractSocketCachingOptions@.
data VZLinuxRosettaAbstractSocketCachingOptions

instance IsObjCObject (Id VZLinuxRosettaAbstractSocketCachingOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZLinuxRosettaAbstractSocketCachingOptions"

class IsVZLinuxRosettaCachingOptions a => IsVZLinuxRosettaAbstractSocketCachingOptions a where
  toVZLinuxRosettaAbstractSocketCachingOptions :: a -> Id VZLinuxRosettaAbstractSocketCachingOptions

instance IsVZLinuxRosettaAbstractSocketCachingOptions (Id VZLinuxRosettaAbstractSocketCachingOptions) where
  toVZLinuxRosettaAbstractSocketCachingOptions = unsafeCastId

instance IsNSObject (Id VZLinuxRosettaAbstractSocketCachingOptions) where
  toNSObject = unsafeCastId

instance IsVZLinuxRosettaCachingOptions (Id VZLinuxRosettaAbstractSocketCachingOptions) where
  toVZLinuxRosettaCachingOptions = unsafeCastId

-- ---------- VZLinuxRosettaUnixSocketCachingOptions ----------

-- | Caching options for a Unix Domain Socket.
--
-- This object configures Rosetta to communicate with the Rosetta daemon using a Unix Domain Socket.
--
-- See: VZLinuxRosettaCachingOptions
-- 
-- Phantom type for @VZLinuxRosettaUnixSocketCachingOptions@.
data VZLinuxRosettaUnixSocketCachingOptions

instance IsObjCObject (Id VZLinuxRosettaUnixSocketCachingOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZLinuxRosettaUnixSocketCachingOptions"

class IsVZLinuxRosettaCachingOptions a => IsVZLinuxRosettaUnixSocketCachingOptions a where
  toVZLinuxRosettaUnixSocketCachingOptions :: a -> Id VZLinuxRosettaUnixSocketCachingOptions

instance IsVZLinuxRosettaUnixSocketCachingOptions (Id VZLinuxRosettaUnixSocketCachingOptions) where
  toVZLinuxRosettaUnixSocketCachingOptions = unsafeCastId

instance IsNSObject (Id VZLinuxRosettaUnixSocketCachingOptions) where
  toNSObject = unsafeCastId

instance IsVZLinuxRosettaCachingOptions (Id VZLinuxRosettaUnixSocketCachingOptions) where
  toVZLinuxRosettaCachingOptions = unsafeCastId

-- ---------- VZVirtioTraditionalMemoryBalloonDevice ----------

-- | Virtio Traditional Memory Balloon Device
--
-- This is a primitive device for managing guest memory.    This device enables memory transfer between the host and the guest as specified by the Virtio specification, which allows the guest to adapt changes    in allowance of underlying physical memory.
--
-- To request a memory balloon device operation for the memory transfer, write the targeted memory size for the virtual machine to the targetVirtualMachineMemorySize property.    When the value written to targetVirtualMachineMemorySize is less than the current value, memory will be taken away from the guest and given to the host by the amount    determined by the difference between those two values. Similarly, when the value written to targetVirtualMachineMemorySize is greater than the current value, memory will be    given back to the guest by the amount determined by the difference between those two values.
--
-- Note that any changes to targetVirtualMachineMemorySize is a request to trigger a memory balloon operation. The actual changes in memory only happen after the guest operating    system handles the request, if at all.
--
-- The targetVirtualMachineMemorySize property is initialized to VZVirtualMachineConfiguration.memorySize. The acceptable values for the targetVirtualMachineMemorySize    property range from VZVirtualMachineConfiguration.minimumAllowedMemorySize to VZVirtualMachineConfiguration.memorySize, and must be a multiple of 1 megabyte    (1024 * 1024 bytes). If those constraints aren't satisfied, targetVirtualMachineMemorySize will be rounded down to the nearest multiple of 1 megabyte, clamped to    VZVirtualMachineConfiguration.minimumAllowedMemorySize and VZVirtualMachineConfiguration.memorySize respectively.
--
-- For optimal performance, it is strongly recommended to perform a memory compaction operation in the guest (e.g. echo 1 > /proc/sys/vm/compact_memory) before invoking the device.    This helps to minimize memory fragmentation in order for the memory allocation/deallocation process to be more effective.
--
-- This device is created through instantiating a VZVirtioTraditionalMemoryBalloonDeviceConfiguration in a VZVirtualMachineConfiguration and is available in the    VZVirtualMachine.memoryBalloonDevices property.
--
-- See: VZVirtioTraditionalMemoryBalloonDeviceConfiguration
-- 
-- Phantom type for @VZVirtioTraditionalMemoryBalloonDevice@.
data VZVirtioTraditionalMemoryBalloonDevice

instance IsObjCObject (Id VZVirtioTraditionalMemoryBalloonDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioTraditionalMemoryBalloonDevice"

class IsVZMemoryBalloonDevice a => IsVZVirtioTraditionalMemoryBalloonDevice a where
  toVZVirtioTraditionalMemoryBalloonDevice :: a -> Id VZVirtioTraditionalMemoryBalloonDevice

instance IsVZVirtioTraditionalMemoryBalloonDevice (Id VZVirtioTraditionalMemoryBalloonDevice) where
  toVZVirtioTraditionalMemoryBalloonDevice = unsafeCastId

instance IsNSObject (Id VZVirtioTraditionalMemoryBalloonDevice) where
  toNSObject = unsafeCastId

instance IsVZMemoryBalloonDevice (Id VZVirtioTraditionalMemoryBalloonDevice) where
  toVZMemoryBalloonDevice = unsafeCastId

-- ---------- VZVirtioTraditionalMemoryBalloonDeviceConfiguration ----------

-- | Configuration of the Virtio traditional memory balloon device.
--
-- This configuration creates a Virtio traditional memory balloon device which allows for managing guest memory.    Only one Virtio traditional memory balloon device can be used per virtual machine.
--
-- See: VZVirtioTraditionalMemoryBalloonDevice
-- 
-- Phantom type for @VZVirtioTraditionalMemoryBalloonDeviceConfiguration@.
data VZVirtioTraditionalMemoryBalloonDeviceConfiguration

instance IsObjCObject (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioTraditionalMemoryBalloonDeviceConfiguration"

class IsVZMemoryBalloonDeviceConfiguration a => IsVZVirtioTraditionalMemoryBalloonDeviceConfiguration a where
  toVZVirtioTraditionalMemoryBalloonDeviceConfiguration :: a -> Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration

instance IsVZVirtioTraditionalMemoryBalloonDeviceConfiguration (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration) where
  toVZVirtioTraditionalMemoryBalloonDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZMemoryBalloonDeviceConfiguration (Id VZVirtioTraditionalMemoryBalloonDeviceConfiguration) where
  toVZMemoryBalloonDeviceConfiguration = unsafeCastId

-- ---------- VZBridgedNetworkDeviceAttachment ----------

-- | Network device attachment bridging a host physical interface with a virtual network device.
--
-- A bridged network allows the virtual machine to use the same physical interface as the host. Both host and virtual machine    send and receive packets on the same physical interface but have distinct network layers.
--
-- The bridge network device attachment is used with a VZNetworkDeviceConfiguration to define a virtual network device.
--
-- Using a VZBridgedNetworkDeviceAttachment requires the app to have the "com.apple.vm.networking" entitlement.
--
-- See: VZBridgedNetworkInterface
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
-- 
-- Phantom type for @VZBridgedNetworkDeviceAttachment@.
data VZBridgedNetworkDeviceAttachment

instance IsObjCObject (Id VZBridgedNetworkDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZBridgedNetworkDeviceAttachment"

class IsVZNetworkDeviceAttachment a => IsVZBridgedNetworkDeviceAttachment a where
  toVZBridgedNetworkDeviceAttachment :: a -> Id VZBridgedNetworkDeviceAttachment

instance IsVZBridgedNetworkDeviceAttachment (Id VZBridgedNetworkDeviceAttachment) where
  toVZBridgedNetworkDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZBridgedNetworkDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZNetworkDeviceAttachment (Id VZBridgedNetworkDeviceAttachment) where
  toVZNetworkDeviceAttachment = unsafeCastId

-- ---------- VZFileHandleNetworkDeviceAttachment ----------

-- | Network device attachment sending raw network packets over a file handle.
--
-- The file handle attachment transmits the raw packets/frames between the virtual network interface and a file handle.    The data transmitted through this attachment is at the level of the data link layer.
--
-- The file handle must hold a connected datagram socket.
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
-- 
-- Phantom type for @VZFileHandleNetworkDeviceAttachment@.
data VZFileHandleNetworkDeviceAttachment

instance IsObjCObject (Id VZFileHandleNetworkDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZFileHandleNetworkDeviceAttachment"

class IsVZNetworkDeviceAttachment a => IsVZFileHandleNetworkDeviceAttachment a where
  toVZFileHandleNetworkDeviceAttachment :: a -> Id VZFileHandleNetworkDeviceAttachment

instance IsVZFileHandleNetworkDeviceAttachment (Id VZFileHandleNetworkDeviceAttachment) where
  toVZFileHandleNetworkDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZFileHandleNetworkDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZNetworkDeviceAttachment (Id VZFileHandleNetworkDeviceAttachment) where
  toVZNetworkDeviceAttachment = unsafeCastId

-- ---------- VZNATNetworkDeviceAttachment ----------

-- | Network device attachment using network address translation (NAT) with outside networks.
--
-- Using the NAT attachment type, the host serves as router and performs network address translation for accesses to outside networks.
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
-- 
-- Phantom type for @VZNATNetworkDeviceAttachment@.
data VZNATNetworkDeviceAttachment

instance IsObjCObject (Id VZNATNetworkDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZNATNetworkDeviceAttachment"

class IsVZNetworkDeviceAttachment a => IsVZNATNetworkDeviceAttachment a where
  toVZNATNetworkDeviceAttachment :: a -> Id VZNATNetworkDeviceAttachment

instance IsVZNATNetworkDeviceAttachment (Id VZNATNetworkDeviceAttachment) where
  toVZNATNetworkDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZNATNetworkDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZNetworkDeviceAttachment (Id VZNATNetworkDeviceAttachment) where
  toVZNetworkDeviceAttachment = unsafeCastId

-- ---------- VZVmnetNetworkDeviceAttachment ----------

-- | Network device attachment that allows custom network topology.
--
-- This attachment is backed by a logical network which is created and customized through    the vmnet framework APIs to allow custom network topology, which allows multiple virtual    machines to appear on the same network and connect with each other.
--
-- See: VZNetworkDeviceConfiguration
--
-- See: VZVirtioNetworkDeviceConfiguration
-- 
-- Phantom type for @VZVmnetNetworkDeviceAttachment@.
data VZVmnetNetworkDeviceAttachment

instance IsObjCObject (Id VZVmnetNetworkDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVmnetNetworkDeviceAttachment"

class IsVZNetworkDeviceAttachment a => IsVZVmnetNetworkDeviceAttachment a where
  toVZVmnetNetworkDeviceAttachment :: a -> Id VZVmnetNetworkDeviceAttachment

instance IsVZVmnetNetworkDeviceAttachment (Id VZVmnetNetworkDeviceAttachment) where
  toVZVmnetNetworkDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZVmnetNetworkDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZNetworkDeviceAttachment (Id VZVmnetNetworkDeviceAttachment) where
  toVZNetworkDeviceAttachment = unsafeCastId

-- ---------- VZVirtioNetworkDeviceConfiguration ----------

-- | Configuration of a paravirtualized network device of type Virtio Network Device.
--
-- The communication channel used on the host is defined through the attachment. It is set with the VZNetworkDeviceConfiguration.attachment property.
--
-- The configuration is only valid with valid MACAddress and attachment.
--
-- See: VZVirtualMachineConfiguration.networkDevices
-- 
-- Phantom type for @VZVirtioNetworkDeviceConfiguration@.
data VZVirtioNetworkDeviceConfiguration

instance IsObjCObject (Id VZVirtioNetworkDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioNetworkDeviceConfiguration"

class IsVZNetworkDeviceConfiguration a => IsVZVirtioNetworkDeviceConfiguration a where
  toVZVirtioNetworkDeviceConfiguration :: a -> Id VZVirtioNetworkDeviceConfiguration

instance IsVZVirtioNetworkDeviceConfiguration (Id VZVirtioNetworkDeviceConfiguration) where
  toVZVirtioNetworkDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioNetworkDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZNetworkDeviceConfiguration (Id VZVirtioNetworkDeviceConfiguration) where
  toVZNetworkDeviceConfiguration = unsafeCastId

-- ---------- VZGenericPlatformConfiguration ----------

-- | The platform configuration for a generic Intel or ARM virtual machine.
--
-- When a virtual machine is saved to disk then loaded again, the @machineIdentifier@    must be restored to the original value.
--
-- If multiple virtual machines are created from the same configuration, each should have a unique @machineIdentifier@.
-- 
-- Phantom type for @VZGenericPlatformConfiguration@.
data VZGenericPlatformConfiguration

instance IsObjCObject (Id VZGenericPlatformConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZGenericPlatformConfiguration"

class IsVZPlatformConfiguration a => IsVZGenericPlatformConfiguration a where
  toVZGenericPlatformConfiguration :: a -> Id VZGenericPlatformConfiguration

instance IsVZGenericPlatformConfiguration (Id VZGenericPlatformConfiguration) where
  toVZGenericPlatformConfiguration = unsafeCastId

instance IsNSObject (Id VZGenericPlatformConfiguration) where
  toNSObject = unsafeCastId

instance IsVZPlatformConfiguration (Id VZGenericPlatformConfiguration) where
  toVZPlatformConfiguration = unsafeCastId

-- ---------- VZMacPlatformConfiguration ----------

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
-- Phantom type for @VZMacPlatformConfiguration@.
data VZMacPlatformConfiguration

instance IsObjCObject (Id VZMacPlatformConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacPlatformConfiguration"

class IsVZPlatformConfiguration a => IsVZMacPlatformConfiguration a where
  toVZMacPlatformConfiguration :: a -> Id VZMacPlatformConfiguration

instance IsVZMacPlatformConfiguration (Id VZMacPlatformConfiguration) where
  toVZMacPlatformConfiguration = unsafeCastId

instance IsNSObject (Id VZMacPlatformConfiguration) where
  toNSObject = unsafeCastId

instance IsVZPlatformConfiguration (Id VZMacPlatformConfiguration) where
  toVZPlatformConfiguration = unsafeCastId

-- ---------- VZMacTrackpadConfiguration ----------

-- | Configuration for a Mac trackpad.
--
-- This device can be used by VZVirtualMachineView to send pointer events and multi-touch trackpad gestures to the virtual machine.    Note: this device is only recognized by virtual machines running macOS 13.0 and later. In order to support both macOS 13.0 and earlier    guests, VZVirtualMachineConfiguration.pointingDevices can be set to an array containing both a VZMacTrackpadConfiguration and    a VZUSBScreenCoordinatePointingDeviceConfiguration object. macOS 13.0 and later guests will use the multi-touch trackpad device,    while earlier versions of macOS will use the USB pointing device.
-- 
-- Phantom type for @VZMacTrackpadConfiguration@.
data VZMacTrackpadConfiguration

instance IsObjCObject (Id VZMacTrackpadConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacTrackpadConfiguration"

class IsVZPointingDeviceConfiguration a => IsVZMacTrackpadConfiguration a where
  toVZMacTrackpadConfiguration :: a -> Id VZMacTrackpadConfiguration

instance IsVZMacTrackpadConfiguration (Id VZMacTrackpadConfiguration) where
  toVZMacTrackpadConfiguration = unsafeCastId

instance IsNSObject (Id VZMacTrackpadConfiguration) where
  toNSObject = unsafeCastId

instance IsVZPointingDeviceConfiguration (Id VZMacTrackpadConfiguration) where
  toVZPointingDeviceConfiguration = unsafeCastId

-- ---------- VZUSBScreenCoordinatePointingDeviceConfiguration ----------

-- | Configuration for a USB pointing device that reports absolute coordinates.
--
-- This device can be used by VZVirtualMachineView to send pointer events to the virtual machine.
-- 
-- Phantom type for @VZUSBScreenCoordinatePointingDeviceConfiguration@.
data VZUSBScreenCoordinatePointingDeviceConfiguration

instance IsObjCObject (Id VZUSBScreenCoordinatePointingDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZUSBScreenCoordinatePointingDeviceConfiguration"

class IsVZPointingDeviceConfiguration a => IsVZUSBScreenCoordinatePointingDeviceConfiguration a where
  toVZUSBScreenCoordinatePointingDeviceConfiguration :: a -> Id VZUSBScreenCoordinatePointingDeviceConfiguration

instance IsVZUSBScreenCoordinatePointingDeviceConfiguration (Id VZUSBScreenCoordinatePointingDeviceConfiguration) where
  toVZUSBScreenCoordinatePointingDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZUSBScreenCoordinatePointingDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZPointingDeviceConfiguration (Id VZUSBScreenCoordinatePointingDeviceConfiguration) where
  toVZPointingDeviceConfiguration = unsafeCastId

-- ---------- VZFileHandleSerialPortAttachment ----------

-- | File handle serial port attachment.
--
-- VZFileHandleSerialPortAttachment defines a serial port attachment from a file handle.    Data written to fileHandleForReading goes to the guest. Data sent from the guest appears on fileHandleForWriting.
-- 
-- Phantom type for @VZFileHandleSerialPortAttachment@.
data VZFileHandleSerialPortAttachment

instance IsObjCObject (Id VZFileHandleSerialPortAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZFileHandleSerialPortAttachment"

class IsVZSerialPortAttachment a => IsVZFileHandleSerialPortAttachment a where
  toVZFileHandleSerialPortAttachment :: a -> Id VZFileHandleSerialPortAttachment

instance IsVZFileHandleSerialPortAttachment (Id VZFileHandleSerialPortAttachment) where
  toVZFileHandleSerialPortAttachment = unsafeCastId

instance IsNSObject (Id VZFileHandleSerialPortAttachment) where
  toNSObject = unsafeCastId

instance IsVZSerialPortAttachment (Id VZFileHandleSerialPortAttachment) where
  toVZSerialPortAttachment = unsafeCastId

-- ---------- VZFileSerialPortAttachment ----------

-- | File serial port attachment.
--
-- VZFileSerialPortAttachment defines a serial port attachment from a file.    Any data sent by the guest on the serial interface is written to the file.    No data is sent to the guest over serial with this attachment.
-- 
-- Phantom type for @VZFileSerialPortAttachment@.
data VZFileSerialPortAttachment

instance IsObjCObject (Id VZFileSerialPortAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZFileSerialPortAttachment"

class IsVZSerialPortAttachment a => IsVZFileSerialPortAttachment a where
  toVZFileSerialPortAttachment :: a -> Id VZFileSerialPortAttachment

instance IsVZFileSerialPortAttachment (Id VZFileSerialPortAttachment) where
  toVZFileSerialPortAttachment = unsafeCastId

instance IsNSObject (Id VZFileSerialPortAttachment) where
  toNSObject = unsafeCastId

instance IsVZSerialPortAttachment (Id VZFileSerialPortAttachment) where
  toVZSerialPortAttachment = unsafeCastId

-- ---------- VZSpiceAgentPortAttachment ----------

-- | Phantom type for @VZSpiceAgentPortAttachment@.
data VZSpiceAgentPortAttachment

instance IsObjCObject (Id VZSpiceAgentPortAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZSpiceAgentPortAttachment"

class IsVZSerialPortAttachment a => IsVZSpiceAgentPortAttachment a where
  toVZSpiceAgentPortAttachment :: a -> Id VZSpiceAgentPortAttachment

instance IsVZSpiceAgentPortAttachment (Id VZSpiceAgentPortAttachment) where
  toVZSpiceAgentPortAttachment = unsafeCastId

instance IsNSObject (Id VZSpiceAgentPortAttachment) where
  toNSObject = unsafeCastId

instance IsVZSerialPortAttachment (Id VZSpiceAgentPortAttachment) where
  toVZSerialPortAttachment = unsafeCastId

-- ---------- VZVirtioConsoleDeviceSerialPortConfiguration ----------

-- | Virtio Console Serial Port Device
--
-- The device creates a console which enables communication between the host and the guest through the Virtio interface.
--
-- The device sets up a single port on the Virtio console device.
-- 
-- Phantom type for @VZVirtioConsoleDeviceSerialPortConfiguration@.
data VZVirtioConsoleDeviceSerialPortConfiguration

instance IsObjCObject (Id VZVirtioConsoleDeviceSerialPortConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioConsoleDeviceSerialPortConfiguration"

class IsVZSerialPortConfiguration a => IsVZVirtioConsoleDeviceSerialPortConfiguration a where
  toVZVirtioConsoleDeviceSerialPortConfiguration :: a -> Id VZVirtioConsoleDeviceSerialPortConfiguration

instance IsVZVirtioConsoleDeviceSerialPortConfiguration (Id VZVirtioConsoleDeviceSerialPortConfiguration) where
  toVZVirtioConsoleDeviceSerialPortConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioConsoleDeviceSerialPortConfiguration) where
  toNSObject = unsafeCastId

instance IsVZSerialPortConfiguration (Id VZVirtioConsoleDeviceSerialPortConfiguration) where
  toVZSerialPortConfiguration = unsafeCastId

-- ---------- VZVirtioSocketDevice ----------

-- | Virtio Socket Device
--
-- This is a paravirtualized socket device which facilitates data transfer between the guest and the host without using Ethernet or IP protocols.    This device is created through instantiating a VZVirtioSocketDeviceConfiguration in a VZVirtualMachineConfiguration and is available in the VZVirtualMachine.socketDevices property.
--
-- See: VZVirtioSocketDeviceConfiguration
--
-- See: VZVirtioSocketDeviceConnection
--
-- See: VZVirtioSocketDeviceListener
-- 
-- Phantom type for @VZVirtioSocketDevice@.
data VZVirtioSocketDevice

instance IsObjCObject (Id VZVirtioSocketDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSocketDevice"

class IsVZSocketDevice a => IsVZVirtioSocketDevice a where
  toVZVirtioSocketDevice :: a -> Id VZVirtioSocketDevice

instance IsVZVirtioSocketDevice (Id VZVirtioSocketDevice) where
  toVZVirtioSocketDevice = unsafeCastId

instance IsNSObject (Id VZVirtioSocketDevice) where
  toNSObject = unsafeCastId

instance IsVZSocketDevice (Id VZVirtioSocketDevice) where
  toVZSocketDevice = unsafeCastId

-- ---------- VZVirtioSocketDeviceConfiguration ----------

-- | Configuration of the Virtio socket device.
--
-- This configuration creates a Virtio socket device for the guest which communicates with the host through the Virtio interface.
--
-- Only one Virtio socket device can be used per virtual machine.
--
-- See: VZVirtioSocketDevice
-- 
-- Phantom type for @VZVirtioSocketDeviceConfiguration@.
data VZVirtioSocketDeviceConfiguration

instance IsObjCObject (Id VZVirtioSocketDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSocketDeviceConfiguration"

class IsVZSocketDeviceConfiguration a => IsVZVirtioSocketDeviceConfiguration a where
  toVZVirtioSocketDeviceConfiguration :: a -> Id VZVirtioSocketDeviceConfiguration

instance IsVZVirtioSocketDeviceConfiguration (Id VZVirtioSocketDeviceConfiguration) where
  toVZVirtioSocketDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioSocketDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZSocketDeviceConfiguration (Id VZVirtioSocketDeviceConfiguration) where
  toVZSocketDeviceConfiguration = unsafeCastId

-- ---------- VZUSBMassStorageDevice ----------

-- | Class representing a hot-pluggable USB Mass Storage device.
--
-- This device is created through either instantiating it directly and passing VZUSBMassStorageDeviceConfiguration to its initializer    or instantiating a VZUSBMassStorageDeviceConfiguration in a VZVirtualMachineConfiguration. Direct instantiation will create    an object that can be passed to -[VZUSBController attachDevice:completionHandler:] method. Instantiation via VZUSBMassStorageDeviceConfiguration    will make the device available in VZUSBController.usbDevices property.
--
-- See: VZUSBController
-- 
-- Phantom type for @VZUSBMassStorageDevice@.
data VZUSBMassStorageDevice

instance IsObjCObject (Id VZUSBMassStorageDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZUSBMassStorageDevice"

class IsVZStorageDevice a => IsVZUSBMassStorageDevice a where
  toVZUSBMassStorageDevice :: a -> Id VZUSBMassStorageDevice

instance IsVZUSBMassStorageDevice (Id VZUSBMassStorageDevice) where
  toVZUSBMassStorageDevice = unsafeCastId

instance IsNSObject (Id VZUSBMassStorageDevice) where
  toNSObject = unsafeCastId

instance IsVZStorageDevice (Id VZUSBMassStorageDevice) where
  toVZStorageDevice = unsafeCastId

-- ---------- VZDiskBlockDeviceStorageDeviceAttachment ----------

-- | Storage device attachment using a disk block device to store data.
--
-- The disk block device implements a storage attachment by using an actual disk rather than a disk image on a file system.
--
-- For example, a disk block device on the disk at @/dev/rdisk42@ would execute the I/O operations directly on that disk    rather than through a file system.
--
-- Note that if the disk has a file system formatted on it, the guest is able to destroy data in a way that is not recoverable.    The disk passed to this attachment needs to be handled with caution.
--
-- An example use of this API is:    ```    NSFileHandle *fileHandle = [NSFileHandle fileHandleForReadingAtPath:"/dev/rdisk42"];    if (!fileHandle) {        // Handle errors.    }
--
-- NSError *error;    VZDiskBlockDeviceStorageDeviceAttachment *attachment =        [[VZDiskBlockDeviceStorageDeviceAttachment alloc] initWithFileHandle:fileHandle                                                                     readOnly:YES                                                          synchronizationMode:VZDiskSynchronizationModeFull                                                                        error:error];    if (!attachment) {        // Handle errors.    }    ```
--
-- Disk file handles are typically only accessible by the @root@ user unless permission is explicitly granted.    Running virtual machines as root is not recommended. If @root@ access is required to open the file descriptor, it is recommended to do that operation    in a separate process then pass the file descriptor to a less privileged process running Virtualization framework.
--
-- See: VZDiskImageStorageDeviceAttachment
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
-- 
-- Phantom type for @VZDiskBlockDeviceStorageDeviceAttachment@.
data VZDiskBlockDeviceStorageDeviceAttachment

instance IsObjCObject (Id VZDiskBlockDeviceStorageDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZDiskBlockDeviceStorageDeviceAttachment"

class IsVZStorageDeviceAttachment a => IsVZDiskBlockDeviceStorageDeviceAttachment a where
  toVZDiskBlockDeviceStorageDeviceAttachment :: a -> Id VZDiskBlockDeviceStorageDeviceAttachment

instance IsVZDiskBlockDeviceStorageDeviceAttachment (Id VZDiskBlockDeviceStorageDeviceAttachment) where
  toVZDiskBlockDeviceStorageDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZDiskBlockDeviceStorageDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZStorageDeviceAttachment (Id VZDiskBlockDeviceStorageDeviceAttachment) where
  toVZStorageDeviceAttachment = unsafeCastId

-- ---------- VZDiskImageStorageDeviceAttachment ----------

-- | Storage device attachment using a disk image to implement the storage.
--
-- This storage device attachment uses a disk image on the host file system as the drive of the storage device.
--
-- Only RAW data disk images are supported.
--
-- An empty RAW disk image can be created with @FileDescriptor.resize(to:retryOnInterrupt:)@ method in Swift,    the @ftruncate()@ function in Swift or Objective-C, or the @truncate@ command on the command line.    The size of the file must be a multiple of 512 bytes, the block size.
--
-- See: VZNVMExpressControllerDeviceConfiguration
--
-- See: VZUSBMassStorageDeviceConfiguration
--
-- See: VZVirtioBlockDeviceConfiguration
-- 
-- Phantom type for @VZDiskImageStorageDeviceAttachment@.
data VZDiskImageStorageDeviceAttachment

instance IsObjCObject (Id VZDiskImageStorageDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZDiskImageStorageDeviceAttachment"

class IsVZStorageDeviceAttachment a => IsVZDiskImageStorageDeviceAttachment a where
  toVZDiskImageStorageDeviceAttachment :: a -> Id VZDiskImageStorageDeviceAttachment

instance IsVZDiskImageStorageDeviceAttachment (Id VZDiskImageStorageDeviceAttachment) where
  toVZDiskImageStorageDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZDiskImageStorageDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZStorageDeviceAttachment (Id VZDiskImageStorageDeviceAttachment) where
  toVZStorageDeviceAttachment = unsafeCastId

-- ---------- VZNetworkBlockDeviceStorageDeviceAttachment ----------

-- | Storage device attachment backed by a Network Block Device (NBD) client.
--
-- This storage device attachment provides an NBD client implementation. The NBD client is connected    to an NBD server referred to by an NBD Uniform Resource Indicator (URI), represented as an URL in    this API. The NBD server runs outside of Virtualization framework and is not controlled by    Virtualization framework. The NBD client forwards the guest's I/O operations to the NBD server,    where the I/O operations are handled.
--
-- The NBD client will attempt to connect to the NBD server referred to by the URL when you start the virtual    machine (e.g. when @[VZVirtualMachine startWithCompletionHandler:]@ is called). A connection attempt is NOT    made when the attachment object is initialized. Reconnection attempts will take place throughout the life    cycle of the virtual machine when the NBD client encounters a recoverable error such as connection timeout    and unexpected connection errors. The NBD client will disconnect from the server when the virtual machine    shuts down.
--
-- Using this attachment requires the app to have the "com.apple.security.network.client" entitlement as this attachment opens an outgoing    network connection.
--
-- For more information about NBD, see https://github.com/NetworkBlockDevice/nbd/blob/master/doc/proto.md.    For more information about the NBD URL format, see https://github.com/NetworkBlockDevice/nbd/blob/master/doc/uri.md.
--
-- An example use of this API is:    ```    NSURL *url = [[NSURL alloc] initWithString:"nbd://localhost:10809/myDisk"]    NSError *error = nil;    VZNetworkBlockDeviceStorageDeviceAttachment *attachment =        [[VZNetworkBlockDeviceStorageDeviceAttachment alloc] initWithURL:url                                                                 timeout:5.0                                                          forcedReadOnly:NO                                                     synchronizationMode:VZDiskSynchronizationModeFull                                                                   error:&error];
--
-- if (!attachment) {        // Handle the @error@.    }
--
-- VZVirtioBlockDeviceConfiguration *blockDevice = [[VZVirtioBlockDeviceConfiguration alloc] initWithAttachment:attachment];    ```
-- 
-- Phantom type for @VZNetworkBlockDeviceStorageDeviceAttachment@.
data VZNetworkBlockDeviceStorageDeviceAttachment

instance IsObjCObject (Id VZNetworkBlockDeviceStorageDeviceAttachment) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZNetworkBlockDeviceStorageDeviceAttachment"

class IsVZStorageDeviceAttachment a => IsVZNetworkBlockDeviceStorageDeviceAttachment a where
  toVZNetworkBlockDeviceStorageDeviceAttachment :: a -> Id VZNetworkBlockDeviceStorageDeviceAttachment

instance IsVZNetworkBlockDeviceStorageDeviceAttachment (Id VZNetworkBlockDeviceStorageDeviceAttachment) where
  toVZNetworkBlockDeviceStorageDeviceAttachment = unsafeCastId

instance IsNSObject (Id VZNetworkBlockDeviceStorageDeviceAttachment) where
  toNSObject = unsafeCastId

instance IsVZStorageDeviceAttachment (Id VZNetworkBlockDeviceStorageDeviceAttachment) where
  toVZStorageDeviceAttachment = unsafeCastId

-- ---------- VZNVMExpressControllerDeviceConfiguration ----------

-- | Configuration of an NVM Express Controller storage device.
--
-- This device configuration creates a storage device that conforms to the NVM Express specification revision 1.1b.
--
-- The device configuration is valid only if used with VZGenericPlatformConfiguration.
--
-- See: VZGenericPlatformConfiguration
-- 
-- Phantom type for @VZNVMExpressControllerDeviceConfiguration@.
data VZNVMExpressControllerDeviceConfiguration

instance IsObjCObject (Id VZNVMExpressControllerDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZNVMExpressControllerDeviceConfiguration"

class IsVZStorageDeviceConfiguration a => IsVZNVMExpressControllerDeviceConfiguration a where
  toVZNVMExpressControllerDeviceConfiguration :: a -> Id VZNVMExpressControllerDeviceConfiguration

instance IsVZNVMExpressControllerDeviceConfiguration (Id VZNVMExpressControllerDeviceConfiguration) where
  toVZNVMExpressControllerDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZNVMExpressControllerDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZStorageDeviceConfiguration (Id VZNVMExpressControllerDeviceConfiguration) where
  toVZStorageDeviceConfiguration = unsafeCastId

-- ---------- VZUSBMassStorageDeviceConfiguration ----------

-- | Configuration of a USB Mass Storage storage device.
--
-- This device configuration creates a storage device that conforms to the USB Mass Storage specification.
-- 
-- Phantom type for @VZUSBMassStorageDeviceConfiguration@.
data VZUSBMassStorageDeviceConfiguration

instance IsObjCObject (Id VZUSBMassStorageDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZUSBMassStorageDeviceConfiguration"

class IsVZStorageDeviceConfiguration a => IsVZUSBMassStorageDeviceConfiguration a where
  toVZUSBMassStorageDeviceConfiguration :: a -> Id VZUSBMassStorageDeviceConfiguration

instance IsVZUSBMassStorageDeviceConfiguration (Id VZUSBMassStorageDeviceConfiguration) where
  toVZUSBMassStorageDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZUSBMassStorageDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZStorageDeviceConfiguration (Id VZUSBMassStorageDeviceConfiguration) where
  toVZStorageDeviceConfiguration = unsafeCastId

-- ---------- VZVirtioBlockDeviceConfiguration ----------

-- | Configuration of a paravirtualized storage device of type Virtio Block Device.
--
-- This device configuration creates a storage device using paravirtualization.    The emulated device follows the Virtio Block Device specification.
--
-- The host implementation of the device is done through an attachment subclassing VZStorageDeviceAttachment    like VZDiskImageStorageDeviceAttachment.
-- 
-- Phantom type for @VZVirtioBlockDeviceConfiguration@.
data VZVirtioBlockDeviceConfiguration

instance IsObjCObject (Id VZVirtioBlockDeviceConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioBlockDeviceConfiguration"

class IsVZStorageDeviceConfiguration a => IsVZVirtioBlockDeviceConfiguration a where
  toVZVirtioBlockDeviceConfiguration :: a -> Id VZVirtioBlockDeviceConfiguration

instance IsVZVirtioBlockDeviceConfiguration (Id VZVirtioBlockDeviceConfiguration) where
  toVZVirtioBlockDeviceConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioBlockDeviceConfiguration) where
  toNSObject = unsafeCastId

instance IsVZStorageDeviceConfiguration (Id VZVirtioBlockDeviceConfiguration) where
  toVZStorageDeviceConfiguration = unsafeCastId

-- ---------- VZXHCIController ----------

-- | Class representing a USB XHCI controller in a virtual machine.
--
-- VZXHCIController should not be instantiated directly.    In order to create a runtime VZXHCIController object, the usbControllers property of    VZVirtualMachineConfiguration object needs to be populated with VZXHCIControllerConfiguration object.
--
-- See: VZUSBController
--
-- See: VZXHCIControllerConfiguration
-- 
-- Phantom type for @VZXHCIController@.
data VZXHCIController

instance IsObjCObject (Id VZXHCIController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZXHCIController"

class IsVZUSBController a => IsVZXHCIController a where
  toVZXHCIController :: a -> Id VZXHCIController

instance IsVZXHCIController (Id VZXHCIController) where
  toVZXHCIController = unsafeCastId

instance IsNSObject (Id VZXHCIController) where
  toNSObject = unsafeCastId

instance IsVZUSBController (Id VZXHCIController) where
  toVZUSBController = unsafeCastId

-- ---------- VZXHCIControllerConfiguration ----------

-- | Configuration for the USB XHCI controller.
--
-- This configuration creates a USB XHCI controller device for the guest.
-- 
-- Phantom type for @VZXHCIControllerConfiguration@.
data VZXHCIControllerConfiguration

instance IsObjCObject (Id VZXHCIControllerConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZXHCIControllerConfiguration"

class IsVZUSBControllerConfiguration a => IsVZXHCIControllerConfiguration a where
  toVZXHCIControllerConfiguration :: a -> Id VZXHCIControllerConfiguration

instance IsVZXHCIControllerConfiguration (Id VZXHCIControllerConfiguration) where
  toVZXHCIControllerConfiguration = unsafeCastId

instance IsNSObject (Id VZXHCIControllerConfiguration) where
  toNSObject = unsafeCastId

instance IsVZUSBControllerConfiguration (Id VZXHCIControllerConfiguration) where
  toVZUSBControllerConfiguration = unsafeCastId

-- ---------- VZVirtioSoundDeviceInputStreamConfiguration ----------

-- | Virtio Sound Device Input Stream Configuration.
--
-- A PCM stream of input audio data, such as from a microphone.
-- 
-- Phantom type for @VZVirtioSoundDeviceInputStreamConfiguration@.
data VZVirtioSoundDeviceInputStreamConfiguration

instance IsObjCObject (Id VZVirtioSoundDeviceInputStreamConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSoundDeviceInputStreamConfiguration"

class IsVZVirtioSoundDeviceStreamConfiguration a => IsVZVirtioSoundDeviceInputStreamConfiguration a where
  toVZVirtioSoundDeviceInputStreamConfiguration :: a -> Id VZVirtioSoundDeviceInputStreamConfiguration

instance IsVZVirtioSoundDeviceInputStreamConfiguration (Id VZVirtioSoundDeviceInputStreamConfiguration) where
  toVZVirtioSoundDeviceInputStreamConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioSoundDeviceInputStreamConfiguration) where
  toNSObject = unsafeCastId

instance IsVZVirtioSoundDeviceStreamConfiguration (Id VZVirtioSoundDeviceInputStreamConfiguration) where
  toVZVirtioSoundDeviceStreamConfiguration = unsafeCastId

-- ---------- VZVirtioSoundDeviceOutputStreamConfiguration ----------

-- | Virtio Sound Device Output Stream Configuration.
--
-- A PCM stream of output audio data, such as to a speaker.
-- 
-- Phantom type for @VZVirtioSoundDeviceOutputStreamConfiguration@.
data VZVirtioSoundDeviceOutputStreamConfiguration

instance IsObjCObject (Id VZVirtioSoundDeviceOutputStreamConfiguration) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtioSoundDeviceOutputStreamConfiguration"

class IsVZVirtioSoundDeviceStreamConfiguration a => IsVZVirtioSoundDeviceOutputStreamConfiguration a where
  toVZVirtioSoundDeviceOutputStreamConfiguration :: a -> Id VZVirtioSoundDeviceOutputStreamConfiguration

instance IsVZVirtioSoundDeviceOutputStreamConfiguration (Id VZVirtioSoundDeviceOutputStreamConfiguration) where
  toVZVirtioSoundDeviceOutputStreamConfiguration = unsafeCastId

instance IsNSObject (Id VZVirtioSoundDeviceOutputStreamConfiguration) where
  toNSObject = unsafeCastId

instance IsVZVirtioSoundDeviceStreamConfiguration (Id VZVirtioSoundDeviceOutputStreamConfiguration) where
  toVZVirtioSoundDeviceStreamConfiguration = unsafeCastId

-- ---------- VZMacOSVirtualMachineStartOptions ----------

-- | Options controlling startup behavior of a virtual machine using VZMacOSBootLoader.
-- 
-- Phantom type for @VZMacOSVirtualMachineStartOptions@.
data VZMacOSVirtualMachineStartOptions

instance IsObjCObject (Id VZMacOSVirtualMachineStartOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZMacOSVirtualMachineStartOptions"

class IsVZVirtualMachineStartOptions a => IsVZMacOSVirtualMachineStartOptions a where
  toVZMacOSVirtualMachineStartOptions :: a -> Id VZMacOSVirtualMachineStartOptions

instance IsVZMacOSVirtualMachineStartOptions (Id VZMacOSVirtualMachineStartOptions) where
  toVZMacOSVirtualMachineStartOptions = unsafeCastId

instance IsNSObject (Id VZMacOSVirtualMachineStartOptions) where
  toNSObject = unsafeCastId

instance IsVZVirtualMachineStartOptions (Id VZMacOSVirtualMachineStartOptions) where
  toVZVirtualMachineStartOptions = unsafeCastId

-- ---------- VZVirtualMachineView ----------

-- | A view that allows user interaction with a virtual machine.
--
-- The VZVirtualMachineView shows the contents of the virtual machine framebuffer. If the virtual machine configuration includes a keyboard and a pointing device,    the view forwards keyboard and mouse events to the virtual machine via those devices.
--
-- VZVirtualMachine
-- 
-- Phantom type for @VZVirtualMachineView@.
data VZVirtualMachineView

instance IsObjCObject (Id VZVirtualMachineView) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "VZVirtualMachineView"

class IsNSView a => IsVZVirtualMachineView a where
  toVZVirtualMachineView :: a -> Id VZVirtualMachineView

instance IsVZVirtualMachineView (Id VZVirtualMachineView) where
  toVZVirtualMachineView = unsafeCastId

instance IsNSObject (Id VZVirtualMachineView) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id VZVirtualMachineView) where
  toNSResponder = unsafeCastId

instance IsNSView (Id VZVirtualMachineView) where
  toNSView = unsafeCastId
