{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreMIDI.Internal.Classes (
    module ObjC.CoreMIDI.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- MIDI2DeviceInfo ----------

-- | MIDI2DeviceInfo
--
-- An NSObject containing basic information about a MIDI 2.0 device. Used by				MIDIUMPEndpointPair and MIDICIDevice.
-- 
-- Phantom type for @MIDI2DeviceInfo@.
data MIDI2DeviceInfo

instance IsObjCObject (Id MIDI2DeviceInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDI2DeviceInfo"

class IsNSObject a => IsMIDI2DeviceInfo a where
  toMIDI2DeviceInfo :: a -> Id MIDI2DeviceInfo

instance IsMIDI2DeviceInfo (Id MIDI2DeviceInfo) where
  toMIDI2DeviceInfo = unsafeCastId

instance IsNSObject (Id MIDI2DeviceInfo) where
  toNSObject = unsafeCastId

-- ---------- MIDICIDevice ----------

-- | MIDICIDevice
--
-- An object representing a MIDI-CI Device.
--
-- The client instance MIDICIDeviceManager maintains a list of discovered CI devices.				MIDICIDevice objects are not constructible via API.
-- 
-- Phantom type for @MIDICIDevice@.
data MIDICIDevice

instance IsObjCObject (Id MIDICIDevice) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIDevice"

class IsNSObject a => IsMIDICIDevice a where
  toMIDICIDevice :: a -> Id MIDICIDevice

instance IsMIDICIDevice (Id MIDICIDevice) where
  toMIDICIDevice = unsafeCastId

instance IsNSObject (Id MIDICIDevice) where
  toNSObject = unsafeCastId

-- ---------- MIDICIDeviceInfo ----------

-- | Phantom type for @MIDICIDeviceInfo@.
data MIDICIDeviceInfo

instance IsObjCObject (Id MIDICIDeviceInfo) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIDeviceInfo"

class IsNSObject a => IsMIDICIDeviceInfo a where
  toMIDICIDeviceInfo :: a -> Id MIDICIDeviceInfo

instance IsMIDICIDeviceInfo (Id MIDICIDeviceInfo) where
  toMIDICIDeviceInfo = unsafeCastId

instance IsNSObject (Id MIDICIDeviceInfo) where
  toNSObject = unsafeCastId

-- ---------- MIDICIDeviceManager ----------

-- | MIDICIDeviceManager
--
-- A singleton object that performs system-wide MIDI-CI Device bookkeeping.
--
-- MIDICIDeviceManager is used to retrieve information about MIDI-CI devices that				to MIDI-CI Discovery.
-- 
-- Phantom type for @MIDICIDeviceManager@.
data MIDICIDeviceManager

instance IsObjCObject (Id MIDICIDeviceManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIDeviceManager"

class IsNSObject a => IsMIDICIDeviceManager a where
  toMIDICIDeviceManager :: a -> Id MIDICIDeviceManager

instance IsMIDICIDeviceManager (Id MIDICIDeviceManager) where
  toMIDICIDeviceManager = unsafeCastId

instance IsNSObject (Id MIDICIDeviceManager) where
  toNSObject = unsafeCastId

-- ---------- MIDICIDiscoveredNode ----------

-- | Phantom type for @MIDICIDiscoveredNode@.
data MIDICIDiscoveredNode

instance IsObjCObject (Id MIDICIDiscoveredNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIDiscoveredNode"

class IsNSObject a => IsMIDICIDiscoveredNode a where
  toMIDICIDiscoveredNode :: a -> Id MIDICIDiscoveredNode

instance IsMIDICIDiscoveredNode (Id MIDICIDiscoveredNode) where
  toMIDICIDiscoveredNode = unsafeCastId

instance IsNSObject (Id MIDICIDiscoveredNode) where
  toNSObject = unsafeCastId

-- ---------- MIDICIDiscoveryManager ----------

-- | Phantom type for @MIDICIDiscoveryManager@.
data MIDICIDiscoveryManager

instance IsObjCObject (Id MIDICIDiscoveryManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIDiscoveryManager"

class IsNSObject a => IsMIDICIDiscoveryManager a where
  toMIDICIDiscoveryManager :: a -> Id MIDICIDiscoveryManager

instance IsMIDICIDiscoveryManager (Id MIDICIDiscoveryManager) where
  toMIDICIDiscoveryManager = unsafeCastId

instance IsNSObject (Id MIDICIDiscoveryManager) where
  toNSObject = unsafeCastId

-- ---------- MIDICIProfile ----------

-- | MIDICIProfile
--
-- An NSObject representing Capability Inquiry profile. MIDI-CI profiles describe a mapping				of MIDI messages to specific sounds and synthesis behaviors, e.g. General MIDI, a drawbar organ,				etc. A MIDI-CI profile may be a standard registered profile or vendor-specific.
--
-- Standard Profile				Vendor-Specific Profile				Profile ID Byte 1:	0x7E Standard Profile			Manufacturer SysEx ID 1 Profile				Profile ID Byte 2:	Profile Bank				Manufacturer SysEx ID 2 Profile				Profile ID Byte 3:	Profile Number				Manufacturer SysEx ID 3 Profile				Profile ID Byte 4:	Profile Version				Manufacturer-specific Info				Profile ID Byte 5:	Profile Level				Manufacturer-specific Info
-- 
-- Phantom type for @MIDICIProfile@.
data MIDICIProfile

instance IsObjCObject (Id MIDICIProfile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIProfile"

class IsNSObject a => IsMIDICIProfile a where
  toMIDICIProfile :: a -> Id MIDICIProfile

instance IsMIDICIProfile (Id MIDICIProfile) where
  toMIDICIProfile = unsafeCastId

instance IsNSObject (Id MIDICIProfile) where
  toNSObject = unsafeCastId

-- ---------- MIDICIProfileState ----------

-- | Phantom type for @MIDICIProfileState@.
data MIDICIProfileState

instance IsObjCObject (Id MIDICIProfileState) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIProfileState"

class IsNSObject a => IsMIDICIProfileState a where
  toMIDICIProfileState :: a -> Id MIDICIProfileState

instance IsMIDICIProfileState (Id MIDICIProfileState) where
  toMIDICIProfileState = unsafeCastId

instance IsNSObject (Id MIDICIProfileState) where
  toNSObject = unsafeCastId

-- ---------- MIDICIResponder ----------

-- | Phantom type for @MIDICIResponder@.
data MIDICIResponder

instance IsObjCObject (Id MIDICIResponder) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICIResponder"

class IsNSObject a => IsMIDICIResponder a where
  toMIDICIResponder :: a -> Id MIDICIResponder

instance IsMIDICIResponder (Id MIDICIResponder) where
  toMIDICIResponder = unsafeCastId

instance IsNSObject (Id MIDICIResponder) where
  toNSObject = unsafeCastId

-- ---------- MIDICISession ----------

-- | Phantom type for @MIDICISession@.
data MIDICISession

instance IsObjCObject (Id MIDICISession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDICISession"

class IsNSObject a => IsMIDICISession a where
  toMIDICISession :: a -> Id MIDICISession

instance IsMIDICISession (Id MIDICISession) where
  toMIDICISession = unsafeCastId

instance IsNSObject (Id MIDICISession) where
  toNSObject = unsafeCastId

-- ---------- MIDINetworkConnection ----------

-- | Phantom type for @MIDINetworkConnection@.
data MIDINetworkConnection

instance IsObjCObject (Id MIDINetworkConnection) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDINetworkConnection"

class IsNSObject a => IsMIDINetworkConnection a where
  toMIDINetworkConnection :: a -> Id MIDINetworkConnection

instance IsMIDINetworkConnection (Id MIDINetworkConnection) where
  toMIDINetworkConnection = unsafeCastId

instance IsNSObject (Id MIDINetworkConnection) where
  toNSObject = unsafeCastId

-- ---------- MIDINetworkHost ----------

-- | Phantom type for @MIDINetworkHost@.
data MIDINetworkHost

instance IsObjCObject (Id MIDINetworkHost) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDINetworkHost"

class IsNSObject a => IsMIDINetworkHost a where
  toMIDINetworkHost :: a -> Id MIDINetworkHost

instance IsMIDINetworkHost (Id MIDINetworkHost) where
  toMIDINetworkHost = unsafeCastId

instance IsNSObject (Id MIDINetworkHost) where
  toNSObject = unsafeCastId

-- ---------- MIDINetworkSession ----------

-- | Phantom type for @MIDINetworkSession@.
data MIDINetworkSession

instance IsObjCObject (Id MIDINetworkSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDINetworkSession"

class IsNSObject a => IsMIDINetworkSession a where
  toMIDINetworkSession :: a -> Id MIDINetworkSession

instance IsMIDINetworkSession (Id MIDINetworkSession) where
  toMIDINetworkSession = unsafeCastId

instance IsNSObject (Id MIDINetworkSession) where
  toNSObject = unsafeCastId

-- ---------- MIDIUMPCIProfile ----------

-- | MIDIUMPCIProfile
--
-- An object representing Capability Inquiry Profile on a MIDICIDevice.
--
-- MIDI-CI profiles describe a mapping of MIDI messages to specific sounds and synthesis				behaviors, e.g. General MIDI, a drawbar organ, etc. A MIDI-CI Profile may be a standard				registered Profile or vendor-specific.
--
-- A MIDI-CI Profile ID consists of 5 bytes.				===============================================================								Standard Profile			Vendor-Specific Profile				Profile ID Byte 1:	0x7E Standard Profile		Manufacturer SysEx ID 1 Profile				Profile ID Byte 2:	Profile Bank			Manufacturer SysEx ID 2 Profile				Profile ID Byte 3:	Profile Number			Manufacturer SysEx ID 3 Profile				Profile ID Byte 4:	Profile Version			Manufacturer-specific Info				Profile ID Byte 5:	Profile Level			Manufacturer-specific Info
--
-- MIDI-CI Profiles have the following generalized types:
--
-- Profile Type		Source		Channels		Channel Range				============		==========	===========	========================				Single-channel 		0x00~0x0F 	1			1 channel (1 to 16) of a Group				Group			0x7E			16			All channels of a Group				Function Block		0x7F			16 per Group 	All channels of a Function Block				Multi-channel		0x00~0x0F	2 or more		Profile-specific
--
-- MIDIUMPCIProfile objects may only be registered to a single CI device, and any number				of MIDIUMPCIProfile objects containg the same profile ID may be registered to CI				devices in the MIDI 2.0 subsystem.
-- 
-- Phantom type for @MIDIUMPCIProfile@.
data MIDIUMPCIProfile

instance IsObjCObject (Id MIDIUMPCIProfile) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDIUMPCIProfile"

class IsNSObject a => IsMIDIUMPCIProfile a where
  toMIDIUMPCIProfile :: a -> Id MIDIUMPCIProfile

instance IsMIDIUMPCIProfile (Id MIDIUMPCIProfile) where
  toMIDIUMPCIProfile = unsafeCastId

instance IsNSObject (Id MIDIUMPCIProfile) where
  toNSObject = unsafeCastId

-- ---------- MIDIUMPEndpoint ----------

-- | MIDIUMPEndpoint
--
-- An object representating a UMP Endpoint.
--
-- MIDIUMPEndpoint encapsulates a MIDI source and MIDI destination as a				bidirectional MIDI 2.0 communication node along with any associated stream				configuration metadata.
--
-- It is not necessary to create a MIDIUMPEndpoint or other MIDI endpoint in order to				use UMP natively. Any standard MIDI endpoint created with a specified MIDIProtocolID				is assumed to use all 16 UMP groups for the same unspecified function and to neither				transmit nor receive jitter-reduction timestamps.
-- 
-- Phantom type for @MIDIUMPEndpoint@.
data MIDIUMPEndpoint

instance IsObjCObject (Id MIDIUMPEndpoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDIUMPEndpoint"

class IsNSObject a => IsMIDIUMPEndpoint a where
  toMIDIUMPEndpoint :: a -> Id MIDIUMPEndpoint

instance IsMIDIUMPEndpoint (Id MIDIUMPEndpoint) where
  toMIDIUMPEndpoint = unsafeCastId

instance IsNSObject (Id MIDIUMPEndpoint) where
  toNSObject = unsafeCastId

-- ---------- MIDIUMPEndpointManager ----------

-- | MIDIUMPEndpointManager
--
-- A singleton object that performs system-wide UMP Endpoint bookkeeping.
--
-- MIDIUMPEndpointManager is used to retrieve information about UMP Endpoint				pairs detected by or explicitly declared to the MIDI UMP subsystem.
-- 
-- Phantom type for @MIDIUMPEndpointManager@.
data MIDIUMPEndpointManager

instance IsObjCObject (Id MIDIUMPEndpointManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDIUMPEndpointManager"

class IsNSObject a => IsMIDIUMPEndpointManager a where
  toMIDIUMPEndpointManager :: a -> Id MIDIUMPEndpointManager

instance IsMIDIUMPEndpointManager (Id MIDIUMPEndpointManager) where
  toMIDIUMPEndpointManager = unsafeCastId

instance IsNSObject (Id MIDIUMPEndpointManager) where
  toNSObject = unsafeCastId

-- ---------- MIDIUMPFunctionBlock ----------

-- | MIDIUMPFunctionBlock
--
-- An object representing a Function Block.
--
-- A Function Block encapsulates one or more UMP groups with a single function, allowing				agents communicating with that UMP Endpoint to route and process UMP traffic				properly. Unless the owning MIDIUMPEndpoint has a static Function Block configuration,				any Function Block metadata may change in response to a configuration change in the				owning UMP endpoint.
-- 
-- Phantom type for @MIDIUMPFunctionBlock@.
data MIDIUMPFunctionBlock

instance IsObjCObject (Id MIDIUMPFunctionBlock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDIUMPFunctionBlock"

class IsNSObject a => IsMIDIUMPFunctionBlock a where
  toMIDIUMPFunctionBlock :: a -> Id MIDIUMPFunctionBlock

instance IsMIDIUMPFunctionBlock (Id MIDIUMPFunctionBlock) where
  toMIDIUMPFunctionBlock = unsafeCastId

instance IsNSObject (Id MIDIUMPFunctionBlock) where
  toNSObject = unsafeCastId

-- ---------- MIDIUMPMutableEndpoint ----------

-- | MIDIUMPMutableEndpoint
--
-- A mutable MIDIUMPEndpoint object.
--
-- It is not necessary to create a MIDIUMPEndpoint or other MIDI endpoint in order to				use UMP natively. Any standard MIDI endpoint created with a specified MIDIProtocolID				is assumed to use all 16 UMP groups for the same unspecified function and to neither				transmit nor receive jitter-reduction timestamps.
--
-- This API is not realtime-safe, all interaction with the mutable endpoint should be done on the				main thread.
-- 
-- Phantom type for @MIDIUMPMutableEndpoint@.
data MIDIUMPMutableEndpoint

instance IsObjCObject (Id MIDIUMPMutableEndpoint) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDIUMPMutableEndpoint"

class IsMIDIUMPEndpoint a => IsMIDIUMPMutableEndpoint a where
  toMIDIUMPMutableEndpoint :: a -> Id MIDIUMPMutableEndpoint

instance IsMIDIUMPMutableEndpoint (Id MIDIUMPMutableEndpoint) where
  toMIDIUMPMutableEndpoint = unsafeCastId

instance IsMIDIUMPEndpoint (Id MIDIUMPMutableEndpoint) where
  toMIDIUMPEndpoint = unsafeCastId

instance IsNSObject (Id MIDIUMPMutableEndpoint) where
  toNSObject = unsafeCastId

-- ---------- MIDIUMPMutableFunctionBlock ----------

-- | MIDIUMPMutableFunctionBlock
--
-- A mutable Function Block object created by the client process.
--
-- A Function Block created with this API may be used in the Function Block configuration				of a client-created MIDIUMPMutableEndpoint.
--
-- This API is not realtime-safe, all interaction with the function block should be done on the				main thread.
-- 
-- Phantom type for @MIDIUMPMutableFunctionBlock@.
data MIDIUMPMutableFunctionBlock

instance IsObjCObject (Id MIDIUMPMutableFunctionBlock) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MIDIUMPMutableFunctionBlock"

class IsMIDIUMPFunctionBlock a => IsMIDIUMPMutableFunctionBlock a where
  toMIDIUMPMutableFunctionBlock :: a -> Id MIDIUMPMutableFunctionBlock

instance IsMIDIUMPMutableFunctionBlock (Id MIDIUMPMutableFunctionBlock) where
  toMIDIUMPMutableFunctionBlock = unsafeCastId

instance IsMIDIUMPFunctionBlock (Id MIDIUMPMutableFunctionBlock) where
  toMIDIUMPFunctionBlock = unsafeCastId

instance IsNSObject (Id MIDIUMPMutableFunctionBlock) where
  toNSObject = unsafeCastId
