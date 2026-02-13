{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDIUMPEndpoint
--
-- An object representating a UMP Endpoint.
--
-- MIDIUMPEndpoint encapsulates a MIDI source and MIDI destination as a				bidirectional MIDI 2.0 communication node along with any associated stream				configuration metadata.
--
-- It is not necessary to create a MIDIUMPEndpoint or other MIDI endpoint in order to				use UMP natively. Any standard MIDI endpoint created with a specified MIDIProtocolID				is assumed to use all 16 UMP groups for the same unspecified function and to neither				transmit nor receive jitter-reduction timestamps.
--
-- Generated bindings for @MIDIUMPEndpoint@.
module ObjC.CoreMIDI.MIDIUMPEndpoint
  ( MIDIUMPEndpoint
  , IsMIDIUMPEndpoint(..)
  , init_
  , name
  , midiProtocol
  , supportedMIDIProtocols
  , midiDestination
  , midiSource
  , deviceInfo
  , productInstanceID
  , hasStaticFunctionBlocks
  , hasJRTSReceiveCapability
  , hasJRTSTransmitCapability
  , endpointType
  , functionBlocks
  , setFunctionBlocks
  , deviceInfoSelector
  , endpointTypeSelector
  , functionBlocksSelector
  , hasJRTSReceiveCapabilitySelector
  , hasJRTSTransmitCapabilitySelector
  , hasStaticFunctionBlocksSelector
  , initSelector
  , midiDestinationSelector
  , midiProtocolSelector
  , midiSourceSelector
  , nameSelector
  , productInstanceIDSelector
  , setFunctionBlocksSelector
  , supportedMIDIProtocolsSelector

  -- * Enum types
  , MIDIProtocolID(MIDIProtocolID)
  , pattern KMIDIProtocol_1_0
  , pattern KMIDIProtocol_2_0
  , MIDIUMPCIObjectBackingType(MIDIUMPCIObjectBackingType)
  , pattern KMIDIUMPCIObjectBackingTypeUnknown
  , pattern KMIDIUMPCIObjectBackingTypeVirtual
  , pattern KMIDIUMPCIObjectBackingTypeDriverDevice
  , pattern KMIDIUMPCIObjectBackingTypeUSBMIDI
  , MIDIUMPProtocolOptions(MIDIUMPProtocolOptions)
  , pattern KMIDIUMPProtocolOptionsMIDI1
  , pattern KMIDIUMPProtocolOptionsMIDI2

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreMIDI.Internal.Classes
import ObjC.CoreMIDI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id MIDIUMPEndpoint)
init_ midiumpEndpoint =
  sendOwnedMessage midiumpEndpoint initSelector

-- | name
--
-- The UTF-8 encoded name of the UMP endpoint.
--
-- The name shall not be any longer than 98 bytes of UTF-8 Text.
--
-- ObjC selector: @- name@
name :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id NSString)
name midiumpEndpoint =
  sendMessage midiumpEndpoint nameSelector

-- | MIDIProtocol
--
-- The MIDI protocol currently used by the UMP endpoint.
--
-- ObjC selector: @- MIDIProtocol@
midiProtocol :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO MIDIProtocolID
midiProtocol midiumpEndpoint =
  sendMessage midiumpEndpoint midiProtocolSelector

-- | supportedMIDIProtocols
--
-- All protocols the UMP endpoint is capable of using for communication.
--
-- ObjC selector: @- supportedMIDIProtocols@
supportedMIDIProtocols :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO MIDIUMPProtocolOptions
supportedMIDIProtocols midiumpEndpoint =
  sendMessage midiumpEndpoint supportedMIDIProtocolsSelector

-- | MIDIDestination
--
-- The MIDI destination for the UMP endpoint.
--
-- ObjC selector: @- MIDIDestination@
midiDestination :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO CUInt
midiDestination midiumpEndpoint =
  sendMessage midiumpEndpoint midiDestinationSelector

-- | MIDISource
--
-- The MIDI source for the UMP endpoint.
--
-- ObjC selector: @- MIDISource@
midiSource :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO CUInt
midiSource midiumpEndpoint =
  sendMessage midiumpEndpoint midiSourceSelector

-- | deviceInfo
--
-- The MIDI 2.0 Device identity information associated with the device.
--
-- ObjC selector: @- deviceInfo@
deviceInfo :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id MIDI2DeviceInfo)
deviceInfo midiumpEndpoint =
  sendMessage midiumpEndpoint deviceInfoSelector

-- | productInstanceID
--
-- Serial number (or similar value) uniquely identifying this manufacturer/family/model,				up to 42 bytes of ASCII Text in the ordinal range 32-126.
--
-- ObjC selector: @- productInstanceID@
productInstanceID :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id NSString)
productInstanceID midiumpEndpoint =
  sendMessage midiumpEndpoint productInstanceIDSelector

-- | hasStaticFunctionBlocks
--
-- Indicates if the Function Block state will never change once discovered.
--
-- ObjC selector: @- hasStaticFunctionBlocks@
hasStaticFunctionBlocks :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO Bool
hasStaticFunctionBlocks midiumpEndpoint =
  sendMessage midiumpEndpoint hasStaticFunctionBlocksSelector

-- | hasJRTSReceiveCapability
--
-- Jitter-reduction timestamp receive capability.
--
-- ObjC selector: @- hasJRTSReceiveCapability@
hasJRTSReceiveCapability :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO Bool
hasJRTSReceiveCapability midiumpEndpoint =
  sendMessage midiumpEndpoint hasJRTSReceiveCapabilitySelector

-- | hasJRTSTransmitCapability
--
-- Jitter-reduction timestamp transmit capability
--
-- ObjC selector: @- hasJRTSTransmitCapability@
hasJRTSTransmitCapability :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO Bool
hasJRTSTransmitCapability midiumpEndpoint =
  sendMessage midiumpEndpoint hasJRTSTransmitCapabilitySelector

-- | endpointType
--
-- Indicates the type of UMP Endpoint, if known.
--
-- ObjC selector: @- endpointType@
endpointType :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO MIDIUMPCIObjectBackingType
endpointType midiumpEndpoint =
  sendMessage midiumpEndpoint endpointTypeSelector

-- | functionBlocks
--
-- The Function Blocks associated with the UMP endpoint, if any.
--
-- ObjC selector: @- functionBlocks@
functionBlocks :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id NSArray)
functionBlocks midiumpEndpoint =
  sendMessage midiumpEndpoint functionBlocksSelector

-- | functionBlocks
--
-- The Function Blocks associated with the UMP endpoint, if any.
--
-- ObjC selector: @- setFunctionBlocks:@
setFunctionBlocks :: (IsMIDIUMPEndpoint midiumpEndpoint, IsNSArray value) => midiumpEndpoint -> value -> IO ()
setFunctionBlocks midiumpEndpoint value =
  sendMessage midiumpEndpoint setFunctionBlocksSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDIUMPEndpoint)
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @MIDIProtocol@
midiProtocolSelector :: Selector '[] MIDIProtocolID
midiProtocolSelector = mkSelector "MIDIProtocol"

-- | @Selector@ for @supportedMIDIProtocols@
supportedMIDIProtocolsSelector :: Selector '[] MIDIUMPProtocolOptions
supportedMIDIProtocolsSelector = mkSelector "supportedMIDIProtocols"

-- | @Selector@ for @MIDIDestination@
midiDestinationSelector :: Selector '[] CUInt
midiDestinationSelector = mkSelector "MIDIDestination"

-- | @Selector@ for @MIDISource@
midiSourceSelector :: Selector '[] CUInt
midiSourceSelector = mkSelector "MIDISource"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector '[] (Id MIDI2DeviceInfo)
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @productInstanceID@
productInstanceIDSelector :: Selector '[] (Id NSString)
productInstanceIDSelector = mkSelector "productInstanceID"

-- | @Selector@ for @hasStaticFunctionBlocks@
hasStaticFunctionBlocksSelector :: Selector '[] Bool
hasStaticFunctionBlocksSelector = mkSelector "hasStaticFunctionBlocks"

-- | @Selector@ for @hasJRTSReceiveCapability@
hasJRTSReceiveCapabilitySelector :: Selector '[] Bool
hasJRTSReceiveCapabilitySelector = mkSelector "hasJRTSReceiveCapability"

-- | @Selector@ for @hasJRTSTransmitCapability@
hasJRTSTransmitCapabilitySelector :: Selector '[] Bool
hasJRTSTransmitCapabilitySelector = mkSelector "hasJRTSTransmitCapability"

-- | @Selector@ for @endpointType@
endpointTypeSelector :: Selector '[] MIDIUMPCIObjectBackingType
endpointTypeSelector = mkSelector "endpointType"

-- | @Selector@ for @functionBlocks@
functionBlocksSelector :: Selector '[] (Id NSArray)
functionBlocksSelector = mkSelector "functionBlocks"

-- | @Selector@ for @setFunctionBlocks:@
setFunctionBlocksSelector :: Selector '[Id NSArray] ()
setFunctionBlocksSelector = mkSelector "setFunctionBlocks:"

