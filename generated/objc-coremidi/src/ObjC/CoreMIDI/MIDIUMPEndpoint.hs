{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , nameSelector
  , midiProtocolSelector
  , supportedMIDIProtocolsSelector
  , midiDestinationSelector
  , midiSourceSelector
  , deviceInfoSelector
  , productInstanceIDSelector
  , hasStaticFunctionBlocksSelector
  , hasJRTSReceiveCapabilitySelector
  , hasJRTSTransmitCapabilitySelector
  , endpointTypeSelector
  , functionBlocksSelector
  , setFunctionBlocksSelector

  -- * Enum types
  , MIDIUMPCIObjectBackingType(MIDIUMPCIObjectBackingType)
  , pattern KMIDIUMPCIObjectBackingTypeUnknown
  , pattern KMIDIUMPCIObjectBackingTypeVirtual
  , pattern KMIDIUMPCIObjectBackingTypeDriverDevice
  , pattern KMIDIUMPCIObjectBackingTypeUSBMIDI
  , MIDIUMPProtocolOptions(MIDIUMPProtocolOptions)
  , pattern KMIDIUMPProtocolOptionsMIDI1
  , pattern KMIDIUMPProtocolOptionsMIDI2

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

import ObjC.CoreMIDI.Internal.Classes
import ObjC.CoreMIDI.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id MIDIUMPEndpoint)
init_ midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | name
--
-- The UTF-8 encoded name of the UMP endpoint.
--
-- The name shall not be any longer than 98 bytes of UTF-8 Text.
--
-- ObjC selector: @- name@
name :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id NSString)
name midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | MIDIProtocol
--
-- The MIDI protocol currently used by the UMP endpoint.
--
-- ObjC selector: @- MIDIProtocol@
midiProtocol :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO MIDIProtocolID
midiProtocol midiumpEndpoint  =
  fmap (coerce :: CInt -> MIDIProtocolID) $ sendMsg midiumpEndpoint (mkSelector "MIDIProtocol") retCInt []

-- | supportedMIDIProtocols
--
-- All protocols the UMP endpoint is capable of using for communication.
--
-- ObjC selector: @- supportedMIDIProtocols@
supportedMIDIProtocols :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO MIDIUMPProtocolOptions
supportedMIDIProtocols midiumpEndpoint  =
  fmap (coerce :: CUChar -> MIDIUMPProtocolOptions) $ sendMsg midiumpEndpoint (mkSelector "supportedMIDIProtocols") retCUChar []

-- | MIDIDestination
--
-- The MIDI destination for the UMP endpoint.
--
-- ObjC selector: @- MIDIDestination@
midiDestination :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO CUInt
midiDestination midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "MIDIDestination") retCUInt []

-- | MIDISource
--
-- The MIDI source for the UMP endpoint.
--
-- ObjC selector: @- MIDISource@
midiSource :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO CUInt
midiSource midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "MIDISource") retCUInt []

-- | deviceInfo
--
-- The MIDI 2.0 Device identity information associated with the device.
--
-- ObjC selector: @- deviceInfo@
deviceInfo :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id MIDI2DeviceInfo)
deviceInfo midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "deviceInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | productInstanceID
--
-- Serial number (or similar value) uniquely identifying this manufacturer/family/model,				up to 42 bytes of ASCII Text in the ordinal range 32-126.
--
-- ObjC selector: @- productInstanceID@
productInstanceID :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id NSString)
productInstanceID midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "productInstanceID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hasStaticFunctionBlocks
--
-- Indicates if the Function Block state will never change once discovered.
--
-- ObjC selector: @- hasStaticFunctionBlocks@
hasStaticFunctionBlocks :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO Bool
hasStaticFunctionBlocks midiumpEndpoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiumpEndpoint (mkSelector "hasStaticFunctionBlocks") retCULong []

-- | hasJRTSReceiveCapability
--
-- Jitter-reduction timestamp receive capability.
--
-- ObjC selector: @- hasJRTSReceiveCapability@
hasJRTSReceiveCapability :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO Bool
hasJRTSReceiveCapability midiumpEndpoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiumpEndpoint (mkSelector "hasJRTSReceiveCapability") retCULong []

-- | hasJRTSTransmitCapability
--
-- Jitter-reduction timestamp transmit capability
--
-- ObjC selector: @- hasJRTSTransmitCapability@
hasJRTSTransmitCapability :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO Bool
hasJRTSTransmitCapability midiumpEndpoint  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiumpEndpoint (mkSelector "hasJRTSTransmitCapability") retCULong []

-- | endpointType
--
-- Indicates the type of UMP Endpoint, if known.
--
-- ObjC selector: @- endpointType@
endpointType :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO MIDIUMPCIObjectBackingType
endpointType midiumpEndpoint  =
  fmap (coerce :: CUChar -> MIDIUMPCIObjectBackingType) $ sendMsg midiumpEndpoint (mkSelector "endpointType") retCUChar []

-- | functionBlocks
--
-- The Function Blocks associated with the UMP endpoint, if any.
--
-- ObjC selector: @- functionBlocks@
functionBlocks :: IsMIDIUMPEndpoint midiumpEndpoint => midiumpEndpoint -> IO (Id NSArray)
functionBlocks midiumpEndpoint  =
  sendMsg midiumpEndpoint (mkSelector "functionBlocks") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | functionBlocks
--
-- The Function Blocks associated with the UMP endpoint, if any.
--
-- ObjC selector: @- setFunctionBlocks:@
setFunctionBlocks :: (IsMIDIUMPEndpoint midiumpEndpoint, IsNSArray value) => midiumpEndpoint -> value -> IO ()
setFunctionBlocks midiumpEndpoint  value =
withObjCPtr value $ \raw_value ->
    sendMsg midiumpEndpoint (mkSelector "setFunctionBlocks:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @MIDIProtocol@
midiProtocolSelector :: Selector
midiProtocolSelector = mkSelector "MIDIProtocol"

-- | @Selector@ for @supportedMIDIProtocols@
supportedMIDIProtocolsSelector :: Selector
supportedMIDIProtocolsSelector = mkSelector "supportedMIDIProtocols"

-- | @Selector@ for @MIDIDestination@
midiDestinationSelector :: Selector
midiDestinationSelector = mkSelector "MIDIDestination"

-- | @Selector@ for @MIDISource@
midiSourceSelector :: Selector
midiSourceSelector = mkSelector "MIDISource"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @productInstanceID@
productInstanceIDSelector :: Selector
productInstanceIDSelector = mkSelector "productInstanceID"

-- | @Selector@ for @hasStaticFunctionBlocks@
hasStaticFunctionBlocksSelector :: Selector
hasStaticFunctionBlocksSelector = mkSelector "hasStaticFunctionBlocks"

-- | @Selector@ for @hasJRTSReceiveCapability@
hasJRTSReceiveCapabilitySelector :: Selector
hasJRTSReceiveCapabilitySelector = mkSelector "hasJRTSReceiveCapability"

-- | @Selector@ for @hasJRTSTransmitCapability@
hasJRTSTransmitCapabilitySelector :: Selector
hasJRTSTransmitCapabilitySelector = mkSelector "hasJRTSTransmitCapability"

-- | @Selector@ for @endpointType@
endpointTypeSelector :: Selector
endpointTypeSelector = mkSelector "endpointType"

-- | @Selector@ for @functionBlocks@
functionBlocksSelector :: Selector
functionBlocksSelector = mkSelector "functionBlocks"

-- | @Selector@ for @setFunctionBlocks:@
setFunctionBlocksSelector :: Selector
setFunctionBlocksSelector = mkSelector "setFunctionBlocks:"

