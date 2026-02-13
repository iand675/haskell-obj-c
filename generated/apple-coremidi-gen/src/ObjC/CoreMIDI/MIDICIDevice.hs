{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MIDICIDevice
--
-- An object representing a MIDI-CI Device.
--
-- The client instance MIDICIDeviceManager maintains a list of discovered CI devices.				MIDICIDevice objects are not constructible via API.
--
-- Generated bindings for @MIDICIDevice@.
module ObjC.CoreMIDI.MIDICIDevice
  ( MIDICIDevice
  , IsMIDICIDevice(..)
  , init_
  , deviceInfo
  , muid
  , supportsProtocolNegotiation
  , supportsProfileConfiguration
  , supportsPropertyExchange
  , supportsProcessInquiry
  , maxSysExSize
  , maxPropertyExchangeRequests
  , deviceType
  , profiles
  , deviceInfoSelector
  , deviceTypeSelector
  , initSelector
  , maxPropertyExchangeRequestsSelector
  , maxSysExSizeSelector
  , muidSelector
  , profilesSelector
  , supportsProcessInquirySelector
  , supportsProfileConfigurationSelector
  , supportsPropertyExchangeSelector
  , supportsProtocolNegotiationSelector

  -- * Enum types
  , MIDICIDeviceType(MIDICIDeviceType)
  , pattern KMIDICIDeviceTypeUnknown
  , pattern KMIDICIDeviceTypeLegacyMIDI1
  , pattern KMIDICIDeviceTypeVirtual
  , pattern KMIDICIDeviceTypeUSBMIDI

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
init_ :: IsMIDICIDevice midiciDevice => midiciDevice -> IO (Id MIDICIDevice)
init_ midiciDevice =
  sendOwnedMessage midiciDevice initSelector

-- | deviceInfo
--
-- The basic information describing the CI device.
--
-- ObjC selector: @- deviceInfo@
deviceInfo :: IsMIDICIDevice midiciDevice => midiciDevice -> IO (Id MIDI2DeviceInfo)
deviceInfo midiciDevice =
  sendMessage midiciDevice deviceInfoSelector

-- | MUID
--
-- The MIDI unique identifier (MUID) assigned to the CI device.
--
-- ObjC selector: @- MUID@
muid :: IsMIDICIDevice midiciDevice => midiciDevice -> IO CUInt
muid midiciDevice =
  sendMessage midiciDevice muidSelector

-- | supportsProtocolNegotiation
--
-- MIDI-CI Protocol Negotiation capability.
--
-- ObjC selector: @- supportsProtocolNegotiation@
supportsProtocolNegotiation :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsProtocolNegotiation midiciDevice =
  sendMessage midiciDevice supportsProtocolNegotiationSelector

-- | supportsProfileConfiguration
--
-- MIDI-CI Profile Configuration capability.
--
-- ObjC selector: @- supportsProfileConfiguration@
supportsProfileConfiguration :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsProfileConfiguration midiciDevice =
  sendMessage midiciDevice supportsProfileConfigurationSelector

-- | supportsPropertyExchange
--
-- MIDI-CI Property Exchange capability.
--
-- ObjC selector: @- supportsPropertyExchange@
supportsPropertyExchange :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsPropertyExchange midiciDevice =
  sendMessage midiciDevice supportsPropertyExchangeSelector

-- | supportsProcessInquiry
--
-- MIDI-CI Process Inquiry capability.
--
-- ObjC selector: @- supportsProcessInquiry@
supportsProcessInquiry :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsProcessInquiry midiciDevice =
  sendMessage midiciDevice supportsProcessInquirySelector

-- | maxSysExSize
--
-- The maximum receivable MIDI System Exclusive size for this CI device.
--
-- ObjC selector: @- maxSysExSize@
maxSysExSize :: IsMIDICIDevice midiciDevice => midiciDevice -> IO CULong
maxSysExSize midiciDevice =
  sendMessage midiciDevice maxSysExSizeSelector

-- | maxPropertyExchangeRequests
--
-- The maximum number of simultaneous Property Exchange requests, if supported.
--
-- ObjC selector: @- maxPropertyExchangeRequests@
maxPropertyExchangeRequests :: IsMIDICIDevice midiciDevice => midiciDevice -> IO CULong
maxPropertyExchangeRequests midiciDevice =
  sendMessage midiciDevice maxPropertyExchangeRequestsSelector

-- | deviceType
--
-- The type of MIDI-CI device.
--
-- ObjC selector: @- deviceType@
deviceType :: IsMIDICIDevice midiciDevice => midiciDevice -> IO MIDICIDeviceType
deviceType midiciDevice =
  sendMessage midiciDevice deviceTypeSelector

-- | profiles
--
-- The MIDI-CI Profiles that are registered to the  Function Block.
--
-- ObjC selector: @- profiles@
profiles :: IsMIDICIDevice midiciDevice => midiciDevice -> IO (Id NSArray)
profiles midiciDevice =
  sendMessage midiciDevice profilesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MIDICIDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector '[] (Id MIDI2DeviceInfo)
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @MUID@
muidSelector :: Selector '[] CUInt
muidSelector = mkSelector "MUID"

-- | @Selector@ for @supportsProtocolNegotiation@
supportsProtocolNegotiationSelector :: Selector '[] Bool
supportsProtocolNegotiationSelector = mkSelector "supportsProtocolNegotiation"

-- | @Selector@ for @supportsProfileConfiguration@
supportsProfileConfigurationSelector :: Selector '[] Bool
supportsProfileConfigurationSelector = mkSelector "supportsProfileConfiguration"

-- | @Selector@ for @supportsPropertyExchange@
supportsPropertyExchangeSelector :: Selector '[] Bool
supportsPropertyExchangeSelector = mkSelector "supportsPropertyExchange"

-- | @Selector@ for @supportsProcessInquiry@
supportsProcessInquirySelector :: Selector '[] Bool
supportsProcessInquirySelector = mkSelector "supportsProcessInquiry"

-- | @Selector@ for @maxSysExSize@
maxSysExSizeSelector :: Selector '[] CULong
maxSysExSizeSelector = mkSelector "maxSysExSize"

-- | @Selector@ for @maxPropertyExchangeRequests@
maxPropertyExchangeRequestsSelector :: Selector '[] CULong
maxPropertyExchangeRequestsSelector = mkSelector "maxPropertyExchangeRequests"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector '[] MIDICIDeviceType
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @profiles@
profilesSelector :: Selector '[] (Id NSArray)
profilesSelector = mkSelector "profiles"

