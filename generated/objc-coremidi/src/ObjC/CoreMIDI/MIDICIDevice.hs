{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , deviceInfoSelector
  , muidSelector
  , supportsProtocolNegotiationSelector
  , supportsProfileConfigurationSelector
  , supportsPropertyExchangeSelector
  , supportsProcessInquirySelector
  , maxSysExSizeSelector
  , maxPropertyExchangeRequestsSelector
  , deviceTypeSelector
  , profilesSelector

  -- * Enum types
  , MIDICIDeviceType(MIDICIDeviceType)
  , pattern KMIDICIDeviceTypeUnknown
  , pattern KMIDICIDeviceTypeLegacyMIDI1
  , pattern KMIDICIDeviceTypeVirtual
  , pattern KMIDICIDeviceTypeUSBMIDI

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
init_ :: IsMIDICIDevice midiciDevice => midiciDevice -> IO (Id MIDICIDevice)
init_ midiciDevice  =
  sendMsg midiciDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | deviceInfo
--
-- The basic information describing the CI device.
--
-- ObjC selector: @- deviceInfo@
deviceInfo :: IsMIDICIDevice midiciDevice => midiciDevice -> IO (Id MIDI2DeviceInfo)
deviceInfo midiciDevice  =
  sendMsg midiciDevice (mkSelector "deviceInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | MUID
--
-- The MIDI unique identifier (MUID) assigned to the CI device.
--
-- ObjC selector: @- MUID@
muid :: IsMIDICIDevice midiciDevice => midiciDevice -> IO CUInt
muid midiciDevice  =
  sendMsg midiciDevice (mkSelector "MUID") retCUInt []

-- | supportsProtocolNegotiation
--
-- MIDI-CI Protocol Negotiation capability.
--
-- ObjC selector: @- supportsProtocolNegotiation@
supportsProtocolNegotiation :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsProtocolNegotiation midiciDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciDevice (mkSelector "supportsProtocolNegotiation") retCULong []

-- | supportsProfileConfiguration
--
-- MIDI-CI Profile Configuration capability.
--
-- ObjC selector: @- supportsProfileConfiguration@
supportsProfileConfiguration :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsProfileConfiguration midiciDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciDevice (mkSelector "supportsProfileConfiguration") retCULong []

-- | supportsPropertyExchange
--
-- MIDI-CI Property Exchange capability.
--
-- ObjC selector: @- supportsPropertyExchange@
supportsPropertyExchange :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsPropertyExchange midiciDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciDevice (mkSelector "supportsPropertyExchange") retCULong []

-- | supportsProcessInquiry
--
-- MIDI-CI Process Inquiry capability.
--
-- ObjC selector: @- supportsProcessInquiry@
supportsProcessInquiry :: IsMIDICIDevice midiciDevice => midiciDevice -> IO Bool
supportsProcessInquiry midiciDevice  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg midiciDevice (mkSelector "supportsProcessInquiry") retCULong []

-- | maxSysExSize
--
-- The maximum receivable MIDI System Exclusive size for this CI device.
--
-- ObjC selector: @- maxSysExSize@
maxSysExSize :: IsMIDICIDevice midiciDevice => midiciDevice -> IO CULong
maxSysExSize midiciDevice  =
  sendMsg midiciDevice (mkSelector "maxSysExSize") retCULong []

-- | maxPropertyExchangeRequests
--
-- The maximum number of simultaneous Property Exchange requests, if supported.
--
-- ObjC selector: @- maxPropertyExchangeRequests@
maxPropertyExchangeRequests :: IsMIDICIDevice midiciDevice => midiciDevice -> IO CULong
maxPropertyExchangeRequests midiciDevice  =
  sendMsg midiciDevice (mkSelector "maxPropertyExchangeRequests") retCULong []

-- | deviceType
--
-- The type of MIDI-CI device.
--
-- ObjC selector: @- deviceType@
deviceType :: IsMIDICIDevice midiciDevice => midiciDevice -> IO MIDICIDeviceType
deviceType midiciDevice  =
  fmap (coerce :: CUChar -> MIDICIDeviceType) $ sendMsg midiciDevice (mkSelector "deviceType") retCUChar []

-- | profiles
--
-- The MIDI-CI Profiles that are registered to the  Function Block.
--
-- ObjC selector: @- profiles@
profiles :: IsMIDICIDevice midiciDevice => midiciDevice -> IO (Id NSArray)
profiles midiciDevice  =
  sendMsg midiciDevice (mkSelector "profiles") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @deviceInfo@
deviceInfoSelector :: Selector
deviceInfoSelector = mkSelector "deviceInfo"

-- | @Selector@ for @MUID@
muidSelector :: Selector
muidSelector = mkSelector "MUID"

-- | @Selector@ for @supportsProtocolNegotiation@
supportsProtocolNegotiationSelector :: Selector
supportsProtocolNegotiationSelector = mkSelector "supportsProtocolNegotiation"

-- | @Selector@ for @supportsProfileConfiguration@
supportsProfileConfigurationSelector :: Selector
supportsProfileConfigurationSelector = mkSelector "supportsProfileConfiguration"

-- | @Selector@ for @supportsPropertyExchange@
supportsPropertyExchangeSelector :: Selector
supportsPropertyExchangeSelector = mkSelector "supportsPropertyExchange"

-- | @Selector@ for @supportsProcessInquiry@
supportsProcessInquirySelector :: Selector
supportsProcessInquirySelector = mkSelector "supportsProcessInquiry"

-- | @Selector@ for @maxSysExSize@
maxSysExSizeSelector :: Selector
maxSysExSizeSelector = mkSelector "maxSysExSize"

-- | @Selector@ for @maxPropertyExchangeRequests@
maxPropertyExchangeRequestsSelector :: Selector
maxPropertyExchangeRequestsSelector = mkSelector "maxPropertyExchangeRequests"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @profiles@
profilesSelector :: Selector
profilesSelector = mkSelector "profiles"

