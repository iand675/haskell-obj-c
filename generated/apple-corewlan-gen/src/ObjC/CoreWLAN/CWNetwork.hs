{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents a device participating in a Wi-Fi network, providing accessors to various network attributes.
--
-- Generated bindings for @CWNetwork@.
module ObjC.CoreWLAN.CWNetwork
  ( CWNetwork
  , IsCWNetwork(..)
  , isEqualToNetwork
  , supportsSecurity
  , supportsPHYMode
  , ssid
  , ssidData
  , bssid
  , wlanChannel
  , rssiValue
  , noiseMeasurement
  , informationElementData
  , countryCode
  , beaconInterval
  , ibss
  , beaconIntervalSelector
  , bssidSelector
  , countryCodeSelector
  , ibssSelector
  , informationElementDataSelector
  , isEqualToNetworkSelector
  , noiseMeasurementSelector
  , rssiValueSelector
  , ssidDataSelector
  , ssidSelector
  , supportsPHYModeSelector
  , supportsSecuritySelector
  , wlanChannelSelector

  -- * Enum types
  , CWPHYMode(CWPHYMode)
  , pattern KCWPHYModeNone
  , pattern KCWPHYMode11a
  , pattern KCWPHYMode11b
  , pattern KCWPHYMode11g
  , pattern KCWPHYMode11n
  , pattern KCWPHYMode11ac
  , pattern KCWPHYMode11ax
  , CWSecurity(CWSecurity)
  , pattern KCWSecurityNone
  , pattern KCWSecurityWEP
  , pattern KCWSecurityWPAPersonal
  , pattern KCWSecurityWPAPersonalMixed
  , pattern KCWSecurityWPA2Personal
  , pattern KCWSecurityPersonal
  , pattern KCWSecurityDynamicWEP
  , pattern KCWSecurityWPAEnterprise
  , pattern KCWSecurityWPAEnterpriseMixed
  , pattern KCWSecurityWPA2Enterprise
  , pattern KCWSecurityEnterprise
  , pattern KCWSecurityWPA3Personal
  , pattern KCWSecurityWPA3Enterprise
  , pattern KCWSecurityWPA3Transition
  , pattern KCWSecurityOWE
  , pattern KCWSecurityOWETransition
  , pattern KCWSecurityUnknown

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreWLAN.Internal.Classes
import ObjC.CoreWLAN.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @network@ — A CWNetwork object.
--
-- Returns: YES if the objects are equal, NO otherwise.
--
-- Determine CWNetwork equality.
--
-- CWNetwork objects are considered equal if their corresponding ssidData and bssid properties are equal.
--
-- ObjC selector: @- isEqualToNetwork:@
isEqualToNetwork :: (IsCWNetwork cwNetwork, IsCWNetwork network) => cwNetwork -> network -> IO Bool
isEqualToNetwork cwNetwork network =
  sendMessage cwNetwork isEqualToNetworkSelector (toCWNetwork network)

-- | @security@ — A CWSecurity type value.
--
-- Returns: YES if the Wi-Fi device supports the specified security type, NO otherwise.
--
-- Determine which security types a Wi-Fi device supports.
--
-- ObjC selector: @- supportsSecurity:@
supportsSecurity :: IsCWNetwork cwNetwork => cwNetwork -> CWSecurity -> IO Bool
supportsSecurity cwNetwork security =
  sendMessage cwNetwork supportsSecuritySelector security

-- | @phyMode@ — A CWPHYMode type value.
--
-- Returns: YES if the Wi-Fi device supports the specified PHY mode, NO otherwise.
--
-- Determine which PHY modes a Wi-Fi device supports.
--
-- ObjC selector: @- supportsPHYMode:@
supportsPHYMode :: IsCWNetwork cwNetwork => cwNetwork -> CWPHYMode -> IO Bool
supportsPHYMode cwNetwork phyMode =
  sendMessage cwNetwork supportsPHYModeSelector phyMode

-- | Returns the service set identifier (SSID) for the Wi-Fi network device, encoded as a string.
--
-- Returns nil if the SSID can not be encoded as a valid UTF-8 or WinLatin1 string.
--
-- Note: SSID information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- ssid@
ssid :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
ssid cwNetwork =
  sendMessage cwNetwork ssidSelector

-- | Returns the service set identifier (SSID) for the Wi-Fi network device, encapsulated in an NSData object.
--
-- The SSID is defined as 1-32 octets.
--
-- Note: SSID information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- ssidData@
ssidData :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
ssidData cwNetwork =
  sendMessage cwNetwork ssidDataSelector

-- | Returns the basic service set identifier (BSSID) for the Wi-Fi network device, returned as UTF-8 string.
--
-- Returns a UTF-8 string using hexadecimal characters formatted as XX:XX:XX:XX:XX:XX.
--
-- Note: BSSID information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- bssid@
bssid :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
bssid cwNetwork =
  sendMessage cwNetwork bssidSelector

-- | The operating channel of the Wi-Fi device.
--
-- ObjC selector: @- wlanChannel@
wlanChannel :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
wlanChannel cwNetwork =
  sendMessage cwNetwork wlanChannelSelector

-- | Returns the received signal strength indication (RSSI) measurement (dBm) for the Wi-Fi device.
--
-- ObjC selector: @- rssiValue@
rssiValue :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
rssiValue cwNetwork =
  sendMessage cwNetwork rssiValueSelector

-- | Returns the noise measurement (dBm) for the Wi-Fi device.
--
-- ObjC selector: @- noiseMeasurement@
noiseMeasurement :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
noiseMeasurement cwNetwork =
  sendMessage cwNetwork noiseMeasurementSelector

-- | Returns information element data included in beacon or probe response frames.
--
-- ObjC selector: @- informationElementData@
informationElementData :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
informationElementData cwNetwork =
  sendMessage cwNetwork informationElementDataSelector

-- | Returns the advertised country code (ISO/IEC 3166-1:1997) for the Wi-Fi device.
--
-- Note: Country code information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- countryCode@
countryCode :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
countryCode cwNetwork =
  sendMessage cwNetwork countryCodeSelector

-- | Returns the beacon interval (ms) for the Wi-Fi device.
--
-- ObjC selector: @- beaconInterval@
beaconInterval :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
beaconInterval cwNetwork =
  sendMessage cwNetwork beaconIntervalSelector

-- | Returns: YES if the Wi-Fi device is part of an IBSS network, NO otherwise.
--
-- Indicates whether or not the Wi-Fi device is participating in an independent basic service set (IBSS), or ad-hoc Wi-Fi network.
--
-- ObjC selector: @- ibss@
ibss :: IsCWNetwork cwNetwork => cwNetwork -> IO Bool
ibss cwNetwork =
  sendMessage cwNetwork ibssSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToNetwork:@
isEqualToNetworkSelector :: Selector '[Id CWNetwork] Bool
isEqualToNetworkSelector = mkSelector "isEqualToNetwork:"

-- | @Selector@ for @supportsSecurity:@
supportsSecuritySelector :: Selector '[CWSecurity] Bool
supportsSecuritySelector = mkSelector "supportsSecurity:"

-- | @Selector@ for @supportsPHYMode:@
supportsPHYModeSelector :: Selector '[CWPHYMode] Bool
supportsPHYModeSelector = mkSelector "supportsPHYMode:"

-- | @Selector@ for @ssid@
ssidSelector :: Selector '[] RawId
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector '[] RawId
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @bssid@
bssidSelector :: Selector '[] RawId
bssidSelector = mkSelector "bssid"

-- | @Selector@ for @wlanChannel@
wlanChannelSelector :: Selector '[] RawId
wlanChannelSelector = mkSelector "wlanChannel"

-- | @Selector@ for @rssiValue@
rssiValueSelector :: Selector '[] CLong
rssiValueSelector = mkSelector "rssiValue"

-- | @Selector@ for @noiseMeasurement@
noiseMeasurementSelector :: Selector '[] CLong
noiseMeasurementSelector = mkSelector "noiseMeasurement"

-- | @Selector@ for @informationElementData@
informationElementDataSelector :: Selector '[] RawId
informationElementDataSelector = mkSelector "informationElementData"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector '[] RawId
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @beaconInterval@
beaconIntervalSelector :: Selector '[] CLong
beaconIntervalSelector = mkSelector "beaconInterval"

-- | @Selector@ for @ibss@
ibssSelector :: Selector '[] Bool
ibssSelector = mkSelector "ibss"

