{-# LANGUAGE PatternSynonyms #-}
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
  , isEqualToNetworkSelector
  , supportsSecuritySelector
  , supportsPHYModeSelector
  , ssidSelector
  , ssidDataSelector
  , bssidSelector
  , wlanChannelSelector
  , rssiValueSelector
  , noiseMeasurementSelector
  , informationElementDataSelector
  , countryCodeSelector
  , beaconIntervalSelector
  , ibssSelector

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
isEqualToNetwork cwNetwork  network =
  withObjCPtr network $ \raw_network ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "isEqualToNetwork:") retCULong [argPtr (castPtr raw_network :: Ptr ())]

-- | @security@ — A CWSecurity type value.
--
-- Returns: YES if the Wi-Fi device supports the specified security type, NO otherwise.
--
-- Determine which security types a Wi-Fi device supports.
--
-- ObjC selector: @- supportsSecurity:@
supportsSecurity :: IsCWNetwork cwNetwork => cwNetwork -> CWSecurity -> IO Bool
supportsSecurity cwNetwork  security =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "supportsSecurity:") retCULong [argCLong (coerce security)]

-- | @phyMode@ — A CWPHYMode type value.
--
-- Returns: YES if the Wi-Fi device supports the specified PHY mode, NO otherwise.
--
-- Determine which PHY modes a Wi-Fi device supports.
--
-- ObjC selector: @- supportsPHYMode:@
supportsPHYMode :: IsCWNetwork cwNetwork => cwNetwork -> CWPHYMode -> IO Bool
supportsPHYMode cwNetwork  phyMode =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "supportsPHYMode:") retCULong [argCLong (coerce phyMode)]

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
ssid cwNetwork  =
    fmap (RawId . castPtr) $ sendMsg cwNetwork (mkSelector "ssid") (retPtr retVoid) []

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
ssidData cwNetwork  =
    fmap (RawId . castPtr) $ sendMsg cwNetwork (mkSelector "ssidData") (retPtr retVoid) []

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
bssid cwNetwork  =
    fmap (RawId . castPtr) $ sendMsg cwNetwork (mkSelector "bssid") (retPtr retVoid) []

-- | The operating channel of the Wi-Fi device.
--
-- ObjC selector: @- wlanChannel@
wlanChannel :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
wlanChannel cwNetwork  =
    fmap (RawId . castPtr) $ sendMsg cwNetwork (mkSelector "wlanChannel") (retPtr retVoid) []

-- | Returns the received signal strength indication (RSSI) measurement (dBm) for the Wi-Fi device.
--
-- ObjC selector: @- rssiValue@
rssiValue :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
rssiValue cwNetwork  =
    sendMsg cwNetwork (mkSelector "rssiValue") retCLong []

-- | Returns the noise measurement (dBm) for the Wi-Fi device.
--
-- ObjC selector: @- noiseMeasurement@
noiseMeasurement :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
noiseMeasurement cwNetwork  =
    sendMsg cwNetwork (mkSelector "noiseMeasurement") retCLong []

-- | Returns information element data included in beacon or probe response frames.
--
-- ObjC selector: @- informationElementData@
informationElementData :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
informationElementData cwNetwork  =
    fmap (RawId . castPtr) $ sendMsg cwNetwork (mkSelector "informationElementData") (retPtr retVoid) []

-- | Returns the advertised country code (ISO/IEC 3166-1:1997) for the Wi-Fi device.
--
-- Note: Country code information is not available unless Location Services is enabled and the user has authorized the calling app to use location services.
--
-- CLLocationManager
--
-- ObjC selector: @- countryCode@
countryCode :: IsCWNetwork cwNetwork => cwNetwork -> IO RawId
countryCode cwNetwork  =
    fmap (RawId . castPtr) $ sendMsg cwNetwork (mkSelector "countryCode") (retPtr retVoid) []

-- | Returns the beacon interval (ms) for the Wi-Fi device.
--
-- ObjC selector: @- beaconInterval@
beaconInterval :: IsCWNetwork cwNetwork => cwNetwork -> IO CLong
beaconInterval cwNetwork  =
    sendMsg cwNetwork (mkSelector "beaconInterval") retCLong []

-- | Returns: YES if the Wi-Fi device is part of an IBSS network, NO otherwise.
--
-- Indicates whether or not the Wi-Fi device is participating in an independent basic service set (IBSS), or ad-hoc Wi-Fi network.
--
-- ObjC selector: @- ibss@
ibss :: IsCWNetwork cwNetwork => cwNetwork -> IO Bool
ibss cwNetwork  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg cwNetwork (mkSelector "ibss") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @isEqualToNetwork:@
isEqualToNetworkSelector :: Selector
isEqualToNetworkSelector = mkSelector "isEqualToNetwork:"

-- | @Selector@ for @supportsSecurity:@
supportsSecuritySelector :: Selector
supportsSecuritySelector = mkSelector "supportsSecurity:"

-- | @Selector@ for @supportsPHYMode:@
supportsPHYModeSelector :: Selector
supportsPHYModeSelector = mkSelector "supportsPHYMode:"

-- | @Selector@ for @ssid@
ssidSelector :: Selector
ssidSelector = mkSelector "ssid"

-- | @Selector@ for @ssidData@
ssidDataSelector :: Selector
ssidDataSelector = mkSelector "ssidData"

-- | @Selector@ for @bssid@
bssidSelector :: Selector
bssidSelector = mkSelector "bssid"

-- | @Selector@ for @wlanChannel@
wlanChannelSelector :: Selector
wlanChannelSelector = mkSelector "wlanChannel"

-- | @Selector@ for @rssiValue@
rssiValueSelector :: Selector
rssiValueSelector = mkSelector "rssiValue"

-- | @Selector@ for @noiseMeasurement@
noiseMeasurementSelector :: Selector
noiseMeasurementSelector = mkSelector "noiseMeasurement"

-- | @Selector@ for @informationElementData@
informationElementDataSelector :: Selector
informationElementDataSelector = mkSelector "informationElementData"

-- | @Selector@ for @countryCode@
countryCodeSelector :: Selector
countryCodeSelector = mkSelector "countryCode"

-- | @Selector@ for @beaconInterval@
beaconIntervalSelector :: Selector
beaconIntervalSelector = mkSelector "beaconInterval"

-- | @Selector@ for @ibss@
ibssSelector :: Selector
ibssSelector = mkSelector "ibss"

