{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @ASDiscoveryDescriptor@.
module ObjC.AccessorySetupKit.ASDiscoveryDescriptor
  ( ASDiscoveryDescriptor
  , IsASDiscoveryDescriptor(..)
  , supportedOptions
  , setSupportedOptions
  , bluetoothCompanyIdentifier
  , setBluetoothCompanyIdentifier
  , bluetoothManufacturerDataBlob
  , setBluetoothManufacturerDataBlob
  , bluetoothManufacturerDataMask
  , setBluetoothManufacturerDataMask
  , bluetoothNameSubstringCompareOptions
  , setBluetoothNameSubstringCompareOptions
  , bluetoothNameSubstring
  , setBluetoothNameSubstring
  , bluetoothRange
  , setBluetoothRange
  , bluetoothServiceDataBlob
  , setBluetoothServiceDataBlob
  , bluetoothServiceDataMask
  , setBluetoothServiceDataMask
  , ssid
  , setSSID
  , ssidPrefix
  , setSSIDPrefix
  , wifiAwareServiceName
  , setWifiAwareServiceName
  , wifiAwareServiceRole
  , setWifiAwareServiceRole
  , wifiAwareModelNameMatch
  , setWifiAwareModelNameMatch
  , wifiAwareVendorNameMatch
  , setWifiAwareVendorNameMatch
  , bluetoothCompanyIdentifierSelector
  , bluetoothManufacturerDataBlobSelector
  , bluetoothManufacturerDataMaskSelector
  , bluetoothNameSubstringCompareOptionsSelector
  , bluetoothNameSubstringSelector
  , bluetoothRangeSelector
  , bluetoothServiceDataBlobSelector
  , bluetoothServiceDataMaskSelector
  , setBluetoothCompanyIdentifierSelector
  , setBluetoothManufacturerDataBlobSelector
  , setBluetoothManufacturerDataMaskSelector
  , setBluetoothNameSubstringCompareOptionsSelector
  , setBluetoothNameSubstringSelector
  , setBluetoothRangeSelector
  , setBluetoothServiceDataBlobSelector
  , setBluetoothServiceDataMaskSelector
  , setSSIDPrefixSelector
  , setSSIDSelector
  , setSupportedOptionsSelector
  , setWifiAwareModelNameMatchSelector
  , setWifiAwareServiceNameSelector
  , setWifiAwareServiceRoleSelector
  , setWifiAwareVendorNameMatchSelector
  , ssidPrefixSelector
  , ssidSelector
  , supportedOptionsSelector
  , wifiAwareModelNameMatchSelector
  , wifiAwareServiceNameSelector
  , wifiAwareServiceRoleSelector
  , wifiAwareVendorNameMatchSelector

  -- * Enum types
  , ASAccessorySupportOptions(ASAccessorySupportOptions)
  , pattern ASAccessorySupportBluetoothPairingLE
  , pattern ASAccessorySupportBluetoothTransportBridging
  , pattern ASAccessorySupportBluetoothHID
  , ASDiscoveryDescriptorRange(ASDiscoveryDescriptorRange)
  , pattern ASDiscoveryDescriptorRangeDefault
  , pattern ASDiscoveryDescriptorRangeImmediate
  , ASDiscoveryDescriptorWiFiAwareServiceRole(ASDiscoveryDescriptorWiFiAwareServiceRole)
  , pattern ASDiscoveryDescriptorWiFiAwareServiceRoleSubscriber
  , pattern ASDiscoveryDescriptorWiFiAwareServiceRolePublisher
  , NSStringCompareOptions(NSStringCompareOptions)
  , pattern NSCaseInsensitiveSearch
  , pattern NSLiteralSearch
  , pattern NSBackwardsSearch
  , pattern NSAnchoredSearch
  , pattern NSNumericSearch
  , pattern NSDiacriticInsensitiveSearch
  , pattern NSWidthInsensitiveSearch
  , pattern NSForcedOrderingSearch
  , pattern NSRegularExpressionSearch

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Options supported by an accessory.
--
-- ObjC selector: @- supportedOptions@
supportedOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO ASAccessorySupportOptions
supportedOptions asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor supportedOptionsSelector

-- | Options supported by an accessory.
--
-- ObjC selector: @- setSupportedOptions:@
setSupportedOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> ASAccessorySupportOptions -> IO ()
setSupportedOptions asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setSupportedOptionsSelector value

-- | The accessory's 16-bit Bluetooth Company Identifier.
--
-- ObjC selector: @- bluetoothCompanyIdentifier@
bluetoothCompanyIdentifier :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO CUShort
bluetoothCompanyIdentifier asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothCompanyIdentifierSelector

-- | The accessory's 16-bit Bluetooth Company Identifier.
--
-- ObjC selector: @- setBluetoothCompanyIdentifier:@
setBluetoothCompanyIdentifier :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> CUShort -> IO ()
setBluetoothCompanyIdentifier asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothCompanyIdentifierSelector value

-- | A byte buffer that matches the accessory's Bluetooth manufacturer data.
--
-- ObjC selector: @- bluetoothManufacturerDataBlob@
bluetoothManufacturerDataBlob :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothManufacturerDataBlob asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothManufacturerDataBlobSelector

-- | A byte buffer that matches the accessory's Bluetooth manufacturer data.
--
-- ObjC selector: @- setBluetoothManufacturerDataBlob:@
setBluetoothManufacturerDataBlob :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothManufacturerDataBlob asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothManufacturerDataBlobSelector (toNSData value)

-- | The accessory's Bluetooth manufacturer data mask.
--
-- ObjC selector: @- bluetoothManufacturerDataMask@
bluetoothManufacturerDataMask :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothManufacturerDataMask asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothManufacturerDataMaskSelector

-- | The accessory's Bluetooth manufacturer data mask.
--
-- ObjC selector: @- setBluetoothManufacturerDataMask:@
setBluetoothManufacturerDataMask :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothManufacturerDataMask asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothManufacturerDataMaskSelector (toNSData value)

-- | The accessory's over-the-air Bluetooth name substring compare options.
--
-- ObjC selector: @- bluetoothNameSubstringCompareOptions@
bluetoothNameSubstringCompareOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO NSStringCompareOptions
bluetoothNameSubstringCompareOptions asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothNameSubstringCompareOptionsSelector

-- | The accessory's over-the-air Bluetooth name substring compare options.
--
-- ObjC selector: @- setBluetoothNameSubstringCompareOptions:@
setBluetoothNameSubstringCompareOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> NSStringCompareOptions -> IO ()
setBluetoothNameSubstringCompareOptions asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothNameSubstringCompareOptionsSelector value

-- | The accessory's over-the-air Bluetooth name substring.
--
-- ObjC selector: @- bluetoothNameSubstring@
bluetoothNameSubstring :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
bluetoothNameSubstring asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothNameSubstringSelector

-- | The accessory's over-the-air Bluetooth name substring.
--
-- ObjC selector: @- setBluetoothNameSubstring:@
setBluetoothNameSubstring :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothNameSubstring asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothNameSubstringSelector (toNSString value)

-- | A property that tells the session to discover accessories within a specific Bluetooth range.
--
-- ObjC selector: @- bluetoothRange@
bluetoothRange :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO ASDiscoveryDescriptorRange
bluetoothRange asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothRangeSelector

-- | A property that tells the session to discover accessories within a specific Bluetooth range.
--
-- ObjC selector: @- setBluetoothRange:@
setBluetoothRange :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> ASDiscoveryDescriptorRange -> IO ()
setBluetoothRange asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothRangeSelector value

-- | A byte buffer that matches the accessory's Bluetooth service data.
--
-- ObjC selector: @- bluetoothServiceDataBlob@
bluetoothServiceDataBlob :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothServiceDataBlob asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothServiceDataBlobSelector

-- | A byte buffer that matches the accessory's Bluetooth service data.
--
-- ObjC selector: @- setBluetoothServiceDataBlob:@
setBluetoothServiceDataBlob :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothServiceDataBlob asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothServiceDataBlobSelector (toNSData value)

-- | The accessory's Bluetooth service data mask.
--
-- ObjC selector: @- bluetoothServiceDataMask@
bluetoothServiceDataMask :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothServiceDataMask asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor bluetoothServiceDataMaskSelector

-- | The accessory's Bluetooth service data mask.
--
-- ObjC selector: @- setBluetoothServiceDataMask:@
setBluetoothServiceDataMask :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothServiceDataMask asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setBluetoothServiceDataMaskSelector (toNSData value)

-- | The SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- SSID@
ssid :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
ssid asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor ssidSelector

-- | The SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- setSSID:@
setSSID :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setSSID asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setSSIDSelector (toNSString value)

-- | The prefix string of SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- SSIDPrefix@
ssidPrefix :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
ssidPrefix asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor ssidPrefixSelector

-- | The prefix string of SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- setSSIDPrefix:@
setSSIDPrefix :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setSSIDPrefix asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setSSIDPrefixSelector (toNSString value)

-- | The accessory's Wi-Fi Aware's service name if available.
--
-- ObjC selector: @- wifiAwareServiceName@
wifiAwareServiceName :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
wifiAwareServiceName asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor wifiAwareServiceNameSelector

-- | The accessory's Wi-Fi Aware's service name if available.
--
-- ObjC selector: @- setWifiAwareServiceName:@
setWifiAwareServiceName :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setWifiAwareServiceName asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setWifiAwareServiceNameSelector (toNSString value)

-- | The role of the accessory's Wi-Fi Aware's service.
--
-- This property defaults to ``ASDiscoveryDescriptor/WiFiAwareServiceRole/subscriber``
--
-- ObjC selector: @- wifiAwareServiceRole@
wifiAwareServiceRole :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO ASDiscoveryDescriptorWiFiAwareServiceRole
wifiAwareServiceRole asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor wifiAwareServiceRoleSelector

-- | The role of the accessory's Wi-Fi Aware's service.
--
-- This property defaults to ``ASDiscoveryDescriptor/WiFiAwareServiceRole/subscriber``
--
-- ObjC selector: @- setWifiAwareServiceRole:@
setWifiAwareServiceRole :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> ASDiscoveryDescriptorWiFiAwareServiceRole -> IO ()
setWifiAwareServiceRole asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setWifiAwareServiceRoleSelector value

-- | The accessory's Wi-Fi Aware model name and matching options.
--
-- ObjC selector: @- wifiAwareModelNameMatch@
wifiAwareModelNameMatch :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id ASPropertyCompareString)
wifiAwareModelNameMatch asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor wifiAwareModelNameMatchSelector

-- | The accessory's Wi-Fi Aware model name and matching options.
--
-- ObjC selector: @- setWifiAwareModelNameMatch:@
setWifiAwareModelNameMatch :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsASPropertyCompareString value) => asDiscoveryDescriptor -> value -> IO ()
setWifiAwareModelNameMatch asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setWifiAwareModelNameMatchSelector (toASPropertyCompareString value)

-- | The accessory's Wi-Fi Aware vendor name and matching options.
--
-- ObjC selector: @- wifiAwareVendorNameMatch@
wifiAwareVendorNameMatch :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id ASPropertyCompareString)
wifiAwareVendorNameMatch asDiscoveryDescriptor =
  sendMessage asDiscoveryDescriptor wifiAwareVendorNameMatchSelector

-- | The accessory's Wi-Fi Aware vendor name and matching options.
--
-- ObjC selector: @- setWifiAwareVendorNameMatch:@
setWifiAwareVendorNameMatch :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsASPropertyCompareString value) => asDiscoveryDescriptor -> value -> IO ()
setWifiAwareVendorNameMatch asDiscoveryDescriptor value =
  sendMessage asDiscoveryDescriptor setWifiAwareVendorNameMatchSelector (toASPropertyCompareString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedOptions@
supportedOptionsSelector :: Selector '[] ASAccessorySupportOptions
supportedOptionsSelector = mkSelector "supportedOptions"

-- | @Selector@ for @setSupportedOptions:@
setSupportedOptionsSelector :: Selector '[ASAccessorySupportOptions] ()
setSupportedOptionsSelector = mkSelector "setSupportedOptions:"

-- | @Selector@ for @bluetoothCompanyIdentifier@
bluetoothCompanyIdentifierSelector :: Selector '[] CUShort
bluetoothCompanyIdentifierSelector = mkSelector "bluetoothCompanyIdentifier"

-- | @Selector@ for @setBluetoothCompanyIdentifier:@
setBluetoothCompanyIdentifierSelector :: Selector '[CUShort] ()
setBluetoothCompanyIdentifierSelector = mkSelector "setBluetoothCompanyIdentifier:"

-- | @Selector@ for @bluetoothManufacturerDataBlob@
bluetoothManufacturerDataBlobSelector :: Selector '[] (Id NSData)
bluetoothManufacturerDataBlobSelector = mkSelector "bluetoothManufacturerDataBlob"

-- | @Selector@ for @setBluetoothManufacturerDataBlob:@
setBluetoothManufacturerDataBlobSelector :: Selector '[Id NSData] ()
setBluetoothManufacturerDataBlobSelector = mkSelector "setBluetoothManufacturerDataBlob:"

-- | @Selector@ for @bluetoothManufacturerDataMask@
bluetoothManufacturerDataMaskSelector :: Selector '[] (Id NSData)
bluetoothManufacturerDataMaskSelector = mkSelector "bluetoothManufacturerDataMask"

-- | @Selector@ for @setBluetoothManufacturerDataMask:@
setBluetoothManufacturerDataMaskSelector :: Selector '[Id NSData] ()
setBluetoothManufacturerDataMaskSelector = mkSelector "setBluetoothManufacturerDataMask:"

-- | @Selector@ for @bluetoothNameSubstringCompareOptions@
bluetoothNameSubstringCompareOptionsSelector :: Selector '[] NSStringCompareOptions
bluetoothNameSubstringCompareOptionsSelector = mkSelector "bluetoothNameSubstringCompareOptions"

-- | @Selector@ for @setBluetoothNameSubstringCompareOptions:@
setBluetoothNameSubstringCompareOptionsSelector :: Selector '[NSStringCompareOptions] ()
setBluetoothNameSubstringCompareOptionsSelector = mkSelector "setBluetoothNameSubstringCompareOptions:"

-- | @Selector@ for @bluetoothNameSubstring@
bluetoothNameSubstringSelector :: Selector '[] (Id NSString)
bluetoothNameSubstringSelector = mkSelector "bluetoothNameSubstring"

-- | @Selector@ for @setBluetoothNameSubstring:@
setBluetoothNameSubstringSelector :: Selector '[Id NSString] ()
setBluetoothNameSubstringSelector = mkSelector "setBluetoothNameSubstring:"

-- | @Selector@ for @bluetoothRange@
bluetoothRangeSelector :: Selector '[] ASDiscoveryDescriptorRange
bluetoothRangeSelector = mkSelector "bluetoothRange"

-- | @Selector@ for @setBluetoothRange:@
setBluetoothRangeSelector :: Selector '[ASDiscoveryDescriptorRange] ()
setBluetoothRangeSelector = mkSelector "setBluetoothRange:"

-- | @Selector@ for @bluetoothServiceDataBlob@
bluetoothServiceDataBlobSelector :: Selector '[] (Id NSData)
bluetoothServiceDataBlobSelector = mkSelector "bluetoothServiceDataBlob"

-- | @Selector@ for @setBluetoothServiceDataBlob:@
setBluetoothServiceDataBlobSelector :: Selector '[Id NSData] ()
setBluetoothServiceDataBlobSelector = mkSelector "setBluetoothServiceDataBlob:"

-- | @Selector@ for @bluetoothServiceDataMask@
bluetoothServiceDataMaskSelector :: Selector '[] (Id NSData)
bluetoothServiceDataMaskSelector = mkSelector "bluetoothServiceDataMask"

-- | @Selector@ for @setBluetoothServiceDataMask:@
setBluetoothServiceDataMaskSelector :: Selector '[Id NSData] ()
setBluetoothServiceDataMaskSelector = mkSelector "setBluetoothServiceDataMask:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @setSSID:@
setSSIDSelector :: Selector '[Id NSString] ()
setSSIDSelector = mkSelector "setSSID:"

-- | @Selector@ for @SSIDPrefix@
ssidPrefixSelector :: Selector '[] (Id NSString)
ssidPrefixSelector = mkSelector "SSIDPrefix"

-- | @Selector@ for @setSSIDPrefix:@
setSSIDPrefixSelector :: Selector '[Id NSString] ()
setSSIDPrefixSelector = mkSelector "setSSIDPrefix:"

-- | @Selector@ for @wifiAwareServiceName@
wifiAwareServiceNameSelector :: Selector '[] (Id NSString)
wifiAwareServiceNameSelector = mkSelector "wifiAwareServiceName"

-- | @Selector@ for @setWifiAwareServiceName:@
setWifiAwareServiceNameSelector :: Selector '[Id NSString] ()
setWifiAwareServiceNameSelector = mkSelector "setWifiAwareServiceName:"

-- | @Selector@ for @wifiAwareServiceRole@
wifiAwareServiceRoleSelector :: Selector '[] ASDiscoveryDescriptorWiFiAwareServiceRole
wifiAwareServiceRoleSelector = mkSelector "wifiAwareServiceRole"

-- | @Selector@ for @setWifiAwareServiceRole:@
setWifiAwareServiceRoleSelector :: Selector '[ASDiscoveryDescriptorWiFiAwareServiceRole] ()
setWifiAwareServiceRoleSelector = mkSelector "setWifiAwareServiceRole:"

-- | @Selector@ for @wifiAwareModelNameMatch@
wifiAwareModelNameMatchSelector :: Selector '[] (Id ASPropertyCompareString)
wifiAwareModelNameMatchSelector = mkSelector "wifiAwareModelNameMatch"

-- | @Selector@ for @setWifiAwareModelNameMatch:@
setWifiAwareModelNameMatchSelector :: Selector '[Id ASPropertyCompareString] ()
setWifiAwareModelNameMatchSelector = mkSelector "setWifiAwareModelNameMatch:"

-- | @Selector@ for @wifiAwareVendorNameMatch@
wifiAwareVendorNameMatchSelector :: Selector '[] (Id ASPropertyCompareString)
wifiAwareVendorNameMatchSelector = mkSelector "wifiAwareVendorNameMatch"

-- | @Selector@ for @setWifiAwareVendorNameMatch:@
setWifiAwareVendorNameMatchSelector :: Selector '[Id ASPropertyCompareString] ()
setWifiAwareVendorNameMatchSelector = mkSelector "setWifiAwareVendorNameMatch:"

