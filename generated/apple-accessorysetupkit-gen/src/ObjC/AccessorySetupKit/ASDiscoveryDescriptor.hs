{-# LANGUAGE PatternSynonyms #-}
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
  , supportedOptionsSelector
  , setSupportedOptionsSelector
  , bluetoothCompanyIdentifierSelector
  , setBluetoothCompanyIdentifierSelector
  , bluetoothManufacturerDataBlobSelector
  , setBluetoothManufacturerDataBlobSelector
  , bluetoothManufacturerDataMaskSelector
  , setBluetoothManufacturerDataMaskSelector
  , bluetoothNameSubstringCompareOptionsSelector
  , setBluetoothNameSubstringCompareOptionsSelector
  , bluetoothNameSubstringSelector
  , setBluetoothNameSubstringSelector
  , bluetoothRangeSelector
  , setBluetoothRangeSelector
  , bluetoothServiceDataBlobSelector
  , setBluetoothServiceDataBlobSelector
  , bluetoothServiceDataMaskSelector
  , setBluetoothServiceDataMaskSelector
  , ssidSelector
  , setSSIDSelector
  , ssidPrefixSelector
  , setSSIDPrefixSelector
  , wifiAwareServiceNameSelector
  , setWifiAwareServiceNameSelector
  , wifiAwareServiceRoleSelector
  , setWifiAwareServiceRoleSelector
  , wifiAwareModelNameMatchSelector
  , setWifiAwareModelNameMatchSelector
  , wifiAwareVendorNameMatchSelector
  , setWifiAwareVendorNameMatchSelector

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

import ObjC.AccessorySetupKit.Internal.Classes
import ObjC.AccessorySetupKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Options supported by an accessory.
--
-- ObjC selector: @- supportedOptions@
supportedOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO ASAccessorySupportOptions
supportedOptions asDiscoveryDescriptor  =
    fmap (coerce :: CULong -> ASAccessorySupportOptions) $ sendMsg asDiscoveryDescriptor (mkSelector "supportedOptions") retCULong []

-- | Options supported by an accessory.
--
-- ObjC selector: @- setSupportedOptions:@
setSupportedOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> ASAccessorySupportOptions -> IO ()
setSupportedOptions asDiscoveryDescriptor  value =
    sendMsg asDiscoveryDescriptor (mkSelector "setSupportedOptions:") retVoid [argCULong (coerce value)]

-- | The accessory's 16-bit Bluetooth Company Identifier.
--
-- ObjC selector: @- bluetoothCompanyIdentifier@
bluetoothCompanyIdentifier :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO CUShort
bluetoothCompanyIdentifier asDiscoveryDescriptor  =
    fmap fromIntegral $ sendMsg asDiscoveryDescriptor (mkSelector "bluetoothCompanyIdentifier") retCUInt []

-- | The accessory's 16-bit Bluetooth Company Identifier.
--
-- ObjC selector: @- setBluetoothCompanyIdentifier:@
setBluetoothCompanyIdentifier :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> CUShort -> IO ()
setBluetoothCompanyIdentifier asDiscoveryDescriptor  value =
    sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothCompanyIdentifier:") retVoid [argCUInt (fromIntegral value)]

-- | A byte buffer that matches the accessory's Bluetooth manufacturer data.
--
-- ObjC selector: @- bluetoothManufacturerDataBlob@
bluetoothManufacturerDataBlob :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothManufacturerDataBlob asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "bluetoothManufacturerDataBlob") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A byte buffer that matches the accessory's Bluetooth manufacturer data.
--
-- ObjC selector: @- setBluetoothManufacturerDataBlob:@
setBluetoothManufacturerDataBlob :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothManufacturerDataBlob asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothManufacturerDataBlob:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory's Bluetooth manufacturer data mask.
--
-- ObjC selector: @- bluetoothManufacturerDataMask@
bluetoothManufacturerDataMask :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothManufacturerDataMask asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "bluetoothManufacturerDataMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Bluetooth manufacturer data mask.
--
-- ObjC selector: @- setBluetoothManufacturerDataMask:@
setBluetoothManufacturerDataMask :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothManufacturerDataMask asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothManufacturerDataMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory's over-the-air Bluetooth name substring compare options.
--
-- ObjC selector: @- bluetoothNameSubstringCompareOptions@
bluetoothNameSubstringCompareOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO NSStringCompareOptions
bluetoothNameSubstringCompareOptions asDiscoveryDescriptor  =
    fmap (coerce :: CULong -> NSStringCompareOptions) $ sendMsg asDiscoveryDescriptor (mkSelector "bluetoothNameSubstringCompareOptions") retCULong []

-- | The accessory's over-the-air Bluetooth name substring compare options.
--
-- ObjC selector: @- setBluetoothNameSubstringCompareOptions:@
setBluetoothNameSubstringCompareOptions :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> NSStringCompareOptions -> IO ()
setBluetoothNameSubstringCompareOptions asDiscoveryDescriptor  value =
    sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothNameSubstringCompareOptions:") retVoid [argCULong (coerce value)]

-- | The accessory's over-the-air Bluetooth name substring.
--
-- ObjC selector: @- bluetoothNameSubstring@
bluetoothNameSubstring :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
bluetoothNameSubstring asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "bluetoothNameSubstring") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's over-the-air Bluetooth name substring.
--
-- ObjC selector: @- setBluetoothNameSubstring:@
setBluetoothNameSubstring :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothNameSubstring asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothNameSubstring:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | A property that tells the session to discover accessories within a specific Bluetooth range.
--
-- ObjC selector: @- bluetoothRange@
bluetoothRange :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO ASDiscoveryDescriptorRange
bluetoothRange asDiscoveryDescriptor  =
    fmap (coerce :: CLong -> ASDiscoveryDescriptorRange) $ sendMsg asDiscoveryDescriptor (mkSelector "bluetoothRange") retCLong []

-- | A property that tells the session to discover accessories within a specific Bluetooth range.
--
-- ObjC selector: @- setBluetoothRange:@
setBluetoothRange :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> ASDiscoveryDescriptorRange -> IO ()
setBluetoothRange asDiscoveryDescriptor  value =
    sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothRange:") retVoid [argCLong (coerce value)]

-- | A byte buffer that matches the accessory's Bluetooth service data.
--
-- ObjC selector: @- bluetoothServiceDataBlob@
bluetoothServiceDataBlob :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothServiceDataBlob asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "bluetoothServiceDataBlob") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A byte buffer that matches the accessory's Bluetooth service data.
--
-- ObjC selector: @- setBluetoothServiceDataBlob:@
setBluetoothServiceDataBlob :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothServiceDataBlob asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothServiceDataBlob:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory's Bluetooth service data mask.
--
-- ObjC selector: @- bluetoothServiceDataMask@
bluetoothServiceDataMask :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSData)
bluetoothServiceDataMask asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "bluetoothServiceDataMask") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Bluetooth service data mask.
--
-- ObjC selector: @- setBluetoothServiceDataMask:@
setBluetoothServiceDataMask :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSData value) => asDiscoveryDescriptor -> value -> IO ()
setBluetoothServiceDataMask asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setBluetoothServiceDataMask:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- SSID@
ssid :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
ssid asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "SSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- setSSID:@
setSSID :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setSSID asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setSSID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The prefix string of SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- SSIDPrefix@
ssidPrefix :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
ssidPrefix asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "SSIDPrefix") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The prefix string of SSID of the accessory's Wi-Fi network.
--
-- ObjC selector: @- setSSIDPrefix:@
setSSIDPrefix :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setSSIDPrefix asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setSSIDPrefix:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory's Wi-Fi Aware's service name if available.
--
-- ObjC selector: @- wifiAwareServiceName@
wifiAwareServiceName :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id NSString)
wifiAwareServiceName asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "wifiAwareServiceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Wi-Fi Aware's service name if available.
--
-- ObjC selector: @- setWifiAwareServiceName:@
setWifiAwareServiceName :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsNSString value) => asDiscoveryDescriptor -> value -> IO ()
setWifiAwareServiceName asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setWifiAwareServiceName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The role of the accessory's Wi-Fi Aware's service.
--
-- This property defaults to ``ASDiscoveryDescriptor/WiFiAwareServiceRole/subscriber``
--
-- ObjC selector: @- wifiAwareServiceRole@
wifiAwareServiceRole :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO ASDiscoveryDescriptorWiFiAwareServiceRole
wifiAwareServiceRole asDiscoveryDescriptor  =
    fmap (coerce :: CLong -> ASDiscoveryDescriptorWiFiAwareServiceRole) $ sendMsg asDiscoveryDescriptor (mkSelector "wifiAwareServiceRole") retCLong []

-- | The role of the accessory's Wi-Fi Aware's service.
--
-- This property defaults to ``ASDiscoveryDescriptor/WiFiAwareServiceRole/subscriber``
--
-- ObjC selector: @- setWifiAwareServiceRole:@
setWifiAwareServiceRole :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> ASDiscoveryDescriptorWiFiAwareServiceRole -> IO ()
setWifiAwareServiceRole asDiscoveryDescriptor  value =
    sendMsg asDiscoveryDescriptor (mkSelector "setWifiAwareServiceRole:") retVoid [argCLong (coerce value)]

-- | The accessory's Wi-Fi Aware model name and matching options.
--
-- ObjC selector: @- wifiAwareModelNameMatch@
wifiAwareModelNameMatch :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id ASPropertyCompareString)
wifiAwareModelNameMatch asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "wifiAwareModelNameMatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Wi-Fi Aware model name and matching options.
--
-- ObjC selector: @- setWifiAwareModelNameMatch:@
setWifiAwareModelNameMatch :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsASPropertyCompareString value) => asDiscoveryDescriptor -> value -> IO ()
setWifiAwareModelNameMatch asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setWifiAwareModelNameMatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The accessory's Wi-Fi Aware vendor name and matching options.
--
-- ObjC selector: @- wifiAwareVendorNameMatch@
wifiAwareVendorNameMatch :: IsASDiscoveryDescriptor asDiscoveryDescriptor => asDiscoveryDescriptor -> IO (Id ASPropertyCompareString)
wifiAwareVendorNameMatch asDiscoveryDescriptor  =
    sendMsg asDiscoveryDescriptor (mkSelector "wifiAwareVendorNameMatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The accessory's Wi-Fi Aware vendor name and matching options.
--
-- ObjC selector: @- setWifiAwareVendorNameMatch:@
setWifiAwareVendorNameMatch :: (IsASDiscoveryDescriptor asDiscoveryDescriptor, IsASPropertyCompareString value) => asDiscoveryDescriptor -> value -> IO ()
setWifiAwareVendorNameMatch asDiscoveryDescriptor  value =
  withObjCPtr value $ \raw_value ->
      sendMsg asDiscoveryDescriptor (mkSelector "setWifiAwareVendorNameMatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @supportedOptions@
supportedOptionsSelector :: Selector
supportedOptionsSelector = mkSelector "supportedOptions"

-- | @Selector@ for @setSupportedOptions:@
setSupportedOptionsSelector :: Selector
setSupportedOptionsSelector = mkSelector "setSupportedOptions:"

-- | @Selector@ for @bluetoothCompanyIdentifier@
bluetoothCompanyIdentifierSelector :: Selector
bluetoothCompanyIdentifierSelector = mkSelector "bluetoothCompanyIdentifier"

-- | @Selector@ for @setBluetoothCompanyIdentifier:@
setBluetoothCompanyIdentifierSelector :: Selector
setBluetoothCompanyIdentifierSelector = mkSelector "setBluetoothCompanyIdentifier:"

-- | @Selector@ for @bluetoothManufacturerDataBlob@
bluetoothManufacturerDataBlobSelector :: Selector
bluetoothManufacturerDataBlobSelector = mkSelector "bluetoothManufacturerDataBlob"

-- | @Selector@ for @setBluetoothManufacturerDataBlob:@
setBluetoothManufacturerDataBlobSelector :: Selector
setBluetoothManufacturerDataBlobSelector = mkSelector "setBluetoothManufacturerDataBlob:"

-- | @Selector@ for @bluetoothManufacturerDataMask@
bluetoothManufacturerDataMaskSelector :: Selector
bluetoothManufacturerDataMaskSelector = mkSelector "bluetoothManufacturerDataMask"

-- | @Selector@ for @setBluetoothManufacturerDataMask:@
setBluetoothManufacturerDataMaskSelector :: Selector
setBluetoothManufacturerDataMaskSelector = mkSelector "setBluetoothManufacturerDataMask:"

-- | @Selector@ for @bluetoothNameSubstringCompareOptions@
bluetoothNameSubstringCompareOptionsSelector :: Selector
bluetoothNameSubstringCompareOptionsSelector = mkSelector "bluetoothNameSubstringCompareOptions"

-- | @Selector@ for @setBluetoothNameSubstringCompareOptions:@
setBluetoothNameSubstringCompareOptionsSelector :: Selector
setBluetoothNameSubstringCompareOptionsSelector = mkSelector "setBluetoothNameSubstringCompareOptions:"

-- | @Selector@ for @bluetoothNameSubstring@
bluetoothNameSubstringSelector :: Selector
bluetoothNameSubstringSelector = mkSelector "bluetoothNameSubstring"

-- | @Selector@ for @setBluetoothNameSubstring:@
setBluetoothNameSubstringSelector :: Selector
setBluetoothNameSubstringSelector = mkSelector "setBluetoothNameSubstring:"

-- | @Selector@ for @bluetoothRange@
bluetoothRangeSelector :: Selector
bluetoothRangeSelector = mkSelector "bluetoothRange"

-- | @Selector@ for @setBluetoothRange:@
setBluetoothRangeSelector :: Selector
setBluetoothRangeSelector = mkSelector "setBluetoothRange:"

-- | @Selector@ for @bluetoothServiceDataBlob@
bluetoothServiceDataBlobSelector :: Selector
bluetoothServiceDataBlobSelector = mkSelector "bluetoothServiceDataBlob"

-- | @Selector@ for @setBluetoothServiceDataBlob:@
setBluetoothServiceDataBlobSelector :: Selector
setBluetoothServiceDataBlobSelector = mkSelector "setBluetoothServiceDataBlob:"

-- | @Selector@ for @bluetoothServiceDataMask@
bluetoothServiceDataMaskSelector :: Selector
bluetoothServiceDataMaskSelector = mkSelector "bluetoothServiceDataMask"

-- | @Selector@ for @setBluetoothServiceDataMask:@
setBluetoothServiceDataMaskSelector :: Selector
setBluetoothServiceDataMaskSelector = mkSelector "setBluetoothServiceDataMask:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @setSSID:@
setSSIDSelector :: Selector
setSSIDSelector = mkSelector "setSSID:"

-- | @Selector@ for @SSIDPrefix@
ssidPrefixSelector :: Selector
ssidPrefixSelector = mkSelector "SSIDPrefix"

-- | @Selector@ for @setSSIDPrefix:@
setSSIDPrefixSelector :: Selector
setSSIDPrefixSelector = mkSelector "setSSIDPrefix:"

-- | @Selector@ for @wifiAwareServiceName@
wifiAwareServiceNameSelector :: Selector
wifiAwareServiceNameSelector = mkSelector "wifiAwareServiceName"

-- | @Selector@ for @setWifiAwareServiceName:@
setWifiAwareServiceNameSelector :: Selector
setWifiAwareServiceNameSelector = mkSelector "setWifiAwareServiceName:"

-- | @Selector@ for @wifiAwareServiceRole@
wifiAwareServiceRoleSelector :: Selector
wifiAwareServiceRoleSelector = mkSelector "wifiAwareServiceRole"

-- | @Selector@ for @setWifiAwareServiceRole:@
setWifiAwareServiceRoleSelector :: Selector
setWifiAwareServiceRoleSelector = mkSelector "setWifiAwareServiceRole:"

-- | @Selector@ for @wifiAwareModelNameMatch@
wifiAwareModelNameMatchSelector :: Selector
wifiAwareModelNameMatchSelector = mkSelector "wifiAwareModelNameMatch"

-- | @Selector@ for @setWifiAwareModelNameMatch:@
setWifiAwareModelNameMatchSelector :: Selector
setWifiAwareModelNameMatchSelector = mkSelector "setWifiAwareModelNameMatch:"

-- | @Selector@ for @wifiAwareVendorNameMatch@
wifiAwareVendorNameMatchSelector :: Selector
wifiAwareVendorNameMatchSelector = mkSelector "wifiAwareVendorNameMatch"

-- | @Selector@ for @setWifiAwareVendorNameMatch:@
setWifiAwareVendorNameMatchSelector :: Selector
setWifiAwareVendorNameMatchSelector = mkSelector "setWifiAwareVendorNameMatch:"

