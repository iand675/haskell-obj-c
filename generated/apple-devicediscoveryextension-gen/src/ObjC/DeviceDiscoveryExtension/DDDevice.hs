{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | DeviceDiscoveryExtension device.
--
-- Generated bindings for @DDDevice@.
module ObjC.DeviceDiscoveryExtension.DDDevice
  ( DDDevice
  , IsDDDevice(..)
  , init_
  , initWithDisplayName_category_protocolType_identifier
  , deviceSupports
  , setDeviceSupports
  , bluetoothIdentifier
  , setBluetoothIdentifier
  , category
  , setCategory
  , displayImageName
  , setDisplayImageName
  , displayName
  , setDisplayName
  , identifier
  , setIdentifier
  , mediaPlaybackState
  , setMediaPlaybackState
  , mediaContentTitle
  , setMediaContentTitle
  , mediaContentSubtitle
  , setMediaContentSubtitle
  , networkEndpoint
  , setNetworkEndpoint
  , protocol
  , setProtocol
  , protocolType
  , setProtocolType
  , state
  , setState
  , ssid
  , setSSID
  , supportsGrouping
  , setSupportsGrouping
  , txtRecordData
  , setTxtRecordData
  , url
  , setUrl
  , wifiAwareServiceName
  , setWifiAwareServiceName
  , wifiAwareServiceRole
  , setWifiAwareServiceRole
  , wifiAwareModelName
  , setWifiAwareModelName
  , wifiAwareVendorName
  , setWifiAwareVendorName
  , bluetoothIdentifierSelector
  , categorySelector
  , deviceSupportsSelector
  , displayImageNameSelector
  , displayNameSelector
  , identifierSelector
  , initSelector
  , initWithDisplayName_category_protocolType_identifierSelector
  , mediaContentSubtitleSelector
  , mediaContentTitleSelector
  , mediaPlaybackStateSelector
  , networkEndpointSelector
  , protocolSelector
  , protocolTypeSelector
  , setBluetoothIdentifierSelector
  , setCategorySelector
  , setDeviceSupportsSelector
  , setDisplayImageNameSelector
  , setDisplayNameSelector
  , setIdentifierSelector
  , setMediaContentSubtitleSelector
  , setMediaContentTitleSelector
  , setMediaPlaybackStateSelector
  , setNetworkEndpointSelector
  , setProtocolSelector
  , setProtocolTypeSelector
  , setSSIDSelector
  , setStateSelector
  , setSupportsGroupingSelector
  , setTxtRecordDataSelector
  , setUrlSelector
  , setWifiAwareModelNameSelector
  , setWifiAwareServiceNameSelector
  , setWifiAwareServiceRoleSelector
  , setWifiAwareVendorNameSelector
  , ssidSelector
  , stateSelector
  , supportsGroupingSelector
  , txtRecordDataSelector
  , urlSelector
  , wifiAwareModelNameSelector
  , wifiAwareServiceNameSelector
  , wifiAwareServiceRoleSelector
  , wifiAwareVendorNameSelector

  -- * Enum types
  , DDDeviceCategory(DDDeviceCategory)
  , pattern DDDeviceCategoryHiFiSpeaker
  , pattern DDDeviceCategoryHiFiSpeakerMultiple
  , pattern DDDeviceCategoryTVWithMediaBox
  , pattern DDDeviceCategoryTV
  , pattern DDDeviceCategoryLaptopComputer
  , pattern DDDeviceCategoryDesktopComputer
  , pattern DDDeviceCategoryAccessorySetup
  , DDDeviceMediaPlaybackState(DDDeviceMediaPlaybackState)
  , pattern DDDeviceMediaPlaybackStateNoContent
  , pattern DDDeviceMediaPlaybackStatePaused
  , pattern DDDeviceMediaPlaybackStatePlaying
  , DDDeviceProtocol(DDDeviceProtocol)
  , pattern DDDeviceProtocolInvalid
  , pattern DDDeviceProtocolDIAL
  , DDDeviceState(DDDeviceState)
  , pattern DDDeviceStateInvalid
  , pattern DDDeviceStateActivating
  , pattern DDDeviceStateActivated
  , pattern DDDeviceStateAuthorized
  , pattern DDDeviceStateInvalidating
  , DDDeviceSupports(DDDeviceSupports)
  , pattern DDDeviceSupportsBluetoothPairingLE
  , pattern DDDeviceSupportsBluetoothTransportBridging
  , pattern DDDeviceSupportsBluetoothHID
  , DDDeviceWiFiAwareServiceRole(DDDeviceWiFiAwareServiceRole)
  , pattern DDDeviceWiFiAwareServiceRoleSubscriber
  , pattern DDDeviceWiFiAwareServiceRolePublisher

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DeviceDiscoveryExtension.Internal.Classes
import ObjC.DeviceDiscoveryExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsDDDevice ddDevice => ddDevice -> IO (Id DDDevice)
init_ ddDevice =
  sendOwnedMessage ddDevice initSelector

-- | Initializes a DD device with display name, category, protocol type, and identifier.
--
-- ObjC selector: @- initWithDisplayName:category:protocolType:identifier:@
initWithDisplayName_category_protocolType_identifier :: (IsDDDevice ddDevice, IsNSString displayName, IsUTType protocolType, IsNSString identifier) => ddDevice -> displayName -> DDDeviceCategory -> protocolType -> identifier -> IO (Id DDDevice)
initWithDisplayName_category_protocolType_identifier ddDevice displayName category protocolType identifier =
  sendOwnedMessage ddDevice initWithDisplayName_category_protocolType_identifierSelector (toNSString displayName) category (toUTType protocolType) (toNSString identifier)

-- | Device supported capabilities.
--
-- ObjC selector: @- deviceSupports@
deviceSupports :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceSupports
deviceSupports ddDevice =
  sendMessage ddDevice deviceSupportsSelector

-- | Device supported capabilities.
--
-- ObjC selector: @- setDeviceSupports:@
setDeviceSupports :: IsDDDevice ddDevice => ddDevice -> DDDeviceSupports -> IO ()
setDeviceSupports ddDevice value =
  sendMessage ddDevice setDeviceSupportsSelector value

-- | Identifier to communicate with the device via Bluetooth.
--
-- ObjC selector: @- bluetoothIdentifier@
bluetoothIdentifier :: IsDDDevice ddDevice => ddDevice -> IO (Id NSUUID)
bluetoothIdentifier ddDevice =
  sendMessage ddDevice bluetoothIdentifierSelector

-- | Identifier to communicate with the device via Bluetooth.
--
-- ObjC selector: @- setBluetoothIdentifier:@
setBluetoothIdentifier :: (IsDDDevice ddDevice, IsNSUUID value) => ddDevice -> value -> IO ()
setBluetoothIdentifier ddDevice value =
  sendMessage ddDevice setBluetoothIdentifierSelector (toNSUUID value)

-- | Category of the device.
--
-- ObjC selector: @- category@
category :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceCategory
category ddDevice =
  sendMessage ddDevice categorySelector

-- | Category of the device.
--
-- ObjC selector: @- setCategory:@
setCategory :: IsDDDevice ddDevice => ddDevice -> DDDeviceCategory -> IO ()
setCategory ddDevice value =
  sendMessage ddDevice setCategorySelector value

-- | Device's custom asset for product image name in the main App bundle.
--
-- ObjC selector: @- displayImageName@
displayImageName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
displayImageName ddDevice =
  sendMessage ddDevice displayImageNameSelector

-- | Device's custom asset for product image name in the main App bundle.
--
-- ObjC selector: @- setDisplayImageName:@
setDisplayImageName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setDisplayImageName ddDevice value =
  sendMessage ddDevice setDisplayImageNameSelector (toNSString value)

-- | Name of the device. Should be suitable for displaying to a user.
--
-- ObjC selector: @- displayName@
displayName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
displayName ddDevice =
  sendMessage ddDevice displayNameSelector

-- | Name of the device. Should be suitable for displaying to a user.
--
-- ObjC selector: @- setDisplayName:@
setDisplayName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setDisplayName ddDevice value =
  sendMessage ddDevice setDisplayNameSelector (toNSString value)

-- | Identifier of the device.
--
-- ObjC selector: @- identifier@
identifier :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
identifier ddDevice =
  sendMessage ddDevice identifierSelector

-- | Identifier of the device.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setIdentifier ddDevice value =
  sendMessage ddDevice setIdentifierSelector (toNSString value)

-- | Current state of media playback on this device.
--
-- ObjC selector: @- mediaPlaybackState@
mediaPlaybackState :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceMediaPlaybackState
mediaPlaybackState ddDevice =
  sendMessage ddDevice mediaPlaybackStateSelector

-- | Current state of media playback on this device.
--
-- ObjC selector: @- setMediaPlaybackState:@
setMediaPlaybackState :: IsDDDevice ddDevice => ddDevice -> DDDeviceMediaPlaybackState -> IO ()
setMediaPlaybackState ddDevice value =
  sendMessage ddDevice setMediaPlaybackStateSelector value

-- | Title of the media content being played.
--
-- ObjC selector: @- mediaContentTitle@
mediaContentTitle :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
mediaContentTitle ddDevice =
  sendMessage ddDevice mediaContentTitleSelector

-- | Title of the media content being played.
--
-- ObjC selector: @- setMediaContentTitle:@
setMediaContentTitle :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setMediaContentTitle ddDevice value =
  sendMessage ddDevice setMediaContentTitleSelector (toNSString value)

-- | Subtitle of the media content being played. It can be used to display extra information about the content, such as the name of the artist.
--
-- ObjC selector: @- mediaContentSubtitle@
mediaContentSubtitle :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
mediaContentSubtitle ddDevice =
  sendMessage ddDevice mediaContentSubtitleSelector

-- | Subtitle of the media content being played. It can be used to display extra information about the content, such as the name of the artist.
--
-- ObjC selector: @- setMediaContentSubtitle:@
setMediaContentSubtitle :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setMediaContentSubtitle ddDevice value =
  sendMessage ddDevice setMediaContentSubtitleSelector (toNSString value)

-- | Endpoint to communicate with the device via networking.
--
-- ObjC selector: @- networkEndpoint@
networkEndpoint :: IsDDDevice ddDevice => ddDevice -> IO (Id NSObject)
networkEndpoint ddDevice =
  sendMessage ddDevice networkEndpointSelector

-- | Endpoint to communicate with the device via networking.
--
-- ObjC selector: @- setNetworkEndpoint:@
setNetworkEndpoint :: (IsDDDevice ddDevice, IsNSObject value) => ddDevice -> value -> IO ()
setNetworkEndpoint ddDevice value =
  sendMessage ddDevice setNetworkEndpointSelector (toNSObject value)

-- | Protocol of the device.
--
-- ObjC selector: @- protocol@
protocol :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceProtocol
protocol ddDevice =
  sendMessage ddDevice protocolSelector

-- | Protocol of the device.
--
-- ObjC selector: @- setProtocol:@
setProtocol :: IsDDDevice ddDevice => ddDevice -> DDDeviceProtocol -> IO ()
setProtocol ddDevice value =
  sendMessage ddDevice setProtocolSelector value

-- | Uniform Type for the protocol.
--
-- ObjC selector: @- protocolType@
protocolType :: IsDDDevice ddDevice => ddDevice -> IO (Id UTType)
protocolType ddDevice =
  sendMessage ddDevice protocolTypeSelector

-- | Uniform Type for the protocol.
--
-- ObjC selector: @- setProtocolType:@
setProtocolType :: (IsDDDevice ddDevice, IsUTType value) => ddDevice -> value -> IO ()
setProtocolType ddDevice value =
  sendMessage ddDevice setProtocolTypeSelector (toUTType value)

-- | State of the device.
--
-- ObjC selector: @- state@
state :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceState
state ddDevice =
  sendMessage ddDevice stateSelector

-- | State of the device.
--
-- ObjC selector: @- setState:@
setState :: IsDDDevice ddDevice => ddDevice -> DDDeviceState -> IO ()
setState ddDevice value =
  sendMessage ddDevice setStateSelector value

-- | Device's WiFi Hotspot SSID.
--
-- ObjC selector: @- SSID@
ssid :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
ssid ddDevice =
  sendMessage ddDevice ssidSelector

-- | Device's WiFi Hotspot SSID.
--
-- ObjC selector: @- setSSID:@
setSSID :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setSSID ddDevice value =
  sendMessage ddDevice setSSIDSelector (toNSString value)

-- | Whether the device supports grouping with other devices with the same protocol.
--
-- ObjC selector: @- supportsGrouping@
supportsGrouping :: IsDDDevice ddDevice => ddDevice -> IO Bool
supportsGrouping ddDevice =
  sendMessage ddDevice supportsGroupingSelector

-- | Whether the device supports grouping with other devices with the same protocol.
--
-- ObjC selector: @- setSupportsGrouping:@
setSupportsGrouping :: IsDDDevice ddDevice => ddDevice -> Bool -> IO ()
setSupportsGrouping ddDevice value =
  sendMessage ddDevice setSupportsGroupingSelector value

-- | TXT record of the device.
--
-- ObjC selector: @- txtRecordData@
txtRecordData :: IsDDDevice ddDevice => ddDevice -> IO (Id NSData)
txtRecordData ddDevice =
  sendMessage ddDevice txtRecordDataSelector

-- | TXT record of the device.
--
-- ObjC selector: @- setTxtRecordData:@
setTxtRecordData :: (IsDDDevice ddDevice, IsNSData value) => ddDevice -> value -> IO ()
setTxtRecordData ddDevice value =
  sendMessage ddDevice setTxtRecordDataSelector (toNSData value)

-- | URL used for SSDP connection. The URL must have a valid hostname, no query parameters, and a maximum size of 100 bytes.
--
-- ObjC selector: @- url@
url :: IsDDDevice ddDevice => ddDevice -> IO (Id NSURL)
url ddDevice =
  sendMessage ddDevice urlSelector

-- | URL used for SSDP connection. The URL must have a valid hostname, no query parameters, and a maximum size of 100 bytes.
--
-- ObjC selector: @- setUrl:@
setUrl :: (IsDDDevice ddDevice, IsNSURL value) => ddDevice -> value -> IO ()
setUrl ddDevice value =
  sendMessage ddDevice setUrlSelector (toNSURL value)

-- | Device's Wi-Fi Aware's service name.
--
-- ObjC selector: @- wifiAwareServiceName@
wifiAwareServiceName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
wifiAwareServiceName ddDevice =
  sendMessage ddDevice wifiAwareServiceNameSelector

-- | Device's Wi-Fi Aware's service name.
--
-- ObjC selector: @- setWifiAwareServiceName:@
setWifiAwareServiceName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setWifiAwareServiceName ddDevice value =
  sendMessage ddDevice setWifiAwareServiceNameSelector (toNSString value)

-- | Device's Wi-Fi Aware's service. Default is @DDDeviceWiFiAwareServiceRoleSubscriber@
--
-- ObjC selector: @- wifiAwareServiceRole@
wifiAwareServiceRole :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceWiFiAwareServiceRole
wifiAwareServiceRole ddDevice =
  sendMessage ddDevice wifiAwareServiceRoleSelector

-- | Device's Wi-Fi Aware's service. Default is @DDDeviceWiFiAwareServiceRoleSubscriber@
--
-- ObjC selector: @- setWifiAwareServiceRole:@
setWifiAwareServiceRole :: IsDDDevice ddDevice => ddDevice -> DDDeviceWiFiAwareServiceRole -> IO ()
setWifiAwareServiceRole ddDevice value =
  sendMessage ddDevice setWifiAwareServiceRoleSelector value

-- | Device's Wi-Fi Aware model name.
--
-- ObjC selector: @- wifiAwareModelName@
wifiAwareModelName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
wifiAwareModelName ddDevice =
  sendMessage ddDevice wifiAwareModelNameSelector

-- | Device's Wi-Fi Aware model name.
--
-- ObjC selector: @- setWifiAwareModelName:@
setWifiAwareModelName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setWifiAwareModelName ddDevice value =
  sendMessage ddDevice setWifiAwareModelNameSelector (toNSString value)

-- | Device's Wi-Fi Aware vendor name.
--
-- ObjC selector: @- wifiAwareVendorName@
wifiAwareVendorName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
wifiAwareVendorName ddDevice =
  sendMessage ddDevice wifiAwareVendorNameSelector

-- | Device's Wi-Fi Aware vendor name.
--
-- ObjC selector: @- setWifiAwareVendorName:@
setWifiAwareVendorName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setWifiAwareVendorName ddDevice value =
  sendMessage ddDevice setWifiAwareVendorNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id DDDevice)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDisplayName:category:protocolType:identifier:@
initWithDisplayName_category_protocolType_identifierSelector :: Selector '[Id NSString, DDDeviceCategory, Id UTType, Id NSString] (Id DDDevice)
initWithDisplayName_category_protocolType_identifierSelector = mkSelector "initWithDisplayName:category:protocolType:identifier:"

-- | @Selector@ for @deviceSupports@
deviceSupportsSelector :: Selector '[] DDDeviceSupports
deviceSupportsSelector = mkSelector "deviceSupports"

-- | @Selector@ for @setDeviceSupports:@
setDeviceSupportsSelector :: Selector '[DDDeviceSupports] ()
setDeviceSupportsSelector = mkSelector "setDeviceSupports:"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector '[] (Id NSUUID)
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

-- | @Selector@ for @setBluetoothIdentifier:@
setBluetoothIdentifierSelector :: Selector '[Id NSUUID] ()
setBluetoothIdentifierSelector = mkSelector "setBluetoothIdentifier:"

-- | @Selector@ for @category@
categorySelector :: Selector '[] DDDeviceCategory
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector '[DDDeviceCategory] ()
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @displayImageName@
displayImageNameSelector :: Selector '[] (Id NSString)
displayImageNameSelector = mkSelector "displayImageName"

-- | @Selector@ for @setDisplayImageName:@
setDisplayImageNameSelector :: Selector '[Id NSString] ()
setDisplayImageNameSelector = mkSelector "setDisplayImageName:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector '[] (Id NSString)
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector '[Id NSString] ()
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSString)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector '[Id NSString] ()
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @mediaPlaybackState@
mediaPlaybackStateSelector :: Selector '[] DDDeviceMediaPlaybackState
mediaPlaybackStateSelector = mkSelector "mediaPlaybackState"

-- | @Selector@ for @setMediaPlaybackState:@
setMediaPlaybackStateSelector :: Selector '[DDDeviceMediaPlaybackState] ()
setMediaPlaybackStateSelector = mkSelector "setMediaPlaybackState:"

-- | @Selector@ for @mediaContentTitle@
mediaContentTitleSelector :: Selector '[] (Id NSString)
mediaContentTitleSelector = mkSelector "mediaContentTitle"

-- | @Selector@ for @setMediaContentTitle:@
setMediaContentTitleSelector :: Selector '[Id NSString] ()
setMediaContentTitleSelector = mkSelector "setMediaContentTitle:"

-- | @Selector@ for @mediaContentSubtitle@
mediaContentSubtitleSelector :: Selector '[] (Id NSString)
mediaContentSubtitleSelector = mkSelector "mediaContentSubtitle"

-- | @Selector@ for @setMediaContentSubtitle:@
setMediaContentSubtitleSelector :: Selector '[Id NSString] ()
setMediaContentSubtitleSelector = mkSelector "setMediaContentSubtitle:"

-- | @Selector@ for @networkEndpoint@
networkEndpointSelector :: Selector '[] (Id NSObject)
networkEndpointSelector = mkSelector "networkEndpoint"

-- | @Selector@ for @setNetworkEndpoint:@
setNetworkEndpointSelector :: Selector '[Id NSObject] ()
setNetworkEndpointSelector = mkSelector "setNetworkEndpoint:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] DDDeviceProtocol
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @setProtocol:@
setProtocolSelector :: Selector '[DDDeviceProtocol] ()
setProtocolSelector = mkSelector "setProtocol:"

-- | @Selector@ for @protocolType@
protocolTypeSelector :: Selector '[] (Id UTType)
protocolTypeSelector = mkSelector "protocolType"

-- | @Selector@ for @setProtocolType:@
setProtocolTypeSelector :: Selector '[Id UTType] ()
setProtocolTypeSelector = mkSelector "setProtocolType:"

-- | @Selector@ for @state@
stateSelector :: Selector '[] DDDeviceState
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector '[DDDeviceState] ()
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector '[] (Id NSString)
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @setSSID:@
setSSIDSelector :: Selector '[Id NSString] ()
setSSIDSelector = mkSelector "setSSID:"

-- | @Selector@ for @supportsGrouping@
supportsGroupingSelector :: Selector '[] Bool
supportsGroupingSelector = mkSelector "supportsGrouping"

-- | @Selector@ for @setSupportsGrouping:@
setSupportsGroupingSelector :: Selector '[Bool] ()
setSupportsGroupingSelector = mkSelector "setSupportsGrouping:"

-- | @Selector@ for @txtRecordData@
txtRecordDataSelector :: Selector '[] (Id NSData)
txtRecordDataSelector = mkSelector "txtRecordData"

-- | @Selector@ for @setTxtRecordData:@
setTxtRecordDataSelector :: Selector '[Id NSData] ()
setTxtRecordDataSelector = mkSelector "setTxtRecordData:"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[Id NSURL] ()
setUrlSelector = mkSelector "setUrl:"

-- | @Selector@ for @wifiAwareServiceName@
wifiAwareServiceNameSelector :: Selector '[] (Id NSString)
wifiAwareServiceNameSelector = mkSelector "wifiAwareServiceName"

-- | @Selector@ for @setWifiAwareServiceName:@
setWifiAwareServiceNameSelector :: Selector '[Id NSString] ()
setWifiAwareServiceNameSelector = mkSelector "setWifiAwareServiceName:"

-- | @Selector@ for @wifiAwareServiceRole@
wifiAwareServiceRoleSelector :: Selector '[] DDDeviceWiFiAwareServiceRole
wifiAwareServiceRoleSelector = mkSelector "wifiAwareServiceRole"

-- | @Selector@ for @setWifiAwareServiceRole:@
setWifiAwareServiceRoleSelector :: Selector '[DDDeviceWiFiAwareServiceRole] ()
setWifiAwareServiceRoleSelector = mkSelector "setWifiAwareServiceRole:"

-- | @Selector@ for @wifiAwareModelName@
wifiAwareModelNameSelector :: Selector '[] (Id NSString)
wifiAwareModelNameSelector = mkSelector "wifiAwareModelName"

-- | @Selector@ for @setWifiAwareModelName:@
setWifiAwareModelNameSelector :: Selector '[Id NSString] ()
setWifiAwareModelNameSelector = mkSelector "setWifiAwareModelName:"

-- | @Selector@ for @wifiAwareVendorName@
wifiAwareVendorNameSelector :: Selector '[] (Id NSString)
wifiAwareVendorNameSelector = mkSelector "wifiAwareVendorName"

-- | @Selector@ for @setWifiAwareVendorName:@
setWifiAwareVendorNameSelector :: Selector '[Id NSString] ()
setWifiAwareVendorNameSelector = mkSelector "setWifiAwareVendorName:"

