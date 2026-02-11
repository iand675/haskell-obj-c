{-# LANGUAGE PatternSynonyms #-}
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
  , initSelector
  , initWithDisplayName_category_protocolType_identifierSelector
  , deviceSupportsSelector
  , setDeviceSupportsSelector
  , bluetoothIdentifierSelector
  , setBluetoothIdentifierSelector
  , categorySelector
  , setCategorySelector
  , displayImageNameSelector
  , setDisplayImageNameSelector
  , displayNameSelector
  , setDisplayNameSelector
  , identifierSelector
  , setIdentifierSelector
  , mediaPlaybackStateSelector
  , setMediaPlaybackStateSelector
  , mediaContentTitleSelector
  , setMediaContentTitleSelector
  , mediaContentSubtitleSelector
  , setMediaContentSubtitleSelector
  , networkEndpointSelector
  , setNetworkEndpointSelector
  , protocolSelector
  , setProtocolSelector
  , protocolTypeSelector
  , setProtocolTypeSelector
  , stateSelector
  , setStateSelector
  , ssidSelector
  , setSSIDSelector
  , supportsGroupingSelector
  , setSupportsGroupingSelector
  , txtRecordDataSelector
  , setTxtRecordDataSelector
  , urlSelector
  , setUrlSelector
  , wifiAwareServiceNameSelector
  , setWifiAwareServiceNameSelector
  , wifiAwareServiceRoleSelector
  , setWifiAwareServiceRoleSelector
  , wifiAwareModelNameSelector
  , setWifiAwareModelNameSelector
  , wifiAwareVendorNameSelector
  , setWifiAwareVendorNameSelector

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

import ObjC.DeviceDiscoveryExtension.Internal.Classes
import ObjC.DeviceDiscoveryExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- init@
init_ :: IsDDDevice ddDevice => ddDevice -> IO (Id DDDevice)
init_ ddDevice  =
    sendMsg ddDevice (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes a DD device with display name, category, protocol type, and identifier.
--
-- ObjC selector: @- initWithDisplayName:category:protocolType:identifier:@
initWithDisplayName_category_protocolType_identifier :: (IsDDDevice ddDevice, IsNSString displayName, IsUTType protocolType, IsNSString identifier) => ddDevice -> displayName -> DDDeviceCategory -> protocolType -> identifier -> IO (Id DDDevice)
initWithDisplayName_category_protocolType_identifier ddDevice  displayName category protocolType identifier =
  withObjCPtr displayName $ \raw_displayName ->
    withObjCPtr protocolType $ \raw_protocolType ->
      withObjCPtr identifier $ \raw_identifier ->
          sendMsg ddDevice (mkSelector "initWithDisplayName:category:protocolType:identifier:") (retPtr retVoid) [argPtr (castPtr raw_displayName :: Ptr ()), argCLong (coerce category), argPtr (castPtr raw_protocolType :: Ptr ()), argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | Device supported capabilities.
--
-- ObjC selector: @- deviceSupports@
deviceSupports :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceSupports
deviceSupports ddDevice  =
    fmap (coerce :: CULong -> DDDeviceSupports) $ sendMsg ddDevice (mkSelector "deviceSupports") retCULong []

-- | Device supported capabilities.
--
-- ObjC selector: @- setDeviceSupports:@
setDeviceSupports :: IsDDDevice ddDevice => ddDevice -> DDDeviceSupports -> IO ()
setDeviceSupports ddDevice  value =
    sendMsg ddDevice (mkSelector "setDeviceSupports:") retVoid [argCULong (coerce value)]

-- | Identifier to communicate with the device via Bluetooth.
--
-- ObjC selector: @- bluetoothIdentifier@
bluetoothIdentifier :: IsDDDevice ddDevice => ddDevice -> IO (Id NSUUID)
bluetoothIdentifier ddDevice  =
    sendMsg ddDevice (mkSelector "bluetoothIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identifier to communicate with the device via Bluetooth.
--
-- ObjC selector: @- setBluetoothIdentifier:@
setBluetoothIdentifier :: (IsDDDevice ddDevice, IsNSUUID value) => ddDevice -> value -> IO ()
setBluetoothIdentifier ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setBluetoothIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Category of the device.
--
-- ObjC selector: @- category@
category :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceCategory
category ddDevice  =
    fmap (coerce :: CLong -> DDDeviceCategory) $ sendMsg ddDevice (mkSelector "category") retCLong []

-- | Category of the device.
--
-- ObjC selector: @- setCategory:@
setCategory :: IsDDDevice ddDevice => ddDevice -> DDDeviceCategory -> IO ()
setCategory ddDevice  value =
    sendMsg ddDevice (mkSelector "setCategory:") retVoid [argCLong (coerce value)]

-- | Device's custom asset for product image name in the main App bundle.
--
-- ObjC selector: @- displayImageName@
displayImageName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
displayImageName ddDevice  =
    sendMsg ddDevice (mkSelector "displayImageName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Device's custom asset for product image name in the main App bundle.
--
-- ObjC selector: @- setDisplayImageName:@
setDisplayImageName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setDisplayImageName ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setDisplayImageName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Name of the device. Should be suitable for displaying to a user.
--
-- ObjC selector: @- displayName@
displayName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
displayName ddDevice  =
    sendMsg ddDevice (mkSelector "displayName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Name of the device. Should be suitable for displaying to a user.
--
-- ObjC selector: @- setDisplayName:@
setDisplayName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setDisplayName ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setDisplayName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Identifier of the device.
--
-- ObjC selector: @- identifier@
identifier :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
identifier ddDevice  =
    sendMsg ddDevice (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Identifier of the device.
--
-- ObjC selector: @- setIdentifier:@
setIdentifier :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setIdentifier ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Current state of media playback on this device.
--
-- ObjC selector: @- mediaPlaybackState@
mediaPlaybackState :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceMediaPlaybackState
mediaPlaybackState ddDevice  =
    fmap (coerce :: CLong -> DDDeviceMediaPlaybackState) $ sendMsg ddDevice (mkSelector "mediaPlaybackState") retCLong []

-- | Current state of media playback on this device.
--
-- ObjC selector: @- setMediaPlaybackState:@
setMediaPlaybackState :: IsDDDevice ddDevice => ddDevice -> DDDeviceMediaPlaybackState -> IO ()
setMediaPlaybackState ddDevice  value =
    sendMsg ddDevice (mkSelector "setMediaPlaybackState:") retVoid [argCLong (coerce value)]

-- | Title of the media content being played.
--
-- ObjC selector: @- mediaContentTitle@
mediaContentTitle :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
mediaContentTitle ddDevice  =
    sendMsg ddDevice (mkSelector "mediaContentTitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Title of the media content being played.
--
-- ObjC selector: @- setMediaContentTitle:@
setMediaContentTitle :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setMediaContentTitle ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setMediaContentTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Subtitle of the media content being played. It can be used to display extra information about the content, such as the name of the artist.
--
-- ObjC selector: @- mediaContentSubtitle@
mediaContentSubtitle :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
mediaContentSubtitle ddDevice  =
    sendMsg ddDevice (mkSelector "mediaContentSubtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Subtitle of the media content being played. It can be used to display extra information about the content, such as the name of the artist.
--
-- ObjC selector: @- setMediaContentSubtitle:@
setMediaContentSubtitle :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setMediaContentSubtitle ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setMediaContentSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Endpoint to communicate with the device via networking.
--
-- ObjC selector: @- networkEndpoint@
networkEndpoint :: IsDDDevice ddDevice => ddDevice -> IO (Id NSObject)
networkEndpoint ddDevice  =
    sendMsg ddDevice (mkSelector "networkEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Endpoint to communicate with the device via networking.
--
-- ObjC selector: @- setNetworkEndpoint:@
setNetworkEndpoint :: (IsDDDevice ddDevice, IsNSObject value) => ddDevice -> value -> IO ()
setNetworkEndpoint ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setNetworkEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Protocol of the device.
--
-- ObjC selector: @- protocol@
protocol :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceProtocol
protocol ddDevice  =
    fmap (coerce :: CLong -> DDDeviceProtocol) $ sendMsg ddDevice (mkSelector "protocol") retCLong []

-- | Protocol of the device.
--
-- ObjC selector: @- setProtocol:@
setProtocol :: IsDDDevice ddDevice => ddDevice -> DDDeviceProtocol -> IO ()
setProtocol ddDevice  value =
    sendMsg ddDevice (mkSelector "setProtocol:") retVoid [argCLong (coerce value)]

-- | Uniform Type for the protocol.
--
-- ObjC selector: @- protocolType@
protocolType :: IsDDDevice ddDevice => ddDevice -> IO (Id UTType)
protocolType ddDevice  =
    sendMsg ddDevice (mkSelector "protocolType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Uniform Type for the protocol.
--
-- ObjC selector: @- setProtocolType:@
setProtocolType :: (IsDDDevice ddDevice, IsUTType value) => ddDevice -> value -> IO ()
setProtocolType ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setProtocolType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | State of the device.
--
-- ObjC selector: @- state@
state :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceState
state ddDevice  =
    fmap (coerce :: CLong -> DDDeviceState) $ sendMsg ddDevice (mkSelector "state") retCLong []

-- | State of the device.
--
-- ObjC selector: @- setState:@
setState :: IsDDDevice ddDevice => ddDevice -> DDDeviceState -> IO ()
setState ddDevice  value =
    sendMsg ddDevice (mkSelector "setState:") retVoid [argCLong (coerce value)]

-- | Device's WiFi Hotspot SSID.
--
-- ObjC selector: @- SSID@
ssid :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
ssid ddDevice  =
    sendMsg ddDevice (mkSelector "SSID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Device's WiFi Hotspot SSID.
--
-- ObjC selector: @- setSSID:@
setSSID :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setSSID ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setSSID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Whether the device supports grouping with other devices with the same protocol.
--
-- ObjC selector: @- supportsGrouping@
supportsGrouping :: IsDDDevice ddDevice => ddDevice -> IO Bool
supportsGrouping ddDevice  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg ddDevice (mkSelector "supportsGrouping") retCULong []

-- | Whether the device supports grouping with other devices with the same protocol.
--
-- ObjC selector: @- setSupportsGrouping:@
setSupportsGrouping :: IsDDDevice ddDevice => ddDevice -> Bool -> IO ()
setSupportsGrouping ddDevice  value =
    sendMsg ddDevice (mkSelector "setSupportsGrouping:") retVoid [argCULong (if value then 1 else 0)]

-- | TXT record of the device.
--
-- ObjC selector: @- txtRecordData@
txtRecordData :: IsDDDevice ddDevice => ddDevice -> IO (Id NSData)
txtRecordData ddDevice  =
    sendMsg ddDevice (mkSelector "txtRecordData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | TXT record of the device.
--
-- ObjC selector: @- setTxtRecordData:@
setTxtRecordData :: (IsDDDevice ddDevice, IsNSData value) => ddDevice -> value -> IO ()
setTxtRecordData ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setTxtRecordData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | URL used for SSDP connection. The URL must have a valid hostname, no query parameters, and a maximum size of 100 bytes.
--
-- ObjC selector: @- url@
url :: IsDDDevice ddDevice => ddDevice -> IO (Id NSURL)
url ddDevice  =
    sendMsg ddDevice (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | URL used for SSDP connection. The URL must have a valid hostname, no query parameters, and a maximum size of 100 bytes.
--
-- ObjC selector: @- setUrl:@
setUrl :: (IsDDDevice ddDevice, IsNSURL value) => ddDevice -> value -> IO ()
setUrl ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Device's Wi-Fi Aware's service name.
--
-- ObjC selector: @- wifiAwareServiceName@
wifiAwareServiceName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
wifiAwareServiceName ddDevice  =
    sendMsg ddDevice (mkSelector "wifiAwareServiceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Device's Wi-Fi Aware's service name.
--
-- ObjC selector: @- setWifiAwareServiceName:@
setWifiAwareServiceName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setWifiAwareServiceName ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setWifiAwareServiceName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Device's Wi-Fi Aware's service. Default is @DDDeviceWiFiAwareServiceRoleSubscriber@
--
-- ObjC selector: @- wifiAwareServiceRole@
wifiAwareServiceRole :: IsDDDevice ddDevice => ddDevice -> IO DDDeviceWiFiAwareServiceRole
wifiAwareServiceRole ddDevice  =
    fmap (coerce :: CLong -> DDDeviceWiFiAwareServiceRole) $ sendMsg ddDevice (mkSelector "wifiAwareServiceRole") retCLong []

-- | Device's Wi-Fi Aware's service. Default is @DDDeviceWiFiAwareServiceRoleSubscriber@
--
-- ObjC selector: @- setWifiAwareServiceRole:@
setWifiAwareServiceRole :: IsDDDevice ddDevice => ddDevice -> DDDeviceWiFiAwareServiceRole -> IO ()
setWifiAwareServiceRole ddDevice  value =
    sendMsg ddDevice (mkSelector "setWifiAwareServiceRole:") retVoid [argCLong (coerce value)]

-- | Device's Wi-Fi Aware model name.
--
-- ObjC selector: @- wifiAwareModelName@
wifiAwareModelName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
wifiAwareModelName ddDevice  =
    sendMsg ddDevice (mkSelector "wifiAwareModelName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Device's Wi-Fi Aware model name.
--
-- ObjC selector: @- setWifiAwareModelName:@
setWifiAwareModelName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setWifiAwareModelName ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setWifiAwareModelName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Device's Wi-Fi Aware vendor name.
--
-- ObjC selector: @- wifiAwareVendorName@
wifiAwareVendorName :: IsDDDevice ddDevice => ddDevice -> IO (Id NSString)
wifiAwareVendorName ddDevice  =
    sendMsg ddDevice (mkSelector "wifiAwareVendorName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Device's Wi-Fi Aware vendor name.
--
-- ObjC selector: @- setWifiAwareVendorName:@
setWifiAwareVendorName :: (IsDDDevice ddDevice, IsNSString value) => ddDevice -> value -> IO ()
setWifiAwareVendorName ddDevice  value =
  withObjCPtr value $ \raw_value ->
      sendMsg ddDevice (mkSelector "setWifiAwareVendorName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithDisplayName:category:protocolType:identifier:@
initWithDisplayName_category_protocolType_identifierSelector :: Selector
initWithDisplayName_category_protocolType_identifierSelector = mkSelector "initWithDisplayName:category:protocolType:identifier:"

-- | @Selector@ for @deviceSupports@
deviceSupportsSelector :: Selector
deviceSupportsSelector = mkSelector "deviceSupports"

-- | @Selector@ for @setDeviceSupports:@
setDeviceSupportsSelector :: Selector
setDeviceSupportsSelector = mkSelector "setDeviceSupports:"

-- | @Selector@ for @bluetoothIdentifier@
bluetoothIdentifierSelector :: Selector
bluetoothIdentifierSelector = mkSelector "bluetoothIdentifier"

-- | @Selector@ for @setBluetoothIdentifier:@
setBluetoothIdentifierSelector :: Selector
setBluetoothIdentifierSelector = mkSelector "setBluetoothIdentifier:"

-- | @Selector@ for @category@
categorySelector :: Selector
categorySelector = mkSelector "category"

-- | @Selector@ for @setCategory:@
setCategorySelector :: Selector
setCategorySelector = mkSelector "setCategory:"

-- | @Selector@ for @displayImageName@
displayImageNameSelector :: Selector
displayImageNameSelector = mkSelector "displayImageName"

-- | @Selector@ for @setDisplayImageName:@
setDisplayImageNameSelector :: Selector
setDisplayImageNameSelector = mkSelector "setDisplayImageName:"

-- | @Selector@ for @displayName@
displayNameSelector :: Selector
displayNameSelector = mkSelector "displayName"

-- | @Selector@ for @setDisplayName:@
setDisplayNameSelector :: Selector
setDisplayNameSelector = mkSelector "setDisplayName:"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @setIdentifier:@
setIdentifierSelector :: Selector
setIdentifierSelector = mkSelector "setIdentifier:"

-- | @Selector@ for @mediaPlaybackState@
mediaPlaybackStateSelector :: Selector
mediaPlaybackStateSelector = mkSelector "mediaPlaybackState"

-- | @Selector@ for @setMediaPlaybackState:@
setMediaPlaybackStateSelector :: Selector
setMediaPlaybackStateSelector = mkSelector "setMediaPlaybackState:"

-- | @Selector@ for @mediaContentTitle@
mediaContentTitleSelector :: Selector
mediaContentTitleSelector = mkSelector "mediaContentTitle"

-- | @Selector@ for @setMediaContentTitle:@
setMediaContentTitleSelector :: Selector
setMediaContentTitleSelector = mkSelector "setMediaContentTitle:"

-- | @Selector@ for @mediaContentSubtitle@
mediaContentSubtitleSelector :: Selector
mediaContentSubtitleSelector = mkSelector "mediaContentSubtitle"

-- | @Selector@ for @setMediaContentSubtitle:@
setMediaContentSubtitleSelector :: Selector
setMediaContentSubtitleSelector = mkSelector "setMediaContentSubtitle:"

-- | @Selector@ for @networkEndpoint@
networkEndpointSelector :: Selector
networkEndpointSelector = mkSelector "networkEndpoint"

-- | @Selector@ for @setNetworkEndpoint:@
setNetworkEndpointSelector :: Selector
setNetworkEndpointSelector = mkSelector "setNetworkEndpoint:"

-- | @Selector@ for @protocol@
protocolSelector :: Selector
protocolSelector = mkSelector "protocol"

-- | @Selector@ for @setProtocol:@
setProtocolSelector :: Selector
setProtocolSelector = mkSelector "setProtocol:"

-- | @Selector@ for @protocolType@
protocolTypeSelector :: Selector
protocolTypeSelector = mkSelector "protocolType"

-- | @Selector@ for @setProtocolType:@
setProtocolTypeSelector :: Selector
setProtocolTypeSelector = mkSelector "setProtocolType:"

-- | @Selector@ for @state@
stateSelector :: Selector
stateSelector = mkSelector "state"

-- | @Selector@ for @setState:@
setStateSelector :: Selector
setStateSelector = mkSelector "setState:"

-- | @Selector@ for @SSID@
ssidSelector :: Selector
ssidSelector = mkSelector "SSID"

-- | @Selector@ for @setSSID:@
setSSIDSelector :: Selector
setSSIDSelector = mkSelector "setSSID:"

-- | @Selector@ for @supportsGrouping@
supportsGroupingSelector :: Selector
supportsGroupingSelector = mkSelector "supportsGrouping"

-- | @Selector@ for @setSupportsGrouping:@
setSupportsGroupingSelector :: Selector
setSupportsGroupingSelector = mkSelector "setSupportsGrouping:"

-- | @Selector@ for @txtRecordData@
txtRecordDataSelector :: Selector
txtRecordDataSelector = mkSelector "txtRecordData"

-- | @Selector@ for @setTxtRecordData:@
setTxtRecordDataSelector :: Selector
setTxtRecordDataSelector = mkSelector "setTxtRecordData:"

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector
setUrlSelector = mkSelector "setUrl:"

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

-- | @Selector@ for @wifiAwareModelName@
wifiAwareModelNameSelector :: Selector
wifiAwareModelNameSelector = mkSelector "wifiAwareModelName"

-- | @Selector@ for @setWifiAwareModelName:@
setWifiAwareModelNameSelector :: Selector
setWifiAwareModelNameSelector = mkSelector "setWifiAwareModelName:"

-- | @Selector@ for @wifiAwareVendorName@
wifiAwareVendorNameSelector :: Selector
wifiAwareVendorNameSelector = mkSelector "wifiAwareVendorName"

-- | @Selector@ for @setWifiAwareVendorName:@
setWifiAwareVendorNameSelector :: Selector
setWifiAwareVendorNameSelector = mkSelector "setWifiAwareVendorName:"

