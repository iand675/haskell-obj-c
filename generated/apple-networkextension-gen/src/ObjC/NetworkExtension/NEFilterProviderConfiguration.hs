{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEFilterProviderConfiguration
--
-- The NEFilterProviderConfiguration class declares the programmatic interface of an object that configures a plugin-based content filter.
--
-- Generated bindings for @NEFilterProviderConfiguration@.
module ObjC.NetworkExtension.NEFilterProviderConfiguration
  ( NEFilterProviderConfiguration
  , IsNEFilterProviderConfiguration(..)
  , filterBrowsers
  , setFilterBrowsers
  , filterSockets
  , setFilterSockets
  , filterPackets
  , setFilterPackets
  , vendorConfiguration
  , setVendorConfiguration
  , serverAddress
  , setServerAddress
  , username
  , setUsername
  , organization
  , setOrganization
  , passwordReference
  , setPasswordReference
  , identityReference
  , setIdentityReference
  , filterDataProviderBundleIdentifier
  , setFilterDataProviderBundleIdentifier
  , filterPacketProviderBundleIdentifier
  , setFilterPacketProviderBundleIdentifier
  , filterBrowsersSelector
  , filterDataProviderBundleIdentifierSelector
  , filterPacketProviderBundleIdentifierSelector
  , filterPacketsSelector
  , filterSocketsSelector
  , identityReferenceSelector
  , organizationSelector
  , passwordReferenceSelector
  , serverAddressSelector
  , setFilterBrowsersSelector
  , setFilterDataProviderBundleIdentifierSelector
  , setFilterPacketProviderBundleIdentifierSelector
  , setFilterPacketsSelector
  , setFilterSocketsSelector
  , setIdentityReferenceSelector
  , setOrganizationSelector
  , setPasswordReferenceSelector
  , setServerAddressSelector
  , setUsernameSelector
  , setVendorConfigurationSelector
  , usernameSelector
  , vendorConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | filterBrowsers
--
-- If YES, the filter plugin will be allowed to filter browser traffic. If NO, the filter plugin will not see any browser flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- filterBrowsers@
filterBrowsers :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO Bool
filterBrowsers neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration filterBrowsersSelector

-- | filterBrowsers
--
-- If YES, the filter plugin will be allowed to filter browser traffic. If NO, the filter plugin will not see any browser flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- setFilterBrowsers:@
setFilterBrowsers :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> Bool -> IO ()
setFilterBrowsers neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setFilterBrowsersSelector value

-- | filterSockets
--
-- If YES, the filter plugin will be allowed to filter socket traffic. If NO, the filter plugin will not see any socket flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- filterSockets@
filterSockets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO Bool
filterSockets neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration filterSocketsSelector

-- | filterSockets
--
-- If YES, the filter plugin will be allowed to filter socket traffic. If NO, the filter plugin will not see any socket flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- setFilterSockets:@
setFilterSockets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> Bool -> IO ()
setFilterSockets neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setFilterSocketsSelector value

-- | filterPackets
--
-- If YES, a NEFilterPacketProvider will be instantiated and will be allowed to filter packets.
--
-- ObjC selector: @- filterPackets@
filterPackets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO Bool
filterPackets neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration filterPacketsSelector

-- | filterPackets
--
-- If YES, a NEFilterPacketProvider will be instantiated and will be allowed to filter packets.
--
-- ObjC selector: @- setFilterPackets:@
setFilterPackets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> Bool -> IO ()
setFilterPackets neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setFilterPacketsSelector value

-- | vendorConfiguration
--
-- An optional dictionary of plugin-specific keys to be passed to the plugin.
--
-- ObjC selector: @- vendorConfiguration@
vendorConfiguration :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSDictionary)
vendorConfiguration neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration vendorConfigurationSelector

-- | vendorConfiguration
--
-- An optional dictionary of plugin-specific keys to be passed to the plugin.
--
-- ObjC selector: @- setVendorConfiguration:@
setVendorConfiguration :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSDictionary value) => neFilterProviderConfiguration -> value -> IO ()
setVendorConfiguration neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setVendorConfigurationSelector (toNSDictionary value)

-- | serverAddress
--
-- The optional address of the server used to support the filter.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
serverAddress neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration serverAddressSelector

-- | serverAddress
--
-- The optional address of the server used to support the filter.
--
-- ObjC selector: @- setServerAddress:@
setServerAddress :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setServerAddress neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setServerAddressSelector (toNSString value)

-- | username
--
-- The optional username associated with the filter.
--
-- ObjC selector: @- username@
username :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
username neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration usernameSelector

-- | username
--
-- The optional username associated with the filter.
--
-- ObjC selector: @- setUsername:@
setUsername :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setUsername neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setUsernameSelector (toNSString value)

-- | organization
--
-- The optional organization associated with the filter.
--
-- ObjC selector: @- organization@
organization :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
organization neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration organizationSelector

-- | organization
--
-- The optional organization associated with the filter.
--
-- ObjC selector: @- setOrganization:@
setOrganization :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setOrganization neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setOrganizationSelector (toNSString value)

-- | passwordReference
--
-- The optional password keychain reference associated with the filter.
--
-- ObjC selector: @- passwordReference@
passwordReference :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSData)
passwordReference neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration passwordReferenceSelector

-- | passwordReference
--
-- The optional password keychain reference associated with the filter.
--
-- ObjC selector: @- setPasswordReference:@
setPasswordReference :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSData value) => neFilterProviderConfiguration -> value -> IO ()
setPasswordReference neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setPasswordReferenceSelector (toNSData value)

-- | identityReference
--
-- The optional certificate identity keychain reference associated with the filter.
--
-- ObjC selector: @- identityReference@
identityReference :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSData)
identityReference neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration identityReferenceSelector

-- | identityReference
--
-- The optional certificate identity keychain reference associated with the filter.
--
-- ObjC selector: @- setIdentityReference:@
setIdentityReference :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSData value) => neFilterProviderConfiguration -> value -> IO ()
setIdentityReference neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setIdentityReferenceSelector (toNSData value)

-- | filterDataProviderBundleIdentifier
--
-- A string containing the bundle identifier of the NEFilterDataProvider app extension or system extension.     If this property is nil, then the bundle identifier of the NEFilterDataProvider extension in the calling app's     bundle is used, and if the calling app's bundle contains more than one NEFilterDataProvider extension then which one will     be used is undefined.
--
-- ObjC selector: @- filterDataProviderBundleIdentifier@
filterDataProviderBundleIdentifier :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
filterDataProviderBundleIdentifier neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration filterDataProviderBundleIdentifierSelector

-- | filterDataProviderBundleIdentifier
--
-- A string containing the bundle identifier of the NEFilterDataProvider app extension or system extension.     If this property is nil, then the bundle identifier of the NEFilterDataProvider extension in the calling app's     bundle is used, and if the calling app's bundle contains more than one NEFilterDataProvider extension then which one will     be used is undefined.
--
-- ObjC selector: @- setFilterDataProviderBundleIdentifier:@
setFilterDataProviderBundleIdentifier :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setFilterDataProviderBundleIdentifier neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setFilterDataProviderBundleIdentifierSelector (toNSString value)

-- | filterPacketProviderBundleIdentifier
--
-- A string containing the bundle identifier of the NEFilterPacketProvider app extension or system extension.     If this property is nil, then the bundle identifier of the NEFilterPacketProvider extension in the calling app's     bundle is used, and if the calling app's bundle contains more than one NEFilterPacketProvider extension then which one will     be used is undefined.
--
-- ObjC selector: @- filterPacketProviderBundleIdentifier@
filterPacketProviderBundleIdentifier :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
filterPacketProviderBundleIdentifier neFilterProviderConfiguration =
  sendMessage neFilterProviderConfiguration filterPacketProviderBundleIdentifierSelector

-- | filterPacketProviderBundleIdentifier
--
-- A string containing the bundle identifier of the NEFilterPacketProvider app extension or system extension.     If this property is nil, then the bundle identifier of the NEFilterPacketProvider extension in the calling app's     bundle is used, and if the calling app's bundle contains more than one NEFilterPacketProvider extension then which one will     be used is undefined.
--
-- ObjC selector: @- setFilterPacketProviderBundleIdentifier:@
setFilterPacketProviderBundleIdentifier :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setFilterPacketProviderBundleIdentifier neFilterProviderConfiguration value =
  sendMessage neFilterProviderConfiguration setFilterPacketProviderBundleIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterBrowsers@
filterBrowsersSelector :: Selector '[] Bool
filterBrowsersSelector = mkSelector "filterBrowsers"

-- | @Selector@ for @setFilterBrowsers:@
setFilterBrowsersSelector :: Selector '[Bool] ()
setFilterBrowsersSelector = mkSelector "setFilterBrowsers:"

-- | @Selector@ for @filterSockets@
filterSocketsSelector :: Selector '[] Bool
filterSocketsSelector = mkSelector "filterSockets"

-- | @Selector@ for @setFilterSockets:@
setFilterSocketsSelector :: Selector '[Bool] ()
setFilterSocketsSelector = mkSelector "setFilterSockets:"

-- | @Selector@ for @filterPackets@
filterPacketsSelector :: Selector '[] Bool
filterPacketsSelector = mkSelector "filterPackets"

-- | @Selector@ for @setFilterPackets:@
setFilterPacketsSelector :: Selector '[Bool] ()
setFilterPacketsSelector = mkSelector "setFilterPackets:"

-- | @Selector@ for @vendorConfiguration@
vendorConfigurationSelector :: Selector '[] (Id NSDictionary)
vendorConfigurationSelector = mkSelector "vendorConfiguration"

-- | @Selector@ for @setVendorConfiguration:@
setVendorConfigurationSelector :: Selector '[Id NSDictionary] ()
setVendorConfigurationSelector = mkSelector "setVendorConfiguration:"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector '[] (Id NSString)
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @setServerAddress:@
setServerAddressSelector :: Selector '[Id NSString] ()
setServerAddressSelector = mkSelector "setServerAddress:"

-- | @Selector@ for @username@
usernameSelector :: Selector '[] (Id NSString)
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector '[Id NSString] ()
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @organization@
organizationSelector :: Selector '[] (Id NSString)
organizationSelector = mkSelector "organization"

-- | @Selector@ for @setOrganization:@
setOrganizationSelector :: Selector '[Id NSString] ()
setOrganizationSelector = mkSelector "setOrganization:"

-- | @Selector@ for @passwordReference@
passwordReferenceSelector :: Selector '[] (Id NSData)
passwordReferenceSelector = mkSelector "passwordReference"

-- | @Selector@ for @setPasswordReference:@
setPasswordReferenceSelector :: Selector '[Id NSData] ()
setPasswordReferenceSelector = mkSelector "setPasswordReference:"

-- | @Selector@ for @identityReference@
identityReferenceSelector :: Selector '[] (Id NSData)
identityReferenceSelector = mkSelector "identityReference"

-- | @Selector@ for @setIdentityReference:@
setIdentityReferenceSelector :: Selector '[Id NSData] ()
setIdentityReferenceSelector = mkSelector "setIdentityReference:"

-- | @Selector@ for @filterDataProviderBundleIdentifier@
filterDataProviderBundleIdentifierSelector :: Selector '[] (Id NSString)
filterDataProviderBundleIdentifierSelector = mkSelector "filterDataProviderBundleIdentifier"

-- | @Selector@ for @setFilterDataProviderBundleIdentifier:@
setFilterDataProviderBundleIdentifierSelector :: Selector '[Id NSString] ()
setFilterDataProviderBundleIdentifierSelector = mkSelector "setFilterDataProviderBundleIdentifier:"

-- | @Selector@ for @filterPacketProviderBundleIdentifier@
filterPacketProviderBundleIdentifierSelector :: Selector '[] (Id NSString)
filterPacketProviderBundleIdentifierSelector = mkSelector "filterPacketProviderBundleIdentifier"

-- | @Selector@ for @setFilterPacketProviderBundleIdentifier:@
setFilterPacketProviderBundleIdentifierSelector :: Selector '[Id NSString] ()
setFilterPacketProviderBundleIdentifierSelector = mkSelector "setFilterPacketProviderBundleIdentifier:"

