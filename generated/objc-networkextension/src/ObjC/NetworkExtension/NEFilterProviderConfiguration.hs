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
  , filterBrowsersSelector
  , setFilterBrowsersSelector
  , filterSocketsSelector
  , setFilterSocketsSelector
  , filterPacketsSelector
  , setFilterPacketsSelector
  , vendorConfigurationSelector
  , setVendorConfigurationSelector
  , serverAddressSelector
  , setServerAddressSelector
  , usernameSelector
  , setUsernameSelector
  , organizationSelector
  , setOrganizationSelector
  , passwordReferenceSelector
  , setPasswordReferenceSelector
  , identityReferenceSelector
  , setIdentityReferenceSelector


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

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | filterBrowsers
--
-- If YES, the filter plugin will be allowed to filter browser traffic. If NO, the filter plugin will not see any browser flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- filterBrowsers@
filterBrowsers :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO Bool
filterBrowsers neFilterProviderConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neFilterProviderConfiguration (mkSelector "filterBrowsers") retCULong []

-- | filterBrowsers
--
-- If YES, the filter plugin will be allowed to filter browser traffic. If NO, the filter plugin will not see any browser flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- setFilterBrowsers:@
setFilterBrowsers :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> Bool -> IO ()
setFilterBrowsers neFilterProviderConfiguration  value =
  sendMsg neFilterProviderConfiguration (mkSelector "setFilterBrowsers:") retVoid [argCULong (if value then 1 else 0)]

-- | filterSockets
--
-- If YES, the filter plugin will be allowed to filter socket traffic. If NO, the filter plugin will not see any socket flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- filterSockets@
filterSockets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO Bool
filterSockets neFilterProviderConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neFilterProviderConfiguration (mkSelector "filterSockets") retCULong []

-- | filterSockets
--
-- If YES, the filter plugin will be allowed to filter socket traffic. If NO, the filter plugin will not see any socket flows. Defaults to NO. At least one of filterBrowsers and filterSockets should be set to YES to make the filter take effect.
--
-- ObjC selector: @- setFilterSockets:@
setFilterSockets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> Bool -> IO ()
setFilterSockets neFilterProviderConfiguration  value =
  sendMsg neFilterProviderConfiguration (mkSelector "setFilterSockets:") retVoid [argCULong (if value then 1 else 0)]

-- | filterPackets
--
-- If YES, a NEFilterPacketProvider will be instantiated and will be allowed to filter packets.
--
-- ObjC selector: @- filterPackets@
filterPackets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO Bool
filterPackets neFilterProviderConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neFilterProviderConfiguration (mkSelector "filterPackets") retCULong []

-- | filterPackets
--
-- If YES, a NEFilterPacketProvider will be instantiated and will be allowed to filter packets.
--
-- ObjC selector: @- setFilterPackets:@
setFilterPackets :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> Bool -> IO ()
setFilterPackets neFilterProviderConfiguration  value =
  sendMsg neFilterProviderConfiguration (mkSelector "setFilterPackets:") retVoid [argCULong (if value then 1 else 0)]

-- | vendorConfiguration
--
-- An optional dictionary of plugin-specific keys to be passed to the plugin.
--
-- ObjC selector: @- vendorConfiguration@
vendorConfiguration :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSDictionary)
vendorConfiguration neFilterProviderConfiguration  =
  sendMsg neFilterProviderConfiguration (mkSelector "vendorConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | vendorConfiguration
--
-- An optional dictionary of plugin-specific keys to be passed to the plugin.
--
-- ObjC selector: @- setVendorConfiguration:@
setVendorConfiguration :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSDictionary value) => neFilterProviderConfiguration -> value -> IO ()
setVendorConfiguration neFilterProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterProviderConfiguration (mkSelector "setVendorConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | serverAddress
--
-- The optional address of the server used to support the filter.
--
-- ObjC selector: @- serverAddress@
serverAddress :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
serverAddress neFilterProviderConfiguration  =
  sendMsg neFilterProviderConfiguration (mkSelector "serverAddress") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | serverAddress
--
-- The optional address of the server used to support the filter.
--
-- ObjC selector: @- setServerAddress:@
setServerAddress :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setServerAddress neFilterProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterProviderConfiguration (mkSelector "setServerAddress:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | username
--
-- The optional username associated with the filter.
--
-- ObjC selector: @- username@
username :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
username neFilterProviderConfiguration  =
  sendMsg neFilterProviderConfiguration (mkSelector "username") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | username
--
-- The optional username associated with the filter.
--
-- ObjC selector: @- setUsername:@
setUsername :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setUsername neFilterProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterProviderConfiguration (mkSelector "setUsername:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | organization
--
-- The optional organization associated with the filter.
--
-- ObjC selector: @- organization@
organization :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSString)
organization neFilterProviderConfiguration  =
  sendMsg neFilterProviderConfiguration (mkSelector "organization") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | organization
--
-- The optional organization associated with the filter.
--
-- ObjC selector: @- setOrganization:@
setOrganization :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSString value) => neFilterProviderConfiguration -> value -> IO ()
setOrganization neFilterProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterProviderConfiguration (mkSelector "setOrganization:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | passwordReference
--
-- The optional password keychain reference associated with the filter.
--
-- ObjC selector: @- passwordReference@
passwordReference :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSData)
passwordReference neFilterProviderConfiguration  =
  sendMsg neFilterProviderConfiguration (mkSelector "passwordReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | passwordReference
--
-- The optional password keychain reference associated with the filter.
--
-- ObjC selector: @- setPasswordReference:@
setPasswordReference :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSData value) => neFilterProviderConfiguration -> value -> IO ()
setPasswordReference neFilterProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterProviderConfiguration (mkSelector "setPasswordReference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | identityReference
--
-- The optional certificate identity keychain reference associated with the filter.
--
-- ObjC selector: @- identityReference@
identityReference :: IsNEFilterProviderConfiguration neFilterProviderConfiguration => neFilterProviderConfiguration -> IO (Id NSData)
identityReference neFilterProviderConfiguration  =
  sendMsg neFilterProviderConfiguration (mkSelector "identityReference") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | identityReference
--
-- The optional certificate identity keychain reference associated with the filter.
--
-- ObjC selector: @- setIdentityReference:@
setIdentityReference :: (IsNEFilterProviderConfiguration neFilterProviderConfiguration, IsNSData value) => neFilterProviderConfiguration -> value -> IO ()
setIdentityReference neFilterProviderConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg neFilterProviderConfiguration (mkSelector "setIdentityReference:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @filterBrowsers@
filterBrowsersSelector :: Selector
filterBrowsersSelector = mkSelector "filterBrowsers"

-- | @Selector@ for @setFilterBrowsers:@
setFilterBrowsersSelector :: Selector
setFilterBrowsersSelector = mkSelector "setFilterBrowsers:"

-- | @Selector@ for @filterSockets@
filterSocketsSelector :: Selector
filterSocketsSelector = mkSelector "filterSockets"

-- | @Selector@ for @setFilterSockets:@
setFilterSocketsSelector :: Selector
setFilterSocketsSelector = mkSelector "setFilterSockets:"

-- | @Selector@ for @filterPackets@
filterPacketsSelector :: Selector
filterPacketsSelector = mkSelector "filterPackets"

-- | @Selector@ for @setFilterPackets:@
setFilterPacketsSelector :: Selector
setFilterPacketsSelector = mkSelector "setFilterPackets:"

-- | @Selector@ for @vendorConfiguration@
vendorConfigurationSelector :: Selector
vendorConfigurationSelector = mkSelector "vendorConfiguration"

-- | @Selector@ for @setVendorConfiguration:@
setVendorConfigurationSelector :: Selector
setVendorConfigurationSelector = mkSelector "setVendorConfiguration:"

-- | @Selector@ for @serverAddress@
serverAddressSelector :: Selector
serverAddressSelector = mkSelector "serverAddress"

-- | @Selector@ for @setServerAddress:@
setServerAddressSelector :: Selector
setServerAddressSelector = mkSelector "setServerAddress:"

-- | @Selector@ for @username@
usernameSelector :: Selector
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @organization@
organizationSelector :: Selector
organizationSelector = mkSelector "organization"

-- | @Selector@ for @setOrganization:@
setOrganizationSelector :: Selector
setOrganizationSelector = mkSelector "setOrganization:"

-- | @Selector@ for @passwordReference@
passwordReferenceSelector :: Selector
passwordReferenceSelector = mkSelector "passwordReference"

-- | @Selector@ for @setPasswordReference:@
setPasswordReferenceSelector :: Selector
setPasswordReferenceSelector = mkSelector "setPasswordReference:"

-- | @Selector@ for @identityReference@
identityReferenceSelector :: Selector
identityReferenceSelector = mkSelector "identityReference"

-- | @Selector@ for @setIdentityReference:@
setIdentityReferenceSelector :: Selector
setIdentityReferenceSelector = mkSelector "setIdentityReference:"

