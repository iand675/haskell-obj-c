{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEAppPushManager
--
-- The NEAppPushManager class declares a programmatic interface to configure NEAppPushProvider.
--
-- NEAppPushManager declares methods and properties for configuring and managing life cycle of app push provider.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEAppPushManager@.
module ObjC.NetworkExtension.NEAppPushManager
  ( NEAppPushManager
  , IsNEAppPushManager(..)
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , matchSSIDs
  , setMatchSSIDs
  , matchPrivateLTENetworks
  , setMatchPrivateLTENetworks
  , matchEthernet
  , setMatchEthernet
  , providerConfiguration
  , setProviderConfiguration
  , providerBundleIdentifier
  , setProviderBundleIdentifier
  , delegate
  , setDelegate
  , localizedDescription
  , setLocalizedDescription
  , enabled
  , setEnabled
  , active
  , activeSelector
  , delegateSelector
  , enabledSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , matchEthernetSelector
  , matchPrivateLTENetworksSelector
  , matchSSIDsSelector
  , providerBundleIdentifierSelector
  , providerConfigurationSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setDelegateSelector
  , setEnabledSelector
  , setLocalizedDescriptionSelector
  , setMatchEthernetSelector
  , setMatchPrivateLTENetworksSelector
  , setMatchSSIDsSelector
  , setProviderBundleIdentifierSelector
  , setProviderConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | loadFromPreferencesWithCompletionHandler:
--
-- This method loads the saved configuration from the persistent store.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError object passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler neAppPushManager completionHandler =
  sendMessage neAppPushManager loadFromPreferencesWithCompletionHandlerSelector completionHandler

-- | removeFromPreferencesWithCompletionHandler:
--
-- This method removes the configuration from the persistent store.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError object passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler neAppPushManager completionHandler =
  sendMessage neAppPushManager removeFromPreferencesWithCompletionHandlerSelector completionHandler

-- | saveToPreferencesWithCompletionHandler:
--
-- This method saves the configuration in the persistent store.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError object passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler neAppPushManager completionHandler =
  sendMessage neAppPushManager saveToPreferencesWithCompletionHandlerSelector completionHandler

-- | matchSSIDs
--
-- An array of Wi-Fi SSID strings. If the SSID string of current Wi-Fi network matches with one of these strings and the Wi-Fi network is the primary route  on the device then the NEAppPushProvider is started. The upper limit of number of SSIDs is 10.
--
-- ObjC selector: @- matchSSIDs@
matchSSIDs :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO (Id NSArray)
matchSSIDs neAppPushManager =
  sendMessage neAppPushManager matchSSIDsSelector

-- | matchSSIDs
--
-- An array of Wi-Fi SSID strings. If the SSID string of current Wi-Fi network matches with one of these strings and the Wi-Fi network is the primary route  on the device then the NEAppPushProvider is started. The upper limit of number of SSIDs is 10.
--
-- ObjC selector: @- setMatchSSIDs:@
setMatchSSIDs :: (IsNEAppPushManager neAppPushManager, IsNSArray value) => neAppPushManager -> value -> IO ()
setMatchSSIDs neAppPushManager value =
  sendMessage neAppPushManager setMatchSSIDsSelector (toNSArray value)

-- | matchPrivateLTENetworks
--
-- An array of NEPrivateLTENetwork objects. If the properties of current private LTE network match with properties of one of these NEPrivateLTENetwork  objects and the private LTE network is the primary route on the device then the NEAppPushProvider is started. The upper limit of number of private LTE networks is 10.  For private LTE networks that are not band 48, the device must be supervised in order to perform the match
--
-- ObjC selector: @- matchPrivateLTENetworks@
matchPrivateLTENetworks :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO (Id NSArray)
matchPrivateLTENetworks neAppPushManager =
  sendMessage neAppPushManager matchPrivateLTENetworksSelector

-- | matchPrivateLTENetworks
--
-- An array of NEPrivateLTENetwork objects. If the properties of current private LTE network match with properties of one of these NEPrivateLTENetwork  objects and the private LTE network is the primary route on the device then the NEAppPushProvider is started. The upper limit of number of private LTE networks is 10.  For private LTE networks that are not band 48, the device must be supervised in order to perform the match
--
-- ObjC selector: @- setMatchPrivateLTENetworks:@
setMatchPrivateLTENetworks :: (IsNEAppPushManager neAppPushManager, IsNSArray value) => neAppPushManager -> value -> IO ()
setMatchPrivateLTENetworks neAppPushManager value =
  sendMessage neAppPushManager setMatchPrivateLTENetworksSelector (toNSArray value)

-- | matchEthernet
--
-- If set to YES NEAppPushProvider is started when iOS device is connected to an Ethernet network and the ethernet network is the primary route  on the device. NEAppPushProvider must determine viability of its functionality on the network. If the network does not support its operation it must call  [NEAppPushProvider unmatchEthernet:] method to stop itself.
--
-- ObjC selector: @- matchEthernet@
matchEthernet :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO Bool
matchEthernet neAppPushManager =
  sendMessage neAppPushManager matchEthernetSelector

-- | matchEthernet
--
-- If set to YES NEAppPushProvider is started when iOS device is connected to an Ethernet network and the ethernet network is the primary route  on the device. NEAppPushProvider must determine viability of its functionality on the network. If the network does not support its operation it must call  [NEAppPushProvider unmatchEthernet:] method to stop itself.
--
-- ObjC selector: @- setMatchEthernet:@
setMatchEthernet :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Bool -> IO ()
setMatchEthernet neAppPushManager value =
  sendMessage neAppPushManager setMatchEthernetSelector value

-- | providerConfiguration
--
-- A dictionary containing vendor-specific key-value pairs, where the data type of values must be one of the data types supported by property list. Values of user defined data 	type are not supported. This dictionary is passed as-is to NEAppPushProvider when is it is started or notified for other specified reasons.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO (Id NSDictionary)
providerConfiguration neAppPushManager =
  sendMessage neAppPushManager providerConfigurationSelector

-- | providerConfiguration
--
-- A dictionary containing vendor-specific key-value pairs, where the data type of values must be one of the data types supported by property list. Values of user defined data 	type are not supported. This dictionary is passed as-is to NEAppPushProvider when is it is started or notified for other specified reasons.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNEAppPushManager neAppPushManager, IsNSDictionary value) => neAppPushManager -> value -> IO ()
setProviderConfiguration neAppPushManager value =
  sendMessage neAppPushManager setProviderConfigurationSelector (toNSDictionary value)

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NEAppPushProvider.
--
-- ObjC selector: @- providerBundleIdentifier@
providerBundleIdentifier :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO (Id NSString)
providerBundleIdentifier neAppPushManager =
  sendMessage neAppPushManager providerBundleIdentifierSelector

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NEAppPushProvider.
--
-- ObjC selector: @- setProviderBundleIdentifier:@
setProviderBundleIdentifier :: (IsNEAppPushManager neAppPushManager, IsNSString value) => neAppPushManager -> value -> IO ()
setProviderBundleIdentifier neAppPushManager value =
  sendMessage neAppPushManager setProviderBundleIdentifierSelector (toNSString value)

-- | delegate
--
-- An instance of type NEAppPushDelegate that is required to receive incoming call informarion from the provider.
--
-- ObjC selector: @- delegate@
delegate :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO RawId
delegate neAppPushManager =
  sendMessage neAppPushManager delegateSelector

-- | delegate
--
-- An instance of type NEAppPushDelegate that is required to receive incoming call informarion from the provider.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsNEAppPushManager neAppPushManager => neAppPushManager -> RawId -> IO ()
setDelegate neAppPushManager value =
  sendMessage neAppPushManager setDelegateSelector value

-- | localizedDescription
--
-- A string containing a description of the app push manager.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO (Id NSString)
localizedDescription neAppPushManager =
  sendMessage neAppPushManager localizedDescriptionSelector

-- | localizedDescription
--
-- A string containing a description of the app push manager.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEAppPushManager neAppPushManager, IsNSString value) => neAppPushManager -> value -> IO ()
setLocalizedDescription neAppPushManager value =
  sendMessage neAppPushManager setLocalizedDescriptionSelector (toNSString value)

-- | enabled
--
-- Toggles the enabled status of the configuration. This property will be set to NO when the same app saves another configuration that overlaps with this configuration.
--
-- ObjC selector: @- enabled@
enabled :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO Bool
enabled neAppPushManager =
  sendMessage neAppPushManager enabledSelector

-- | enabled
--
-- Toggles the enabled status of the configuration. This property will be set to NO when the same app saves another configuration that overlaps with this configuration.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNEAppPushManager neAppPushManager => neAppPushManager -> Bool -> IO ()
setEnabled neAppPushManager value =
  sendMessage neAppPushManager setEnabledSelector value

-- | active
--
-- If set to YES, it indicates the associated configuration is in use. Use KVO to watch for changes.
--
-- ObjC selector: @- active@
active :: IsNEAppPushManager neAppPushManager => neAppPushManager -> IO Bool
active neAppPushManager =
  sendMessage neAppPushManager activeSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @matchSSIDs@
matchSSIDsSelector :: Selector '[] (Id NSArray)
matchSSIDsSelector = mkSelector "matchSSIDs"

-- | @Selector@ for @setMatchSSIDs:@
setMatchSSIDsSelector :: Selector '[Id NSArray] ()
setMatchSSIDsSelector = mkSelector "setMatchSSIDs:"

-- | @Selector@ for @matchPrivateLTENetworks@
matchPrivateLTENetworksSelector :: Selector '[] (Id NSArray)
matchPrivateLTENetworksSelector = mkSelector "matchPrivateLTENetworks"

-- | @Selector@ for @setMatchPrivateLTENetworks:@
setMatchPrivateLTENetworksSelector :: Selector '[Id NSArray] ()
setMatchPrivateLTENetworksSelector = mkSelector "setMatchPrivateLTENetworks:"

-- | @Selector@ for @matchEthernet@
matchEthernetSelector :: Selector '[] Bool
matchEthernetSelector = mkSelector "matchEthernet"

-- | @Selector@ for @setMatchEthernet:@
setMatchEthernetSelector :: Selector '[Bool] ()
setMatchEthernetSelector = mkSelector "setMatchEthernet:"

-- | @Selector@ for @providerConfiguration@
providerConfigurationSelector :: Selector '[] (Id NSDictionary)
providerConfigurationSelector = mkSelector "providerConfiguration"

-- | @Selector@ for @setProviderConfiguration:@
setProviderConfigurationSelector :: Selector '[Id NSDictionary] ()
setProviderConfigurationSelector = mkSelector "setProviderConfiguration:"

-- | @Selector@ for @providerBundleIdentifier@
providerBundleIdentifierSelector :: Selector '[] (Id NSString)
providerBundleIdentifierSelector = mkSelector "providerBundleIdentifier"

-- | @Selector@ for @setProviderBundleIdentifier:@
setProviderBundleIdentifierSelector :: Selector '[Id NSString] ()
setProviderBundleIdentifierSelector = mkSelector "setProviderBundleIdentifier:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @active@
activeSelector :: Selector '[] Bool
activeSelector = mkSelector "active"

