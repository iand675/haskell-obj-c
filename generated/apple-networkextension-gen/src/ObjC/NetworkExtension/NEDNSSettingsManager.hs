{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEDNSSettingsManager
--
-- The NEDNSSettingsManager class declares the programmatic interface for an object that manages DNS settings configurations.
--
-- NEDNSSettingsManager declares methods and properties for configuring and controlling DNS settings on the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEDNSSettingsManager@.
module ObjC.NetworkExtension.NEDNSSettingsManager
  ( NEDNSSettingsManager
  , IsNEDNSSettingsManager(..)
  , sharedManager
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , localizedDescription
  , setLocalizedDescription
  , dnsSettings
  , setDnsSettings
  , onDemandRules
  , setOnDemandRules
  , enabled
  , dnsSettingsSelector
  , enabledSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , onDemandRulesSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setDnsSettingsSelector
  , setLocalizedDescriptionSelector
  , setOnDemandRulesSelector
  , sharedManagerSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | sharedManager
--
-- Returns: The singleton NEDNSSettingsManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NEDNSSettingsManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEDNSSettingsManager"
    sendClassMessage cls' sharedManagerSelector

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current DNS settings configuration from the caller's DNS settings preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler nednsSettingsManager completionHandler =
  sendMessage nednsSettingsManager loadFromPreferencesWithCompletionHandlerSelector completionHandler

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the DNS settings configuration from the caller's DNS settings preferences. If the DNS settings are enabled, the DNS settings becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler nednsSettingsManager completionHandler =
  sendMessage nednsSettingsManager removeFromPreferencesWithCompletionHandlerSelector completionHandler

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the DNS settingsconfiguration in the caller's DNS settings preferences. If the DNS settings are enabled, they will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler nednsSettingsManager completionHandler =
  sendMessage nednsSettingsManager saveToPreferencesWithCompletionHandlerSelector completionHandler

-- | localizedDescription
--
-- A string containing a description of the DNS settings.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO (Id NSString)
localizedDescription nednsSettingsManager =
  sendMessage nednsSettingsManager localizedDescriptionSelector

-- | localizedDescription
--
-- A string containing a description of the DNS settings.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEDNSSettingsManager nednsSettingsManager, IsNSString value) => nednsSettingsManager -> value -> IO ()
setLocalizedDescription nednsSettingsManager value =
  sendMessage nednsSettingsManager setLocalizedDescriptionSelector (toNSString value)

-- | dnsSettings
--
-- An NEDNSSettings object containing the DNS resolver configuration to apply to the system.
--
-- ObjC selector: @- dnsSettings@
dnsSettings :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO (Id NEDNSSettings)
dnsSettings nednsSettingsManager =
  sendMessage nednsSettingsManager dnsSettingsSelector

-- | dnsSettings
--
-- An NEDNSSettings object containing the DNS resolver configuration to apply to the system.
--
-- ObjC selector: @- setDnsSettings:@
setDnsSettings :: (IsNEDNSSettingsManager nednsSettingsManager, IsNEDNSSettings value) => nednsSettingsManager -> value -> IO ()
setDnsSettings nednsSettingsManager value =
  sendMessage nednsSettingsManager setDnsSettingsSelector (toNEDNSSettings value)

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated DNS settings will always apply. If non-nil, the array describes the networks on which the DNS configuration should take effect or not.
--
-- ObjC selector: @- onDemandRules@
onDemandRules :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO (Id NSArray)
onDemandRules nednsSettingsManager =
  sendMessage nednsSettingsManager onDemandRulesSelector

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated DNS settings will always apply. If non-nil, the array describes the networks on which the DNS configuration should take effect or not.
--
-- ObjC selector: @- setOnDemandRules:@
setOnDemandRules :: (IsNEDNSSettingsManager nednsSettingsManager, IsNSArray value) => nednsSettingsManager -> value -> IO ()
setOnDemandRules nednsSettingsManager value =
  sendMessage nednsSettingsManager setOnDemandRulesSelector (toNSArray value)

-- | enabled
--
-- Checks the enabled status of the DNS settings. DNS settings must be enabled by the user in Settings or System Preferences.
--
-- ObjC selector: @- enabled@
enabled :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO Bool
enabled nednsSettingsManager =
  sendMessage nednsSettingsManager enabledSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id NEDNSSettingsManager)
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector '[Ptr ()] ()
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector '[] (Id NSString)
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector '[Id NSString] ()
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @dnsSettings@
dnsSettingsSelector :: Selector '[] (Id NEDNSSettings)
dnsSettingsSelector = mkSelector "dnsSettings"

-- | @Selector@ for @setDnsSettings:@
setDnsSettingsSelector :: Selector '[Id NEDNSSettings] ()
setDnsSettingsSelector = mkSelector "setDnsSettings:"

-- | @Selector@ for @onDemandRules@
onDemandRulesSelector :: Selector '[] (Id NSArray)
onDemandRulesSelector = mkSelector "onDemandRules"

-- | @Selector@ for @setOnDemandRules:@
setOnDemandRulesSelector :: Selector '[Id NSArray] ()
setOnDemandRulesSelector = mkSelector "setOnDemandRules:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

