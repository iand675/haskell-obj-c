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
  , sharedManagerSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , dnsSettingsSelector
  , setDnsSettingsSelector
  , onDemandRulesSelector
  , setOnDemandRulesSelector
  , enabledSelector


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

-- | sharedManager
--
-- Returns: The singleton NEDNSSettingsManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NEDNSSettingsManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NEDNSSettingsManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current DNS settings configuration from the caller's DNS settings preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler nednsSettingsManager  completionHandler =
  sendMsg nednsSettingsManager (mkSelector "loadFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the DNS settings configuration from the caller's DNS settings preferences. If the DNS settings are enabled, the DNS settings becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler nednsSettingsManager  completionHandler =
  sendMsg nednsSettingsManager (mkSelector "removeFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the DNS settingsconfiguration in the caller's DNS settings preferences. If the DNS settings are enabled, they will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler nednsSettingsManager  completionHandler =
  sendMsg nednsSettingsManager (mkSelector "saveToPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | localizedDescription
--
-- A string containing a description of the DNS settings.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO (Id NSString)
localizedDescription nednsSettingsManager  =
  sendMsg nednsSettingsManager (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedDescription
--
-- A string containing a description of the DNS settings.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNEDNSSettingsManager nednsSettingsManager, IsNSString value) => nednsSettingsManager -> value -> IO ()
setLocalizedDescription nednsSettingsManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsSettingsManager (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | dnsSettings
--
-- An NEDNSSettings object containing the DNS resolver configuration to apply to the system.
--
-- ObjC selector: @- dnsSettings@
dnsSettings :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO (Id NEDNSSettings)
dnsSettings nednsSettingsManager  =
  sendMsg nednsSettingsManager (mkSelector "dnsSettings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dnsSettings
--
-- An NEDNSSettings object containing the DNS resolver configuration to apply to the system.
--
-- ObjC selector: @- setDnsSettings:@
setDnsSettings :: (IsNEDNSSettingsManager nednsSettingsManager, IsNEDNSSettings value) => nednsSettingsManager -> value -> IO ()
setDnsSettings nednsSettingsManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsSettingsManager (mkSelector "setDnsSettings:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated DNS settings will always apply. If non-nil, the array describes the networks on which the DNS configuration should take effect or not.
--
-- ObjC selector: @- onDemandRules@
onDemandRules :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO (Id NSArray)
onDemandRules nednsSettingsManager  =
  sendMsg nednsSettingsManager (mkSelector "onDemandRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated DNS settings will always apply. If non-nil, the array describes the networks on which the DNS configuration should take effect or not.
--
-- ObjC selector: @- setOnDemandRules:@
setOnDemandRules :: (IsNEDNSSettingsManager nednsSettingsManager, IsNSArray value) => nednsSettingsManager -> value -> IO ()
setOnDemandRules nednsSettingsManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsSettingsManager (mkSelector "setOnDemandRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | enabled
--
-- Checks the enabled status of the DNS settings. DNS settings must be enabled by the user in Settings or System Preferences.
--
-- ObjC selector: @- enabled@
enabled :: IsNEDNSSettingsManager nednsSettingsManager => nednsSettingsManager -> IO Bool
enabled nednsSettingsManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nednsSettingsManager (mkSelector "enabled") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector
sharedManagerSelector = mkSelector "sharedManager"

-- | @Selector@ for @loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandlerSelector :: Selector
loadFromPreferencesWithCompletionHandlerSelector = mkSelector "loadFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandlerSelector :: Selector
removeFromPreferencesWithCompletionHandlerSelector = mkSelector "removeFromPreferencesWithCompletionHandler:"

-- | @Selector@ for @saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandlerSelector :: Selector
saveToPreferencesWithCompletionHandlerSelector = mkSelector "saveToPreferencesWithCompletionHandler:"

-- | @Selector@ for @localizedDescription@
localizedDescriptionSelector :: Selector
localizedDescriptionSelector = mkSelector "localizedDescription"

-- | @Selector@ for @setLocalizedDescription:@
setLocalizedDescriptionSelector :: Selector
setLocalizedDescriptionSelector = mkSelector "setLocalizedDescription:"

-- | @Selector@ for @dnsSettings@
dnsSettingsSelector :: Selector
dnsSettingsSelector = mkSelector "dnsSettings"

-- | @Selector@ for @setDnsSettings:@
setDnsSettingsSelector :: Selector
setDnsSettingsSelector = mkSelector "setDnsSettings:"

-- | @Selector@ for @onDemandRules@
onDemandRulesSelector :: Selector
onDemandRulesSelector = mkSelector "onDemandRules"

-- | @Selector@ for @setOnDemandRules:@
setOnDemandRulesSelector :: Selector
setOnDemandRulesSelector = mkSelector "setOnDemandRules:"

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

