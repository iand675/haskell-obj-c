{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NERelayManager
--
-- The NERelayManager class declares the programmatic interface for an object that manages relay configurations.
--
-- NERelayManager declares methods and properties for configuring and controlling relay settings on the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NERelayManager@.
module ObjC.NetworkExtension.NERelayManager
  ( NERelayManager
  , IsNERelayManager(..)
  , sharedManager
  , loadFromPreferencesWithCompletionHandler
  , removeFromPreferencesWithCompletionHandler
  , saveToPreferencesWithCompletionHandler
  , localizedDescription
  , setLocalizedDescription
  , enabled
  , setEnabled
  , uiToggleEnabled
  , setUIToggleEnabled
  , allowDNSFailover
  , setAllowDNSFailover
  , relays
  , setRelays
  , matchDomains
  , setMatchDomains
  , matchFQDNs
  , setMatchFQDNs
  , excludedDomains
  , setExcludedDomains
  , excludedFQDNs
  , setExcludedFQDNs
  , onDemandRules
  , setOnDemandRules
  , allowDNSFailoverSelector
  , enabledSelector
  , excludedDomainsSelector
  , excludedFQDNsSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , matchDomainsSelector
  , matchFQDNsSelector
  , onDemandRulesSelector
  , relaysSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , setAllowDNSFailoverSelector
  , setEnabledSelector
  , setExcludedDomainsSelector
  , setExcludedFQDNsSelector
  , setLocalizedDescriptionSelector
  , setMatchDomainsSelector
  , setMatchFQDNsSelector
  , setOnDemandRulesSelector
  , setRelaysSelector
  , setUIToggleEnabledSelector
  , sharedManagerSelector
  , uiToggleEnabledSelector


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
-- Returns: The singleton NERelayManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NERelayManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NERelayManager"
    sendClassMessage cls' sharedManagerSelector

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current relay configuration from the caller's relay preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNERelayManager neRelayManager => neRelayManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler neRelayManager completionHandler =
  sendMessage neRelayManager loadFromPreferencesWithCompletionHandlerSelector completionHandler

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the relay configuration from the caller's relay preferences. If the relay is enabled, the relay becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNERelayManager neRelayManager => neRelayManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler neRelayManager completionHandler =
  sendMessage neRelayManager removeFromPreferencesWithCompletionHandlerSelector completionHandler

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the relay configuration in the caller's relay preferences. If the relay are enabled, they will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNERelayManager neRelayManager => neRelayManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler neRelayManager completionHandler =
  sendMessage neRelayManager saveToPreferencesWithCompletionHandlerSelector completionHandler

-- | localizedDescription
--
-- A string containing a description of the relay.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSString)
localizedDescription neRelayManager =
  sendMessage neRelayManager localizedDescriptionSelector

-- | localizedDescription
--
-- A string containing a description of the relay.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNERelayManager neRelayManager, IsNSString value) => neRelayManager -> value -> IO ()
setLocalizedDescription neRelayManager value =
  sendMessage neRelayManager setLocalizedDescriptionSelector (toNSString value)

-- | enabled
--
-- Toggles the enabled status of the relay.
--
-- ObjC selector: @- enabled@
enabled :: IsNERelayManager neRelayManager => neRelayManager -> IO Bool
enabled neRelayManager =
  sendMessage neRelayManager enabledSelector

-- | enabled
--
-- Toggles the enabled status of the relay.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNERelayManager neRelayManager => neRelayManager -> Bool -> IO ()
setEnabled neRelayManager value =
  sendMessage neRelayManager setEnabledSelector value

-- | uiToggleEnabled
--
-- Determines if the user will have the ability to enable and disable the relay
--
-- ObjC selector: @- UIToggleEnabled@
uiToggleEnabled :: IsNERelayManager neRelayManager => neRelayManager -> IO Bool
uiToggleEnabled neRelayManager =
  sendMessage neRelayManager uiToggleEnabledSelector

-- | uiToggleEnabled
--
-- Determines if the user will have the ability to enable and disable the relay
--
-- ObjC selector: @- setUIToggleEnabled:@
setUIToggleEnabled :: IsNERelayManager neRelayManager => neRelayManager -> Bool -> IO ()
setUIToggleEnabled neRelayManager value =
  sendMessage neRelayManager setUIToggleEnabledSelector value

-- | allowDNSFailover
--
-- Determines if DNS queries that fail over relay can fallback to default DNS
--
-- ObjC selector: @- allowDNSFailover@
allowDNSFailover :: IsNERelayManager neRelayManager => neRelayManager -> IO Bool
allowDNSFailover neRelayManager =
  sendMessage neRelayManager allowDNSFailoverSelector

-- | allowDNSFailover
--
-- Determines if DNS queries that fail over relay can fallback to default DNS
--
-- ObjC selector: @- setAllowDNSFailover:@
setAllowDNSFailover :: IsNERelayManager neRelayManager => neRelayManager -> Bool -> IO ()
setAllowDNSFailover neRelayManager value =
  sendMessage neRelayManager setAllowDNSFailoverSelector value

-- | relays
--
-- An array of relay configurations describing one or more relay hops.
--
-- ObjC selector: @- relays@
relays :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
relays neRelayManager =
  sendMessage neRelayManager relaysSelector

-- | relays
--
-- An array of relay configurations describing one or more relay hops.
--
-- ObjC selector: @- setRelays:@
setRelays :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setRelays neRelayManager value =
  sendMessage neRelayManager setRelaysSelector (toNSArray value)

-- | matchDomains
--
-- An array of strings containing domain names. If this property is non-nil, the relay will be used to access hosts within the specified domains. If this and the match FQDNs property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
matchDomains neRelayManager =
  sendMessage neRelayManager matchDomainsSelector

-- | matchDomains
--
-- An array of strings containing domain names. If this property is non-nil, the relay will be used to access hosts within the specified domains. If this and the match FQDNs property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setMatchDomains neRelayManager value =
  sendMessage neRelayManager setMatchDomainsSelector (toNSArray value)

-- | matchFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If this property is non-nil, the relay will be used to access the specified hosts.  If this and the matchDomains property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- matchFQDNs@
matchFQDNs :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
matchFQDNs neRelayManager =
  sendMessage neRelayManager matchFQDNsSelector

-- | matchFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If this property is non-nil, the relay will be used to access the specified hosts.  If this and the matchDomains property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- setMatchFQDNs:@
setMatchFQDNs :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setMatchFQDNs neRelayManager value =
  sendMessage neRelayManager setMatchFQDNsSelector (toNSArray value)

-- | excludedDomains
--
-- An array of strings containing domain names. If the destination host name of a connection shares a suffix with one of these strings then the relay will not be used.
--
-- ObjC selector: @- excludedDomains@
excludedDomains :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
excludedDomains neRelayManager =
  sendMessage neRelayManager excludedDomainsSelector

-- | excludedDomains
--
-- An array of strings containing domain names. If the destination host name of a connection shares a suffix with one of these strings then the relay will not be used.
--
-- ObjC selector: @- setExcludedDomains:@
setExcludedDomains :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setExcludedDomains neRelayManager value =
  sendMessage neRelayManager setExcludedDomainsSelector (toNSArray value)

-- | excludedFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If the destination host matches one of these strings then the relay will not be used.  An excluded FQDN takes priority over the matchDomain property.  This means the relay will not be used if the hostname matches an FQDN in this array even if the matchDomains contains a domain that would have been considered a match.
--
-- ObjC selector: @- excludedFQDNs@
excludedFQDNs :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
excludedFQDNs neRelayManager =
  sendMessage neRelayManager excludedFQDNsSelector

-- | excludedFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If the destination host matches one of these strings then the relay will not be used.  An excluded FQDN takes priority over the matchDomain property.  This means the relay will not be used if the hostname matches an FQDN in this array even if the matchDomains contains a domain that would have been considered a match.
--
-- ObjC selector: @- setExcludedFQDNs:@
setExcludedFQDNs :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setExcludedFQDNs neRelayManager value =
  sendMessage neRelayManager setExcludedFQDNsSelector (toNSArray value)

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated relay will always apply. If non-nil, the array describes the networks on which the relay should be used or not.
--
-- ObjC selector: @- onDemandRules@
onDemandRules :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
onDemandRules neRelayManager =
  sendMessage neRelayManager onDemandRulesSelector

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated relay will always apply. If non-nil, the array describes the networks on which the relay should be used or not.
--
-- ObjC selector: @- setOnDemandRules:@
setOnDemandRules :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setOnDemandRules neRelayManager value =
  sendMessage neRelayManager setOnDemandRulesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @sharedManager@
sharedManagerSelector :: Selector '[] (Id NERelayManager)
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

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @UIToggleEnabled@
uiToggleEnabledSelector :: Selector '[] Bool
uiToggleEnabledSelector = mkSelector "UIToggleEnabled"

-- | @Selector@ for @setUIToggleEnabled:@
setUIToggleEnabledSelector :: Selector '[Bool] ()
setUIToggleEnabledSelector = mkSelector "setUIToggleEnabled:"

-- | @Selector@ for @allowDNSFailover@
allowDNSFailoverSelector :: Selector '[] Bool
allowDNSFailoverSelector = mkSelector "allowDNSFailover"

-- | @Selector@ for @setAllowDNSFailover:@
setAllowDNSFailoverSelector :: Selector '[Bool] ()
setAllowDNSFailoverSelector = mkSelector "setAllowDNSFailover:"

-- | @Selector@ for @relays@
relaysSelector :: Selector '[] (Id NSArray)
relaysSelector = mkSelector "relays"

-- | @Selector@ for @setRelays:@
setRelaysSelector :: Selector '[Id NSArray] ()
setRelaysSelector = mkSelector "setRelays:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector '[] (Id NSArray)
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector '[Id NSArray] ()
setMatchDomainsSelector = mkSelector "setMatchDomains:"

-- | @Selector@ for @matchFQDNs@
matchFQDNsSelector :: Selector '[] (Id NSArray)
matchFQDNsSelector = mkSelector "matchFQDNs"

-- | @Selector@ for @setMatchFQDNs:@
setMatchFQDNsSelector :: Selector '[Id NSArray] ()
setMatchFQDNsSelector = mkSelector "setMatchFQDNs:"

-- | @Selector@ for @excludedDomains@
excludedDomainsSelector :: Selector '[] (Id NSArray)
excludedDomainsSelector = mkSelector "excludedDomains"

-- | @Selector@ for @setExcludedDomains:@
setExcludedDomainsSelector :: Selector '[Id NSArray] ()
setExcludedDomainsSelector = mkSelector "setExcludedDomains:"

-- | @Selector@ for @excludedFQDNs@
excludedFQDNsSelector :: Selector '[] (Id NSArray)
excludedFQDNsSelector = mkSelector "excludedFQDNs"

-- | @Selector@ for @setExcludedFQDNs:@
setExcludedFQDNsSelector :: Selector '[Id NSArray] ()
setExcludedFQDNsSelector = mkSelector "setExcludedFQDNs:"

-- | @Selector@ for @onDemandRules@
onDemandRulesSelector :: Selector '[] (Id NSArray)
onDemandRulesSelector = mkSelector "onDemandRules"

-- | @Selector@ for @setOnDemandRules:@
setOnDemandRulesSelector :: Selector '[Id NSArray] ()
setOnDemandRulesSelector = mkSelector "setOnDemandRules:"

