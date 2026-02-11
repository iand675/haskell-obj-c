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
  , sharedManagerSelector
  , loadFromPreferencesWithCompletionHandlerSelector
  , removeFromPreferencesWithCompletionHandlerSelector
  , saveToPreferencesWithCompletionHandlerSelector
  , localizedDescriptionSelector
  , setLocalizedDescriptionSelector
  , enabledSelector
  , setEnabledSelector
  , uiToggleEnabledSelector
  , setUIToggleEnabledSelector
  , allowDNSFailoverSelector
  , setAllowDNSFailoverSelector
  , relaysSelector
  , setRelaysSelector
  , matchDomainsSelector
  , setMatchDomainsSelector
  , matchFQDNsSelector
  , setMatchFQDNsSelector
  , excludedDomainsSelector
  , setExcludedDomainsSelector
  , excludedFQDNsSelector
  , setExcludedFQDNsSelector
  , onDemandRulesSelector
  , setOnDemandRulesSelector


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
-- Returns: The singleton NERelayManager object for the calling process.
--
-- ObjC selector: @+ sharedManager@
sharedManager :: IO (Id NERelayManager)
sharedManager  =
  do
    cls' <- getRequiredClass "NERelayManager"
    sendClassMsg cls' (mkSelector "sharedManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | loadFromPreferencesWithCompletionHandler:
--
-- This function loads the current relay configuration from the caller's relay preferences.
--
-- @completionHandler@ — A block that will be called when the load operation is completed. The NSError passed to this block will be nil if the load operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- loadFromPreferencesWithCompletionHandler:@
loadFromPreferencesWithCompletionHandler :: IsNERelayManager neRelayManager => neRelayManager -> Ptr () -> IO ()
loadFromPreferencesWithCompletionHandler neRelayManager  completionHandler =
  sendMsg neRelayManager (mkSelector "loadFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | removeFromPreferencesWithCompletionHandler:
--
-- This function removes the relay configuration from the caller's relay preferences. If the relay is enabled, the relay becomes disabled.
--
-- @completionHandler@ — A block that will be called when the remove operation is completed. The NSError passed to this block will be nil if the remove operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- removeFromPreferencesWithCompletionHandler:@
removeFromPreferencesWithCompletionHandler :: IsNERelayManager neRelayManager => neRelayManager -> Ptr () -> IO ()
removeFromPreferencesWithCompletionHandler neRelayManager  completionHandler =
  sendMsg neRelayManager (mkSelector "removeFromPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | saveToPreferencesWithCompletionHandler:
--
-- This function saves the relay configuration in the caller's relay preferences. If the relay are enabled, they will become active.
--
-- @completionHandler@ — A block that will be called when the save operation is completed. The NSError passed to this block will be nil if the save operation succeeded, non-nil otherwise.
--
-- ObjC selector: @- saveToPreferencesWithCompletionHandler:@
saveToPreferencesWithCompletionHandler :: IsNERelayManager neRelayManager => neRelayManager -> Ptr () -> IO ()
saveToPreferencesWithCompletionHandler neRelayManager  completionHandler =
  sendMsg neRelayManager (mkSelector "saveToPreferencesWithCompletionHandler:") retVoid [argPtr (castPtr completionHandler :: Ptr ())]

-- | localizedDescription
--
-- A string containing a description of the relay.
--
-- ObjC selector: @- localizedDescription@
localizedDescription :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSString)
localizedDescription neRelayManager  =
  sendMsg neRelayManager (mkSelector "localizedDescription") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | localizedDescription
--
-- A string containing a description of the relay.
--
-- ObjC selector: @- setLocalizedDescription:@
setLocalizedDescription :: (IsNERelayManager neRelayManager, IsNSString value) => neRelayManager -> value -> IO ()
setLocalizedDescription neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setLocalizedDescription:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | enabled
--
-- Toggles the enabled status of the relay.
--
-- ObjC selector: @- enabled@
enabled :: IsNERelayManager neRelayManager => neRelayManager -> IO Bool
enabled neRelayManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neRelayManager (mkSelector "enabled") retCULong []

-- | enabled
--
-- Toggles the enabled status of the relay.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsNERelayManager neRelayManager => neRelayManager -> Bool -> IO ()
setEnabled neRelayManager  value =
  sendMsg neRelayManager (mkSelector "setEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | uiToggleEnabled
--
-- Determines if the user will have the ability to enable and disable the relay
--
-- ObjC selector: @- UIToggleEnabled@
uiToggleEnabled :: IsNERelayManager neRelayManager => neRelayManager -> IO Bool
uiToggleEnabled neRelayManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neRelayManager (mkSelector "UIToggleEnabled") retCULong []

-- | uiToggleEnabled
--
-- Determines if the user will have the ability to enable and disable the relay
--
-- ObjC selector: @- setUIToggleEnabled:@
setUIToggleEnabled :: IsNERelayManager neRelayManager => neRelayManager -> Bool -> IO ()
setUIToggleEnabled neRelayManager  value =
  sendMsg neRelayManager (mkSelector "setUIToggleEnabled:") retVoid [argCULong (if value then 1 else 0)]

-- | allowDNSFailover
--
-- Determines if DNS queries that fail over relay can fallback to default DNS
--
-- ObjC selector: @- allowDNSFailover@
allowDNSFailover :: IsNERelayManager neRelayManager => neRelayManager -> IO Bool
allowDNSFailover neRelayManager  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg neRelayManager (mkSelector "allowDNSFailover") retCULong []

-- | allowDNSFailover
--
-- Determines if DNS queries that fail over relay can fallback to default DNS
--
-- ObjC selector: @- setAllowDNSFailover:@
setAllowDNSFailover :: IsNERelayManager neRelayManager => neRelayManager -> Bool -> IO ()
setAllowDNSFailover neRelayManager  value =
  sendMsg neRelayManager (mkSelector "setAllowDNSFailover:") retVoid [argCULong (if value then 1 else 0)]

-- | relays
--
-- An array of relay configurations describing one or more relay hops.
--
-- ObjC selector: @- relays@
relays :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
relays neRelayManager  =
  sendMsg neRelayManager (mkSelector "relays") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | relays
--
-- An array of relay configurations describing one or more relay hops.
--
-- ObjC selector: @- setRelays:@
setRelays :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setRelays neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setRelays:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | matchDomains
--
-- An array of strings containing domain names. If this property is non-nil, the relay will be used to access hosts within the specified domains. If this and the match FQDNs property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
matchDomains neRelayManager  =
  sendMsg neRelayManager (mkSelector "matchDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchDomains
--
-- An array of strings containing domain names. If this property is non-nil, the relay will be used to access hosts within the specified domains. If this and the match FQDNs property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setMatchDomains neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setMatchDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | matchFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If this property is non-nil, the relay will be used to access the specified hosts.  If this and the matchDomains property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- matchFQDNs@
matchFQDNs :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
matchFQDNs neRelayManager  =
  sendMsg neRelayManager (mkSelector "matchFQDNs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If this property is non-nil, the relay will be used to access the specified hosts.  If this and the matchDomains property is nil, the relay will be used for all domains.
--
-- ObjC selector: @- setMatchFQDNs:@
setMatchFQDNs :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setMatchFQDNs neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setMatchFQDNs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedDomains
--
-- An array of strings containing domain names. If the destination host name of a connection shares a suffix with one of these strings then the relay will not be used.
--
-- ObjC selector: @- excludedDomains@
excludedDomains :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
excludedDomains neRelayManager  =
  sendMsg neRelayManager (mkSelector "excludedDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedDomains
--
-- An array of strings containing domain names. If the destination host name of a connection shares a suffix with one of these strings then the relay will not be used.
--
-- ObjC selector: @- setExcludedDomains:@
setExcludedDomains :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setExcludedDomains neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setExcludedDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If the destination host matches one of these strings then the relay will not be used.  An excluded FQDN takes priority over the matchDomain property.  This means the relay will not be used if the hostname matches an FQDN in this array even if the matchDomains contains a domain that would have been considered a match.
--
-- ObjC selector: @- excludedFQDNs@
excludedFQDNs :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
excludedFQDNs neRelayManager  =
  sendMsg neRelayManager (mkSelector "excludedFQDNs") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedFQDNs
--
-- An array of strings containing Fully Qualified Domain Names (FQDNs). If the destination host matches one of these strings then the relay will not be used.  An excluded FQDN takes priority over the matchDomain property.  This means the relay will not be used if the hostname matches an FQDN in this array even if the matchDomains contains a domain that would have been considered a match.
--
-- ObjC selector: @- setExcludedFQDNs:@
setExcludedFQDNs :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setExcludedFQDNs neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setExcludedFQDNs:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated relay will always apply. If non-nil, the array describes the networks on which the relay should be used or not.
--
-- ObjC selector: @- onDemandRules@
onDemandRules :: IsNERelayManager neRelayManager => neRelayManager -> IO (Id NSArray)
onDemandRules neRelayManager  =
  sendMsg neRelayManager (mkSelector "onDemandRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | onDemandRules
--
-- An array of NEOnDemandRule objects. If nil, the associated relay will always apply. If non-nil, the array describes the networks on which the relay should be used or not.
--
-- ObjC selector: @- setOnDemandRules:@
setOnDemandRules :: (IsNERelayManager neRelayManager, IsNSArray value) => neRelayManager -> value -> IO ()
setOnDemandRules neRelayManager  value =
withObjCPtr value $ \raw_value ->
    sendMsg neRelayManager (mkSelector "setOnDemandRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

-- | @Selector@ for @enabled@
enabledSelector :: Selector
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector
setEnabledSelector = mkSelector "setEnabled:"

-- | @Selector@ for @UIToggleEnabled@
uiToggleEnabledSelector :: Selector
uiToggleEnabledSelector = mkSelector "UIToggleEnabled"

-- | @Selector@ for @setUIToggleEnabled:@
setUIToggleEnabledSelector :: Selector
setUIToggleEnabledSelector = mkSelector "setUIToggleEnabled:"

-- | @Selector@ for @allowDNSFailover@
allowDNSFailoverSelector :: Selector
allowDNSFailoverSelector = mkSelector "allowDNSFailover"

-- | @Selector@ for @setAllowDNSFailover:@
setAllowDNSFailoverSelector :: Selector
setAllowDNSFailoverSelector = mkSelector "setAllowDNSFailover:"

-- | @Selector@ for @relays@
relaysSelector :: Selector
relaysSelector = mkSelector "relays"

-- | @Selector@ for @setRelays:@
setRelaysSelector :: Selector
setRelaysSelector = mkSelector "setRelays:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector
setMatchDomainsSelector = mkSelector "setMatchDomains:"

-- | @Selector@ for @matchFQDNs@
matchFQDNsSelector :: Selector
matchFQDNsSelector = mkSelector "matchFQDNs"

-- | @Selector@ for @setMatchFQDNs:@
setMatchFQDNsSelector :: Selector
setMatchFQDNsSelector = mkSelector "setMatchFQDNs:"

-- | @Selector@ for @excludedDomains@
excludedDomainsSelector :: Selector
excludedDomainsSelector = mkSelector "excludedDomains"

-- | @Selector@ for @setExcludedDomains:@
setExcludedDomainsSelector :: Selector
setExcludedDomainsSelector = mkSelector "setExcludedDomains:"

-- | @Selector@ for @excludedFQDNs@
excludedFQDNsSelector :: Selector
excludedFQDNsSelector = mkSelector "excludedFQDNs"

-- | @Selector@ for @setExcludedFQDNs:@
setExcludedFQDNsSelector :: Selector
setExcludedFQDNsSelector = mkSelector "setExcludedFQDNs:"

-- | @Selector@ for @onDemandRules@
onDemandRulesSelector :: Selector
onDemandRulesSelector = mkSelector "onDemandRules"

-- | @Selector@ for @setOnDemandRules:@
setOnDemandRulesSelector :: Selector
setOnDemandRulesSelector = mkSelector "setOnDemandRules:"

