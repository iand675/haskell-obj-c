{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelProviderManager
--
-- The NETunnelProviderManager class declares the programmatic interface for an object that is used to configure and control network tunnels provided by NETunnelProviders.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETunnelProviderManager@.
module ObjC.NetworkExtension.NETunnelProviderManager
  ( NETunnelProviderManager
  , IsNETunnelProviderManager(..)
  , forPerAppVPN
  , copyAppRules
  , routingMethod
  , safariDomains
  , setSafariDomains
  , mailDomains
  , setMailDomains
  , calendarDomains
  , setCalendarDomains
  , contactsDomains
  , setContactsDomains
  , appRules
  , setAppRules
  , excludedDomains
  , setExcludedDomains
  , associatedDomains
  , setAssociatedDomains
  , appRulesSelector
  , associatedDomainsSelector
  , calendarDomainsSelector
  , contactsDomainsSelector
  , copyAppRulesSelector
  , excludedDomainsSelector
  , forPerAppVPNSelector
  , mailDomainsSelector
  , routingMethodSelector
  , safariDomainsSelector
  , setAppRulesSelector
  , setAssociatedDomainsSelector
  , setCalendarDomainsSelector
  , setContactsDomainsSelector
  , setExcludedDomainsSelector
  , setMailDomainsSelector
  , setSafariDomainsSelector

  -- * Enum types
  , NETunnelProviderRoutingMethod(NETunnelProviderRoutingMethod)
  , pattern NETunnelProviderRoutingMethodDestinationIP
  , pattern NETunnelProviderRoutingMethodSourceApplication
  , pattern NETunnelProviderRoutingMethodNetworkRule

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.NetworkExtension.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | forPerAppVPN
--
-- Create a NETunnelProviderManager instance that is used to manage a per-app VPN configuration.
--
-- ObjC selector: @+ forPerAppVPN@
forPerAppVPN :: IO (Id NETunnelProviderManager)
forPerAppVPN  =
  do
    cls' <- getRequiredClass "NETunnelProviderManager"
    sendClassMessage cls' forPerAppVPNSelector

-- | copyAppRules
--
-- This function returns an array of NEAppRule objects.
--
-- ObjC selector: @- copyAppRules@
copyAppRules :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
copyAppRules neTunnelProviderManager =
  sendOwnedMessage neTunnelProviderManager copyAppRulesSelector

-- | routingMethod
--
-- The method by which network traffic is routed to the tunnel. The default is NETunnelProviderRoutingMethodDestinationIP.
--
-- ObjC selector: @- routingMethod@
routingMethod :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO NETunnelProviderRoutingMethod
routingMethod neTunnelProviderManager =
  sendMessage neTunnelProviderManager routingMethodSelector

-- | safariDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled and the user navigates in Safari to a web site within one of these domains, 	the web site network traffic is routed through the per-app VPN.
--
-- ObjC selector: @- safariDomains@
safariDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
safariDomains neTunnelProviderManager =
  sendMessage neTunnelProviderManager safariDomainsSelector

-- | safariDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled and the user navigates in Safari to a web site within one of these domains, 	the web site network traffic is routed through the per-app VPN.
--
-- ObjC selector: @- setSafariDomains:@
setSafariDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setSafariDomains neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setSafariDomainsSelector (toNSArray value)

-- | mailDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Mail app to mail servers within 	one of these domains are routed through the per-app VPN.
--
-- ObjC selector: @- mailDomains@
mailDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
mailDomains neTunnelProviderManager =
  sendMessage neTunnelProviderManager mailDomainsSelector

-- | mailDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Mail app to mail servers within 	one of these domains are routed through the per-app VPN.
--
-- ObjC selector: @- setMailDomains:@
setMailDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setMailDomains neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setMailDomainsSelector (toNSArray value)

-- | calendarDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Calendar app to calendar servers within one of 	these domains are routed through the per-app VPN.
--
-- ObjC selector: @- calendarDomains@
calendarDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
calendarDomains neTunnelProviderManager =
  sendMessage neTunnelProviderManager calendarDomainsSelector

-- | calendarDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Calendar app to calendar servers within one of 	these domains are routed through the per-app VPN.
--
-- ObjC selector: @- setCalendarDomains:@
setCalendarDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setCalendarDomains neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setCalendarDomainsSelector (toNSArray value)

-- | contactsDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Contacts app to contacts servers within one of these 	domains are routed through the per-app VPN.
--
-- ObjC selector: @- contactsDomains@
contactsDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
contactsDomains neTunnelProviderManager =
  sendMessage neTunnelProviderManager contactsDomainsSelector

-- | contactsDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Contacts app to contacts servers within one of these 	domains are routed through the per-app VPN.
--
-- ObjC selector: @- setContactsDomains:@
setContactsDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setContactsDomains neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setContactsDomainsSelector (toNSArray value)

-- | appRules
--
-- An array of NEAppRule objects. Only applies to per-app VPN configurations. Network traffic originating from apps matching one of these rules is routed through the per-app VPN.
--
-- ObjC selector: @- appRules@
appRules :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
appRules neTunnelProviderManager =
  sendMessage neTunnelProviderManager appRulesSelector

-- | appRules
--
-- An array of NEAppRule objects. Only applies to per-app VPN configurations. Network traffic originating from apps matching one of these rules is routed through the per-app VPN.
--
-- ObjC selector: @- setAppRules:@
setAppRules :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setAppRules neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setAppRulesSelector (toNSArray value)

-- | excludedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections to servers within one of these domains are excluded from the
--
-- per-app VPN.
--
-- ObjC selector: @- excludedDomains@
excludedDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
excludedDomains neTunnelProviderManager =
  sendMessage neTunnelProviderManager excludedDomainsSelector

-- | excludedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections to servers within one of these domains are excluded from the
--
-- per-app VPN.
--
-- ObjC selector: @- setExcludedDomains:@
setExcludedDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setExcludedDomains neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setExcludedDomainsSelector (toNSArray value)

-- | associatedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. HTTP requests to download the Apple App Site Association files for domains in this list are routed through the per-app VPN.
--
-- ObjC selector: @- associatedDomains@
associatedDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
associatedDomains neTunnelProviderManager =
  sendMessage neTunnelProviderManager associatedDomainsSelector

-- | associatedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. HTTP requests to download the Apple App Site Association files for domains in this list are routed through the per-app VPN.
--
-- ObjC selector: @- setAssociatedDomains:@
setAssociatedDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setAssociatedDomains neTunnelProviderManager value =
  sendMessage neTunnelProviderManager setAssociatedDomainsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forPerAppVPN@
forPerAppVPNSelector :: Selector '[] (Id NETunnelProviderManager)
forPerAppVPNSelector = mkSelector "forPerAppVPN"

-- | @Selector@ for @copyAppRules@
copyAppRulesSelector :: Selector '[] (Id NSArray)
copyAppRulesSelector = mkSelector "copyAppRules"

-- | @Selector@ for @routingMethod@
routingMethodSelector :: Selector '[] NETunnelProviderRoutingMethod
routingMethodSelector = mkSelector "routingMethod"

-- | @Selector@ for @safariDomains@
safariDomainsSelector :: Selector '[] (Id NSArray)
safariDomainsSelector = mkSelector "safariDomains"

-- | @Selector@ for @setSafariDomains:@
setSafariDomainsSelector :: Selector '[Id NSArray] ()
setSafariDomainsSelector = mkSelector "setSafariDomains:"

-- | @Selector@ for @mailDomains@
mailDomainsSelector :: Selector '[] (Id NSArray)
mailDomainsSelector = mkSelector "mailDomains"

-- | @Selector@ for @setMailDomains:@
setMailDomainsSelector :: Selector '[Id NSArray] ()
setMailDomainsSelector = mkSelector "setMailDomains:"

-- | @Selector@ for @calendarDomains@
calendarDomainsSelector :: Selector '[] (Id NSArray)
calendarDomainsSelector = mkSelector "calendarDomains"

-- | @Selector@ for @setCalendarDomains:@
setCalendarDomainsSelector :: Selector '[Id NSArray] ()
setCalendarDomainsSelector = mkSelector "setCalendarDomains:"

-- | @Selector@ for @contactsDomains@
contactsDomainsSelector :: Selector '[] (Id NSArray)
contactsDomainsSelector = mkSelector "contactsDomains"

-- | @Selector@ for @setContactsDomains:@
setContactsDomainsSelector :: Selector '[Id NSArray] ()
setContactsDomainsSelector = mkSelector "setContactsDomains:"

-- | @Selector@ for @appRules@
appRulesSelector :: Selector '[] (Id NSArray)
appRulesSelector = mkSelector "appRules"

-- | @Selector@ for @setAppRules:@
setAppRulesSelector :: Selector '[Id NSArray] ()
setAppRulesSelector = mkSelector "setAppRules:"

-- | @Selector@ for @excludedDomains@
excludedDomainsSelector :: Selector '[] (Id NSArray)
excludedDomainsSelector = mkSelector "excludedDomains"

-- | @Selector@ for @setExcludedDomains:@
setExcludedDomainsSelector :: Selector '[Id NSArray] ()
setExcludedDomainsSelector = mkSelector "setExcludedDomains:"

-- | @Selector@ for @associatedDomains@
associatedDomainsSelector :: Selector '[] (Id NSArray)
associatedDomainsSelector = mkSelector "associatedDomains"

-- | @Selector@ for @setAssociatedDomains:@
setAssociatedDomainsSelector :: Selector '[Id NSArray] ()
setAssociatedDomainsSelector = mkSelector "setAssociatedDomains:"

