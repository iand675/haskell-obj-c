{-# LANGUAGE PatternSynonyms #-}
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
  , forPerAppVPNSelector
  , copyAppRulesSelector
  , routingMethodSelector
  , safariDomainsSelector
  , setSafariDomainsSelector
  , mailDomainsSelector
  , setMailDomainsSelector
  , calendarDomainsSelector
  , setCalendarDomainsSelector
  , contactsDomainsSelector
  , setContactsDomainsSelector
  , appRulesSelector
  , setAppRulesSelector
  , excludedDomainsSelector
  , setExcludedDomainsSelector
  , associatedDomainsSelector
  , setAssociatedDomainsSelector

  -- * Enum types
  , NETunnelProviderRoutingMethod(NETunnelProviderRoutingMethod)
  , pattern NETunnelProviderRoutingMethodDestinationIP
  , pattern NETunnelProviderRoutingMethodSourceApplication
  , pattern NETunnelProviderRoutingMethodNetworkRule

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
    sendClassMsg cls' (mkSelector "forPerAppVPN") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | copyAppRules
--
-- This function returns an array of NEAppRule objects.
--
-- ObjC selector: @- copyAppRules@
copyAppRules :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
copyAppRules neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "copyAppRules") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | routingMethod
--
-- The method by which network traffic is routed to the tunnel. The default is NETunnelProviderRoutingMethodDestinationIP.
--
-- ObjC selector: @- routingMethod@
routingMethod :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO NETunnelProviderRoutingMethod
routingMethod neTunnelProviderManager  =
    fmap (coerce :: CLong -> NETunnelProviderRoutingMethod) $ sendMsg neTunnelProviderManager (mkSelector "routingMethod") retCLong []

-- | safariDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled and the user navigates in Safari to a web site within one of these domains, 	the web site network traffic is routed through the per-app VPN.
--
-- ObjC selector: @- safariDomains@
safariDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
safariDomains neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "safariDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | safariDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled and the user navigates in Safari to a web site within one of these domains, 	the web site network traffic is routed through the per-app VPN.
--
-- ObjC selector: @- setSafariDomains:@
setSafariDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setSafariDomains neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setSafariDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | mailDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Mail app to mail servers within 	one of these domains are routed through the per-app VPN.
--
-- ObjC selector: @- mailDomains@
mailDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
mailDomains neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "mailDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | mailDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Mail app to mail servers within 	one of these domains are routed through the per-app VPN.
--
-- ObjC selector: @- setMailDomains:@
setMailDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setMailDomains neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setMailDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | calendarDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Calendar app to calendar servers within one of 	these domains are routed through the per-app VPN.
--
-- ObjC selector: @- calendarDomains@
calendarDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
calendarDomains neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "calendarDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | calendarDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Calendar app to calendar servers within one of 	these domains are routed through the per-app VPN.
--
-- ObjC selector: @- setCalendarDomains:@
setCalendarDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setCalendarDomains neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setCalendarDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | contactsDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Contacts app to contacts servers within one of these 	domains are routed through the per-app VPN.
--
-- ObjC selector: @- contactsDomains@
contactsDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
contactsDomains neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "contactsDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | contactsDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections from the Contacts app to contacts servers within one of these 	domains are routed through the per-app VPN.
--
-- ObjC selector: @- setContactsDomains:@
setContactsDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setContactsDomains neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setContactsDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | appRules
--
-- An array of NEAppRule objects. Only applies to per-app VPN configurations. Network traffic originating from apps matching one of these rules is routed through the per-app VPN.
--
-- ObjC selector: @- appRules@
appRules :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
appRules neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "appRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | appRules
--
-- An array of NEAppRule objects. Only applies to per-app VPN configurations. Network traffic originating from apps matching one of these rules is routed through the per-app VPN.
--
-- ObjC selector: @- setAppRules:@
setAppRules :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setAppRules neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setAppRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections to servers within one of these domains are excluded from the
--
-- per-app VPN.
--
-- ObjC selector: @- excludedDomains@
excludedDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
excludedDomains neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "excludedDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. When the per-app VPN is enabled, connections to servers within one of these domains are excluded from the
--
-- per-app VPN.
--
-- ObjC selector: @- setExcludedDomains:@
setExcludedDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setExcludedDomains neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setExcludedDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | associatedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. HTTP requests to download the Apple App Site Association files for domains in this list are routed through the per-app VPN.
--
-- ObjC selector: @- associatedDomains@
associatedDomains :: IsNETunnelProviderManager neTunnelProviderManager => neTunnelProviderManager -> IO (Id NSArray)
associatedDomains neTunnelProviderManager  =
    sendMsg neTunnelProviderManager (mkSelector "associatedDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | associatedDomains
--
-- An array of domain strings. Only applies to per-app VPN configurations. HTTP requests to download the Apple App Site Association files for domains in this list are routed through the per-app VPN.
--
-- ObjC selector: @- setAssociatedDomains:@
setAssociatedDomains :: (IsNETunnelProviderManager neTunnelProviderManager, IsNSArray value) => neTunnelProviderManager -> value -> IO ()
setAssociatedDomains neTunnelProviderManager  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderManager (mkSelector "setAssociatedDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @forPerAppVPN@
forPerAppVPNSelector :: Selector
forPerAppVPNSelector = mkSelector "forPerAppVPN"

-- | @Selector@ for @copyAppRules@
copyAppRulesSelector :: Selector
copyAppRulesSelector = mkSelector "copyAppRules"

-- | @Selector@ for @routingMethod@
routingMethodSelector :: Selector
routingMethodSelector = mkSelector "routingMethod"

-- | @Selector@ for @safariDomains@
safariDomainsSelector :: Selector
safariDomainsSelector = mkSelector "safariDomains"

-- | @Selector@ for @setSafariDomains:@
setSafariDomainsSelector :: Selector
setSafariDomainsSelector = mkSelector "setSafariDomains:"

-- | @Selector@ for @mailDomains@
mailDomainsSelector :: Selector
mailDomainsSelector = mkSelector "mailDomains"

-- | @Selector@ for @setMailDomains:@
setMailDomainsSelector :: Selector
setMailDomainsSelector = mkSelector "setMailDomains:"

-- | @Selector@ for @calendarDomains@
calendarDomainsSelector :: Selector
calendarDomainsSelector = mkSelector "calendarDomains"

-- | @Selector@ for @setCalendarDomains:@
setCalendarDomainsSelector :: Selector
setCalendarDomainsSelector = mkSelector "setCalendarDomains:"

-- | @Selector@ for @contactsDomains@
contactsDomainsSelector :: Selector
contactsDomainsSelector = mkSelector "contactsDomains"

-- | @Selector@ for @setContactsDomains:@
setContactsDomainsSelector :: Selector
setContactsDomainsSelector = mkSelector "setContactsDomains:"

-- | @Selector@ for @appRules@
appRulesSelector :: Selector
appRulesSelector = mkSelector "appRules"

-- | @Selector@ for @setAppRules:@
setAppRulesSelector :: Selector
setAppRulesSelector = mkSelector "setAppRules:"

-- | @Selector@ for @excludedDomains@
excludedDomainsSelector :: Selector
excludedDomainsSelector = mkSelector "excludedDomains"

-- | @Selector@ for @setExcludedDomains:@
setExcludedDomainsSelector :: Selector
setExcludedDomainsSelector = mkSelector "setExcludedDomains:"

-- | @Selector@ for @associatedDomains@
associatedDomainsSelector :: Selector
associatedDomainsSelector = mkSelector "associatedDomains"

-- | @Selector@ for @setAssociatedDomains:@
setAssociatedDomainsSelector :: Selector
setAssociatedDomainsSelector = mkSelector "setAssociatedDomains:"

