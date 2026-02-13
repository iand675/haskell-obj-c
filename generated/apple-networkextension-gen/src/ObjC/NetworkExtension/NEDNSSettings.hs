{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEDNSSettings
--
-- The NEDNSSettings class declares the programmatic interface for an object that contains DNS settings.
--
-- Generated bindings for @NEDNSSettings@.
module ObjC.NetworkExtension.NEDNSSettings
  ( NEDNSSettings
  , IsNEDNSSettings(..)
  , initWithServers
  , dnsProtocol
  , servers
  , searchDomains
  , setSearchDomains
  , domainName
  , setDomainName
  , matchDomains
  , setMatchDomains
  , matchDomainsNoSearch
  , setMatchDomainsNoSearch
  , allowFailover
  , setAllowFailover
  , allowFailoverSelector
  , dnsProtocolSelector
  , domainNameSelector
  , initWithServersSelector
  , matchDomainsNoSearchSelector
  , matchDomainsSelector
  , searchDomainsSelector
  , serversSelector
  , setAllowFailoverSelector
  , setDomainNameSelector
  , setMatchDomainsNoSearchSelector
  , setMatchDomainsSelector
  , setSearchDomainsSelector

  -- * Enum types
  , NEDNSProtocol(NEDNSProtocol)
  , pattern NEDNSProtocolCleartext
  , pattern NEDNSProtocolTLS
  , pattern NEDNSProtocolHTTPS

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

-- | initWithServers:
--
-- Initialize a newly-allocated NEDNSSettings object.
--
-- @servers@ — An array of DNS server IP address strings.
--
-- ObjC selector: @- initWithServers:@
initWithServers :: (IsNEDNSSettings nednsSettings, IsNSArray servers) => nednsSettings -> servers -> IO (Id NEDNSSettings)
initWithServers nednsSettings servers =
  sendOwnedMessage nednsSettings initWithServersSelector (toNSArray servers)

-- | dnsProtocol
--
-- The DNS protocol used by the settings.
--
-- ObjC selector: @- dnsProtocol@
dnsProtocol :: IsNEDNSSettings nednsSettings => nednsSettings -> IO NEDNSProtocol
dnsProtocol nednsSettings =
  sendMessage nednsSettings dnsProtocolSelector

-- | servers
--
-- An array of DNS server address strings.
--
-- ObjC selector: @- servers@
servers :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSArray)
servers nednsSettings =
  sendMessage nednsSettings serversSelector

-- | searchDomains
--
-- An array of DNS server search domain strings.
--
-- ObjC selector: @- searchDomains@
searchDomains :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSArray)
searchDomains nednsSettings =
  sendMessage nednsSettings searchDomainsSelector

-- | searchDomains
--
-- An array of DNS server search domain strings.
--
-- ObjC selector: @- setSearchDomains:@
setSearchDomains :: (IsNEDNSSettings nednsSettings, IsNSArray value) => nednsSettings -> value -> IO ()
setSearchDomains nednsSettings value =
  sendMessage nednsSettings setSearchDomainsSelector (toNSArray value)

-- | domainName
--
-- A string containing the DNS domain.
--
-- ObjC selector: @- domainName@
domainName :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSString)
domainName nednsSettings =
  sendMessage nednsSettings domainNameSelector

-- | domainName
--
-- A string containing the DNS domain.
--
-- ObjC selector: @- setDomainName:@
setDomainName :: (IsNEDNSSettings nednsSettings, IsNSString value) => nednsSettings -> value -> IO ()
setDomainName nednsSettings value =
  sendMessage nednsSettings setDomainNameSelector (toNSString value)

-- | matchDomains
--
-- An array of strings containing domain strings. If this property is non-nil, the DNS settings will only be used to resolve host names within the specified domains.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSArray)
matchDomains nednsSettings =
  sendMessage nednsSettings matchDomainsSelector

-- | matchDomains
--
-- An array of strings containing domain strings. If this property is non-nil, the DNS settings will only be used to resolve host names within the specified domains.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNEDNSSettings nednsSettings, IsNSArray value) => nednsSettings -> value -> IO ()
setMatchDomains nednsSettings value =
  sendMessage nednsSettings setMatchDomainsSelector (toNSArray value)

-- | matchDomainsNoSearch
--
-- A boolean indicating if the match domains should be appended to the search domain list.  Default is NO (match domains will be appended to the search domain list).
--
-- ObjC selector: @- matchDomainsNoSearch@
matchDomainsNoSearch :: IsNEDNSSettings nednsSettings => nednsSettings -> IO Bool
matchDomainsNoSearch nednsSettings =
  sendMessage nednsSettings matchDomainsNoSearchSelector

-- | matchDomainsNoSearch
--
-- A boolean indicating if the match domains should be appended to the search domain list.  Default is NO (match domains will be appended to the search domain list).
--
-- ObjC selector: @- setMatchDomainsNoSearch:@
setMatchDomainsNoSearch :: IsNEDNSSettings nednsSettings => nednsSettings -> Bool -> IO ()
setMatchDomainsNoSearch nednsSettings value =
  sendMessage nednsSettings setMatchDomainsNoSearchSelector value

-- | allowFailover
--
-- A boolean indicating if failover to the default system resolver is permitted on resolution failure.
--
-- ObjC selector: @- allowFailover@
allowFailover :: IsNEDNSSettings nednsSettings => nednsSettings -> IO Bool
allowFailover nednsSettings =
  sendMessage nednsSettings allowFailoverSelector

-- | allowFailover
--
-- A boolean indicating if failover to the default system resolver is permitted on resolution failure.
--
-- ObjC selector: @- setAllowFailover:@
setAllowFailover :: IsNEDNSSettings nednsSettings => nednsSettings -> Bool -> IO ()
setAllowFailover nednsSettings value =
  sendMessage nednsSettings setAllowFailoverSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServers:@
initWithServersSelector :: Selector '[Id NSArray] (Id NEDNSSettings)
initWithServersSelector = mkSelector "initWithServers:"

-- | @Selector@ for @dnsProtocol@
dnsProtocolSelector :: Selector '[] NEDNSProtocol
dnsProtocolSelector = mkSelector "dnsProtocol"

-- | @Selector@ for @servers@
serversSelector :: Selector '[] (Id NSArray)
serversSelector = mkSelector "servers"

-- | @Selector@ for @searchDomains@
searchDomainsSelector :: Selector '[] (Id NSArray)
searchDomainsSelector = mkSelector "searchDomains"

-- | @Selector@ for @setSearchDomains:@
setSearchDomainsSelector :: Selector '[Id NSArray] ()
setSearchDomainsSelector = mkSelector "setSearchDomains:"

-- | @Selector@ for @domainName@
domainNameSelector :: Selector '[] (Id NSString)
domainNameSelector = mkSelector "domainName"

-- | @Selector@ for @setDomainName:@
setDomainNameSelector :: Selector '[Id NSString] ()
setDomainNameSelector = mkSelector "setDomainName:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector '[] (Id NSArray)
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector '[Id NSArray] ()
setMatchDomainsSelector = mkSelector "setMatchDomains:"

-- | @Selector@ for @matchDomainsNoSearch@
matchDomainsNoSearchSelector :: Selector '[] Bool
matchDomainsNoSearchSelector = mkSelector "matchDomainsNoSearch"

-- | @Selector@ for @setMatchDomainsNoSearch:@
setMatchDomainsNoSearchSelector :: Selector '[Bool] ()
setMatchDomainsNoSearchSelector = mkSelector "setMatchDomainsNoSearch:"

-- | @Selector@ for @allowFailover@
allowFailoverSelector :: Selector '[] Bool
allowFailoverSelector = mkSelector "allowFailover"

-- | @Selector@ for @setAllowFailover:@
setAllowFailoverSelector :: Selector '[Bool] ()
setAllowFailoverSelector = mkSelector "setAllowFailover:"

