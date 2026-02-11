{-# LANGUAGE PatternSynonyms #-}
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
  , initWithServersSelector
  , dnsProtocolSelector
  , serversSelector
  , searchDomainsSelector
  , setSearchDomainsSelector
  , domainNameSelector
  , setDomainNameSelector
  , matchDomainsSelector
  , setMatchDomainsSelector
  , matchDomainsNoSearchSelector
  , setMatchDomainsNoSearchSelector
  , allowFailoverSelector
  , setAllowFailoverSelector

  -- * Enum types
  , NEDNSProtocol(NEDNSProtocol)
  , pattern NEDNSProtocolCleartext
  , pattern NEDNSProtocolTLS
  , pattern NEDNSProtocolHTTPS

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

-- | initWithServers:
--
-- Initialize a newly-allocated NEDNSSettings object.
--
-- @servers@ — An array of DNS server IP address strings.
--
-- ObjC selector: @- initWithServers:@
initWithServers :: (IsNEDNSSettings nednsSettings, IsNSArray servers) => nednsSettings -> servers -> IO (Id NEDNSSettings)
initWithServers nednsSettings  servers =
  withObjCPtr servers $ \raw_servers ->
      sendMsg nednsSettings (mkSelector "initWithServers:") (retPtr retVoid) [argPtr (castPtr raw_servers :: Ptr ())] >>= ownedObject . castPtr

-- | dnsProtocol
--
-- The DNS protocol used by the settings.
--
-- ObjC selector: @- dnsProtocol@
dnsProtocol :: IsNEDNSSettings nednsSettings => nednsSettings -> IO NEDNSProtocol
dnsProtocol nednsSettings  =
    fmap (coerce :: CLong -> NEDNSProtocol) $ sendMsg nednsSettings (mkSelector "dnsProtocol") retCLong []

-- | servers
--
-- An array of DNS server address strings.
--
-- ObjC selector: @- servers@
servers :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSArray)
servers nednsSettings  =
    sendMsg nednsSettings (mkSelector "servers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | searchDomains
--
-- An array of DNS server search domain strings.
--
-- ObjC selector: @- searchDomains@
searchDomains :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSArray)
searchDomains nednsSettings  =
    sendMsg nednsSettings (mkSelector "searchDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | searchDomains
--
-- An array of DNS server search domain strings.
--
-- ObjC selector: @- setSearchDomains:@
setSearchDomains :: (IsNEDNSSettings nednsSettings, IsNSArray value) => nednsSettings -> value -> IO ()
setSearchDomains nednsSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsSettings (mkSelector "setSearchDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | domainName
--
-- A string containing the DNS domain.
--
-- ObjC selector: @- domainName@
domainName :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSString)
domainName nednsSettings  =
    sendMsg nednsSettings (mkSelector "domainName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | domainName
--
-- A string containing the DNS domain.
--
-- ObjC selector: @- setDomainName:@
setDomainName :: (IsNEDNSSettings nednsSettings, IsNSString value) => nednsSettings -> value -> IO ()
setDomainName nednsSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsSettings (mkSelector "setDomainName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | matchDomains
--
-- An array of strings containing domain strings. If this property is non-nil, the DNS settings will only be used to resolve host names within the specified domains.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEDNSSettings nednsSettings => nednsSettings -> IO (Id NSArray)
matchDomains nednsSettings  =
    sendMsg nednsSettings (mkSelector "matchDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | matchDomains
--
-- An array of strings containing domain strings. If this property is non-nil, the DNS settings will only be used to resolve host names within the specified domains.
--
-- ObjC selector: @- setMatchDomains:@
setMatchDomains :: (IsNEDNSSettings nednsSettings, IsNSArray value) => nednsSettings -> value -> IO ()
setMatchDomains nednsSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg nednsSettings (mkSelector "setMatchDomains:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | matchDomainsNoSearch
--
-- A boolean indicating if the match domains should be appended to the search domain list.  Default is NO (match domains will be appended to the search domain list).
--
-- ObjC selector: @- matchDomainsNoSearch@
matchDomainsNoSearch :: IsNEDNSSettings nednsSettings => nednsSettings -> IO Bool
matchDomainsNoSearch nednsSettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nednsSettings (mkSelector "matchDomainsNoSearch") retCULong []

-- | matchDomainsNoSearch
--
-- A boolean indicating if the match domains should be appended to the search domain list.  Default is NO (match domains will be appended to the search domain list).
--
-- ObjC selector: @- setMatchDomainsNoSearch:@
setMatchDomainsNoSearch :: IsNEDNSSettings nednsSettings => nednsSettings -> Bool -> IO ()
setMatchDomainsNoSearch nednsSettings  value =
    sendMsg nednsSettings (mkSelector "setMatchDomainsNoSearch:") retVoid [argCULong (if value then 1 else 0)]

-- | allowFailover
--
-- A boolean indicating if failover to the default system resolver is permitted on resolution failure.
--
-- ObjC selector: @- allowFailover@
allowFailover :: IsNEDNSSettings nednsSettings => nednsSettings -> IO Bool
allowFailover nednsSettings  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nednsSettings (mkSelector "allowFailover") retCULong []

-- | allowFailover
--
-- A boolean indicating if failover to the default system resolver is permitted on resolution failure.
--
-- ObjC selector: @- setAllowFailover:@
setAllowFailover :: IsNEDNSSettings nednsSettings => nednsSettings -> Bool -> IO ()
setAllowFailover nednsSettings  value =
    sendMsg nednsSettings (mkSelector "setAllowFailover:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithServers:@
initWithServersSelector :: Selector
initWithServersSelector = mkSelector "initWithServers:"

-- | @Selector@ for @dnsProtocol@
dnsProtocolSelector :: Selector
dnsProtocolSelector = mkSelector "dnsProtocol"

-- | @Selector@ for @servers@
serversSelector :: Selector
serversSelector = mkSelector "servers"

-- | @Selector@ for @searchDomains@
searchDomainsSelector :: Selector
searchDomainsSelector = mkSelector "searchDomains"

-- | @Selector@ for @setSearchDomains:@
setSearchDomainsSelector :: Selector
setSearchDomainsSelector = mkSelector "setSearchDomains:"

-- | @Selector@ for @domainName@
domainNameSelector :: Selector
domainNameSelector = mkSelector "domainName"

-- | @Selector@ for @setDomainName:@
setDomainNameSelector :: Selector
setDomainNameSelector = mkSelector "setDomainName:"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @setMatchDomains:@
setMatchDomainsSelector :: Selector
setMatchDomainsSelector = mkSelector "setMatchDomains:"

-- | @Selector@ for @matchDomainsNoSearch@
matchDomainsNoSearchSelector :: Selector
matchDomainsNoSearchSelector = mkSelector "matchDomainsNoSearch"

-- | @Selector@ for @setMatchDomainsNoSearch:@
setMatchDomainsNoSearchSelector :: Selector
setMatchDomainsNoSearchSelector = mkSelector "setMatchDomainsNoSearch:"

-- | @Selector@ for @allowFailover@
allowFailoverSelector :: Selector
allowFailoverSelector = mkSelector "allowFailover"

-- | @Selector@ for @setAllowFailover:@
setAllowFailoverSelector :: Selector
setAllowFailoverSelector = mkSelector "setAllowFailover:"

