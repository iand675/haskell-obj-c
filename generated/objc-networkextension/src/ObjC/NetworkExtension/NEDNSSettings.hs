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
  , matchDomainsNoSearch
  , setMatchDomainsNoSearch
  , allowFailover
  , setAllowFailover
  , initWithServersSelector
  , dnsProtocolSelector
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

