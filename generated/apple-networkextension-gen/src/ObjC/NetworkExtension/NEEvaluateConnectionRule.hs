{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEEvaluateConnectionRule
--
-- The NEEvaluateConnectionRule class declares the programmatic interface for an object that associates properties of network connections with an action.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEEvaluateConnectionRule@.
module ObjC.NetworkExtension.NEEvaluateConnectionRule
  ( NEEvaluateConnectionRule
  , IsNEEvaluateConnectionRule(..)
  , initWithMatchDomains_andAction
  , action
  , matchDomains
  , useDNSServers
  , setUseDNSServers
  , probeURL
  , setProbeURL
  , actionSelector
  , initWithMatchDomains_andActionSelector
  , matchDomainsSelector
  , probeURLSelector
  , setProbeURLSelector
  , setUseDNSServersSelector
  , useDNSServersSelector

  -- * Enum types
  , NEEvaluateConnectionRuleAction(NEEvaluateConnectionRuleAction)
  , pattern NEEvaluateConnectionRuleActionConnectIfNeeded
  , pattern NEEvaluateConnectionRuleActionNeverConnect

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

-- | initWithMatchDomains:andAction
--
-- Initialize an NEEvaluateConnectionRule instance with a list of destination host domains and an action
--
-- ObjC selector: @- initWithMatchDomains:andAction:@
initWithMatchDomains_andAction :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSArray domains) => neEvaluateConnectionRule -> domains -> NEEvaluateConnectionRuleAction -> IO (Id NEEvaluateConnectionRule)
initWithMatchDomains_andAction neEvaluateConnectionRule domains action =
  sendOwnedMessage neEvaluateConnectionRule initWithMatchDomains_andActionSelector (toNSArray domains) action

-- | action
--
-- The action to take if the properties of the network connection being established match the rule.
--
-- ObjC selector: @- action@
action :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO NEEvaluateConnectionRuleAction
action neEvaluateConnectionRule =
  sendMessage neEvaluateConnectionRule actionSelector

-- | matchDomains
--
-- An array of NSString objects. If the host name of the destination of the network connection being established shares a suffix with one of the strings in this array, then the rule matches.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO (Id NSArray)
matchDomains neEvaluateConnectionRule =
  sendMessage neEvaluateConnectionRule matchDomainsSelector

-- | useDNSServers
--
-- An array of NSString objects. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded, the DNS servers specified in this array are used to resolve the host name of the destination while evaluating connectivity to the destination. If the resolution fails for any reason, the VPN is started.
--
-- ObjC selector: @- useDNSServers@
useDNSServers :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO (Id NSArray)
useDNSServers neEvaluateConnectionRule =
  sendMessage neEvaluateConnectionRule useDNSServersSelector

-- | useDNSServers
--
-- An array of NSString objects. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded, the DNS servers specified in this array are used to resolve the host name of the destination while evaluating connectivity to the destination. If the resolution fails for any reason, the VPN is started.
--
-- ObjC selector: @- setUseDNSServers:@
setUseDNSServers :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSArray value) => neEvaluateConnectionRule -> value -> IO ()
setUseDNSServers neEvaluateConnectionRule value =
  sendMessage neEvaluateConnectionRule setUseDNSServersSelector (toNSArray value)

-- | probeURL
--
-- An HTTP or HTTPS URL. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded and a request sent to this URL results in a response with an HTTP response code other than 200, then the VPN is started.
--
-- ObjC selector: @- probeURL@
probeURL :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO (Id NSURL)
probeURL neEvaluateConnectionRule =
  sendMessage neEvaluateConnectionRule probeURLSelector

-- | probeURL
--
-- An HTTP or HTTPS URL. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded and a request sent to this URL results in a response with an HTTP response code other than 200, then the VPN is started.
--
-- ObjC selector: @- setProbeURL:@
setProbeURL :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSURL value) => neEvaluateConnectionRule -> value -> IO ()
setProbeURL neEvaluateConnectionRule value =
  sendMessage neEvaluateConnectionRule setProbeURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchDomains:andAction:@
initWithMatchDomains_andActionSelector :: Selector '[Id NSArray, NEEvaluateConnectionRuleAction] (Id NEEvaluateConnectionRule)
initWithMatchDomains_andActionSelector = mkSelector "initWithMatchDomains:andAction:"

-- | @Selector@ for @action@
actionSelector :: Selector '[] NEEvaluateConnectionRuleAction
actionSelector = mkSelector "action"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector '[] (Id NSArray)
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @useDNSServers@
useDNSServersSelector :: Selector '[] (Id NSArray)
useDNSServersSelector = mkSelector "useDNSServers"

-- | @Selector@ for @setUseDNSServers:@
setUseDNSServersSelector :: Selector '[Id NSArray] ()
setUseDNSServersSelector = mkSelector "setUseDNSServers:"

-- | @Selector@ for @probeURL@
probeURLSelector :: Selector '[] (Id NSURL)
probeURLSelector = mkSelector "probeURL"

-- | @Selector@ for @setProbeURL:@
setProbeURLSelector :: Selector '[Id NSURL] ()
setProbeURLSelector = mkSelector "setProbeURL:"

