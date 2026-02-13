{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEOnDemandRule
--
-- The NEOnDemandRule class declares the programmatic interface for an object that defines an On Demand rule.
--
-- NEOnDemandRule is an abstract base class from which other action-specific rule classes are derived.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEOnDemandRule@.
module ObjC.NetworkExtension.NEOnDemandRule
  ( NEOnDemandRule
  , IsNEOnDemandRule(..)
  , action
  , dnsSearchDomainMatch
  , setDNSSearchDomainMatch
  , dnsServerAddressMatch
  , setDNSServerAddressMatch
  , interfaceTypeMatch
  , setInterfaceTypeMatch
  , ssidMatch
  , setSSIDMatch
  , probeURL
  , setProbeURL
  , actionSelector
  , dnsSearchDomainMatchSelector
  , dnsServerAddressMatchSelector
  , interfaceTypeMatchSelector
  , probeURLSelector
  , setDNSSearchDomainMatchSelector
  , setDNSServerAddressMatchSelector
  , setInterfaceTypeMatchSelector
  , setProbeURLSelector
  , setSSIDMatchSelector
  , ssidMatchSelector

  -- * Enum types
  , NEOnDemandRuleAction(NEOnDemandRuleAction)
  , pattern NEOnDemandRuleActionConnect
  , pattern NEOnDemandRuleActionDisconnect
  , pattern NEOnDemandRuleActionEvaluateConnection
  , pattern NEOnDemandRuleActionIgnore
  , NEOnDemandRuleInterfaceType(NEOnDemandRuleInterfaceType)
  , pattern NEOnDemandRuleInterfaceTypeAny
  , pattern NEOnDemandRuleInterfaceTypeEthernet
  , pattern NEOnDemandRuleInterfaceTypeWiFi
  , pattern NEOnDemandRuleInterfaceTypeCellular

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

-- | action
--
-- The rule's action
--
-- ObjC selector: @- action@
action :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO NEOnDemandRuleAction
action neOnDemandRule =
  sendMessage neOnDemandRule actionSelector

-- | DNSSearchDomainMatch
--
-- An array of NSString objects. If the current default search domain is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current default search domain does not factor into the rule match.
--
-- ObjC selector: @- DNSSearchDomainMatch@
dnsSearchDomainMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSArray)
dnsSearchDomainMatch neOnDemandRule =
  sendMessage neOnDemandRule dnsSearchDomainMatchSelector

-- | DNSSearchDomainMatch
--
-- An array of NSString objects. If the current default search domain is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current default search domain does not factor into the rule match.
--
-- ObjC selector: @- setDNSSearchDomainMatch:@
setDNSSearchDomainMatch :: (IsNEOnDemandRule neOnDemandRule, IsNSArray value) => neOnDemandRule -> value -> IO ()
setDNSSearchDomainMatch neOnDemandRule value =
  sendMessage neOnDemandRule setDNSSearchDomainMatchSelector (toNSArray value)

-- | DNSServerAddressMatch
--
-- An array of DNS server IP addresses represented as NSString objects. If each of the current default DNS servers is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the default DNS servers do not factor into the rule match.
--
-- ObjC selector: @- DNSServerAddressMatch@
dnsServerAddressMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSArray)
dnsServerAddressMatch neOnDemandRule =
  sendMessage neOnDemandRule dnsServerAddressMatchSelector

-- | DNSServerAddressMatch
--
-- An array of DNS server IP addresses represented as NSString objects. If each of the current default DNS servers is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the default DNS servers do not factor into the rule match.
--
-- ObjC selector: @- setDNSServerAddressMatch:@
setDNSServerAddressMatch :: (IsNEOnDemandRule neOnDemandRule, IsNSArray value) => neOnDemandRule -> value -> IO ()
setDNSServerAddressMatch neOnDemandRule value =
  sendMessage neOnDemandRule setDNSServerAddressMatchSelector (toNSArray value)

-- | interfaceTypeMatch
--
-- The type of interface that this rule matches. If the current primary network interface is of this type and all of the other conditions in the rule match, then the rule matches. If this property is 0 (the default), then the current primary interface type does not factor into the rule match.
--
-- ObjC selector: @- interfaceTypeMatch@
interfaceTypeMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO NEOnDemandRuleInterfaceType
interfaceTypeMatch neOnDemandRule =
  sendMessage neOnDemandRule interfaceTypeMatchSelector

-- | interfaceTypeMatch
--
-- The type of interface that this rule matches. If the current primary network interface is of this type and all of the other conditions in the rule match, then the rule matches. If this property is 0 (the default), then the current primary interface type does not factor into the rule match.
--
-- ObjC selector: @- setInterfaceTypeMatch:@
setInterfaceTypeMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> NEOnDemandRuleInterfaceType -> IO ()
setInterfaceTypeMatch neOnDemandRule value =
  sendMessage neOnDemandRule setInterfaceTypeMatchSelector value

-- | SSIDMatch
--
-- An array of NSString objects. If the Service Set Identifier (SSID) of the current primary connected network matches one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current primary connected network SSID does not factor into the rule match.
--
-- ObjC selector: @- SSIDMatch@
ssidMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSArray)
ssidMatch neOnDemandRule =
  sendMessage neOnDemandRule ssidMatchSelector

-- | SSIDMatch
--
-- An array of NSString objects. If the Service Set Identifier (SSID) of the current primary connected network matches one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current primary connected network SSID does not factor into the rule match.
--
-- ObjC selector: @- setSSIDMatch:@
setSSIDMatch :: (IsNEOnDemandRule neOnDemandRule, IsNSArray value) => neOnDemandRule -> value -> IO ()
setSSIDMatch neOnDemandRule value =
  sendMessage neOnDemandRule setSSIDMatchSelector (toNSArray value)

-- | probeURL
--
-- An HTTP or HTTPS URL. If a request sent to this URL results in a HTTP 200 OK response and all of the other conditions in the rule match, then then rule matches. If this property is nil (the default), then an HTTP request does not factor into the rule match.
--
-- ObjC selector: @- probeURL@
probeURL :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSURL)
probeURL neOnDemandRule =
  sendMessage neOnDemandRule probeURLSelector

-- | probeURL
--
-- An HTTP or HTTPS URL. If a request sent to this URL results in a HTTP 200 OK response and all of the other conditions in the rule match, then then rule matches. If this property is nil (the default), then an HTTP request does not factor into the rule match.
--
-- ObjC selector: @- setProbeURL:@
setProbeURL :: (IsNEOnDemandRule neOnDemandRule, IsNSURL value) => neOnDemandRule -> value -> IO ()
setProbeURL neOnDemandRule value =
  sendMessage neOnDemandRule setProbeURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @action@
actionSelector :: Selector '[] NEOnDemandRuleAction
actionSelector = mkSelector "action"

-- | @Selector@ for @DNSSearchDomainMatch@
dnsSearchDomainMatchSelector :: Selector '[] (Id NSArray)
dnsSearchDomainMatchSelector = mkSelector "DNSSearchDomainMatch"

-- | @Selector@ for @setDNSSearchDomainMatch:@
setDNSSearchDomainMatchSelector :: Selector '[Id NSArray] ()
setDNSSearchDomainMatchSelector = mkSelector "setDNSSearchDomainMatch:"

-- | @Selector@ for @DNSServerAddressMatch@
dnsServerAddressMatchSelector :: Selector '[] (Id NSArray)
dnsServerAddressMatchSelector = mkSelector "DNSServerAddressMatch"

-- | @Selector@ for @setDNSServerAddressMatch:@
setDNSServerAddressMatchSelector :: Selector '[Id NSArray] ()
setDNSServerAddressMatchSelector = mkSelector "setDNSServerAddressMatch:"

-- | @Selector@ for @interfaceTypeMatch@
interfaceTypeMatchSelector :: Selector '[] NEOnDemandRuleInterfaceType
interfaceTypeMatchSelector = mkSelector "interfaceTypeMatch"

-- | @Selector@ for @setInterfaceTypeMatch:@
setInterfaceTypeMatchSelector :: Selector '[NEOnDemandRuleInterfaceType] ()
setInterfaceTypeMatchSelector = mkSelector "setInterfaceTypeMatch:"

-- | @Selector@ for @SSIDMatch@
ssidMatchSelector :: Selector '[] (Id NSArray)
ssidMatchSelector = mkSelector "SSIDMatch"

-- | @Selector@ for @setSSIDMatch:@
setSSIDMatchSelector :: Selector '[Id NSArray] ()
setSSIDMatchSelector = mkSelector "setSSIDMatch:"

-- | @Selector@ for @probeURL@
probeURLSelector :: Selector '[] (Id NSURL)
probeURLSelector = mkSelector "probeURL"

-- | @Selector@ for @setProbeURL:@
setProbeURLSelector :: Selector '[Id NSURL] ()
setProbeURLSelector = mkSelector "setProbeURL:"

