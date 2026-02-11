{-# LANGUAGE PatternSynonyms #-}
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
  , setDNSSearchDomainMatchSelector
  , dnsServerAddressMatchSelector
  , setDNSServerAddressMatchSelector
  , interfaceTypeMatchSelector
  , setInterfaceTypeMatchSelector
  , ssidMatchSelector
  , setSSIDMatchSelector
  , probeURLSelector
  , setProbeURLSelector

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

-- | action
--
-- The rule's action
--
-- ObjC selector: @- action@
action :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO NEOnDemandRuleAction
action neOnDemandRule  =
    fmap (coerce :: CLong -> NEOnDemandRuleAction) $ sendMsg neOnDemandRule (mkSelector "action") retCLong []

-- | DNSSearchDomainMatch
--
-- An array of NSString objects. If the current default search domain is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current default search domain does not factor into the rule match.
--
-- ObjC selector: @- DNSSearchDomainMatch@
dnsSearchDomainMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSArray)
dnsSearchDomainMatch neOnDemandRule  =
    sendMsg neOnDemandRule (mkSelector "DNSSearchDomainMatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DNSSearchDomainMatch
--
-- An array of NSString objects. If the current default search domain is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current default search domain does not factor into the rule match.
--
-- ObjC selector: @- setDNSSearchDomainMatch:@
setDNSSearchDomainMatch :: (IsNEOnDemandRule neOnDemandRule, IsNSArray value) => neOnDemandRule -> value -> IO ()
setDNSSearchDomainMatch neOnDemandRule  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neOnDemandRule (mkSelector "setDNSSearchDomainMatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | DNSServerAddressMatch
--
-- An array of DNS server IP addresses represented as NSString objects. If each of the current default DNS servers is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the default DNS servers do not factor into the rule match.
--
-- ObjC selector: @- DNSServerAddressMatch@
dnsServerAddressMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSArray)
dnsServerAddressMatch neOnDemandRule  =
    sendMsg neOnDemandRule (mkSelector "DNSServerAddressMatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DNSServerAddressMatch
--
-- An array of DNS server IP addresses represented as NSString objects. If each of the current default DNS servers is equal to one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the default DNS servers do not factor into the rule match.
--
-- ObjC selector: @- setDNSServerAddressMatch:@
setDNSServerAddressMatch :: (IsNEOnDemandRule neOnDemandRule, IsNSArray value) => neOnDemandRule -> value -> IO ()
setDNSServerAddressMatch neOnDemandRule  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neOnDemandRule (mkSelector "setDNSServerAddressMatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | interfaceTypeMatch
--
-- The type of interface that this rule matches. If the current primary network interface is of this type and all of the other conditions in the rule match, then the rule matches. If this property is 0 (the default), then the current primary interface type does not factor into the rule match.
--
-- ObjC selector: @- interfaceTypeMatch@
interfaceTypeMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO NEOnDemandRuleInterfaceType
interfaceTypeMatch neOnDemandRule  =
    fmap (coerce :: CLong -> NEOnDemandRuleInterfaceType) $ sendMsg neOnDemandRule (mkSelector "interfaceTypeMatch") retCLong []

-- | interfaceTypeMatch
--
-- The type of interface that this rule matches. If the current primary network interface is of this type and all of the other conditions in the rule match, then the rule matches. If this property is 0 (the default), then the current primary interface type does not factor into the rule match.
--
-- ObjC selector: @- setInterfaceTypeMatch:@
setInterfaceTypeMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> NEOnDemandRuleInterfaceType -> IO ()
setInterfaceTypeMatch neOnDemandRule  value =
    sendMsg neOnDemandRule (mkSelector "setInterfaceTypeMatch:") retVoid [argCLong (coerce value)]

-- | SSIDMatch
--
-- An array of NSString objects. If the Service Set Identifier (SSID) of the current primary connected network matches one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current primary connected network SSID does not factor into the rule match.
--
-- ObjC selector: @- SSIDMatch@
ssidMatch :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSArray)
ssidMatch neOnDemandRule  =
    sendMsg neOnDemandRule (mkSelector "SSIDMatch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | SSIDMatch
--
-- An array of NSString objects. If the Service Set Identifier (SSID) of the current primary connected network matches one of the strings in this array and all of the other conditions in the rule match, then the rule matches. If this property is nil (the default), then the current primary connected network SSID does not factor into the rule match.
--
-- ObjC selector: @- setSSIDMatch:@
setSSIDMatch :: (IsNEOnDemandRule neOnDemandRule, IsNSArray value) => neOnDemandRule -> value -> IO ()
setSSIDMatch neOnDemandRule  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neOnDemandRule (mkSelector "setSSIDMatch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | probeURL
--
-- An HTTP or HTTPS URL. If a request sent to this URL results in a HTTP 200 OK response and all of the other conditions in the rule match, then then rule matches. If this property is nil (the default), then an HTTP request does not factor into the rule match.
--
-- ObjC selector: @- probeURL@
probeURL :: IsNEOnDemandRule neOnDemandRule => neOnDemandRule -> IO (Id NSURL)
probeURL neOnDemandRule  =
    sendMsg neOnDemandRule (mkSelector "probeURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | probeURL
--
-- An HTTP or HTTPS URL. If a request sent to this URL results in a HTTP 200 OK response and all of the other conditions in the rule match, then then rule matches. If this property is nil (the default), then an HTTP request does not factor into the rule match.
--
-- ObjC selector: @- setProbeURL:@
setProbeURL :: (IsNEOnDemandRule neOnDemandRule, IsNSURL value) => neOnDemandRule -> value -> IO ()
setProbeURL neOnDemandRule  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neOnDemandRule (mkSelector "setProbeURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @DNSSearchDomainMatch@
dnsSearchDomainMatchSelector :: Selector
dnsSearchDomainMatchSelector = mkSelector "DNSSearchDomainMatch"

-- | @Selector@ for @setDNSSearchDomainMatch:@
setDNSSearchDomainMatchSelector :: Selector
setDNSSearchDomainMatchSelector = mkSelector "setDNSSearchDomainMatch:"

-- | @Selector@ for @DNSServerAddressMatch@
dnsServerAddressMatchSelector :: Selector
dnsServerAddressMatchSelector = mkSelector "DNSServerAddressMatch"

-- | @Selector@ for @setDNSServerAddressMatch:@
setDNSServerAddressMatchSelector :: Selector
setDNSServerAddressMatchSelector = mkSelector "setDNSServerAddressMatch:"

-- | @Selector@ for @interfaceTypeMatch@
interfaceTypeMatchSelector :: Selector
interfaceTypeMatchSelector = mkSelector "interfaceTypeMatch"

-- | @Selector@ for @setInterfaceTypeMatch:@
setInterfaceTypeMatchSelector :: Selector
setInterfaceTypeMatchSelector = mkSelector "setInterfaceTypeMatch:"

-- | @Selector@ for @SSIDMatch@
ssidMatchSelector :: Selector
ssidMatchSelector = mkSelector "SSIDMatch"

-- | @Selector@ for @setSSIDMatch:@
setSSIDMatchSelector :: Selector
setSSIDMatchSelector = mkSelector "setSSIDMatch:"

-- | @Selector@ for @probeURL@
probeURLSelector :: Selector
probeURLSelector = mkSelector "probeURL"

-- | @Selector@ for @setProbeURL:@
setProbeURLSelector :: Selector
setProbeURLSelector = mkSelector "setProbeURL:"

