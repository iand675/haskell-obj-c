{-# LANGUAGE PatternSynonyms #-}
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
  , initWithMatchDomains_andActionSelector
  , actionSelector
  , matchDomainsSelector
  , useDNSServersSelector
  , setUseDNSServersSelector
  , probeURLSelector
  , setProbeURLSelector

  -- * Enum types
  , NEEvaluateConnectionRuleAction(NEEvaluateConnectionRuleAction)
  , pattern NEEvaluateConnectionRuleActionConnectIfNeeded
  , pattern NEEvaluateConnectionRuleActionNeverConnect

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

-- | initWithMatchDomains:andAction
--
-- Initialize an NEEvaluateConnectionRule instance with a list of destination host domains and an action
--
-- ObjC selector: @- initWithMatchDomains:andAction:@
initWithMatchDomains_andAction :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSArray domains) => neEvaluateConnectionRule -> domains -> NEEvaluateConnectionRuleAction -> IO (Id NEEvaluateConnectionRule)
initWithMatchDomains_andAction neEvaluateConnectionRule  domains action =
  withObjCPtr domains $ \raw_domains ->
      sendMsg neEvaluateConnectionRule (mkSelector "initWithMatchDomains:andAction:") (retPtr retVoid) [argPtr (castPtr raw_domains :: Ptr ()), argCLong (coerce action)] >>= ownedObject . castPtr

-- | action
--
-- The action to take if the properties of the network connection being established match the rule.
--
-- ObjC selector: @- action@
action :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO NEEvaluateConnectionRuleAction
action neEvaluateConnectionRule  =
    fmap (coerce :: CLong -> NEEvaluateConnectionRuleAction) $ sendMsg neEvaluateConnectionRule (mkSelector "action") retCLong []

-- | matchDomains
--
-- An array of NSString objects. If the host name of the destination of the network connection being established shares a suffix with one of the strings in this array, then the rule matches.
--
-- ObjC selector: @- matchDomains@
matchDomains :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO (Id NSArray)
matchDomains neEvaluateConnectionRule  =
    sendMsg neEvaluateConnectionRule (mkSelector "matchDomains") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | useDNSServers
--
-- An array of NSString objects. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded, the DNS servers specified in this array are used to resolve the host name of the destination while evaluating connectivity to the destination. If the resolution fails for any reason, the VPN is started.
--
-- ObjC selector: @- useDNSServers@
useDNSServers :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO (Id NSArray)
useDNSServers neEvaluateConnectionRule  =
    sendMsg neEvaluateConnectionRule (mkSelector "useDNSServers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | useDNSServers
--
-- An array of NSString objects. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded, the DNS servers specified in this array are used to resolve the host name of the destination while evaluating connectivity to the destination. If the resolution fails for any reason, the VPN is started.
--
-- ObjC selector: @- setUseDNSServers:@
setUseDNSServers :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSArray value) => neEvaluateConnectionRule -> value -> IO ()
setUseDNSServers neEvaluateConnectionRule  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neEvaluateConnectionRule (mkSelector "setUseDNSServers:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | probeURL
--
-- An HTTP or HTTPS URL. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded and a request sent to this URL results in a response with an HTTP response code other than 200, then the VPN is started.
--
-- ObjC selector: @- probeURL@
probeURL :: IsNEEvaluateConnectionRule neEvaluateConnectionRule => neEvaluateConnectionRule -> IO (Id NSURL)
probeURL neEvaluateConnectionRule  =
    sendMsg neEvaluateConnectionRule (mkSelector "probeURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | probeURL
--
-- An HTTP or HTTPS URL. If the rule matches the connection being established and the action is NEEvaluateConnectionRuleActionConnectIfNeeded and a request sent to this URL results in a response with an HTTP response code other than 200, then the VPN is started.
--
-- ObjC selector: @- setProbeURL:@
setProbeURL :: (IsNEEvaluateConnectionRule neEvaluateConnectionRule, IsNSURL value) => neEvaluateConnectionRule -> value -> IO ()
setProbeURL neEvaluateConnectionRule  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neEvaluateConnectionRule (mkSelector "setProbeURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithMatchDomains:andAction:@
initWithMatchDomains_andActionSelector :: Selector
initWithMatchDomains_andActionSelector = mkSelector "initWithMatchDomains:andAction:"

-- | @Selector@ for @action@
actionSelector :: Selector
actionSelector = mkSelector "action"

-- | @Selector@ for @matchDomains@
matchDomainsSelector :: Selector
matchDomainsSelector = mkSelector "matchDomains"

-- | @Selector@ for @useDNSServers@
useDNSServersSelector :: Selector
useDNSServersSelector = mkSelector "useDNSServers"

-- | @Selector@ for @setUseDNSServers:@
setUseDNSServersSelector :: Selector
setUseDNSServersSelector = mkSelector "setUseDNSServers:"

-- | @Selector@ for @probeURL@
probeURLSelector :: Selector
probeURLSelector = mkSelector "probeURL"

-- | @Selector@ for @setProbeURL:@
setProbeURLSelector :: Selector
setProbeURLSelector = mkSelector "setProbeURL:"

