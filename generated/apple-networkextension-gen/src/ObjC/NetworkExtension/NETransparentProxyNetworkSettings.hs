{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETransparentProxyNetworkSettings
--
-- The NETransparentProxyNetworkSettings class declares the programmatic interface for an object that contains network settings.
--
-- NETransparentProxyNetworkSettings is used by NEAppProxyProviders to communicate the desired network settings for the proxy to the framework. The framework takes care of applying the contained settings to the system.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETransparentProxyNetworkSettings@.
module ObjC.NetworkExtension.NETransparentProxyNetworkSettings
  ( NETransparentProxyNetworkSettings
  , IsNETransparentProxyNetworkSettings(..)
  , includedNetworkRules
  , setIncludedNetworkRules
  , excludedNetworkRules
  , setExcludedNetworkRules
  , excludedNetworkRulesSelector
  , includedNetworkRulesSelector
  , setExcludedNetworkRulesSelector
  , setIncludedNetworkRulesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | includedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- includedNetworkRules@
includedNetworkRules :: IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings => neTransparentProxyNetworkSettings -> IO (Id NSArray)
includedNetworkRules neTransparentProxyNetworkSettings =
  sendMessage neTransparentProxyNetworkSettings includedNetworkRulesSelector

-- | includedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- setIncludedNetworkRules:@
setIncludedNetworkRules :: (IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings, IsNSArray value) => neTransparentProxyNetworkSettings -> value -> IO ()
setIncludedNetworkRules neTransparentProxyNetworkSettings value =
  sendMessage neTransparentProxyNetworkSettings setIncludedNetworkRulesSelector (toNSArray value)

-- | excludedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will not be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- excludedNetworkRules@
excludedNetworkRules :: IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings => neTransparentProxyNetworkSettings -> IO (Id NSArray)
excludedNetworkRules neTransparentProxyNetworkSettings =
  sendMessage neTransparentProxyNetworkSettings excludedNetworkRulesSelector

-- | excludedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will not be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- setExcludedNetworkRules:@
setExcludedNetworkRules :: (IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings, IsNSArray value) => neTransparentProxyNetworkSettings -> value -> IO ()
setExcludedNetworkRules neTransparentProxyNetworkSettings value =
  sendMessage neTransparentProxyNetworkSettings setExcludedNetworkRulesSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @includedNetworkRules@
includedNetworkRulesSelector :: Selector '[] (Id NSArray)
includedNetworkRulesSelector = mkSelector "includedNetworkRules"

-- | @Selector@ for @setIncludedNetworkRules:@
setIncludedNetworkRulesSelector :: Selector '[Id NSArray] ()
setIncludedNetworkRulesSelector = mkSelector "setIncludedNetworkRules:"

-- | @Selector@ for @excludedNetworkRules@
excludedNetworkRulesSelector :: Selector '[] (Id NSArray)
excludedNetworkRulesSelector = mkSelector "excludedNetworkRules"

-- | @Selector@ for @setExcludedNetworkRules:@
setExcludedNetworkRulesSelector :: Selector '[Id NSArray] ()
setExcludedNetworkRulesSelector = mkSelector "setExcludedNetworkRules:"

