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
  , includedNetworkRulesSelector
  , setIncludedNetworkRulesSelector
  , excludedNetworkRulesSelector
  , setExcludedNetworkRulesSelector


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
import ObjC.Foundation.Internal.Classes

-- | includedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- includedNetworkRules@
includedNetworkRules :: IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings => neTransparentProxyNetworkSettings -> IO (Id NSArray)
includedNetworkRules neTransparentProxyNetworkSettings  =
    sendMsg neTransparentProxyNetworkSettings (mkSelector "includedNetworkRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | includedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- setIncludedNetworkRules:@
setIncludedNetworkRules :: (IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings, IsNSArray value) => neTransparentProxyNetworkSettings -> value -> IO ()
setIncludedNetworkRules neTransparentProxyNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTransparentProxyNetworkSettings (mkSelector "setIncludedNetworkRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | excludedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will not be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- excludedNetworkRules@
excludedNetworkRules :: IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings => neTransparentProxyNetworkSettings -> IO (Id NSArray)
excludedNetworkRules neTransparentProxyNetworkSettings  =
    sendMsg neTransparentProxyNetworkSettings (mkSelector "excludedNetworkRules") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | excludedNetworkRules
--
-- An array of NENetworkRule objects that collectively specify the traffic that will not be routed through the transparent proxy. The following restrictions    apply to each NENetworkRule in this list:    Restrictions for rules with an address endpoint:        If the port string of the endpoint is "0" or is the empty string, then the address of the endpoint must be a non-wildcard address (i.e. "0.0.0.0" or "::").        If the address is a wildcard address (i.e. "0.0.0.0" or "::"), then the port string of the endpoint must be non-empty and must not be "0".        A port string of "53" is not allowed. Destination Domain-based rules must be used to match DNS traffic.        The matchLocalNetwork property must be nil.        The matchDirection property must be NETrafficDirectionOutbound.
--
-- ObjC selector: @- setExcludedNetworkRules:@
setExcludedNetworkRules :: (IsNETransparentProxyNetworkSettings neTransparentProxyNetworkSettings, IsNSArray value) => neTransparentProxyNetworkSettings -> value -> IO ()
setExcludedNetworkRules neTransparentProxyNetworkSettings  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTransparentProxyNetworkSettings (mkSelector "setExcludedNetworkRules:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @includedNetworkRules@
includedNetworkRulesSelector :: Selector
includedNetworkRulesSelector = mkSelector "includedNetworkRules"

-- | @Selector@ for @setIncludedNetworkRules:@
setIncludedNetworkRulesSelector :: Selector
setIncludedNetworkRulesSelector = mkSelector "setIncludedNetworkRules:"

-- | @Selector@ for @excludedNetworkRules@
excludedNetworkRulesSelector :: Selector
excludedNetworkRulesSelector = mkSelector "excludedNetworkRules"

-- | @Selector@ for @setExcludedNetworkRules:@
setExcludedNetworkRulesSelector :: Selector
setExcludedNetworkRulesSelector = mkSelector "setExcludedNetworkRules:"

