{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NEDNSProxyProviderProtocol
--
-- The NEDNSProxyProviderProtocol class declares the programmatic interface for an object that contains NEDNSProxyProvider-specific configuration settings.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NEDNSProxyProviderProtocol@.
module ObjC.NetworkExtension.NEDNSProxyProviderProtocol
  ( NEDNSProxyProviderProtocol
  , IsNEDNSProxyProviderProtocol(..)
  , providerConfiguration
  , setProviderConfiguration
  , providerBundleIdentifier
  , setProviderBundleIdentifier
  , providerBundleIdentifierSelector
  , providerConfigurationSelector
  , setProviderBundleIdentifierSelector
  , setProviderConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.NetworkExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | providerConfiguration
--
-- A dictionary containing NEDNSProxyProvider vendor-specific configuration parameters. This dictionary is passed as-is to NEDNSProxyProviders when a DNS proxy is started.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol => nednsProxyProviderProtocol -> IO (Id NSDictionary)
providerConfiguration nednsProxyProviderProtocol =
  sendMessage nednsProxyProviderProtocol providerConfigurationSelector

-- | providerConfiguration
--
-- A dictionary containing NEDNSProxyProvider vendor-specific configuration parameters. This dictionary is passed as-is to NEDNSProxyProviders when a DNS proxy is started.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol, IsNSDictionary value) => nednsProxyProviderProtocol -> value -> IO ()
setProviderConfiguration nednsProxyProviderProtocol value =
  sendMessage nednsProxyProviderProtocol setProviderConfigurationSelector (toNSDictionary value)

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NEDNSProxyProvider to be used by this configuration.
--
-- ObjC selector: @- providerBundleIdentifier@
providerBundleIdentifier :: IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol => nednsProxyProviderProtocol -> IO (Id NSString)
providerBundleIdentifier nednsProxyProviderProtocol =
  sendMessage nednsProxyProviderProtocol providerBundleIdentifierSelector

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NEDNSProxyProvider to be used by this configuration.
--
-- ObjC selector: @- setProviderBundleIdentifier:@
setProviderBundleIdentifier :: (IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol, IsNSString value) => nednsProxyProviderProtocol -> value -> IO ()
setProviderBundleIdentifier nednsProxyProviderProtocol value =
  sendMessage nednsProxyProviderProtocol setProviderBundleIdentifierSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerConfiguration@
providerConfigurationSelector :: Selector '[] (Id NSDictionary)
providerConfigurationSelector = mkSelector "providerConfiguration"

-- | @Selector@ for @setProviderConfiguration:@
setProviderConfigurationSelector :: Selector '[Id NSDictionary] ()
setProviderConfigurationSelector = mkSelector "setProviderConfiguration:"

-- | @Selector@ for @providerBundleIdentifier@
providerBundleIdentifierSelector :: Selector '[] (Id NSString)
providerBundleIdentifierSelector = mkSelector "providerBundleIdentifier"

-- | @Selector@ for @setProviderBundleIdentifier:@
setProviderBundleIdentifierSelector :: Selector '[Id NSString] ()
setProviderBundleIdentifierSelector = mkSelector "setProviderBundleIdentifier:"

