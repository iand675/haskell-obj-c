{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | NETunnelProviderProtocol
--
-- The NETunnelProviderProtocol class declares the programmatic interface for an object that contains NETunnelProvider-specific configuration settings.
--
-- Instances of this class are thread safe.
--
-- Generated bindings for @NETunnelProviderProtocol@.
module ObjC.NetworkExtension.NETunnelProviderProtocol
  ( NETunnelProviderProtocol
  , IsNETunnelProviderProtocol(..)
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
-- A dictionary containing NETunnelProvider vendor-specific configuration parameters. This dictionary is passed as-is to NETunnelProviders when a tunnel is started.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNETunnelProviderProtocol neTunnelProviderProtocol => neTunnelProviderProtocol -> IO (Id NSDictionary)
providerConfiguration neTunnelProviderProtocol =
  sendMessage neTunnelProviderProtocol providerConfigurationSelector

-- | providerConfiguration
--
-- A dictionary containing NETunnelProvider vendor-specific configuration parameters. This dictionary is passed as-is to NETunnelProviders when a tunnel is started.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNETunnelProviderProtocol neTunnelProviderProtocol, IsNSDictionary value) => neTunnelProviderProtocol -> value -> IO ()
setProviderConfiguration neTunnelProviderProtocol value =
  sendMessage neTunnelProviderProtocol setProviderConfigurationSelector (toNSDictionary value)

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NETunnelProvider to be used by this configuration.
--
-- ObjC selector: @- providerBundleIdentifier@
providerBundleIdentifier :: IsNETunnelProviderProtocol neTunnelProviderProtocol => neTunnelProviderProtocol -> IO (Id NSString)
providerBundleIdentifier neTunnelProviderProtocol =
  sendMessage neTunnelProviderProtocol providerBundleIdentifierSelector

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NETunnelProvider to be used by this configuration.
--
-- ObjC selector: @- setProviderBundleIdentifier:@
setProviderBundleIdentifier :: (IsNETunnelProviderProtocol neTunnelProviderProtocol, IsNSString value) => neTunnelProviderProtocol -> value -> IO ()
setProviderBundleIdentifier neTunnelProviderProtocol value =
  sendMessage neTunnelProviderProtocol setProviderBundleIdentifierSelector (toNSString value)

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

