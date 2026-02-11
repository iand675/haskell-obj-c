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
  , providerConfigurationSelector
  , setProviderConfigurationSelector
  , providerBundleIdentifierSelector
  , setProviderBundleIdentifierSelector


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

-- | providerConfiguration
--
-- A dictionary containing NETunnelProvider vendor-specific configuration parameters. This dictionary is passed as-is to NETunnelProviders when a tunnel is started.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNETunnelProviderProtocol neTunnelProviderProtocol => neTunnelProviderProtocol -> IO (Id NSDictionary)
providerConfiguration neTunnelProviderProtocol  =
    sendMsg neTunnelProviderProtocol (mkSelector "providerConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | providerConfiguration
--
-- A dictionary containing NETunnelProvider vendor-specific configuration parameters. This dictionary is passed as-is to NETunnelProviders when a tunnel is started.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNETunnelProviderProtocol neTunnelProviderProtocol, IsNSDictionary value) => neTunnelProviderProtocol -> value -> IO ()
setProviderConfiguration neTunnelProviderProtocol  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderProtocol (mkSelector "setProviderConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NETunnelProvider to be used by this configuration.
--
-- ObjC selector: @- providerBundleIdentifier@
providerBundleIdentifier :: IsNETunnelProviderProtocol neTunnelProviderProtocol => neTunnelProviderProtocol -> IO (Id NSString)
providerBundleIdentifier neTunnelProviderProtocol  =
    sendMsg neTunnelProviderProtocol (mkSelector "providerBundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NETunnelProvider to be used by this configuration.
--
-- ObjC selector: @- setProviderBundleIdentifier:@
setProviderBundleIdentifier :: (IsNETunnelProviderProtocol neTunnelProviderProtocol, IsNSString value) => neTunnelProviderProtocol -> value -> IO ()
setProviderBundleIdentifier neTunnelProviderProtocol  value =
  withObjCPtr value $ \raw_value ->
      sendMsg neTunnelProviderProtocol (mkSelector "setProviderBundleIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @providerConfiguration@
providerConfigurationSelector :: Selector
providerConfigurationSelector = mkSelector "providerConfiguration"

-- | @Selector@ for @setProviderConfiguration:@
setProviderConfigurationSelector :: Selector
setProviderConfigurationSelector = mkSelector "setProviderConfiguration:"

-- | @Selector@ for @providerBundleIdentifier@
providerBundleIdentifierSelector :: Selector
providerBundleIdentifierSelector = mkSelector "providerBundleIdentifier"

-- | @Selector@ for @setProviderBundleIdentifier:@
setProviderBundleIdentifierSelector :: Selector
setProviderBundleIdentifierSelector = mkSelector "setProviderBundleIdentifier:"

