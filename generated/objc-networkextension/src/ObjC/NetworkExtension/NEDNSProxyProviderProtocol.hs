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
-- A dictionary containing NEDNSProxyProvider vendor-specific configuration parameters. This dictionary is passed as-is to NEDNSProxyProviders when a DNS proxy is started.
--
-- ObjC selector: @- providerConfiguration@
providerConfiguration :: IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol => nednsProxyProviderProtocol -> IO (Id NSDictionary)
providerConfiguration nednsProxyProviderProtocol  =
  sendMsg nednsProxyProviderProtocol (mkSelector "providerConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | providerConfiguration
--
-- A dictionary containing NEDNSProxyProvider vendor-specific configuration parameters. This dictionary is passed as-is to NEDNSProxyProviders when a DNS proxy is started.
--
-- ObjC selector: @- setProviderConfiguration:@
setProviderConfiguration :: (IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol, IsNSDictionary value) => nednsProxyProviderProtocol -> value -> IO ()
setProviderConfiguration nednsProxyProviderProtocol  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsProxyProviderProtocol (mkSelector "setProviderConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NEDNSProxyProvider to be used by this configuration.
--
-- ObjC selector: @- providerBundleIdentifier@
providerBundleIdentifier :: IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol => nednsProxyProviderProtocol -> IO (Id NSString)
providerBundleIdentifier nednsProxyProviderProtocol  =
  sendMsg nednsProxyProviderProtocol (mkSelector "providerBundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | providerBundleIdentifier
--
-- A string containing the bundle identifier of the NEDNSProxyProvider to be used by this configuration.
--
-- ObjC selector: @- setProviderBundleIdentifier:@
setProviderBundleIdentifier :: (IsNEDNSProxyProviderProtocol nednsProxyProviderProtocol, IsNSString value) => nednsProxyProviderProtocol -> value -> IO ()
setProviderBundleIdentifier nednsProxyProviderProtocol  value =
withObjCPtr value $ \raw_value ->
    sendMsg nednsProxyProviderProtocol (mkSelector "setProviderBundleIdentifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

