{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A ``WKWebExtensionControllerConfiguration`` object with which to initialize a web extension controller.
--
-- Contains properties used to configure a ``WKWebExtensionController``.
--
-- Generated bindings for @WKWebExtensionControllerConfiguration@.
module ObjC.WebKit.WKWebExtensionControllerConfiguration
  ( WKWebExtensionControllerConfiguration
  , IsWKWebExtensionControllerConfiguration(..)
  , new
  , init_
  , defaultConfiguration
  , nonPersistentConfiguration
  , configurationWithIdentifier
  , persistent
  , identifier
  , webViewConfiguration
  , setWebViewConfiguration
  , defaultWebsiteDataStore
  , setDefaultWebsiteDataStore
  , configurationWithIdentifierSelector
  , defaultConfigurationSelector
  , defaultWebsiteDataStoreSelector
  , identifierSelector
  , initSelector
  , newSelector
  , nonPersistentConfigurationSelector
  , persistentSelector
  , setDefaultWebsiteDataStoreSelector
  , setWebViewConfigurationSelector
  , webViewConfigurationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionControllerConfiguration)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionControllerConfiguration"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id WKWebExtensionControllerConfiguration)
init_ wkWebExtensionControllerConfiguration =
  sendOwnedMessage wkWebExtensionControllerConfiguration initSelector

-- | Returns a new default configuration that is persistent and not unique.
--
-- If a ``WKWebExtensionController`` is associated with a persistent configuration, data will be written to the file system in a common location. When using multiple extension controllers, each controller should use a unique configuration to avoid conflicts.
--
-- configurationWithIdentifier:
--
-- ObjC selector: @+ defaultConfiguration@
defaultConfiguration :: IO (Id WKWebExtensionControllerConfiguration)
defaultConfiguration  =
  do
    cls' <- getRequiredClass "WKWebExtensionControllerConfiguration"
    sendClassMessage cls' defaultConfigurationSelector

-- | Returns a new non-persistent configuration.
--
-- If a ``WKWebExtensionController`` is associated with a non-persistent configuration, no data will be written to the file system. This is useful for extensions in "private browsing" situations.
--
-- ObjC selector: @+ nonPersistentConfiguration@
nonPersistentConfiguration :: IO (Id WKWebExtensionControllerConfiguration)
nonPersistentConfiguration  =
  do
    cls' <- getRequiredClass "WKWebExtensionControllerConfiguration"
    sendClassMessage cls' nonPersistentConfigurationSelector

-- | Returns a new configuration that is persistent and unique for the specified identifier.
--
-- If a ``WKWebExtensionController`` is associated with a unique persistent configuration, data will be written to the file system in a unique location based on the specified identifier.
--
-- defaultConfiguration
--
-- ObjC selector: @+ configurationWithIdentifier:@
configurationWithIdentifier :: IsNSUUID identifier => identifier -> IO (Id WKWebExtensionControllerConfiguration)
configurationWithIdentifier identifier =
  do
    cls' <- getRequiredClass "WKWebExtensionControllerConfiguration"
    sendClassMessage cls' configurationWithIdentifierSelector (toNSUUID identifier)

-- | A Boolean value indicating if this context will write data to the the file system.
--
-- ObjC selector: @- persistent@
persistent :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO Bool
persistent wkWebExtensionControllerConfiguration =
  sendMessage wkWebExtensionControllerConfiguration persistentSelector

-- | The unique identifier used for persistent configuration storage, or @nil@ when it is the default or not persistent.
--
-- ObjC selector: @- identifier@
identifier :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id NSUUID)
identifier wkWebExtensionControllerConfiguration =
  sendMessage wkWebExtensionControllerConfiguration identifierSelector

-- | The web view configuration to be used as a basis for configuring web views in extension contexts.
--
-- ObjC selector: @- webViewConfiguration@
webViewConfiguration :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id WKWebViewConfiguration)
webViewConfiguration wkWebExtensionControllerConfiguration =
  sendMessage wkWebExtensionControllerConfiguration webViewConfigurationSelector

-- | The web view configuration to be used as a basis for configuring web views in extension contexts.
--
-- ObjC selector: @- setWebViewConfiguration:@
setWebViewConfiguration :: (IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration, IsWKWebViewConfiguration value) => wkWebExtensionControllerConfiguration -> value -> IO ()
setWebViewConfiguration wkWebExtensionControllerConfiguration value =
  sendMessage wkWebExtensionControllerConfiguration setWebViewConfigurationSelector (toWKWebViewConfiguration value)

-- | The default data store for website data and cookie access in extension contexts.
--
-- This property sets the primary data store for managing website data, including cookies, which extensions can access, subject to the granted permissions within the extension contexts. Defaults to ``WKWebsiteDataStore.defaultDataStore``.
--
-- Note: In addition to this data store, extensions can also access other data stores, such as non-persistent ones, for any open tabs.
--
-- ObjC selector: @- defaultWebsiteDataStore@
defaultWebsiteDataStore :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id WKWebsiteDataStore)
defaultWebsiteDataStore wkWebExtensionControllerConfiguration =
  sendMessage wkWebExtensionControllerConfiguration defaultWebsiteDataStoreSelector

-- | The default data store for website data and cookie access in extension contexts.
--
-- This property sets the primary data store for managing website data, including cookies, which extensions can access, subject to the granted permissions within the extension contexts. Defaults to ``WKWebsiteDataStore.defaultDataStore``.
--
-- Note: In addition to this data store, extensions can also access other data stores, such as non-persistent ones, for any open tabs.
--
-- ObjC selector: @- setDefaultWebsiteDataStore:@
setDefaultWebsiteDataStore :: (IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration, IsWKWebsiteDataStore value) => wkWebExtensionControllerConfiguration -> value -> IO ()
setDefaultWebsiteDataStore wkWebExtensionControllerConfiguration value =
  sendMessage wkWebExtensionControllerConfiguration setDefaultWebsiteDataStoreSelector (toWKWebsiteDataStore value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebExtensionControllerConfiguration)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebExtensionControllerConfiguration)
initSelector = mkSelector "init"

-- | @Selector@ for @defaultConfiguration@
defaultConfigurationSelector :: Selector '[] (Id WKWebExtensionControllerConfiguration)
defaultConfigurationSelector = mkSelector "defaultConfiguration"

-- | @Selector@ for @nonPersistentConfiguration@
nonPersistentConfigurationSelector :: Selector '[] (Id WKWebExtensionControllerConfiguration)
nonPersistentConfigurationSelector = mkSelector "nonPersistentConfiguration"

-- | @Selector@ for @configurationWithIdentifier:@
configurationWithIdentifierSelector :: Selector '[Id NSUUID] (Id WKWebExtensionControllerConfiguration)
configurationWithIdentifierSelector = mkSelector "configurationWithIdentifier:"

-- | @Selector@ for @persistent@
persistentSelector :: Selector '[] Bool
persistentSelector = mkSelector "persistent"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @webViewConfiguration@
webViewConfigurationSelector :: Selector '[] (Id WKWebViewConfiguration)
webViewConfigurationSelector = mkSelector "webViewConfiguration"

-- | @Selector@ for @setWebViewConfiguration:@
setWebViewConfigurationSelector :: Selector '[Id WKWebViewConfiguration] ()
setWebViewConfigurationSelector = mkSelector "setWebViewConfiguration:"

-- | @Selector@ for @defaultWebsiteDataStore@
defaultWebsiteDataStoreSelector :: Selector '[] (Id WKWebsiteDataStore)
defaultWebsiteDataStoreSelector = mkSelector "defaultWebsiteDataStore"

-- | @Selector@ for @setDefaultWebsiteDataStore:@
setDefaultWebsiteDataStoreSelector :: Selector '[Id WKWebsiteDataStore] ()
setDefaultWebsiteDataStoreSelector = mkSelector "setDefaultWebsiteDataStore:"

