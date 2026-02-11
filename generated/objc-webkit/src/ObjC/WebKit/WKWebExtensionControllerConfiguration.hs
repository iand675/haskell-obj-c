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
  , newSelector
  , initSelector
  , defaultConfigurationSelector
  , nonPersistentConfigurationSelector
  , configurationWithIdentifierSelector
  , persistentSelector
  , identifierSelector
  , webViewConfigurationSelector
  , setWebViewConfigurationSelector
  , defaultWebsiteDataStoreSelector
  , setDefaultWebsiteDataStoreSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id WKWebExtensionControllerConfiguration)
new  =
  do
    cls' <- getRequiredClass "WKWebExtensionControllerConfiguration"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id WKWebExtensionControllerConfiguration)
init_ wkWebExtensionControllerConfiguration  =
  sendMsg wkWebExtensionControllerConfiguration (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    sendClassMsg cls' (mkSelector "defaultConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a new non-persistent configuration.
--
-- If a ``WKWebExtensionController`` is associated with a non-persistent configuration, no data will be written to the file system. This is useful for extensions in "private browsing" situations.
--
-- ObjC selector: @+ nonPersistentConfiguration@
nonPersistentConfiguration :: IO (Id WKWebExtensionControllerConfiguration)
nonPersistentConfiguration  =
  do
    cls' <- getRequiredClass "WKWebExtensionControllerConfiguration"
    sendClassMsg cls' (mkSelector "nonPersistentConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "configurationWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

-- | A Boolean value indicating if this context will write data to the the file system.
--
-- ObjC selector: @- persistent@
persistent :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO Bool
persistent wkWebExtensionControllerConfiguration  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebExtensionControllerConfiguration (mkSelector "persistent") retCULong []

-- | The unique identifier used for persistent configuration storage, or @nil@ when it is the default or not persistent.
--
-- ObjC selector: @- identifier@
identifier :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id NSUUID)
identifier wkWebExtensionControllerConfiguration  =
  sendMsg wkWebExtensionControllerConfiguration (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The web view configuration to be used as a basis for configuring web views in extension contexts.
--
-- ObjC selector: @- webViewConfiguration@
webViewConfiguration :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id WKWebViewConfiguration)
webViewConfiguration wkWebExtensionControllerConfiguration  =
  sendMsg wkWebExtensionControllerConfiguration (mkSelector "webViewConfiguration") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The web view configuration to be used as a basis for configuring web views in extension contexts.
--
-- ObjC selector: @- setWebViewConfiguration:@
setWebViewConfiguration :: (IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration, IsWKWebViewConfiguration value) => wkWebExtensionControllerConfiguration -> value -> IO ()
setWebViewConfiguration wkWebExtensionControllerConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionControllerConfiguration (mkSelector "setWebViewConfiguration:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The default data store for website data and cookie access in extension contexts.
--
-- This property sets the primary data store for managing website data, including cookies, which extensions can access, subject to the granted permissions within the extension contexts. Defaults to ``WKWebsiteDataStore.defaultDataStore``.
--
-- Note: In addition to this data store, extensions can also access other data stores, such as non-persistent ones, for any open tabs.
--
-- ObjC selector: @- defaultWebsiteDataStore@
defaultWebsiteDataStore :: IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration => wkWebExtensionControllerConfiguration -> IO (Id WKWebsiteDataStore)
defaultWebsiteDataStore wkWebExtensionControllerConfiguration  =
  sendMsg wkWebExtensionControllerConfiguration (mkSelector "defaultWebsiteDataStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The default data store for website data and cookie access in extension contexts.
--
-- This property sets the primary data store for managing website data, including cookies, which extensions can access, subject to the granted permissions within the extension contexts. Defaults to ``WKWebsiteDataStore.defaultDataStore``.
--
-- Note: In addition to this data store, extensions can also access other data stores, such as non-persistent ones, for any open tabs.
--
-- ObjC selector: @- setDefaultWebsiteDataStore:@
setDefaultWebsiteDataStore :: (IsWKWebExtensionControllerConfiguration wkWebExtensionControllerConfiguration, IsWKWebsiteDataStore value) => wkWebExtensionControllerConfiguration -> value -> IO ()
setDefaultWebsiteDataStore wkWebExtensionControllerConfiguration  value =
withObjCPtr value $ \raw_value ->
    sendMsg wkWebExtensionControllerConfiguration (mkSelector "setDefaultWebsiteDataStore:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @defaultConfiguration@
defaultConfigurationSelector :: Selector
defaultConfigurationSelector = mkSelector "defaultConfiguration"

-- | @Selector@ for @nonPersistentConfiguration@
nonPersistentConfigurationSelector :: Selector
nonPersistentConfigurationSelector = mkSelector "nonPersistentConfiguration"

-- | @Selector@ for @configurationWithIdentifier:@
configurationWithIdentifierSelector :: Selector
configurationWithIdentifierSelector = mkSelector "configurationWithIdentifier:"

-- | @Selector@ for @persistent@
persistentSelector :: Selector
persistentSelector = mkSelector "persistent"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @webViewConfiguration@
webViewConfigurationSelector :: Selector
webViewConfigurationSelector = mkSelector "webViewConfiguration"

-- | @Selector@ for @setWebViewConfiguration:@
setWebViewConfigurationSelector :: Selector
setWebViewConfigurationSelector = mkSelector "setWebViewConfiguration:"

-- | @Selector@ for @defaultWebsiteDataStore@
defaultWebsiteDataStoreSelector :: Selector
defaultWebsiteDataStoreSelector = mkSelector "defaultWebsiteDataStore"

-- | @Selector@ for @setDefaultWebsiteDataStore:@
setDefaultWebsiteDataStoreSelector :: Selector
setDefaultWebsiteDataStoreSelector = mkSelector "setDefaultWebsiteDataStore:"

