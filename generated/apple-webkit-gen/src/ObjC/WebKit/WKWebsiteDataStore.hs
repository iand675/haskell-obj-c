{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A WKWebsiteDataStore represents various types of data that a website might make use of. This includes cookies, disk and memory caches, and persistent data such as WebSQL, IndexedDB databases, and local storage.
--
-- Generated bindings for @WKWebsiteDataStore@.
module ObjC.WebKit.WKWebsiteDataStore
  ( WKWebsiteDataStore
  , IsWKWebsiteDataStore(..)
  , defaultDataStore
  , nonPersistentDataStore
  , new
  , init_
  , allWebsiteDataTypes
  , removeDataOfTypes_forDataRecords_completionHandler
  , removeDataOfTypes_modifiedSince_completionHandler
  , fetchDataOfTypes_completionHandler
  , restoreData_completionHandler
  , dataStoreForIdentifier
  , removeDataStoreForIdentifier_completionHandler
  , persistent
  , httpCookieStore
  , identifier
  , proxyConfigurations
  , setProxyConfigurations
  , allWebsiteDataTypesSelector
  , dataStoreForIdentifierSelector
  , defaultDataStoreSelector
  , fetchDataOfTypes_completionHandlerSelector
  , httpCookieStoreSelector
  , identifierSelector
  , initSelector
  , newSelector
  , nonPersistentDataStoreSelector
  , persistentSelector
  , proxyConfigurationsSelector
  , removeDataOfTypes_forDataRecords_completionHandlerSelector
  , removeDataOfTypes_modifiedSince_completionHandlerSelector
  , removeDataStoreForIdentifier_completionHandlerSelector
  , restoreData_completionHandlerSelector
  , setProxyConfigurationsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ defaultDataStore@
defaultDataStore :: IO (Id WKWebsiteDataStore)
defaultDataStore  =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMessage cls' defaultDataStoreSelector

-- | Returns a new non-persistent data store.
--
-- If a WKWebView is associated with a non-persistent data store, no data will be written to the file system. This is useful for implementing "private browsing" in a web view.
--
-- ObjC selector: @+ nonPersistentDataStore@
nonPersistentDataStore :: IO (Id WKWebsiteDataStore)
nonPersistentDataStore  =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMessage cls' nonPersistentDataStoreSelector

-- | @- new@
new :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id WKWebsiteDataStore)
new wkWebsiteDataStore =
  sendOwnedMessage wkWebsiteDataStore newSelector

-- | @- init@
init_ :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id WKWebsiteDataStore)
init_ wkWebsiteDataStore =
  sendOwnedMessage wkWebsiteDataStore initSelector

-- | Returns a set of all available website data types.
--
-- ObjC selector: @+ allWebsiteDataTypes@
allWebsiteDataTypes :: IO (Id NSSet)
allWebsiteDataTypes  =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMessage cls' allWebsiteDataTypesSelector

-- | Removes website data of the given types for the given data records.
--
-- @dataTypes@ — The website data types that should be removed.
--
-- @dataRecords@ — The website data records to delete website data for.
--
-- @completionHandler@ — A block to invoke when the website data for the records has been removed.
--
-- ObjC selector: @- removeDataOfTypes:forDataRecords:completionHandler:@
removeDataOfTypes_forDataRecords_completionHandler :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSSet dataTypes, IsNSArray dataRecords) => wkWebsiteDataStore -> dataTypes -> dataRecords -> Ptr () -> IO ()
removeDataOfTypes_forDataRecords_completionHandler wkWebsiteDataStore dataTypes dataRecords completionHandler =
  sendMessage wkWebsiteDataStore removeDataOfTypes_forDataRecords_completionHandlerSelector (toNSSet dataTypes) (toNSArray dataRecords) completionHandler

-- | Removes all website data of the given types that has been modified since the given date.
--
-- @dataTypes@ — The website data types that should be removed.
--
-- @date@ — A date. All website data modified after this date will be removed.
--
-- @completionHandler@ — A block to invoke when the website data has been removed.
--
-- ObjC selector: @- removeDataOfTypes:modifiedSince:completionHandler:@
removeDataOfTypes_modifiedSince_completionHandler :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSSet dataTypes, IsNSDate date) => wkWebsiteDataStore -> dataTypes -> date -> Ptr () -> IO ()
removeDataOfTypes_modifiedSince_completionHandler wkWebsiteDataStore dataTypes date completionHandler =
  sendMessage wkWebsiteDataStore removeDataOfTypes_modifiedSince_completionHandlerSelector (toNSSet dataTypes) (toNSDate date) completionHandler

-- | @- fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandler :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSSet dataTypes) => wkWebsiteDataStore -> dataTypes -> Ptr () -> IO ()
fetchDataOfTypes_completionHandler wkWebsiteDataStore dataTypes completionHandler =
  sendMessage wkWebsiteDataStore fetchDataOfTypes_completionHandlerSelector (toNSSet dataTypes) completionHandler

-- | @- restoreData:completionHandler:@
restoreData_completionHandler :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSData data_) => wkWebsiteDataStore -> data_ -> Ptr () -> IO ()
restoreData_completionHandler wkWebsiteDataStore data_ completionHandler =
  sendMessage wkWebsiteDataStore restoreData_completionHandlerSelector (toNSData data_) completionHandler

-- | Get a persistent data store.
--
-- @identifier@ — An identifier that is used to uniquely identify the data store.
--
-- If a data store with this identifier does not exist yet, it will be created. Throws exception if identifier is 0.
--
-- ObjC selector: @+ dataStoreForIdentifier:@
dataStoreForIdentifier :: IsNSUUID identifier => identifier -> IO (Id WKWebsiteDataStore)
dataStoreForIdentifier identifier =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMessage cls' dataStoreForIdentifierSelector (toNSUUID identifier)

-- | Delete a persistent data store.
--
-- @identifier@ — An identifier that is used to uniquely identify the data store.
--
-- @completionHandler@ — A block to invoke with optional error when the operation completes.
--
-- This should be called when the data store is not used any more. Returns error if removal fails to complete. WKWebView using the data store must be released before removal.
--
-- ObjC selector: @+ removeDataStoreForIdentifier:completionHandler:@
removeDataStoreForIdentifier_completionHandler :: IsNSUUID identifier => identifier -> Ptr () -> IO ()
removeDataStoreForIdentifier_completionHandler identifier completionHandler =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMessage cls' removeDataStoreForIdentifier_completionHandlerSelector (toNSUUID identifier) completionHandler

-- | Whether the data store is persistent or not.
--
-- ObjC selector: @- persistent@
persistent :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO Bool
persistent wkWebsiteDataStore =
  sendMessage wkWebsiteDataStore persistentSelector

-- | Returns the cookie store representing HTTP cookies in this website data store.
--
-- ObjC selector: @- httpCookieStore@
httpCookieStore :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id WKHTTPCookieStore)
httpCookieStore wkWebsiteDataStore =
  sendMessage wkWebsiteDataStore httpCookieStoreSelector

-- | Get identifier for a data store.
--
-- Returns nil for default and non-persistent data store .
--
-- ObjC selector: @- identifier@
identifier :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id NSUUID)
identifier wkWebsiteDataStore =
  sendMessage wkWebsiteDataStore identifierSelector

-- | @- proxyConfigurations@
proxyConfigurations :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id NSArray)
proxyConfigurations wkWebsiteDataStore =
  sendMessage wkWebsiteDataStore proxyConfigurationsSelector

-- | @- setProxyConfigurations:@
setProxyConfigurations :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSArray value) => wkWebsiteDataStore -> value -> IO ()
setProxyConfigurations wkWebsiteDataStore value =
  sendMessage wkWebsiteDataStore setProxyConfigurationsSelector (toNSArray value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultDataStore@
defaultDataStoreSelector :: Selector '[] (Id WKWebsiteDataStore)
defaultDataStoreSelector = mkSelector "defaultDataStore"

-- | @Selector@ for @nonPersistentDataStore@
nonPersistentDataStoreSelector :: Selector '[] (Id WKWebsiteDataStore)
nonPersistentDataStoreSelector = mkSelector "nonPersistentDataStore"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id WKWebsiteDataStore)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id WKWebsiteDataStore)
initSelector = mkSelector "init"

-- | @Selector@ for @allWebsiteDataTypes@
allWebsiteDataTypesSelector :: Selector '[] (Id NSSet)
allWebsiteDataTypesSelector = mkSelector "allWebsiteDataTypes"

-- | @Selector@ for @removeDataOfTypes:forDataRecords:completionHandler:@
removeDataOfTypes_forDataRecords_completionHandlerSelector :: Selector '[Id NSSet, Id NSArray, Ptr ()] ()
removeDataOfTypes_forDataRecords_completionHandlerSelector = mkSelector "removeDataOfTypes:forDataRecords:completionHandler:"

-- | @Selector@ for @removeDataOfTypes:modifiedSince:completionHandler:@
removeDataOfTypes_modifiedSince_completionHandlerSelector :: Selector '[Id NSSet, Id NSDate, Ptr ()] ()
removeDataOfTypes_modifiedSince_completionHandlerSelector = mkSelector "removeDataOfTypes:modifiedSince:completionHandler:"

-- | @Selector@ for @fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandlerSelector :: Selector '[Id NSSet, Ptr ()] ()
fetchDataOfTypes_completionHandlerSelector = mkSelector "fetchDataOfTypes:completionHandler:"

-- | @Selector@ for @restoreData:completionHandler:@
restoreData_completionHandlerSelector :: Selector '[Id NSData, Ptr ()] ()
restoreData_completionHandlerSelector = mkSelector "restoreData:completionHandler:"

-- | @Selector@ for @dataStoreForIdentifier:@
dataStoreForIdentifierSelector :: Selector '[Id NSUUID] (Id WKWebsiteDataStore)
dataStoreForIdentifierSelector = mkSelector "dataStoreForIdentifier:"

-- | @Selector@ for @removeDataStoreForIdentifier:completionHandler:@
removeDataStoreForIdentifier_completionHandlerSelector :: Selector '[Id NSUUID, Ptr ()] ()
removeDataStoreForIdentifier_completionHandlerSelector = mkSelector "removeDataStoreForIdentifier:completionHandler:"

-- | @Selector@ for @persistent@
persistentSelector :: Selector '[] Bool
persistentSelector = mkSelector "persistent"

-- | @Selector@ for @httpCookieStore@
httpCookieStoreSelector :: Selector '[] (Id WKHTTPCookieStore)
httpCookieStoreSelector = mkSelector "httpCookieStore"

-- | @Selector@ for @identifier@
identifierSelector :: Selector '[] (Id NSUUID)
identifierSelector = mkSelector "identifier"

-- | @Selector@ for @proxyConfigurations@
proxyConfigurationsSelector :: Selector '[] (Id NSArray)
proxyConfigurationsSelector = mkSelector "proxyConfigurations"

-- | @Selector@ for @setProxyConfigurations:@
setProxyConfigurationsSelector :: Selector '[Id NSArray] ()
setProxyConfigurationsSelector = mkSelector "setProxyConfigurations:"

