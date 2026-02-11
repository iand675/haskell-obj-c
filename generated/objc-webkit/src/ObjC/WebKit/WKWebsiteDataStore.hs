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
  , defaultDataStoreSelector
  , nonPersistentDataStoreSelector
  , newSelector
  , initSelector
  , allWebsiteDataTypesSelector
  , removeDataOfTypes_forDataRecords_completionHandlerSelector
  , removeDataOfTypes_modifiedSince_completionHandlerSelector
  , fetchDataOfTypes_completionHandlerSelector
  , restoreData_completionHandlerSelector
  , dataStoreForIdentifierSelector
  , removeDataStoreForIdentifier_completionHandlerSelector
  , persistentSelector
  , httpCookieStoreSelector
  , identifierSelector


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

-- | @+ defaultDataStore@
defaultDataStore :: IO (Id WKWebsiteDataStore)
defaultDataStore  =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMsg cls' (mkSelector "defaultDataStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a new non-persistent data store.
--
-- If a WKWebView is associated with a non-persistent data store, no data will be written to the file system. This is useful for implementing "private browsing" in a web view.
--
-- ObjC selector: @+ nonPersistentDataStore@
nonPersistentDataStore :: IO (Id WKWebsiteDataStore)
nonPersistentDataStore  =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMsg cls' (mkSelector "nonPersistentDataStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- new@
new :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id WKWebsiteDataStore)
new wkWebsiteDataStore  =
  sendMsg wkWebsiteDataStore (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id WKWebsiteDataStore)
init_ wkWebsiteDataStore  =
  sendMsg wkWebsiteDataStore (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Returns a set of all available website data types.
--
-- ObjC selector: @+ allWebsiteDataTypes@
allWebsiteDataTypes :: IO (Id NSSet)
allWebsiteDataTypes  =
  do
    cls' <- getRequiredClass "WKWebsiteDataStore"
    sendClassMsg cls' (mkSelector "allWebsiteDataTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

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
removeDataOfTypes_forDataRecords_completionHandler wkWebsiteDataStore  dataTypes dataRecords completionHandler =
withObjCPtr dataTypes $ \raw_dataTypes ->
  withObjCPtr dataRecords $ \raw_dataRecords ->
      sendMsg wkWebsiteDataStore (mkSelector "removeDataOfTypes:forDataRecords:completionHandler:") retVoid [argPtr (castPtr raw_dataTypes :: Ptr ()), argPtr (castPtr raw_dataRecords :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
removeDataOfTypes_modifiedSince_completionHandler wkWebsiteDataStore  dataTypes date completionHandler =
withObjCPtr dataTypes $ \raw_dataTypes ->
  withObjCPtr date $ \raw_date ->
      sendMsg wkWebsiteDataStore (mkSelector "removeDataOfTypes:modifiedSince:completionHandler:") retVoid [argPtr (castPtr raw_dataTypes :: Ptr ()), argPtr (castPtr raw_date :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandler :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSSet dataTypes) => wkWebsiteDataStore -> dataTypes -> Ptr () -> IO ()
fetchDataOfTypes_completionHandler wkWebsiteDataStore  dataTypes completionHandler =
withObjCPtr dataTypes $ \raw_dataTypes ->
    sendMsg wkWebsiteDataStore (mkSelector "fetchDataOfTypes:completionHandler:") retVoid [argPtr (castPtr raw_dataTypes :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | @- restoreData:completionHandler:@
restoreData_completionHandler :: (IsWKWebsiteDataStore wkWebsiteDataStore, IsNSData data_) => wkWebsiteDataStore -> data_ -> Ptr () -> IO ()
restoreData_completionHandler wkWebsiteDataStore  data_ completionHandler =
withObjCPtr data_ $ \raw_data_ ->
    sendMsg wkWebsiteDataStore (mkSelector "restoreData:completionHandler:") retVoid [argPtr (castPtr raw_data_ :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

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
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "dataStoreForIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= retainedObject . castPtr

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
    withObjCPtr identifier $ \raw_identifier ->
      sendClassMsg cls' (mkSelector "removeDataStoreForIdentifier:completionHandler:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr completionHandler :: Ptr ())]

-- | Whether the data store is persistent or not.
--
-- ObjC selector: @- persistent@
persistent :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO Bool
persistent wkWebsiteDataStore  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg wkWebsiteDataStore (mkSelector "persistent") retCULong []

-- | Returns the cookie store representing HTTP cookies in this website data store.
--
-- ObjC selector: @- httpCookieStore@
httpCookieStore :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id WKHTTPCookieStore)
httpCookieStore wkWebsiteDataStore  =
  sendMsg wkWebsiteDataStore (mkSelector "httpCookieStore") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Get identifier for a data store.
--
-- Returns nil for default and non-persistent data store .
--
-- ObjC selector: @- identifier@
identifier :: IsWKWebsiteDataStore wkWebsiteDataStore => wkWebsiteDataStore -> IO (Id NSUUID)
identifier wkWebsiteDataStore  =
  sendMsg wkWebsiteDataStore (mkSelector "identifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @defaultDataStore@
defaultDataStoreSelector :: Selector
defaultDataStoreSelector = mkSelector "defaultDataStore"

-- | @Selector@ for @nonPersistentDataStore@
nonPersistentDataStoreSelector :: Selector
nonPersistentDataStoreSelector = mkSelector "nonPersistentDataStore"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @allWebsiteDataTypes@
allWebsiteDataTypesSelector :: Selector
allWebsiteDataTypesSelector = mkSelector "allWebsiteDataTypes"

-- | @Selector@ for @removeDataOfTypes:forDataRecords:completionHandler:@
removeDataOfTypes_forDataRecords_completionHandlerSelector :: Selector
removeDataOfTypes_forDataRecords_completionHandlerSelector = mkSelector "removeDataOfTypes:forDataRecords:completionHandler:"

-- | @Selector@ for @removeDataOfTypes:modifiedSince:completionHandler:@
removeDataOfTypes_modifiedSince_completionHandlerSelector :: Selector
removeDataOfTypes_modifiedSince_completionHandlerSelector = mkSelector "removeDataOfTypes:modifiedSince:completionHandler:"

-- | @Selector@ for @fetchDataOfTypes:completionHandler:@
fetchDataOfTypes_completionHandlerSelector :: Selector
fetchDataOfTypes_completionHandlerSelector = mkSelector "fetchDataOfTypes:completionHandler:"

-- | @Selector@ for @restoreData:completionHandler:@
restoreData_completionHandlerSelector :: Selector
restoreData_completionHandlerSelector = mkSelector "restoreData:completionHandler:"

-- | @Selector@ for @dataStoreForIdentifier:@
dataStoreForIdentifierSelector :: Selector
dataStoreForIdentifierSelector = mkSelector "dataStoreForIdentifier:"

-- | @Selector@ for @removeDataStoreForIdentifier:completionHandler:@
removeDataStoreForIdentifier_completionHandlerSelector :: Selector
removeDataStoreForIdentifier_completionHandlerSelector = mkSelector "removeDataStoreForIdentifier:completionHandler:"

-- | @Selector@ for @persistent@
persistentSelector :: Selector
persistentSelector = mkSelector "persistent"

-- | @Selector@ for @httpCookieStore@
httpCookieStoreSelector :: Selector
httpCookieStoreSelector = mkSelector "httpCookieStore"

-- | @Selector@ for @identifier@
identifierSelector :: Selector
identifierSelector = mkSelector "identifier"

