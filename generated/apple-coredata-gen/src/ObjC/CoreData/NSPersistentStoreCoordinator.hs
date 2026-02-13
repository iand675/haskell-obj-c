{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSPersistentStoreCoordinator@.
module ObjC.CoreData.NSPersistentStoreCoordinator
  ( NSPersistentStoreCoordinator
  , IsNSPersistentStoreCoordinator(..)
  , initWithManagedObjectModel
  , persistentStoreForURL
  , urlForPersistentStore
  , setURL_forPersistentStore
  , addPersistentStoreWithType_configuration_URL_options_error
  , addPersistentStoreWithDescription_completionHandler
  , removePersistentStore_error
  , setMetadata_forPersistentStore
  , metadataForPersistentStore
  , managedObjectIDForURIRepresentation
  , executeRequest_withContext_error
  , registerStoreClass_forStoreType
  , metadataForPersistentStoreOfType_URL_options_error
  , setMetadata_forPersistentStoreOfType_URL_options_error
  , elementsDerivedFromExternalRecordURL
  , importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error
  , migratePersistentStore_toURL_options_withType_error
  , destroyPersistentStoreAtURL_withType_options_error
  , replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_error
  , performBlock
  , performBlockAndWait
  , currentPersistentHistoryTokenFromStores
  , finishDeferredLightweightMigration
  , finishDeferredLightweightMigrationTask
  , managedObjectIDFromUTF8String_length
  , metadataForPersistentStoreWithURL_error
  , lock
  , unlock
  , tryLock
  , metadataForPersistentStoreOfType_URL_error
  , setMetadata_forPersistentStoreOfType_URL_error
  , removeUbiquitousContentAndPersistentStoreAtURL_options_error
  , managedObjectModel
  , persistentStores
  , name
  , setName
  , registeredStoreTypes
  , addPersistentStoreWithDescription_completionHandlerSelector
  , addPersistentStoreWithType_configuration_URL_options_errorSelector
  , currentPersistentHistoryTokenFromStoresSelector
  , destroyPersistentStoreAtURL_withType_options_errorSelector
  , elementsDerivedFromExternalRecordURLSelector
  , executeRequest_withContext_errorSelector
  , finishDeferredLightweightMigrationSelector
  , finishDeferredLightweightMigrationTaskSelector
  , importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector
  , initWithManagedObjectModelSelector
  , lockSelector
  , managedObjectIDForURIRepresentationSelector
  , managedObjectIDFromUTF8String_lengthSelector
  , managedObjectModelSelector
  , metadataForPersistentStoreOfType_URL_errorSelector
  , metadataForPersistentStoreOfType_URL_options_errorSelector
  , metadataForPersistentStoreSelector
  , metadataForPersistentStoreWithURL_errorSelector
  , migratePersistentStore_toURL_options_withType_errorSelector
  , nameSelector
  , performBlockAndWaitSelector
  , performBlockSelector
  , persistentStoreForURLSelector
  , persistentStoresSelector
  , registerStoreClass_forStoreTypeSelector
  , registeredStoreTypesSelector
  , removePersistentStore_errorSelector
  , removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector
  , replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector
  , setMetadata_forPersistentStoreOfType_URL_errorSelector
  , setMetadata_forPersistentStoreOfType_URL_options_errorSelector
  , setMetadata_forPersistentStoreSelector
  , setNameSelector
  , setURL_forPersistentStoreSelector
  , tryLockSelector
  , unlockSelector
  , urlForPersistentStoreSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithManagedObjectModel:@
initWithManagedObjectModel :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSManagedObjectModel model) => nsPersistentStoreCoordinator -> model -> IO (Id NSPersistentStoreCoordinator)
initWithManagedObjectModel nsPersistentStoreCoordinator model =
  sendOwnedMessage nsPersistentStoreCoordinator initWithManagedObjectModelSelector (toNSManagedObjectModel model)

-- | @- persistentStoreForURL:@
persistentStoreForURL :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url) => nsPersistentStoreCoordinator -> url -> IO (Id NSPersistentStore)
persistentStoreForURL nsPersistentStoreCoordinator url =
  sendMessage nsPersistentStoreCoordinator persistentStoreForURLSelector (toNSURL url)

-- | @- URLForPersistentStore:@
urlForPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> store -> IO (Id NSURL)
urlForPersistentStore nsPersistentStoreCoordinator store =
  sendMessage nsPersistentStoreCoordinator urlForPersistentStoreSelector (toNSPersistentStore store)

-- | @- setURL:forPersistentStore:@
setURL_forPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> url -> store -> IO Bool
setURL_forPersistentStore nsPersistentStoreCoordinator url store =
  sendMessage nsPersistentStoreCoordinator setURL_forPersistentStoreSelector (toNSURL url) (toNSPersistentStore store)

-- | @- addPersistentStoreWithType:configuration:URL:options:error:@
addPersistentStoreWithType_configuration_URL_options_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSString storeType, IsNSString configuration, IsNSURL storeURL, IsNSDictionary options, IsNSError error_) => nsPersistentStoreCoordinator -> storeType -> configuration -> storeURL -> options -> error_ -> IO (Id NSPersistentStore)
addPersistentStoreWithType_configuration_URL_options_error nsPersistentStoreCoordinator storeType configuration storeURL options error_ =
  sendMessage nsPersistentStoreCoordinator addPersistentStoreWithType_configuration_URL_options_errorSelector (toNSString storeType) (toNSString configuration) (toNSURL storeURL) (toNSDictionary options) (toNSError error_)

-- | @- addPersistentStoreWithDescription:completionHandler:@
addPersistentStoreWithDescription_completionHandler :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStoreDescription storeDescription) => nsPersistentStoreCoordinator -> storeDescription -> Ptr () -> IO ()
addPersistentStoreWithDescription_completionHandler nsPersistentStoreCoordinator storeDescription block =
  sendMessage nsPersistentStoreCoordinator addPersistentStoreWithDescription_completionHandlerSelector (toNSPersistentStoreDescription storeDescription) block

-- | @- removePersistentStore:error:@
removePersistentStore_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store, IsNSError error_) => nsPersistentStoreCoordinator -> store -> error_ -> IO Bool
removePersistentStore_error nsPersistentStoreCoordinator store error_ =
  sendMessage nsPersistentStoreCoordinator removePersistentStore_errorSelector (toNSPersistentStore store) (toNSError error_)

-- | @- setMetadata:forPersistentStore:@
setMetadata_forPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSDictionary metadata, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> metadata -> store -> IO ()
setMetadata_forPersistentStore nsPersistentStoreCoordinator metadata store =
  sendMessage nsPersistentStoreCoordinator setMetadata_forPersistentStoreSelector (toNSDictionary metadata) (toNSPersistentStore store)

-- | @- metadataForPersistentStore:@
metadataForPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> store -> IO (Id NSDictionary)
metadataForPersistentStore nsPersistentStoreCoordinator store =
  sendMessage nsPersistentStoreCoordinator metadataForPersistentStoreSelector (toNSPersistentStore store)

-- | @- managedObjectIDForURIRepresentation:@
managedObjectIDForURIRepresentation :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url) => nsPersistentStoreCoordinator -> url -> IO (Id NSManagedObjectID)
managedObjectIDForURIRepresentation nsPersistentStoreCoordinator url =
  sendMessage nsPersistentStoreCoordinator managedObjectIDForURIRepresentationSelector (toNSURL url)

-- | @- executeRequest:withContext:error:@
executeRequest_withContext_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStoreRequest request, IsNSManagedObjectContext context, IsNSError error_) => nsPersistentStoreCoordinator -> request -> context -> error_ -> IO RawId
executeRequest_withContext_error nsPersistentStoreCoordinator request context error_ =
  sendMessage nsPersistentStoreCoordinator executeRequest_withContext_errorSelector (toNSPersistentStoreRequest request) (toNSManagedObjectContext context) (toNSError error_)

-- | @+ registerStoreClass:forStoreType:@
registerStoreClass_forStoreType :: IsNSString storeType => Class -> storeType -> IO ()
registerStoreClass_forStoreType storeClass storeType =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' registerStoreClass_forStoreTypeSelector storeClass (toNSString storeType)

-- | @+ metadataForPersistentStoreOfType:URL:options:error:@
metadataForPersistentStoreOfType_URL_options_error :: (IsNSString storeType, IsNSURL url, IsNSDictionary options, IsNSError error_) => storeType -> url -> options -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreOfType_URL_options_error storeType url options error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' metadataForPersistentStoreOfType_URL_options_errorSelector (toNSString storeType) (toNSURL url) (toNSDictionary options) (toNSError error_)

-- | @+ setMetadata:forPersistentStoreOfType:URL:options:error:@
setMetadata_forPersistentStoreOfType_URL_options_error :: (IsNSDictionary metadata, IsNSString storeType, IsNSURL url, IsNSDictionary options, IsNSError error_) => metadata -> storeType -> url -> options -> error_ -> IO Bool
setMetadata_forPersistentStoreOfType_URL_options_error metadata storeType url options error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' setMetadata_forPersistentStoreOfType_URL_options_errorSelector (toNSDictionary metadata) (toNSString storeType) (toNSURL url) (toNSDictionary options) (toNSError error_)

-- | @+ elementsDerivedFromExternalRecordURL:@
elementsDerivedFromExternalRecordURL :: IsNSURL fileURL => fileURL -> IO (Id NSDictionary)
elementsDerivedFromExternalRecordURL fileURL =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' elementsDerivedFromExternalRecordURLSelector (toNSURL fileURL)

-- | @- importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:@
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSString storeIdentifier, IsNSURL externalRecordsURL, IsNSURL destinationURL, IsNSDictionary options, IsNSString storeType, IsNSError error_) => nsPersistentStoreCoordinator -> storeIdentifier -> externalRecordsURL -> destinationURL -> options -> storeType -> error_ -> IO (Id NSPersistentStore)
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error nsPersistentStoreCoordinator storeIdentifier externalRecordsURL destinationURL options storeType error_ =
  sendMessage nsPersistentStoreCoordinator importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector (toNSString storeIdentifier) (toNSURL externalRecordsURL) (toNSURL destinationURL) (toNSDictionary options) (toNSString storeType) (toNSError error_)

-- | @- migratePersistentStore:toURL:options:withType:error:@
migratePersistentStore_toURL_options_withType_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store, IsNSURL url, IsNSDictionary options, IsNSString storeType, IsNSError error_) => nsPersistentStoreCoordinator -> store -> url -> options -> storeType -> error_ -> IO (Id NSPersistentStore)
migratePersistentStore_toURL_options_withType_error nsPersistentStoreCoordinator store url options storeType error_ =
  sendMessage nsPersistentStoreCoordinator migratePersistentStore_toURL_options_withType_errorSelector (toNSPersistentStore store) (toNSURL url) (toNSDictionary options) (toNSString storeType) (toNSError error_)

-- | @- destroyPersistentStoreAtURL:withType:options:error:@
destroyPersistentStoreAtURL_withType_options_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url, IsNSString storeType, IsNSDictionary options, IsNSError error_) => nsPersistentStoreCoordinator -> url -> storeType -> options -> error_ -> IO Bool
destroyPersistentStoreAtURL_withType_options_error nsPersistentStoreCoordinator url storeType options error_ =
  sendMessage nsPersistentStoreCoordinator destroyPersistentStoreAtURL_withType_options_errorSelector (toNSURL url) (toNSString storeType) (toNSDictionary options) (toNSError error_)

-- | @- replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:@
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL destinationURL, IsNSDictionary destinationOptions, IsNSURL sourceURL, IsNSDictionary sourceOptions, IsNSString storeType, IsNSError error_) => nsPersistentStoreCoordinator -> destinationURL -> destinationOptions -> sourceURL -> sourceOptions -> storeType -> error_ -> IO Bool
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_error nsPersistentStoreCoordinator destinationURL destinationOptions sourceURL sourceOptions storeType error_ =
  sendMessage nsPersistentStoreCoordinator replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector (toNSURL destinationURL) (toNSDictionary destinationOptions) (toNSURL sourceURL) (toNSDictionary sourceOptions) (toNSString storeType) (toNSError error_)

-- | @- performBlock:@
performBlock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> Ptr () -> IO ()
performBlock nsPersistentStoreCoordinator block =
  sendMessage nsPersistentStoreCoordinator performBlockSelector block

-- | @- performBlockAndWait:@
performBlockAndWait :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> Ptr () -> IO ()
performBlockAndWait nsPersistentStoreCoordinator block =
  sendMessage nsPersistentStoreCoordinator performBlockAndWaitSelector block

-- | @- currentPersistentHistoryTokenFromStores:@
currentPersistentHistoryTokenFromStores :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSArray stores) => nsPersistentStoreCoordinator -> stores -> IO (Id NSPersistentHistoryToken)
currentPersistentHistoryTokenFromStores nsPersistentStoreCoordinator stores =
  sendMessage nsPersistentStoreCoordinator currentPersistentHistoryTokenFromStoresSelector (toNSArray stores)

-- | @- finishDeferredLightweightMigration:@
finishDeferredLightweightMigration :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSError error_) => nsPersistentStoreCoordinator -> error_ -> IO Bool
finishDeferredLightweightMigration nsPersistentStoreCoordinator error_ =
  sendMessage nsPersistentStoreCoordinator finishDeferredLightweightMigrationSelector (toNSError error_)

-- | @- finishDeferredLightweightMigrationTask:@
finishDeferredLightweightMigrationTask :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSError error_) => nsPersistentStoreCoordinator -> error_ -> IO Bool
finishDeferredLightweightMigrationTask nsPersistentStoreCoordinator error_ =
  sendMessage nsPersistentStoreCoordinator finishDeferredLightweightMigrationTaskSelector (toNSError error_)

-- | @- managedObjectIDFromUTF8String:length:@
managedObjectIDFromUTF8String_length :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> Const (Ptr CChar) -> CULong -> IO (Id NSManagedObjectID)
managedObjectIDFromUTF8String_length nsPersistentStoreCoordinator utf8string len =
  sendMessage nsPersistentStoreCoordinator managedObjectIDFromUTF8String_lengthSelector utf8string len

-- | @+ metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreWithURL_error url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' metadataForPersistentStoreWithURL_errorSelector (toNSURL url) (toNSError error_)

-- | @- lock@
lock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO ()
lock nsPersistentStoreCoordinator =
  sendMessage nsPersistentStoreCoordinator lockSelector

-- | @- unlock@
unlock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO ()
unlock nsPersistentStoreCoordinator =
  sendMessage nsPersistentStoreCoordinator unlockSelector

-- | @- tryLock@
tryLock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO Bool
tryLock nsPersistentStoreCoordinator =
  sendMessage nsPersistentStoreCoordinator tryLockSelector

-- | @+ metadataForPersistentStoreOfType:URL:error:@
metadataForPersistentStoreOfType_URL_error :: (IsNSString storeType, IsNSURL url, IsNSError error_) => storeType -> url -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreOfType_URL_error storeType url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' metadataForPersistentStoreOfType_URL_errorSelector (toNSString storeType) (toNSURL url) (toNSError error_)

-- | @+ setMetadata:forPersistentStoreOfType:URL:error:@
setMetadata_forPersistentStoreOfType_URL_error :: (IsNSDictionary metadata, IsNSString storeType, IsNSURL url, IsNSError error_) => metadata -> storeType -> url -> error_ -> IO Bool
setMetadata_forPersistentStoreOfType_URL_error metadata storeType url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' setMetadata_forPersistentStoreOfType_URL_errorSelector (toNSDictionary metadata) (toNSString storeType) (toNSURL url) (toNSError error_)

-- | @+ removeUbiquitousContentAndPersistentStoreAtURL:options:error:@
removeUbiquitousContentAndPersistentStoreAtURL_options_error :: (IsNSURL storeURL, IsNSDictionary options, IsNSError error_) => storeURL -> options -> error_ -> IO Bool
removeUbiquitousContentAndPersistentStoreAtURL_options_error storeURL options error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector (toNSURL storeURL) (toNSDictionary options) (toNSError error_)

-- | @- managedObjectModel@
managedObjectModel :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO (Id NSManagedObjectModel)
managedObjectModel nsPersistentStoreCoordinator =
  sendMessage nsPersistentStoreCoordinator managedObjectModelSelector

-- | @- persistentStores@
persistentStores :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO (Id NSArray)
persistentStores nsPersistentStoreCoordinator =
  sendMessage nsPersistentStoreCoordinator persistentStoresSelector

-- | @- name@
name :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO (Id NSString)
name nsPersistentStoreCoordinator =
  sendMessage nsPersistentStoreCoordinator nameSelector

-- | @- setName:@
setName :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSString value) => nsPersistentStoreCoordinator -> value -> IO ()
setName nsPersistentStoreCoordinator value =
  sendMessage nsPersistentStoreCoordinator setNameSelector (toNSString value)

-- | @+ registeredStoreTypes@
registeredStoreTypes :: IO (Id NSDictionary)
registeredStoreTypes  =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMessage cls' registeredStoreTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithManagedObjectModel:@
initWithManagedObjectModelSelector :: Selector '[Id NSManagedObjectModel] (Id NSPersistentStoreCoordinator)
initWithManagedObjectModelSelector = mkSelector "initWithManagedObjectModel:"

-- | @Selector@ for @persistentStoreForURL:@
persistentStoreForURLSelector :: Selector '[Id NSURL] (Id NSPersistentStore)
persistentStoreForURLSelector = mkSelector "persistentStoreForURL:"

-- | @Selector@ for @URLForPersistentStore:@
urlForPersistentStoreSelector :: Selector '[Id NSPersistentStore] (Id NSURL)
urlForPersistentStoreSelector = mkSelector "URLForPersistentStore:"

-- | @Selector@ for @setURL:forPersistentStore:@
setURL_forPersistentStoreSelector :: Selector '[Id NSURL, Id NSPersistentStore] Bool
setURL_forPersistentStoreSelector = mkSelector "setURL:forPersistentStore:"

-- | @Selector@ for @addPersistentStoreWithType:configuration:URL:options:error:@
addPersistentStoreWithType_configuration_URL_options_errorSelector :: Selector '[Id NSString, Id NSString, Id NSURL, Id NSDictionary, Id NSError] (Id NSPersistentStore)
addPersistentStoreWithType_configuration_URL_options_errorSelector = mkSelector "addPersistentStoreWithType:configuration:URL:options:error:"

-- | @Selector@ for @addPersistentStoreWithDescription:completionHandler:@
addPersistentStoreWithDescription_completionHandlerSelector :: Selector '[Id NSPersistentStoreDescription, Ptr ()] ()
addPersistentStoreWithDescription_completionHandlerSelector = mkSelector "addPersistentStoreWithDescription:completionHandler:"

-- | @Selector@ for @removePersistentStore:error:@
removePersistentStore_errorSelector :: Selector '[Id NSPersistentStore, Id NSError] Bool
removePersistentStore_errorSelector = mkSelector "removePersistentStore:error:"

-- | @Selector@ for @setMetadata:forPersistentStore:@
setMetadata_forPersistentStoreSelector :: Selector '[Id NSDictionary, Id NSPersistentStore] ()
setMetadata_forPersistentStoreSelector = mkSelector "setMetadata:forPersistentStore:"

-- | @Selector@ for @metadataForPersistentStore:@
metadataForPersistentStoreSelector :: Selector '[Id NSPersistentStore] (Id NSDictionary)
metadataForPersistentStoreSelector = mkSelector "metadataForPersistentStore:"

-- | @Selector@ for @managedObjectIDForURIRepresentation:@
managedObjectIDForURIRepresentationSelector :: Selector '[Id NSURL] (Id NSManagedObjectID)
managedObjectIDForURIRepresentationSelector = mkSelector "managedObjectIDForURIRepresentation:"

-- | @Selector@ for @executeRequest:withContext:error:@
executeRequest_withContext_errorSelector :: Selector '[Id NSPersistentStoreRequest, Id NSManagedObjectContext, Id NSError] RawId
executeRequest_withContext_errorSelector = mkSelector "executeRequest:withContext:error:"

-- | @Selector@ for @registerStoreClass:forStoreType:@
registerStoreClass_forStoreTypeSelector :: Selector '[Class, Id NSString] ()
registerStoreClass_forStoreTypeSelector = mkSelector "registerStoreClass:forStoreType:"

-- | @Selector@ for @metadataForPersistentStoreOfType:URL:options:error:@
metadataForPersistentStoreOfType_URL_options_errorSelector :: Selector '[Id NSString, Id NSURL, Id NSDictionary, Id NSError] (Id NSDictionary)
metadataForPersistentStoreOfType_URL_options_errorSelector = mkSelector "metadataForPersistentStoreOfType:URL:options:error:"

-- | @Selector@ for @setMetadata:forPersistentStoreOfType:URL:options:error:@
setMetadata_forPersistentStoreOfType_URL_options_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSURL, Id NSDictionary, Id NSError] Bool
setMetadata_forPersistentStoreOfType_URL_options_errorSelector = mkSelector "setMetadata:forPersistentStoreOfType:URL:options:error:"

-- | @Selector@ for @elementsDerivedFromExternalRecordURL:@
elementsDerivedFromExternalRecordURLSelector :: Selector '[Id NSURL] (Id NSDictionary)
elementsDerivedFromExternalRecordURLSelector = mkSelector "elementsDerivedFromExternalRecordURL:"

-- | @Selector@ for @importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:@
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector :: Selector '[Id NSString, Id NSURL, Id NSURL, Id NSDictionary, Id NSString, Id NSError] (Id NSPersistentStore)
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector = mkSelector "importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:"

-- | @Selector@ for @migratePersistentStore:toURL:options:withType:error:@
migratePersistentStore_toURL_options_withType_errorSelector :: Selector '[Id NSPersistentStore, Id NSURL, Id NSDictionary, Id NSString, Id NSError] (Id NSPersistentStore)
migratePersistentStore_toURL_options_withType_errorSelector = mkSelector "migratePersistentStore:toURL:options:withType:error:"

-- | @Selector@ for @destroyPersistentStoreAtURL:withType:options:error:@
destroyPersistentStoreAtURL_withType_options_errorSelector :: Selector '[Id NSURL, Id NSString, Id NSDictionary, Id NSError] Bool
destroyPersistentStoreAtURL_withType_options_errorSelector = mkSelector "destroyPersistentStoreAtURL:withType:options:error:"

-- | @Selector@ for @replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:@
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSURL, Id NSDictionary, Id NSString, Id NSError] Bool
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector = mkSelector "replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:"

-- | @Selector@ for @performBlock:@
performBlockSelector :: Selector '[Ptr ()] ()
performBlockSelector = mkSelector "performBlock:"

-- | @Selector@ for @performBlockAndWait:@
performBlockAndWaitSelector :: Selector '[Ptr ()] ()
performBlockAndWaitSelector = mkSelector "performBlockAndWait:"

-- | @Selector@ for @currentPersistentHistoryTokenFromStores:@
currentPersistentHistoryTokenFromStoresSelector :: Selector '[Id NSArray] (Id NSPersistentHistoryToken)
currentPersistentHistoryTokenFromStoresSelector = mkSelector "currentPersistentHistoryTokenFromStores:"

-- | @Selector@ for @finishDeferredLightweightMigration:@
finishDeferredLightweightMigrationSelector :: Selector '[Id NSError] Bool
finishDeferredLightweightMigrationSelector = mkSelector "finishDeferredLightweightMigration:"

-- | @Selector@ for @finishDeferredLightweightMigrationTask:@
finishDeferredLightweightMigrationTaskSelector :: Selector '[Id NSError] Bool
finishDeferredLightweightMigrationTaskSelector = mkSelector "finishDeferredLightweightMigrationTask:"

-- | @Selector@ for @managedObjectIDFromUTF8String:length:@
managedObjectIDFromUTF8String_lengthSelector :: Selector '[Const (Ptr CChar), CULong] (Id NSManagedObjectID)
managedObjectIDFromUTF8String_lengthSelector = mkSelector "managedObjectIDFromUTF8String:length:"

-- | @Selector@ for @metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_errorSelector :: Selector '[Id NSURL, Id NSError] (Id NSDictionary)
metadataForPersistentStoreWithURL_errorSelector = mkSelector "metadataForPersistentStoreWithURL:error:"

-- | @Selector@ for @lock@
lockSelector :: Selector '[] ()
lockSelector = mkSelector "lock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector '[] ()
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector '[] Bool
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @metadataForPersistentStoreOfType:URL:error:@
metadataForPersistentStoreOfType_URL_errorSelector :: Selector '[Id NSString, Id NSURL, Id NSError] (Id NSDictionary)
metadataForPersistentStoreOfType_URL_errorSelector = mkSelector "metadataForPersistentStoreOfType:URL:error:"

-- | @Selector@ for @setMetadata:forPersistentStoreOfType:URL:error:@
setMetadata_forPersistentStoreOfType_URL_errorSelector :: Selector '[Id NSDictionary, Id NSString, Id NSURL, Id NSError] Bool
setMetadata_forPersistentStoreOfType_URL_errorSelector = mkSelector "setMetadata:forPersistentStoreOfType:URL:error:"

-- | @Selector@ for @removeUbiquitousContentAndPersistentStoreAtURL:options:error:@
removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector :: Selector '[Id NSURL, Id NSDictionary, Id NSError] Bool
removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector = mkSelector "removeUbiquitousContentAndPersistentStoreAtURL:options:error:"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector '[] (Id NSManagedObjectModel)
managedObjectModelSelector = mkSelector "managedObjectModel"

-- | @Selector@ for @persistentStores@
persistentStoresSelector :: Selector '[] (Id NSArray)
persistentStoresSelector = mkSelector "persistentStores"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @registeredStoreTypes@
registeredStoreTypesSelector :: Selector '[] (Id NSDictionary)
registeredStoreTypesSelector = mkSelector "registeredStoreTypes"

