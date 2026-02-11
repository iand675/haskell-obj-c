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
  , initWithManagedObjectModelSelector
  , persistentStoreForURLSelector
  , urlForPersistentStoreSelector
  , setURL_forPersistentStoreSelector
  , addPersistentStoreWithType_configuration_URL_options_errorSelector
  , addPersistentStoreWithDescription_completionHandlerSelector
  , removePersistentStore_errorSelector
  , setMetadata_forPersistentStoreSelector
  , metadataForPersistentStoreSelector
  , managedObjectIDForURIRepresentationSelector
  , executeRequest_withContext_errorSelector
  , registerStoreClass_forStoreTypeSelector
  , metadataForPersistentStoreOfType_URL_options_errorSelector
  , setMetadata_forPersistentStoreOfType_URL_options_errorSelector
  , elementsDerivedFromExternalRecordURLSelector
  , importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector
  , migratePersistentStore_toURL_options_withType_errorSelector
  , destroyPersistentStoreAtURL_withType_options_errorSelector
  , replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector
  , performBlockSelector
  , performBlockAndWaitSelector
  , currentPersistentHistoryTokenFromStoresSelector
  , finishDeferredLightweightMigrationSelector
  , finishDeferredLightweightMigrationTaskSelector
  , managedObjectIDFromUTF8String_lengthSelector
  , metadataForPersistentStoreWithURL_errorSelector
  , lockSelector
  , unlockSelector
  , tryLockSelector
  , metadataForPersistentStoreOfType_URL_errorSelector
  , setMetadata_forPersistentStoreOfType_URL_errorSelector
  , removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector
  , managedObjectModelSelector
  , persistentStoresSelector
  , nameSelector
  , setNameSelector
  , registeredStoreTypesSelector


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

import ObjC.CoreData.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithManagedObjectModel:@
initWithManagedObjectModel :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSManagedObjectModel model) => nsPersistentStoreCoordinator -> model -> IO (Id NSPersistentStoreCoordinator)
initWithManagedObjectModel nsPersistentStoreCoordinator  model =
withObjCPtr model $ \raw_model ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "initWithManagedObjectModel:") (retPtr retVoid) [argPtr (castPtr raw_model :: Ptr ())] >>= ownedObject . castPtr

-- | @- persistentStoreForURL:@
persistentStoreForURL :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url) => nsPersistentStoreCoordinator -> url -> IO (Id NSPersistentStore)
persistentStoreForURL nsPersistentStoreCoordinator  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "persistentStoreForURL:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- URLForPersistentStore:@
urlForPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> store -> IO (Id NSURL)
urlForPersistentStore nsPersistentStoreCoordinator  store =
withObjCPtr store $ \raw_store ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "URLForPersistentStore:") (retPtr retVoid) [argPtr (castPtr raw_store :: Ptr ())] >>= retainedObject . castPtr

-- | @- setURL:forPersistentStore:@
setURL_forPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> url -> store -> IO Bool
setURL_forPersistentStore nsPersistentStoreCoordinator  url store =
withObjCPtr url $ \raw_url ->
  withObjCPtr store $ \raw_store ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "setURL:forPersistentStore:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_store :: Ptr ())]

-- | @- addPersistentStoreWithType:configuration:URL:options:error:@
addPersistentStoreWithType_configuration_URL_options_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSString storeType, IsNSString configuration, IsNSURL storeURL, IsNSDictionary options, IsNSError error_) => nsPersistentStoreCoordinator -> storeType -> configuration -> storeURL -> options -> error_ -> IO (Id NSPersistentStore)
addPersistentStoreWithType_configuration_URL_options_error nsPersistentStoreCoordinator  storeType configuration storeURL options error_ =
withObjCPtr storeType $ \raw_storeType ->
  withObjCPtr configuration $ \raw_configuration ->
    withObjCPtr storeURL $ \raw_storeURL ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr error_ $ \raw_error_ ->
            sendMsg nsPersistentStoreCoordinator (mkSelector "addPersistentStoreWithType:configuration:URL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_configuration :: Ptr ()), argPtr (castPtr raw_storeURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- addPersistentStoreWithDescription:completionHandler:@
addPersistentStoreWithDescription_completionHandler :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStoreDescription storeDescription) => nsPersistentStoreCoordinator -> storeDescription -> Ptr () -> IO ()
addPersistentStoreWithDescription_completionHandler nsPersistentStoreCoordinator  storeDescription block =
withObjCPtr storeDescription $ \raw_storeDescription ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "addPersistentStoreWithDescription:completionHandler:") retVoid [argPtr (castPtr raw_storeDescription :: Ptr ()), argPtr (castPtr block :: Ptr ())]

-- | @- removePersistentStore:error:@
removePersistentStore_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store, IsNSError error_) => nsPersistentStoreCoordinator -> store -> error_ -> IO Bool
removePersistentStore_error nsPersistentStoreCoordinator  store error_ =
withObjCPtr store $ \raw_store ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "removePersistentStore:error:") retCULong [argPtr (castPtr raw_store :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- setMetadata:forPersistentStore:@
setMetadata_forPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSDictionary metadata, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> metadata -> store -> IO ()
setMetadata_forPersistentStore nsPersistentStoreCoordinator  metadata store =
withObjCPtr metadata $ \raw_metadata ->
  withObjCPtr store $ \raw_store ->
      sendMsg nsPersistentStoreCoordinator (mkSelector "setMetadata:forPersistentStore:") retVoid [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr raw_store :: Ptr ())]

-- | @- metadataForPersistentStore:@
metadataForPersistentStore :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store) => nsPersistentStoreCoordinator -> store -> IO (Id NSDictionary)
metadataForPersistentStore nsPersistentStoreCoordinator  store =
withObjCPtr store $ \raw_store ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "metadataForPersistentStore:") (retPtr retVoid) [argPtr (castPtr raw_store :: Ptr ())] >>= retainedObject . castPtr

-- | @- managedObjectIDForURIRepresentation:@
managedObjectIDForURIRepresentation :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url) => nsPersistentStoreCoordinator -> url -> IO (Id NSManagedObjectID)
managedObjectIDForURIRepresentation nsPersistentStoreCoordinator  url =
withObjCPtr url $ \raw_url ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "managedObjectIDForURIRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ())] >>= retainedObject . castPtr

-- | @- executeRequest:withContext:error:@
executeRequest_withContext_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStoreRequest request, IsNSManagedObjectContext context, IsNSError error_) => nsPersistentStoreCoordinator -> request -> context -> error_ -> IO RawId
executeRequest_withContext_error nsPersistentStoreCoordinator  request context error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr context $ \raw_context ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg nsPersistentStoreCoordinator (mkSelector "executeRequest:withContext:error:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ registerStoreClass:forStoreType:@
registerStoreClass_forStoreType :: IsNSString storeType => Class -> storeType -> IO ()
registerStoreClass_forStoreType storeClass storeType =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr storeType $ \raw_storeType ->
      sendClassMsg cls' (mkSelector "registerStoreClass:forStoreType:") retVoid [argPtr (unClass storeClass), argPtr (castPtr raw_storeType :: Ptr ())]

-- | @+ metadataForPersistentStoreOfType:URL:options:error:@
metadataForPersistentStoreOfType_URL_options_error :: (IsNSString storeType, IsNSURL url, IsNSDictionary options, IsNSError error_) => storeType -> url -> options -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreOfType_URL_options_error storeType url options error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr storeType $ \raw_storeType ->
      withObjCPtr url $ \raw_url ->
        withObjCPtr options $ \raw_options ->
          withObjCPtr error_ $ \raw_error_ ->
            sendClassMsg cls' (mkSelector "metadataForPersistentStoreOfType:URL:options:error:") (retPtr retVoid) [argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setMetadata:forPersistentStoreOfType:URL:options:error:@
setMetadata_forPersistentStoreOfType_URL_options_error :: (IsNSDictionary metadata, IsNSString storeType, IsNSURL url, IsNSDictionary options, IsNSError error_) => metadata -> storeType -> url -> options -> error_ -> IO Bool
setMetadata_forPersistentStoreOfType_URL_options_error metadata storeType url options error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr metadata $ \raw_metadata ->
      withObjCPtr storeType $ \raw_storeType ->
        withObjCPtr url $ \raw_url ->
          withObjCPtr options $ \raw_options ->
            withObjCPtr error_ $ \raw_error_ ->
              fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "setMetadata:forPersistentStoreOfType:URL:options:error:") retCULong [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ elementsDerivedFromExternalRecordURL:@
elementsDerivedFromExternalRecordURL :: IsNSURL fileURL => fileURL -> IO (Id NSDictionary)
elementsDerivedFromExternalRecordURL fileURL =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr fileURL $ \raw_fileURL ->
      sendClassMsg cls' (mkSelector "elementsDerivedFromExternalRecordURL:") (retPtr retVoid) [argPtr (castPtr raw_fileURL :: Ptr ())] >>= retainedObject . castPtr

-- | @- importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:@
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSString storeIdentifier, IsNSURL externalRecordsURL, IsNSURL destinationURL, IsNSDictionary options, IsNSString storeType, IsNSError error_) => nsPersistentStoreCoordinator -> storeIdentifier -> externalRecordsURL -> destinationURL -> options -> storeType -> error_ -> IO (Id NSPersistentStore)
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_error nsPersistentStoreCoordinator  storeIdentifier externalRecordsURL destinationURL options storeType error_ =
withObjCPtr storeIdentifier $ \raw_storeIdentifier ->
  withObjCPtr externalRecordsURL $ \raw_externalRecordsURL ->
    withObjCPtr destinationURL $ \raw_destinationURL ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr storeType $ \raw_storeType ->
          withObjCPtr error_ $ \raw_error_ ->
              sendMsg nsPersistentStoreCoordinator (mkSelector "importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:") (retPtr retVoid) [argPtr (castPtr raw_storeIdentifier :: Ptr ()), argPtr (castPtr raw_externalRecordsURL :: Ptr ()), argPtr (castPtr raw_destinationURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- migratePersistentStore:toURL:options:withType:error:@
migratePersistentStore_toURL_options_withType_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSPersistentStore store, IsNSURL url, IsNSDictionary options, IsNSString storeType, IsNSError error_) => nsPersistentStoreCoordinator -> store -> url -> options -> storeType -> error_ -> IO (Id NSPersistentStore)
migratePersistentStore_toURL_options_withType_error nsPersistentStoreCoordinator  store url options storeType error_ =
withObjCPtr store $ \raw_store ->
  withObjCPtr url $ \raw_url ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr storeType $ \raw_storeType ->
        withObjCPtr error_ $ \raw_error_ ->
            sendMsg nsPersistentStoreCoordinator (mkSelector "migratePersistentStore:toURL:options:withType:error:") (retPtr retVoid) [argPtr (castPtr raw_store :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- destroyPersistentStoreAtURL:withType:options:error:@
destroyPersistentStoreAtURL_withType_options_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL url, IsNSString storeType, IsNSDictionary options, IsNSError error_) => nsPersistentStoreCoordinator -> url -> storeType -> options -> error_ -> IO Bool
destroyPersistentStoreAtURL_withType_options_error nsPersistentStoreCoordinator  url storeType options error_ =
withObjCPtr url $ \raw_url ->
  withObjCPtr storeType $ \raw_storeType ->
    withObjCPtr options $ \raw_options ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "destroyPersistentStoreAtURL:withType:options:error:") retCULong [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:@
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_error :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSURL destinationURL, IsNSDictionary destinationOptions, IsNSURL sourceURL, IsNSDictionary sourceOptions, IsNSString storeType, IsNSError error_) => nsPersistentStoreCoordinator -> destinationURL -> destinationOptions -> sourceURL -> sourceOptions -> storeType -> error_ -> IO Bool
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_error nsPersistentStoreCoordinator  destinationURL destinationOptions sourceURL sourceOptions storeType error_ =
withObjCPtr destinationURL $ \raw_destinationURL ->
  withObjCPtr destinationOptions $ \raw_destinationOptions ->
    withObjCPtr sourceURL $ \raw_sourceURL ->
      withObjCPtr sourceOptions $ \raw_sourceOptions ->
        withObjCPtr storeType $ \raw_storeType ->
          withObjCPtr error_ $ \raw_error_ ->
              fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:") retCULong [argPtr (castPtr raw_destinationURL :: Ptr ()), argPtr (castPtr raw_destinationOptions :: Ptr ()), argPtr (castPtr raw_sourceURL :: Ptr ()), argPtr (castPtr raw_sourceOptions :: Ptr ()), argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- performBlock:@
performBlock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> Ptr () -> IO ()
performBlock nsPersistentStoreCoordinator  block =
  sendMsg nsPersistentStoreCoordinator (mkSelector "performBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- performBlockAndWait:@
performBlockAndWait :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> Ptr () -> IO ()
performBlockAndWait nsPersistentStoreCoordinator  block =
  sendMsg nsPersistentStoreCoordinator (mkSelector "performBlockAndWait:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- currentPersistentHistoryTokenFromStores:@
currentPersistentHistoryTokenFromStores :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSArray stores) => nsPersistentStoreCoordinator -> stores -> IO (Id NSPersistentHistoryToken)
currentPersistentHistoryTokenFromStores nsPersistentStoreCoordinator  stores =
withObjCPtr stores $ \raw_stores ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "currentPersistentHistoryTokenFromStores:") (retPtr retVoid) [argPtr (castPtr raw_stores :: Ptr ())] >>= retainedObject . castPtr

-- | @- finishDeferredLightweightMigration:@
finishDeferredLightweightMigration :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSError error_) => nsPersistentStoreCoordinator -> error_ -> IO Bool
finishDeferredLightweightMigration nsPersistentStoreCoordinator  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "finishDeferredLightweightMigration:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- finishDeferredLightweightMigrationTask:@
finishDeferredLightweightMigrationTask :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSError error_) => nsPersistentStoreCoordinator -> error_ -> IO Bool
finishDeferredLightweightMigrationTask nsPersistentStoreCoordinator  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "finishDeferredLightweightMigrationTask:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- managedObjectIDFromUTF8String:length:@
managedObjectIDFromUTF8String_length :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> Const (Ptr CChar) -> CULong -> IO (Id NSManagedObjectID)
managedObjectIDFromUTF8String_length nsPersistentStoreCoordinator  utf8string len =
  sendMsg nsPersistentStoreCoordinator (mkSelector "managedObjectIDFromUTF8String:length:") (retPtr retVoid) [argPtr (unConst utf8string), argCULong (fromIntegral len)] >>= retainedObject . castPtr

-- | @+ metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_error :: (IsNSURL url, IsNSError error_) => url -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreWithURL_error url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr url $ \raw_url ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "metadataForPersistentStoreWithURL:error:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- lock@
lock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO ()
lock nsPersistentStoreCoordinator  =
  sendMsg nsPersistentStoreCoordinator (mkSelector "lock") retVoid []

-- | @- unlock@
unlock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO ()
unlock nsPersistentStoreCoordinator  =
  sendMsg nsPersistentStoreCoordinator (mkSelector "unlock") retVoid []

-- | @- tryLock@
tryLock :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO Bool
tryLock nsPersistentStoreCoordinator  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsPersistentStoreCoordinator (mkSelector "tryLock") retCULong []

-- | @+ metadataForPersistentStoreOfType:URL:error:@
metadataForPersistentStoreOfType_URL_error :: (IsNSString storeType, IsNSURL url, IsNSError error_) => storeType -> url -> error_ -> IO (Id NSDictionary)
metadataForPersistentStoreOfType_URL_error storeType url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr storeType $ \raw_storeType ->
      withObjCPtr url $ \raw_url ->
        withObjCPtr error_ $ \raw_error_ ->
          sendClassMsg cls' (mkSelector "metadataForPersistentStoreOfType:URL:error:") (retPtr retVoid) [argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @+ setMetadata:forPersistentStoreOfType:URL:error:@
setMetadata_forPersistentStoreOfType_URL_error :: (IsNSDictionary metadata, IsNSString storeType, IsNSURL url, IsNSError error_) => metadata -> storeType -> url -> error_ -> IO Bool
setMetadata_forPersistentStoreOfType_URL_error metadata storeType url error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr metadata $ \raw_metadata ->
      withObjCPtr storeType $ \raw_storeType ->
        withObjCPtr url $ \raw_url ->
          withObjCPtr error_ $ \raw_error_ ->
            fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "setMetadata:forPersistentStoreOfType:URL:error:") retCULong [argPtr (castPtr raw_metadata :: Ptr ()), argPtr (castPtr raw_storeType :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ removeUbiquitousContentAndPersistentStoreAtURL:options:error:@
removeUbiquitousContentAndPersistentStoreAtURL_options_error :: (IsNSURL storeURL, IsNSDictionary options, IsNSError error_) => storeURL -> options -> error_ -> IO Bool
removeUbiquitousContentAndPersistentStoreAtURL_options_error storeURL options error_ =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    withObjCPtr storeURL $ \raw_storeURL ->
      withObjCPtr options $ \raw_options ->
        withObjCPtr error_ $ \raw_error_ ->
          fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "removeUbiquitousContentAndPersistentStoreAtURL:options:error:") retCULong [argPtr (castPtr raw_storeURL :: Ptr ()), argPtr (castPtr raw_options :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- managedObjectModel@
managedObjectModel :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO (Id NSManagedObjectModel)
managedObjectModel nsPersistentStoreCoordinator  =
  sendMsg nsPersistentStoreCoordinator (mkSelector "managedObjectModel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- persistentStores@
persistentStores :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO (Id NSArray)
persistentStores nsPersistentStoreCoordinator  =
  sendMsg nsPersistentStoreCoordinator (mkSelector "persistentStores") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- name@
name :: IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator => nsPersistentStoreCoordinator -> IO (Id NSString)
name nsPersistentStoreCoordinator  =
  sendMsg nsPersistentStoreCoordinator (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSPersistentStoreCoordinator nsPersistentStoreCoordinator, IsNSString value) => nsPersistentStoreCoordinator -> value -> IO ()
setName nsPersistentStoreCoordinator  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsPersistentStoreCoordinator (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @+ registeredStoreTypes@
registeredStoreTypes :: IO (Id NSDictionary)
registeredStoreTypes  =
  do
    cls' <- getRequiredClass "NSPersistentStoreCoordinator"
    sendClassMsg cls' (mkSelector "registeredStoreTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithManagedObjectModel:@
initWithManagedObjectModelSelector :: Selector
initWithManagedObjectModelSelector = mkSelector "initWithManagedObjectModel:"

-- | @Selector@ for @persistentStoreForURL:@
persistentStoreForURLSelector :: Selector
persistentStoreForURLSelector = mkSelector "persistentStoreForURL:"

-- | @Selector@ for @URLForPersistentStore:@
urlForPersistentStoreSelector :: Selector
urlForPersistentStoreSelector = mkSelector "URLForPersistentStore:"

-- | @Selector@ for @setURL:forPersistentStore:@
setURL_forPersistentStoreSelector :: Selector
setURL_forPersistentStoreSelector = mkSelector "setURL:forPersistentStore:"

-- | @Selector@ for @addPersistentStoreWithType:configuration:URL:options:error:@
addPersistentStoreWithType_configuration_URL_options_errorSelector :: Selector
addPersistentStoreWithType_configuration_URL_options_errorSelector = mkSelector "addPersistentStoreWithType:configuration:URL:options:error:"

-- | @Selector@ for @addPersistentStoreWithDescription:completionHandler:@
addPersistentStoreWithDescription_completionHandlerSelector :: Selector
addPersistentStoreWithDescription_completionHandlerSelector = mkSelector "addPersistentStoreWithDescription:completionHandler:"

-- | @Selector@ for @removePersistentStore:error:@
removePersistentStore_errorSelector :: Selector
removePersistentStore_errorSelector = mkSelector "removePersistentStore:error:"

-- | @Selector@ for @setMetadata:forPersistentStore:@
setMetadata_forPersistentStoreSelector :: Selector
setMetadata_forPersistentStoreSelector = mkSelector "setMetadata:forPersistentStore:"

-- | @Selector@ for @metadataForPersistentStore:@
metadataForPersistentStoreSelector :: Selector
metadataForPersistentStoreSelector = mkSelector "metadataForPersistentStore:"

-- | @Selector@ for @managedObjectIDForURIRepresentation:@
managedObjectIDForURIRepresentationSelector :: Selector
managedObjectIDForURIRepresentationSelector = mkSelector "managedObjectIDForURIRepresentation:"

-- | @Selector@ for @executeRequest:withContext:error:@
executeRequest_withContext_errorSelector :: Selector
executeRequest_withContext_errorSelector = mkSelector "executeRequest:withContext:error:"

-- | @Selector@ for @registerStoreClass:forStoreType:@
registerStoreClass_forStoreTypeSelector :: Selector
registerStoreClass_forStoreTypeSelector = mkSelector "registerStoreClass:forStoreType:"

-- | @Selector@ for @metadataForPersistentStoreOfType:URL:options:error:@
metadataForPersistentStoreOfType_URL_options_errorSelector :: Selector
metadataForPersistentStoreOfType_URL_options_errorSelector = mkSelector "metadataForPersistentStoreOfType:URL:options:error:"

-- | @Selector@ for @setMetadata:forPersistentStoreOfType:URL:options:error:@
setMetadata_forPersistentStoreOfType_URL_options_errorSelector :: Selector
setMetadata_forPersistentStoreOfType_URL_options_errorSelector = mkSelector "setMetadata:forPersistentStoreOfType:URL:options:error:"

-- | @Selector@ for @elementsDerivedFromExternalRecordURL:@
elementsDerivedFromExternalRecordURLSelector :: Selector
elementsDerivedFromExternalRecordURLSelector = mkSelector "elementsDerivedFromExternalRecordURL:"

-- | @Selector@ for @importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:@
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector :: Selector
importStoreWithIdentifier_fromExternalRecordsDirectory_toURL_options_withType_errorSelector = mkSelector "importStoreWithIdentifier:fromExternalRecordsDirectory:toURL:options:withType:error:"

-- | @Selector@ for @migratePersistentStore:toURL:options:withType:error:@
migratePersistentStore_toURL_options_withType_errorSelector :: Selector
migratePersistentStore_toURL_options_withType_errorSelector = mkSelector "migratePersistentStore:toURL:options:withType:error:"

-- | @Selector@ for @destroyPersistentStoreAtURL:withType:options:error:@
destroyPersistentStoreAtURL_withType_options_errorSelector :: Selector
destroyPersistentStoreAtURL_withType_options_errorSelector = mkSelector "destroyPersistentStoreAtURL:withType:options:error:"

-- | @Selector@ for @replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:@
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector :: Selector
replacePersistentStoreAtURL_destinationOptions_withPersistentStoreFromURL_sourceOptions_storeType_errorSelector = mkSelector "replacePersistentStoreAtURL:destinationOptions:withPersistentStoreFromURL:sourceOptions:storeType:error:"

-- | @Selector@ for @performBlock:@
performBlockSelector :: Selector
performBlockSelector = mkSelector "performBlock:"

-- | @Selector@ for @performBlockAndWait:@
performBlockAndWaitSelector :: Selector
performBlockAndWaitSelector = mkSelector "performBlockAndWait:"

-- | @Selector@ for @currentPersistentHistoryTokenFromStores:@
currentPersistentHistoryTokenFromStoresSelector :: Selector
currentPersistentHistoryTokenFromStoresSelector = mkSelector "currentPersistentHistoryTokenFromStores:"

-- | @Selector@ for @finishDeferredLightweightMigration:@
finishDeferredLightweightMigrationSelector :: Selector
finishDeferredLightweightMigrationSelector = mkSelector "finishDeferredLightweightMigration:"

-- | @Selector@ for @finishDeferredLightweightMigrationTask:@
finishDeferredLightweightMigrationTaskSelector :: Selector
finishDeferredLightweightMigrationTaskSelector = mkSelector "finishDeferredLightweightMigrationTask:"

-- | @Selector@ for @managedObjectIDFromUTF8String:length:@
managedObjectIDFromUTF8String_lengthSelector :: Selector
managedObjectIDFromUTF8String_lengthSelector = mkSelector "managedObjectIDFromUTF8String:length:"

-- | @Selector@ for @metadataForPersistentStoreWithURL:error:@
metadataForPersistentStoreWithURL_errorSelector :: Selector
metadataForPersistentStoreWithURL_errorSelector = mkSelector "metadataForPersistentStoreWithURL:error:"

-- | @Selector@ for @lock@
lockSelector :: Selector
lockSelector = mkSelector "lock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @metadataForPersistentStoreOfType:URL:error:@
metadataForPersistentStoreOfType_URL_errorSelector :: Selector
metadataForPersistentStoreOfType_URL_errorSelector = mkSelector "metadataForPersistentStoreOfType:URL:error:"

-- | @Selector@ for @setMetadata:forPersistentStoreOfType:URL:error:@
setMetadata_forPersistentStoreOfType_URL_errorSelector :: Selector
setMetadata_forPersistentStoreOfType_URL_errorSelector = mkSelector "setMetadata:forPersistentStoreOfType:URL:error:"

-- | @Selector@ for @removeUbiquitousContentAndPersistentStoreAtURL:options:error:@
removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector :: Selector
removeUbiquitousContentAndPersistentStoreAtURL_options_errorSelector = mkSelector "removeUbiquitousContentAndPersistentStoreAtURL:options:error:"

-- | @Selector@ for @managedObjectModel@
managedObjectModelSelector :: Selector
managedObjectModelSelector = mkSelector "managedObjectModel"

-- | @Selector@ for @persistentStores@
persistentStoresSelector :: Selector
persistentStoresSelector = mkSelector "persistentStores"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @registeredStoreTypes@
registeredStoreTypesSelector :: Selector
registeredStoreTypesSelector = mkSelector "registeredStoreTypes"

