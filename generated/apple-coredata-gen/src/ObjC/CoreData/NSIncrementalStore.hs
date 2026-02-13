{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSIncrementalStore@.
module ObjC.CoreData.NSIncrementalStore
  ( NSIncrementalStore
  , IsNSIncrementalStore(..)
  , loadMetadata
  , executeRequest_withContext_error
  , newValuesForObjectWithID_withContext_error
  , newValueForRelationship_forObjectWithID_withContext_error
  , identifierForNewStoreAtURL
  , obtainPermanentIDsForObjects_error
  , managedObjectContextDidRegisterObjectsWithIDs
  , managedObjectContextDidUnregisterObjectsWithIDs
  , newObjectIDForEntity_referenceObject
  , referenceObjectForObjectID
  , executeRequest_withContext_errorSelector
  , identifierForNewStoreAtURLSelector
  , loadMetadataSelector
  , managedObjectContextDidRegisterObjectsWithIDsSelector
  , managedObjectContextDidUnregisterObjectsWithIDsSelector
  , newObjectIDForEntity_referenceObjectSelector
  , newValueForRelationship_forObjectWithID_withContext_errorSelector
  , newValuesForObjectWithID_withContext_errorSelector
  , obtainPermanentIDsForObjects_errorSelector
  , referenceObjectForObjectIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- loadMetadata:@
loadMetadata :: (IsNSIncrementalStore nsIncrementalStore, IsNSError error_) => nsIncrementalStore -> error_ -> IO Bool
loadMetadata nsIncrementalStore error_ =
  sendMessage nsIncrementalStore loadMetadataSelector (toNSError error_)

-- | @- executeRequest:withContext:error:@
executeRequest_withContext_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSPersistentStoreRequest request, IsNSManagedObjectContext context, IsNSError error_) => nsIncrementalStore -> request -> context -> error_ -> IO RawId
executeRequest_withContext_error nsIncrementalStore request context error_ =
  sendMessage nsIncrementalStore executeRequest_withContext_errorSelector (toNSPersistentStoreRequest request) (toNSManagedObjectContext context) (toNSError error_)

-- | @- newValuesForObjectWithID:withContext:error:@
newValuesForObjectWithID_withContext_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSManagedObjectID objectID, IsNSManagedObjectContext context, IsNSError error_) => nsIncrementalStore -> objectID -> context -> error_ -> IO (Id NSIncrementalStoreNode)
newValuesForObjectWithID_withContext_error nsIncrementalStore objectID context error_ =
  sendOwnedMessage nsIncrementalStore newValuesForObjectWithID_withContext_errorSelector (toNSManagedObjectID objectID) (toNSManagedObjectContext context) (toNSError error_)

-- | @- newValueForRelationship:forObjectWithID:withContext:error:@
newValueForRelationship_forObjectWithID_withContext_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSRelationshipDescription relationship, IsNSManagedObjectID objectID, IsNSManagedObjectContext context, IsNSError error_) => nsIncrementalStore -> relationship -> objectID -> context -> error_ -> IO RawId
newValueForRelationship_forObjectWithID_withContext_error nsIncrementalStore relationship objectID context error_ =
  sendOwnedMessage nsIncrementalStore newValueForRelationship_forObjectWithID_withContext_errorSelector (toNSRelationshipDescription relationship) (toNSManagedObjectID objectID) (toNSManagedObjectContext context) (toNSError error_)

-- | @+ identifierForNewStoreAtURL:@
identifierForNewStoreAtURL :: IsNSURL storeURL => storeURL -> IO RawId
identifierForNewStoreAtURL storeURL =
  do
    cls' <- getRequiredClass "NSIncrementalStore"
    sendClassMessage cls' identifierForNewStoreAtURLSelector (toNSURL storeURL)

-- | @- obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSArray array, IsNSError error_) => nsIncrementalStore -> array -> error_ -> IO (Id NSArray)
obtainPermanentIDsForObjects_error nsIncrementalStore array error_ =
  sendMessage nsIncrementalStore obtainPermanentIDsForObjects_errorSelector (toNSArray array) (toNSError error_)

-- | @- managedObjectContextDidRegisterObjectsWithIDs:@
managedObjectContextDidRegisterObjectsWithIDs :: (IsNSIncrementalStore nsIncrementalStore, IsNSArray objectIDs) => nsIncrementalStore -> objectIDs -> IO ()
managedObjectContextDidRegisterObjectsWithIDs nsIncrementalStore objectIDs =
  sendMessage nsIncrementalStore managedObjectContextDidRegisterObjectsWithIDsSelector (toNSArray objectIDs)

-- | @- managedObjectContextDidUnregisterObjectsWithIDs:@
managedObjectContextDidUnregisterObjectsWithIDs :: (IsNSIncrementalStore nsIncrementalStore, IsNSArray objectIDs) => nsIncrementalStore -> objectIDs -> IO ()
managedObjectContextDidUnregisterObjectsWithIDs nsIncrementalStore objectIDs =
  sendMessage nsIncrementalStore managedObjectContextDidUnregisterObjectsWithIDsSelector (toNSArray objectIDs)

-- | @- newObjectIDForEntity:referenceObject:@
newObjectIDForEntity_referenceObject :: (IsNSIncrementalStore nsIncrementalStore, IsNSEntityDescription entity) => nsIncrementalStore -> entity -> RawId -> IO (Id NSManagedObjectID)
newObjectIDForEntity_referenceObject nsIncrementalStore entity data_ =
  sendOwnedMessage nsIncrementalStore newObjectIDForEntity_referenceObjectSelector (toNSEntityDescription entity) data_

-- | @- referenceObjectForObjectID:@
referenceObjectForObjectID :: (IsNSIncrementalStore nsIncrementalStore, IsNSManagedObjectID objectID) => nsIncrementalStore -> objectID -> IO RawId
referenceObjectForObjectID nsIncrementalStore objectID =
  sendMessage nsIncrementalStore referenceObjectForObjectIDSelector (toNSManagedObjectID objectID)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadMetadata:@
loadMetadataSelector :: Selector '[Id NSError] Bool
loadMetadataSelector = mkSelector "loadMetadata:"

-- | @Selector@ for @executeRequest:withContext:error:@
executeRequest_withContext_errorSelector :: Selector '[Id NSPersistentStoreRequest, Id NSManagedObjectContext, Id NSError] RawId
executeRequest_withContext_errorSelector = mkSelector "executeRequest:withContext:error:"

-- | @Selector@ for @newValuesForObjectWithID:withContext:error:@
newValuesForObjectWithID_withContext_errorSelector :: Selector '[Id NSManagedObjectID, Id NSManagedObjectContext, Id NSError] (Id NSIncrementalStoreNode)
newValuesForObjectWithID_withContext_errorSelector = mkSelector "newValuesForObjectWithID:withContext:error:"

-- | @Selector@ for @newValueForRelationship:forObjectWithID:withContext:error:@
newValueForRelationship_forObjectWithID_withContext_errorSelector :: Selector '[Id NSRelationshipDescription, Id NSManagedObjectID, Id NSManagedObjectContext, Id NSError] RawId
newValueForRelationship_forObjectWithID_withContext_errorSelector = mkSelector "newValueForRelationship:forObjectWithID:withContext:error:"

-- | @Selector@ for @identifierForNewStoreAtURL:@
identifierForNewStoreAtURLSelector :: Selector '[Id NSURL] RawId
identifierForNewStoreAtURLSelector = mkSelector "identifierForNewStoreAtURL:"

-- | @Selector@ for @obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_errorSelector :: Selector '[Id NSArray, Id NSError] (Id NSArray)
obtainPermanentIDsForObjects_errorSelector = mkSelector "obtainPermanentIDsForObjects:error:"

-- | @Selector@ for @managedObjectContextDidRegisterObjectsWithIDs:@
managedObjectContextDidRegisterObjectsWithIDsSelector :: Selector '[Id NSArray] ()
managedObjectContextDidRegisterObjectsWithIDsSelector = mkSelector "managedObjectContextDidRegisterObjectsWithIDs:"

-- | @Selector@ for @managedObjectContextDidUnregisterObjectsWithIDs:@
managedObjectContextDidUnregisterObjectsWithIDsSelector :: Selector '[Id NSArray] ()
managedObjectContextDidUnregisterObjectsWithIDsSelector = mkSelector "managedObjectContextDidUnregisterObjectsWithIDs:"

-- | @Selector@ for @newObjectIDForEntity:referenceObject:@
newObjectIDForEntity_referenceObjectSelector :: Selector '[Id NSEntityDescription, RawId] (Id NSManagedObjectID)
newObjectIDForEntity_referenceObjectSelector = mkSelector "newObjectIDForEntity:referenceObject:"

-- | @Selector@ for @referenceObjectForObjectID:@
referenceObjectForObjectIDSelector :: Selector '[Id NSManagedObjectID] RawId
referenceObjectForObjectIDSelector = mkSelector "referenceObjectForObjectID:"

