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
  , loadMetadataSelector
  , executeRequest_withContext_errorSelector
  , newValuesForObjectWithID_withContext_errorSelector
  , newValueForRelationship_forObjectWithID_withContext_errorSelector
  , identifierForNewStoreAtURLSelector
  , obtainPermanentIDsForObjects_errorSelector
  , managedObjectContextDidRegisterObjectsWithIDsSelector
  , managedObjectContextDidUnregisterObjectsWithIDsSelector
  , newObjectIDForEntity_referenceObjectSelector
  , referenceObjectForObjectIDSelector


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

-- | @- loadMetadata:@
loadMetadata :: (IsNSIncrementalStore nsIncrementalStore, IsNSError error_) => nsIncrementalStore -> error_ -> IO Bool
loadMetadata nsIncrementalStore  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsIncrementalStore (mkSelector "loadMetadata:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- executeRequest:withContext:error:@
executeRequest_withContext_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSPersistentStoreRequest request, IsNSManagedObjectContext context, IsNSError error_) => nsIncrementalStore -> request -> context -> error_ -> IO RawId
executeRequest_withContext_error nsIncrementalStore  request context error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr context $ \raw_context ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap (RawId . castPtr) $ sendMsg nsIncrementalStore (mkSelector "executeRequest:withContext:error:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- newValuesForObjectWithID:withContext:error:@
newValuesForObjectWithID_withContext_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSManagedObjectID objectID, IsNSManagedObjectContext context, IsNSError error_) => nsIncrementalStore -> objectID -> context -> error_ -> IO (Id NSIncrementalStoreNode)
newValuesForObjectWithID_withContext_error nsIncrementalStore  objectID context error_ =
withObjCPtr objectID $ \raw_objectID ->
  withObjCPtr context $ \raw_context ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg nsIncrementalStore (mkSelector "newValuesForObjectWithID:withContext:error:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- newValueForRelationship:forObjectWithID:withContext:error:@
newValueForRelationship_forObjectWithID_withContext_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSRelationshipDescription relationship, IsNSManagedObjectID objectID, IsNSManagedObjectContext context, IsNSError error_) => nsIncrementalStore -> relationship -> objectID -> context -> error_ -> IO RawId
newValueForRelationship_forObjectWithID_withContext_error nsIncrementalStore  relationship objectID context error_ =
withObjCPtr relationship $ \raw_relationship ->
  withObjCPtr objectID $ \raw_objectID ->
    withObjCPtr context $ \raw_context ->
      withObjCPtr error_ $ \raw_error_ ->
          fmap (RawId . castPtr) $ sendMsg nsIncrementalStore (mkSelector "newValueForRelationship:forObjectWithID:withContext:error:") (retPtr retVoid) [argPtr (castPtr raw_relationship :: Ptr ()), argPtr (castPtr raw_objectID :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @+ identifierForNewStoreAtURL:@
identifierForNewStoreAtURL :: IsNSURL storeURL => storeURL -> IO RawId
identifierForNewStoreAtURL storeURL =
  do
    cls' <- getRequiredClass "NSIncrementalStore"
    withObjCPtr storeURL $ \raw_storeURL ->
      fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "identifierForNewStoreAtURL:") (retPtr retVoid) [argPtr (castPtr raw_storeURL :: Ptr ())]

-- | @- obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_error :: (IsNSIncrementalStore nsIncrementalStore, IsNSArray array, IsNSError error_) => nsIncrementalStore -> array -> error_ -> IO (Id NSArray)
obtainPermanentIDsForObjects_error nsIncrementalStore  array error_ =
withObjCPtr array $ \raw_array ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsIncrementalStore (mkSelector "obtainPermanentIDsForObjects:error:") (retPtr retVoid) [argPtr (castPtr raw_array :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- managedObjectContextDidRegisterObjectsWithIDs:@
managedObjectContextDidRegisterObjectsWithIDs :: (IsNSIncrementalStore nsIncrementalStore, IsNSArray objectIDs) => nsIncrementalStore -> objectIDs -> IO ()
managedObjectContextDidRegisterObjectsWithIDs nsIncrementalStore  objectIDs =
withObjCPtr objectIDs $ \raw_objectIDs ->
    sendMsg nsIncrementalStore (mkSelector "managedObjectContextDidRegisterObjectsWithIDs:") retVoid [argPtr (castPtr raw_objectIDs :: Ptr ())]

-- | @- managedObjectContextDidUnregisterObjectsWithIDs:@
managedObjectContextDidUnregisterObjectsWithIDs :: (IsNSIncrementalStore nsIncrementalStore, IsNSArray objectIDs) => nsIncrementalStore -> objectIDs -> IO ()
managedObjectContextDidUnregisterObjectsWithIDs nsIncrementalStore  objectIDs =
withObjCPtr objectIDs $ \raw_objectIDs ->
    sendMsg nsIncrementalStore (mkSelector "managedObjectContextDidUnregisterObjectsWithIDs:") retVoid [argPtr (castPtr raw_objectIDs :: Ptr ())]

-- | @- newObjectIDForEntity:referenceObject:@
newObjectIDForEntity_referenceObject :: (IsNSIncrementalStore nsIncrementalStore, IsNSEntityDescription entity) => nsIncrementalStore -> entity -> RawId -> IO (Id NSManagedObjectID)
newObjectIDForEntity_referenceObject nsIncrementalStore  entity data_ =
withObjCPtr entity $ \raw_entity ->
    sendMsg nsIncrementalStore (mkSelector "newObjectIDForEntity:referenceObject:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ()), argPtr (castPtr (unRawId data_) :: Ptr ())] >>= ownedObject . castPtr

-- | @- referenceObjectForObjectID:@
referenceObjectForObjectID :: (IsNSIncrementalStore nsIncrementalStore, IsNSManagedObjectID objectID) => nsIncrementalStore -> objectID -> IO RawId
referenceObjectForObjectID nsIncrementalStore  objectID =
withObjCPtr objectID $ \raw_objectID ->
    fmap (RawId . castPtr) $ sendMsg nsIncrementalStore (mkSelector "referenceObjectForObjectID:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @loadMetadata:@
loadMetadataSelector :: Selector
loadMetadataSelector = mkSelector "loadMetadata:"

-- | @Selector@ for @executeRequest:withContext:error:@
executeRequest_withContext_errorSelector :: Selector
executeRequest_withContext_errorSelector = mkSelector "executeRequest:withContext:error:"

-- | @Selector@ for @newValuesForObjectWithID:withContext:error:@
newValuesForObjectWithID_withContext_errorSelector :: Selector
newValuesForObjectWithID_withContext_errorSelector = mkSelector "newValuesForObjectWithID:withContext:error:"

-- | @Selector@ for @newValueForRelationship:forObjectWithID:withContext:error:@
newValueForRelationship_forObjectWithID_withContext_errorSelector :: Selector
newValueForRelationship_forObjectWithID_withContext_errorSelector = mkSelector "newValueForRelationship:forObjectWithID:withContext:error:"

-- | @Selector@ for @identifierForNewStoreAtURL:@
identifierForNewStoreAtURLSelector :: Selector
identifierForNewStoreAtURLSelector = mkSelector "identifierForNewStoreAtURL:"

-- | @Selector@ for @obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_errorSelector :: Selector
obtainPermanentIDsForObjects_errorSelector = mkSelector "obtainPermanentIDsForObjects:error:"

-- | @Selector@ for @managedObjectContextDidRegisterObjectsWithIDs:@
managedObjectContextDidRegisterObjectsWithIDsSelector :: Selector
managedObjectContextDidRegisterObjectsWithIDsSelector = mkSelector "managedObjectContextDidRegisterObjectsWithIDs:"

-- | @Selector@ for @managedObjectContextDidUnregisterObjectsWithIDs:@
managedObjectContextDidUnregisterObjectsWithIDsSelector :: Selector
managedObjectContextDidUnregisterObjectsWithIDsSelector = mkSelector "managedObjectContextDidUnregisterObjectsWithIDs:"

-- | @Selector@ for @newObjectIDForEntity:referenceObject:@
newObjectIDForEntity_referenceObjectSelector :: Selector
newObjectIDForEntity_referenceObjectSelector = mkSelector "newObjectIDForEntity:referenceObject:"

-- | @Selector@ for @referenceObjectForObjectID:@
referenceObjectForObjectIDSelector :: Selector
referenceObjectForObjectIDSelector = mkSelector "referenceObjectForObjectID:"

