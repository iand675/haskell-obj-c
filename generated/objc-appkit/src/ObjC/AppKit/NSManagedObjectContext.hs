{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSManagedObjectContext@.
module ObjC.AppKit.NSManagedObjectContext
  ( NSManagedObjectContext
  , IsNSManagedObjectContext(..)
  , new
  , init_
  , initWithConcurrencyType
  , performBlock
  , performBlockAndWait
  , objectRegisteredForID
  , objectWithID
  , existingObjectWithID_error
  , executeFetchRequest_error
  , countForFetchRequest_error
  , executeRequest_error
  , insertObject
  , deleteObject
  , refreshObject_mergeChanges
  , detectConflictsForObject
  , observeValueForKeyPath_ofObject_change_context
  , processPendingChanges
  , assignObject_toPersistentStore
  , undo
  , redo
  , reset
  , rollback
  , save
  , refreshAllObjects
  , lock
  , unlock
  , tryLock
  , shouldHandleInaccessibleFault_forObjectID_triggeredByProperty
  , obtainPermanentIDsForObjects_error
  , mergeChangesFromContextDidSaveNotification
  , mergeChangesFromRemoteContextSave_intoContexts
  , setQueryGenerationFromToken_error
  , persistentStoreCoordinator
  , setPersistentStoreCoordinator
  , parentContext
  , setParentContext
  , name
  , setName
  , undoManager
  , setUndoManager
  , hasChanges
  , userInfo
  , concurrencyType
  , insertedObjects
  , updatedObjects
  , deletedObjects
  , registeredObjects
  , propagatesDeletesAtEndOfEvent
  , setPropagatesDeletesAtEndOfEvent
  , retainsRegisteredObjects
  , setRetainsRegisteredObjects
  , shouldDeleteInaccessibleFaults
  , setShouldDeleteInaccessibleFaults
  , stalenessInterval
  , setStalenessInterval
  , mergePolicy
  , setMergePolicy
  , queryGenerationToken
  , automaticallyMergesChangesFromParent
  , setAutomaticallyMergesChangesFromParent
  , transactionAuthor
  , setTransactionAuthor
  , newSelector
  , initSelector
  , initWithConcurrencyTypeSelector
  , performBlockSelector
  , performBlockAndWaitSelector
  , objectRegisteredForIDSelector
  , objectWithIDSelector
  , existingObjectWithID_errorSelector
  , executeFetchRequest_errorSelector
  , countForFetchRequest_errorSelector
  , executeRequest_errorSelector
  , insertObjectSelector
  , deleteObjectSelector
  , refreshObject_mergeChangesSelector
  , detectConflictsForObjectSelector
  , observeValueForKeyPath_ofObject_change_contextSelector
  , processPendingChangesSelector
  , assignObject_toPersistentStoreSelector
  , undoSelector
  , redoSelector
  , resetSelector
  , rollbackSelector
  , saveSelector
  , refreshAllObjectsSelector
  , lockSelector
  , unlockSelector
  , tryLockSelector
  , shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector
  , obtainPermanentIDsForObjects_errorSelector
  , mergeChangesFromContextDidSaveNotificationSelector
  , mergeChangesFromRemoteContextSave_intoContextsSelector
  , setQueryGenerationFromToken_errorSelector
  , persistentStoreCoordinatorSelector
  , setPersistentStoreCoordinatorSelector
  , parentContextSelector
  , setParentContextSelector
  , nameSelector
  , setNameSelector
  , undoManagerSelector
  , setUndoManagerSelector
  , hasChangesSelector
  , userInfoSelector
  , concurrencyTypeSelector
  , insertedObjectsSelector
  , updatedObjectsSelector
  , deletedObjectsSelector
  , registeredObjectsSelector
  , propagatesDeletesAtEndOfEventSelector
  , setPropagatesDeletesAtEndOfEventSelector
  , retainsRegisteredObjectsSelector
  , setRetainsRegisteredObjectsSelector
  , shouldDeleteInaccessibleFaultsSelector
  , setShouldDeleteInaccessibleFaultsSelector
  , stalenessIntervalSelector
  , setStalenessIntervalSelector
  , mergePolicySelector
  , setMergePolicySelector
  , queryGenerationTokenSelector
  , automaticallyMergesChangesFromParentSelector
  , setAutomaticallyMergesChangesFromParentSelector
  , transactionAuthorSelector
  , setTransactionAuthorSelector

  -- * Enum types
  , NSManagedObjectContextConcurrencyType(NSManagedObjectContextConcurrencyType)
  , pattern NSConfinementConcurrencyType
  , pattern NSPrivateQueueConcurrencyType
  , pattern NSMainQueueConcurrencyType

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

import ObjC.AppKit.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSManagedObjectContext)
new  =
  do
    cls' <- getRequiredClass "NSManagedObjectContext"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSManagedObjectContext)
init_ nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithConcurrencyType:@
initWithConcurrencyType :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> NSManagedObjectContextConcurrencyType -> IO (Id NSManagedObjectContext)
initWithConcurrencyType nsManagedObjectContext  ct =
  sendMsg nsManagedObjectContext (mkSelector "initWithConcurrencyType:") (retPtr retVoid) [argCULong (coerce ct)] >>= ownedObject . castPtr

-- | @- performBlock:@
performBlock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Ptr () -> IO ()
performBlock nsManagedObjectContext  block =
  sendMsg nsManagedObjectContext (mkSelector "performBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- performBlockAndWait:@
performBlockAndWait :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Ptr () -> IO ()
performBlockAndWait nsManagedObjectContext  block =
  sendMsg nsManagedObjectContext (mkSelector "performBlockAndWait:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- objectRegisteredForID:@
objectRegisteredForID :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectID objectID) => nsManagedObjectContext -> objectID -> IO (Id NSManagedObject)
objectRegisteredForID nsManagedObjectContext  objectID =
withObjCPtr objectID $ \raw_objectID ->
    sendMsg nsManagedObjectContext (mkSelector "objectRegisteredForID:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectWithID:@
objectWithID :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectID objectID) => nsManagedObjectContext -> objectID -> IO (Id NSManagedObject)
objectWithID nsManagedObjectContext  objectID =
withObjCPtr objectID $ \raw_objectID ->
    sendMsg nsManagedObjectContext (mkSelector "objectWithID:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ())] >>= retainedObject . castPtr

-- | @- existingObjectWithID:error:@
existingObjectWithID_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectID objectID, IsNSError error_) => nsManagedObjectContext -> objectID -> error_ -> IO (Id NSManagedObject)
existingObjectWithID_error nsManagedObjectContext  objectID error_ =
withObjCPtr objectID $ \raw_objectID ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsManagedObjectContext (mkSelector "existingObjectWithID:error:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- executeFetchRequest:error:@
executeFetchRequest_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSFetchRequest request, IsNSError error_) => nsManagedObjectContext -> request -> error_ -> IO (Id NSArray)
executeFetchRequest_error nsManagedObjectContext  request error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsManagedObjectContext (mkSelector "executeFetchRequest:error:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- countForFetchRequest:error:@
countForFetchRequest_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSFetchRequest request, IsNSError error_) => nsManagedObjectContext -> request -> error_ -> IO CULong
countForFetchRequest_error nsManagedObjectContext  request error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsManagedObjectContext (mkSelector "countForFetchRequest:error:") retCULong [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- executeRequest:error:@
executeRequest_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSPersistentStoreRequest request, IsNSError error_) => nsManagedObjectContext -> request -> error_ -> IO (Id NSPersistentStoreResult)
executeRequest_error nsManagedObjectContext  request error_ =
withObjCPtr request $ \raw_request ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg nsManagedObjectContext (mkSelector "executeRequest:error:") (retPtr retVoid) [argPtr (castPtr raw_request :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- insertObject:@
insertObject :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> IO ()
insertObject nsManagedObjectContext  object =
withObjCPtr object $ \raw_object ->
    sendMsg nsManagedObjectContext (mkSelector "insertObject:") retVoid [argPtr (castPtr raw_object :: Ptr ())]

-- | @- deleteObject:@
deleteObject :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> IO ()
deleteObject nsManagedObjectContext  object =
withObjCPtr object $ \raw_object ->
    sendMsg nsManagedObjectContext (mkSelector "deleteObject:") retVoid [argPtr (castPtr raw_object :: Ptr ())]

-- | @- refreshObject:mergeChanges:@
refreshObject_mergeChanges :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> Bool -> IO ()
refreshObject_mergeChanges nsManagedObjectContext  object flag =
withObjCPtr object $ \raw_object ->
    sendMsg nsManagedObjectContext (mkSelector "refreshObject:mergeChanges:") retVoid [argPtr (castPtr raw_object :: Ptr ()), argCULong (if flag then 1 else 0)]

-- | @- detectConflictsForObject:@
detectConflictsForObject :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> IO ()
detectConflictsForObject nsManagedObjectContext  object =
withObjCPtr object $ \raw_object ->
    sendMsg nsManagedObjectContext (mkSelector "detectConflictsForObject:") retVoid [argPtr (castPtr raw_object :: Ptr ())]

-- | @- observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_context :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSString keyPath, IsNSDictionary change) => nsManagedObjectContext -> keyPath -> RawId -> change -> Ptr () -> IO ()
observeValueForKeyPath_ofObject_change_context nsManagedObjectContext  keyPath object change context =
withObjCPtr keyPath $ \raw_keyPath ->
  withObjCPtr change $ \raw_change ->
      sendMsg nsManagedObjectContext (mkSelector "observeValueForKeyPath:ofObject:change:context:") retVoid [argPtr (castPtr raw_keyPath :: Ptr ()), argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_change :: Ptr ()), argPtr context]

-- | @- processPendingChanges@
processPendingChanges :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
processPendingChanges nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "processPendingChanges") retVoid []

-- | @- assignObject:toPersistentStore:@
assignObject_toPersistentStore :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSPersistentStore store) => nsManagedObjectContext -> RawId -> store -> IO ()
assignObject_toPersistentStore nsManagedObjectContext  object store =
withObjCPtr store $ \raw_store ->
    sendMsg nsManagedObjectContext (mkSelector "assignObject:toPersistentStore:") retVoid [argPtr (castPtr (unRawId object) :: Ptr ()), argPtr (castPtr raw_store :: Ptr ())]

-- | @- undo@
undo :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
undo nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "undo") retVoid []

-- | @- redo@
redo :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
redo nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "redo") retVoid []

-- | @- reset@
reset :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
reset nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "reset") retVoid []

-- | @- rollback@
rollback :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
rollback nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "rollback") retVoid []

-- | @- save:@
save :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSError error_) => nsManagedObjectContext -> error_ -> IO Bool
save nsManagedObjectContext  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "save:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- refreshAllObjects@
refreshAllObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
refreshAllObjects nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "refreshAllObjects") retVoid []

-- | @- lock@
lock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
lock nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "lock") retVoid []

-- | @- unlock@
unlock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
unlock nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "unlock") retVoid []

-- | @- tryLock@
tryLock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
tryLock nsManagedObjectContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "tryLock") retCULong []

-- | @- shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:@
shouldHandleInaccessibleFault_forObjectID_triggeredByProperty :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject fault, IsNSManagedObjectID oid, IsNSPropertyDescription property) => nsManagedObjectContext -> fault -> oid -> property -> IO Bool
shouldHandleInaccessibleFault_forObjectID_triggeredByProperty nsManagedObjectContext  fault oid property =
withObjCPtr fault $ \raw_fault ->
  withObjCPtr oid $ \raw_oid ->
    withObjCPtr property $ \raw_property ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:") retCULong [argPtr (castPtr raw_fault :: Ptr ()), argPtr (castPtr raw_oid :: Ptr ()), argPtr (castPtr raw_property :: Ptr ())]

-- | @- obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSArray objects, IsNSError error_) => nsManagedObjectContext -> objects -> error_ -> IO Bool
obtainPermanentIDsForObjects_error nsManagedObjectContext  objects error_ =
withObjCPtr objects $ \raw_objects ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "obtainPermanentIDsForObjects:error:") retCULong [argPtr (castPtr raw_objects :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- mergeChangesFromContextDidSaveNotification:@
mergeChangesFromContextDidSaveNotification :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSNotification notification) => nsManagedObjectContext -> notification -> IO ()
mergeChangesFromContextDidSaveNotification nsManagedObjectContext  notification =
withObjCPtr notification $ \raw_notification ->
    sendMsg nsManagedObjectContext (mkSelector "mergeChangesFromContextDidSaveNotification:") retVoid [argPtr (castPtr raw_notification :: Ptr ())]

-- | @+ mergeChangesFromRemoteContextSave:intoContexts:@
mergeChangesFromRemoteContextSave_intoContexts :: (IsNSDictionary changeNotificationData, IsNSArray contexts) => changeNotificationData -> contexts -> IO ()
mergeChangesFromRemoteContextSave_intoContexts changeNotificationData contexts =
  do
    cls' <- getRequiredClass "NSManagedObjectContext"
    withObjCPtr changeNotificationData $ \raw_changeNotificationData ->
      withObjCPtr contexts $ \raw_contexts ->
        sendClassMsg cls' (mkSelector "mergeChangesFromRemoteContextSave:intoContexts:") retVoid [argPtr (castPtr raw_changeNotificationData :: Ptr ()), argPtr (castPtr raw_contexts :: Ptr ())]

-- | @- setQueryGenerationFromToken:error:@
setQueryGenerationFromToken_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSQueryGenerationToken generation, IsNSError error_) => nsManagedObjectContext -> generation -> error_ -> IO Bool
setQueryGenerationFromToken_error nsManagedObjectContext  generation error_ =
withObjCPtr generation $ \raw_generation ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "setQueryGenerationFromToken:error:") retCULong [argPtr (castPtr raw_generation :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- persistentStoreCoordinator@
persistentStoreCoordinator :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSPersistentStoreCoordinator)
persistentStoreCoordinator nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "persistentStoreCoordinator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPersistentStoreCoordinator:@
setPersistentStoreCoordinator :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSPersistentStoreCoordinator value) => nsManagedObjectContext -> value -> IO ()
setPersistentStoreCoordinator nsManagedObjectContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectContext (mkSelector "setPersistentStoreCoordinator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- parentContext@
parentContext :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSManagedObjectContext)
parentContext nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "parentContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setParentContext:@
setParentContext :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectContext value) => nsManagedObjectContext -> value -> IO ()
setParentContext nsManagedObjectContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectContext (mkSelector "setParentContext:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSString)
name nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSString value) => nsManagedObjectContext -> value -> IO ()
setName nsManagedObjectContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectContext (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- undoManager@
undoManager :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSUndoManager)
undoManager nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "undoManager") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUndoManager:@
setUndoManager :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSUndoManager value) => nsManagedObjectContext -> value -> IO ()
setUndoManager nsManagedObjectContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectContext (mkSelector "setUndoManager:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- hasChanges@
hasChanges :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
hasChanges nsManagedObjectContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "hasChanges") retCULong []

-- | @- userInfo@
userInfo :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSMutableDictionary)
userInfo nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "userInfo") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- concurrencyType@
concurrencyType :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO NSManagedObjectContextConcurrencyType
concurrencyType nsManagedObjectContext  =
  fmap (coerce :: CULong -> NSManagedObjectContextConcurrencyType) $ sendMsg nsManagedObjectContext (mkSelector "concurrencyType") retCULong []

-- | @- insertedObjects@
insertedObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
insertedObjects nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "insertedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- updatedObjects@
updatedObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
updatedObjects nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "updatedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- deletedObjects@
deletedObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
deletedObjects nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "deletedObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- registeredObjects@
registeredObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
registeredObjects nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "registeredObjects") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- propagatesDeletesAtEndOfEvent@
propagatesDeletesAtEndOfEvent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
propagatesDeletesAtEndOfEvent nsManagedObjectContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "propagatesDeletesAtEndOfEvent") retCULong []

-- | @- setPropagatesDeletesAtEndOfEvent:@
setPropagatesDeletesAtEndOfEvent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setPropagatesDeletesAtEndOfEvent nsManagedObjectContext  value =
  sendMsg nsManagedObjectContext (mkSelector "setPropagatesDeletesAtEndOfEvent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- retainsRegisteredObjects@
retainsRegisteredObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
retainsRegisteredObjects nsManagedObjectContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "retainsRegisteredObjects") retCULong []

-- | @- setRetainsRegisteredObjects:@
setRetainsRegisteredObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setRetainsRegisteredObjects nsManagedObjectContext  value =
  sendMsg nsManagedObjectContext (mkSelector "setRetainsRegisteredObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | @- shouldDeleteInaccessibleFaults@
shouldDeleteInaccessibleFaults :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
shouldDeleteInaccessibleFaults nsManagedObjectContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "shouldDeleteInaccessibleFaults") retCULong []

-- | @- setShouldDeleteInaccessibleFaults:@
setShouldDeleteInaccessibleFaults :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setShouldDeleteInaccessibleFaults nsManagedObjectContext  value =
  sendMsg nsManagedObjectContext (mkSelector "setShouldDeleteInaccessibleFaults:") retVoid [argCULong (if value then 1 else 0)]

-- | @- stalenessInterval@
stalenessInterval :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO CDouble
stalenessInterval nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "stalenessInterval") retCDouble []

-- | @- setStalenessInterval:@
setStalenessInterval :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> CDouble -> IO ()
setStalenessInterval nsManagedObjectContext  value =
  sendMsg nsManagedObjectContext (mkSelector "setStalenessInterval:") retVoid [argCDouble (fromIntegral value)]

-- | @- mergePolicy@
mergePolicy :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO RawId
mergePolicy nsManagedObjectContext  =
  fmap (RawId . castPtr) $ sendMsg nsManagedObjectContext (mkSelector "mergePolicy") (retPtr retVoid) []

-- | @- setMergePolicy:@
setMergePolicy :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> RawId -> IO ()
setMergePolicy nsManagedObjectContext  value =
  sendMsg nsManagedObjectContext (mkSelector "setMergePolicy:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- queryGenerationToken@
queryGenerationToken :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSQueryGenerationToken)
queryGenerationToken nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "queryGenerationToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- automaticallyMergesChangesFromParent@
automaticallyMergesChangesFromParent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
automaticallyMergesChangesFromParent nsManagedObjectContext  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObjectContext (mkSelector "automaticallyMergesChangesFromParent") retCULong []

-- | @- setAutomaticallyMergesChangesFromParent:@
setAutomaticallyMergesChangesFromParent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setAutomaticallyMergesChangesFromParent nsManagedObjectContext  value =
  sendMsg nsManagedObjectContext (mkSelector "setAutomaticallyMergesChangesFromParent:") retVoid [argCULong (if value then 1 else 0)]

-- | @- transactionAuthor@
transactionAuthor :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSString)
transactionAuthor nsManagedObjectContext  =
  sendMsg nsManagedObjectContext (mkSelector "transactionAuthor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTransactionAuthor:@
setTransactionAuthor :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSString value) => nsManagedObjectContext -> value -> IO ()
setTransactionAuthor nsManagedObjectContext  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsManagedObjectContext (mkSelector "setTransactionAuthor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithConcurrencyType:@
initWithConcurrencyTypeSelector :: Selector
initWithConcurrencyTypeSelector = mkSelector "initWithConcurrencyType:"

-- | @Selector@ for @performBlock:@
performBlockSelector :: Selector
performBlockSelector = mkSelector "performBlock:"

-- | @Selector@ for @performBlockAndWait:@
performBlockAndWaitSelector :: Selector
performBlockAndWaitSelector = mkSelector "performBlockAndWait:"

-- | @Selector@ for @objectRegisteredForID:@
objectRegisteredForIDSelector :: Selector
objectRegisteredForIDSelector = mkSelector "objectRegisteredForID:"

-- | @Selector@ for @objectWithID:@
objectWithIDSelector :: Selector
objectWithIDSelector = mkSelector "objectWithID:"

-- | @Selector@ for @existingObjectWithID:error:@
existingObjectWithID_errorSelector :: Selector
existingObjectWithID_errorSelector = mkSelector "existingObjectWithID:error:"

-- | @Selector@ for @executeFetchRequest:error:@
executeFetchRequest_errorSelector :: Selector
executeFetchRequest_errorSelector = mkSelector "executeFetchRequest:error:"

-- | @Selector@ for @countForFetchRequest:error:@
countForFetchRequest_errorSelector :: Selector
countForFetchRequest_errorSelector = mkSelector "countForFetchRequest:error:"

-- | @Selector@ for @executeRequest:error:@
executeRequest_errorSelector :: Selector
executeRequest_errorSelector = mkSelector "executeRequest:error:"

-- | @Selector@ for @insertObject:@
insertObjectSelector :: Selector
insertObjectSelector = mkSelector "insertObject:"

-- | @Selector@ for @deleteObject:@
deleteObjectSelector :: Selector
deleteObjectSelector = mkSelector "deleteObject:"

-- | @Selector@ for @refreshObject:mergeChanges:@
refreshObject_mergeChangesSelector :: Selector
refreshObject_mergeChangesSelector = mkSelector "refreshObject:mergeChanges:"

-- | @Selector@ for @detectConflictsForObject:@
detectConflictsForObjectSelector :: Selector
detectConflictsForObjectSelector = mkSelector "detectConflictsForObject:"

-- | @Selector@ for @observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_contextSelector :: Selector
observeValueForKeyPath_ofObject_change_contextSelector = mkSelector "observeValueForKeyPath:ofObject:change:context:"

-- | @Selector@ for @processPendingChanges@
processPendingChangesSelector :: Selector
processPendingChangesSelector = mkSelector "processPendingChanges"

-- | @Selector@ for @assignObject:toPersistentStore:@
assignObject_toPersistentStoreSelector :: Selector
assignObject_toPersistentStoreSelector = mkSelector "assignObject:toPersistentStore:"

-- | @Selector@ for @undo@
undoSelector :: Selector
undoSelector = mkSelector "undo"

-- | @Selector@ for @redo@
redoSelector :: Selector
redoSelector = mkSelector "redo"

-- | @Selector@ for @reset@
resetSelector :: Selector
resetSelector = mkSelector "reset"

-- | @Selector@ for @rollback@
rollbackSelector :: Selector
rollbackSelector = mkSelector "rollback"

-- | @Selector@ for @save:@
saveSelector :: Selector
saveSelector = mkSelector "save:"

-- | @Selector@ for @refreshAllObjects@
refreshAllObjectsSelector :: Selector
refreshAllObjectsSelector = mkSelector "refreshAllObjects"

-- | @Selector@ for @lock@
lockSelector :: Selector
lockSelector = mkSelector "lock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:@
shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector :: Selector
shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector = mkSelector "shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:"

-- | @Selector@ for @obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_errorSelector :: Selector
obtainPermanentIDsForObjects_errorSelector = mkSelector "obtainPermanentIDsForObjects:error:"

-- | @Selector@ for @mergeChangesFromContextDidSaveNotification:@
mergeChangesFromContextDidSaveNotificationSelector :: Selector
mergeChangesFromContextDidSaveNotificationSelector = mkSelector "mergeChangesFromContextDidSaveNotification:"

-- | @Selector@ for @mergeChangesFromRemoteContextSave:intoContexts:@
mergeChangesFromRemoteContextSave_intoContextsSelector :: Selector
mergeChangesFromRemoteContextSave_intoContextsSelector = mkSelector "mergeChangesFromRemoteContextSave:intoContexts:"

-- | @Selector@ for @setQueryGenerationFromToken:error:@
setQueryGenerationFromToken_errorSelector :: Selector
setQueryGenerationFromToken_errorSelector = mkSelector "setQueryGenerationFromToken:error:"

-- | @Selector@ for @persistentStoreCoordinator@
persistentStoreCoordinatorSelector :: Selector
persistentStoreCoordinatorSelector = mkSelector "persistentStoreCoordinator"

-- | @Selector@ for @setPersistentStoreCoordinator:@
setPersistentStoreCoordinatorSelector :: Selector
setPersistentStoreCoordinatorSelector = mkSelector "setPersistentStoreCoordinator:"

-- | @Selector@ for @parentContext@
parentContextSelector :: Selector
parentContextSelector = mkSelector "parentContext"

-- | @Selector@ for @setParentContext:@
setParentContextSelector :: Selector
setParentContextSelector = mkSelector "setParentContext:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector
undoManagerSelector = mkSelector "undoManager"

-- | @Selector@ for @setUndoManager:@
setUndoManagerSelector :: Selector
setUndoManagerSelector = mkSelector "setUndoManager:"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector
hasChangesSelector = mkSelector "hasChanges"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @concurrencyType@
concurrencyTypeSelector :: Selector
concurrencyTypeSelector = mkSelector "concurrencyType"

-- | @Selector@ for @insertedObjects@
insertedObjectsSelector :: Selector
insertedObjectsSelector = mkSelector "insertedObjects"

-- | @Selector@ for @updatedObjects@
updatedObjectsSelector :: Selector
updatedObjectsSelector = mkSelector "updatedObjects"

-- | @Selector@ for @deletedObjects@
deletedObjectsSelector :: Selector
deletedObjectsSelector = mkSelector "deletedObjects"

-- | @Selector@ for @registeredObjects@
registeredObjectsSelector :: Selector
registeredObjectsSelector = mkSelector "registeredObjects"

-- | @Selector@ for @propagatesDeletesAtEndOfEvent@
propagatesDeletesAtEndOfEventSelector :: Selector
propagatesDeletesAtEndOfEventSelector = mkSelector "propagatesDeletesAtEndOfEvent"

-- | @Selector@ for @setPropagatesDeletesAtEndOfEvent:@
setPropagatesDeletesAtEndOfEventSelector :: Selector
setPropagatesDeletesAtEndOfEventSelector = mkSelector "setPropagatesDeletesAtEndOfEvent:"

-- | @Selector@ for @retainsRegisteredObjects@
retainsRegisteredObjectsSelector :: Selector
retainsRegisteredObjectsSelector = mkSelector "retainsRegisteredObjects"

-- | @Selector@ for @setRetainsRegisteredObjects:@
setRetainsRegisteredObjectsSelector :: Selector
setRetainsRegisteredObjectsSelector = mkSelector "setRetainsRegisteredObjects:"

-- | @Selector@ for @shouldDeleteInaccessibleFaults@
shouldDeleteInaccessibleFaultsSelector :: Selector
shouldDeleteInaccessibleFaultsSelector = mkSelector "shouldDeleteInaccessibleFaults"

-- | @Selector@ for @setShouldDeleteInaccessibleFaults:@
setShouldDeleteInaccessibleFaultsSelector :: Selector
setShouldDeleteInaccessibleFaultsSelector = mkSelector "setShouldDeleteInaccessibleFaults:"

-- | @Selector@ for @stalenessInterval@
stalenessIntervalSelector :: Selector
stalenessIntervalSelector = mkSelector "stalenessInterval"

-- | @Selector@ for @setStalenessInterval:@
setStalenessIntervalSelector :: Selector
setStalenessIntervalSelector = mkSelector "setStalenessInterval:"

-- | @Selector@ for @mergePolicy@
mergePolicySelector :: Selector
mergePolicySelector = mkSelector "mergePolicy"

-- | @Selector@ for @setMergePolicy:@
setMergePolicySelector :: Selector
setMergePolicySelector = mkSelector "setMergePolicy:"

-- | @Selector@ for @queryGenerationToken@
queryGenerationTokenSelector :: Selector
queryGenerationTokenSelector = mkSelector "queryGenerationToken"

-- | @Selector@ for @automaticallyMergesChangesFromParent@
automaticallyMergesChangesFromParentSelector :: Selector
automaticallyMergesChangesFromParentSelector = mkSelector "automaticallyMergesChangesFromParent"

-- | @Selector@ for @setAutomaticallyMergesChangesFromParent:@
setAutomaticallyMergesChangesFromParentSelector :: Selector
setAutomaticallyMergesChangesFromParentSelector = mkSelector "setAutomaticallyMergesChangesFromParent:"

-- | @Selector@ for @transactionAuthor@
transactionAuthorSelector :: Selector
transactionAuthorSelector = mkSelector "transactionAuthor"

-- | @Selector@ for @setTransactionAuthor:@
setTransactionAuthorSelector :: Selector
setTransactionAuthorSelector = mkSelector "setTransactionAuthor:"

