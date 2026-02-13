{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSManagedObjectContext@.
module ObjC.CoreData.NSManagedObjectContext
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
  , assignObject_toPersistentStoreSelector
  , automaticallyMergesChangesFromParentSelector
  , concurrencyTypeSelector
  , countForFetchRequest_errorSelector
  , deleteObjectSelector
  , deletedObjectsSelector
  , detectConflictsForObjectSelector
  , executeFetchRequest_errorSelector
  , executeRequest_errorSelector
  , existingObjectWithID_errorSelector
  , hasChangesSelector
  , initSelector
  , initWithConcurrencyTypeSelector
  , insertObjectSelector
  , insertedObjectsSelector
  , lockSelector
  , mergeChangesFromContextDidSaveNotificationSelector
  , mergeChangesFromRemoteContextSave_intoContextsSelector
  , mergePolicySelector
  , nameSelector
  , newSelector
  , objectRegisteredForIDSelector
  , objectWithIDSelector
  , observeValueForKeyPath_ofObject_change_contextSelector
  , obtainPermanentIDsForObjects_errorSelector
  , parentContextSelector
  , performBlockAndWaitSelector
  , performBlockSelector
  , persistentStoreCoordinatorSelector
  , processPendingChangesSelector
  , propagatesDeletesAtEndOfEventSelector
  , queryGenerationTokenSelector
  , redoSelector
  , refreshAllObjectsSelector
  , refreshObject_mergeChangesSelector
  , registeredObjectsSelector
  , resetSelector
  , retainsRegisteredObjectsSelector
  , rollbackSelector
  , saveSelector
  , setAutomaticallyMergesChangesFromParentSelector
  , setMergePolicySelector
  , setNameSelector
  , setParentContextSelector
  , setPersistentStoreCoordinatorSelector
  , setPropagatesDeletesAtEndOfEventSelector
  , setQueryGenerationFromToken_errorSelector
  , setRetainsRegisteredObjectsSelector
  , setShouldDeleteInaccessibleFaultsSelector
  , setStalenessIntervalSelector
  , setTransactionAuthorSelector
  , setUndoManagerSelector
  , shouldDeleteInaccessibleFaultsSelector
  , shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector
  , stalenessIntervalSelector
  , transactionAuthorSelector
  , tryLockSelector
  , undoManagerSelector
  , undoSelector
  , unlockSelector
  , updatedObjectsSelector
  , userInfoSelector

  -- * Enum types
  , NSManagedObjectContextConcurrencyType(NSManagedObjectContextConcurrencyType)
  , pattern NSConfinementConcurrencyType
  , pattern NSPrivateQueueConcurrencyType
  , pattern NSMainQueueConcurrencyType

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id NSManagedObjectContext)
new  =
  do
    cls' <- getRequiredClass "NSManagedObjectContext"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSManagedObjectContext)
init_ nsManagedObjectContext =
  sendOwnedMessage nsManagedObjectContext initSelector

-- | @- initWithConcurrencyType:@
initWithConcurrencyType :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> NSManagedObjectContextConcurrencyType -> IO (Id NSManagedObjectContext)
initWithConcurrencyType nsManagedObjectContext ct =
  sendOwnedMessage nsManagedObjectContext initWithConcurrencyTypeSelector ct

-- | @- performBlock:@
performBlock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Ptr () -> IO ()
performBlock nsManagedObjectContext block =
  sendMessage nsManagedObjectContext performBlockSelector block

-- | @- performBlockAndWait:@
performBlockAndWait :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Ptr () -> IO ()
performBlockAndWait nsManagedObjectContext block =
  sendMessage nsManagedObjectContext performBlockAndWaitSelector block

-- | @- objectRegisteredForID:@
objectRegisteredForID :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectID objectID) => nsManagedObjectContext -> objectID -> IO (Id NSManagedObject)
objectRegisteredForID nsManagedObjectContext objectID =
  sendMessage nsManagedObjectContext objectRegisteredForIDSelector (toNSManagedObjectID objectID)

-- | @- objectWithID:@
objectWithID :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectID objectID) => nsManagedObjectContext -> objectID -> IO (Id NSManagedObject)
objectWithID nsManagedObjectContext objectID =
  sendMessage nsManagedObjectContext objectWithIDSelector (toNSManagedObjectID objectID)

-- | @- existingObjectWithID:error:@
existingObjectWithID_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectID objectID, IsNSError error_) => nsManagedObjectContext -> objectID -> error_ -> IO (Id NSManagedObject)
existingObjectWithID_error nsManagedObjectContext objectID error_ =
  sendMessage nsManagedObjectContext existingObjectWithID_errorSelector (toNSManagedObjectID objectID) (toNSError error_)

-- | @- executeFetchRequest:error:@
executeFetchRequest_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSFetchRequest request, IsNSError error_) => nsManagedObjectContext -> request -> error_ -> IO (Id NSArray)
executeFetchRequest_error nsManagedObjectContext request error_ =
  sendMessage nsManagedObjectContext executeFetchRequest_errorSelector (toNSFetchRequest request) (toNSError error_)

-- | @- countForFetchRequest:error:@
countForFetchRequest_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSFetchRequest request, IsNSError error_) => nsManagedObjectContext -> request -> error_ -> IO CULong
countForFetchRequest_error nsManagedObjectContext request error_ =
  sendMessage nsManagedObjectContext countForFetchRequest_errorSelector (toNSFetchRequest request) (toNSError error_)

-- | @- executeRequest:error:@
executeRequest_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSPersistentStoreRequest request, IsNSError error_) => nsManagedObjectContext -> request -> error_ -> IO (Id NSPersistentStoreResult)
executeRequest_error nsManagedObjectContext request error_ =
  sendMessage nsManagedObjectContext executeRequest_errorSelector (toNSPersistentStoreRequest request) (toNSError error_)

-- | @- insertObject:@
insertObject :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> IO ()
insertObject nsManagedObjectContext object =
  sendMessage nsManagedObjectContext insertObjectSelector (toNSManagedObject object)

-- | @- deleteObject:@
deleteObject :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> IO ()
deleteObject nsManagedObjectContext object =
  sendMessage nsManagedObjectContext deleteObjectSelector (toNSManagedObject object)

-- | @- refreshObject:mergeChanges:@
refreshObject_mergeChanges :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> Bool -> IO ()
refreshObject_mergeChanges nsManagedObjectContext object flag =
  sendMessage nsManagedObjectContext refreshObject_mergeChangesSelector (toNSManagedObject object) flag

-- | @- detectConflictsForObject:@
detectConflictsForObject :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject object) => nsManagedObjectContext -> object -> IO ()
detectConflictsForObject nsManagedObjectContext object =
  sendMessage nsManagedObjectContext detectConflictsForObjectSelector (toNSManagedObject object)

-- | @- observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_context :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSString keyPath, IsNSDictionary change) => nsManagedObjectContext -> keyPath -> RawId -> change -> Ptr () -> IO ()
observeValueForKeyPath_ofObject_change_context nsManagedObjectContext keyPath object change context =
  sendMessage nsManagedObjectContext observeValueForKeyPath_ofObject_change_contextSelector (toNSString keyPath) object (toNSDictionary change) context

-- | @- processPendingChanges@
processPendingChanges :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
processPendingChanges nsManagedObjectContext =
  sendMessage nsManagedObjectContext processPendingChangesSelector

-- | @- assignObject:toPersistentStore:@
assignObject_toPersistentStore :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSPersistentStore store) => nsManagedObjectContext -> RawId -> store -> IO ()
assignObject_toPersistentStore nsManagedObjectContext object store =
  sendMessage nsManagedObjectContext assignObject_toPersistentStoreSelector object (toNSPersistentStore store)

-- | @- undo@
undo :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
undo nsManagedObjectContext =
  sendMessage nsManagedObjectContext undoSelector

-- | @- redo@
redo :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
redo nsManagedObjectContext =
  sendMessage nsManagedObjectContext redoSelector

-- | @- reset@
reset :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
reset nsManagedObjectContext =
  sendMessage nsManagedObjectContext resetSelector

-- | @- rollback@
rollback :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
rollback nsManagedObjectContext =
  sendMessage nsManagedObjectContext rollbackSelector

-- | @- save:@
save :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSError error_) => nsManagedObjectContext -> error_ -> IO Bool
save nsManagedObjectContext error_ =
  sendMessage nsManagedObjectContext saveSelector (toNSError error_)

-- | @- refreshAllObjects@
refreshAllObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
refreshAllObjects nsManagedObjectContext =
  sendMessage nsManagedObjectContext refreshAllObjectsSelector

-- | @- lock@
lock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
lock nsManagedObjectContext =
  sendMessage nsManagedObjectContext lockSelector

-- | @- unlock@
unlock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO ()
unlock nsManagedObjectContext =
  sendMessage nsManagedObjectContext unlockSelector

-- | @- tryLock@
tryLock :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
tryLock nsManagedObjectContext =
  sendMessage nsManagedObjectContext tryLockSelector

-- | @- shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:@
shouldHandleInaccessibleFault_forObjectID_triggeredByProperty :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObject fault, IsNSManagedObjectID oid, IsNSPropertyDescription property) => nsManagedObjectContext -> fault -> oid -> property -> IO Bool
shouldHandleInaccessibleFault_forObjectID_triggeredByProperty nsManagedObjectContext fault oid property =
  sendMessage nsManagedObjectContext shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector (toNSManagedObject fault) (toNSManagedObjectID oid) (toNSPropertyDescription property)

-- | @- obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSArray objects, IsNSError error_) => nsManagedObjectContext -> objects -> error_ -> IO Bool
obtainPermanentIDsForObjects_error nsManagedObjectContext objects error_ =
  sendMessage nsManagedObjectContext obtainPermanentIDsForObjects_errorSelector (toNSArray objects) (toNSError error_)

-- | @- mergeChangesFromContextDidSaveNotification:@
mergeChangesFromContextDidSaveNotification :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSNotification notification) => nsManagedObjectContext -> notification -> IO ()
mergeChangesFromContextDidSaveNotification nsManagedObjectContext notification =
  sendMessage nsManagedObjectContext mergeChangesFromContextDidSaveNotificationSelector (toNSNotification notification)

-- | @+ mergeChangesFromRemoteContextSave:intoContexts:@
mergeChangesFromRemoteContextSave_intoContexts :: (IsNSDictionary changeNotificationData, IsNSArray contexts) => changeNotificationData -> contexts -> IO ()
mergeChangesFromRemoteContextSave_intoContexts changeNotificationData contexts =
  do
    cls' <- getRequiredClass "NSManagedObjectContext"
    sendClassMessage cls' mergeChangesFromRemoteContextSave_intoContextsSelector (toNSDictionary changeNotificationData) (toNSArray contexts)

-- | @- setQueryGenerationFromToken:error:@
setQueryGenerationFromToken_error :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSQueryGenerationToken generation, IsNSError error_) => nsManagedObjectContext -> generation -> error_ -> IO Bool
setQueryGenerationFromToken_error nsManagedObjectContext generation error_ =
  sendMessage nsManagedObjectContext setQueryGenerationFromToken_errorSelector (toNSQueryGenerationToken generation) (toNSError error_)

-- | @- persistentStoreCoordinator@
persistentStoreCoordinator :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSPersistentStoreCoordinator)
persistentStoreCoordinator nsManagedObjectContext =
  sendMessage nsManagedObjectContext persistentStoreCoordinatorSelector

-- | @- setPersistentStoreCoordinator:@
setPersistentStoreCoordinator :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSPersistentStoreCoordinator value) => nsManagedObjectContext -> value -> IO ()
setPersistentStoreCoordinator nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setPersistentStoreCoordinatorSelector (toNSPersistentStoreCoordinator value)

-- | @- parentContext@
parentContext :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSManagedObjectContext)
parentContext nsManagedObjectContext =
  sendMessage nsManagedObjectContext parentContextSelector

-- | @- setParentContext:@
setParentContext :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSManagedObjectContext value) => nsManagedObjectContext -> value -> IO ()
setParentContext nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setParentContextSelector (toNSManagedObjectContext value)

-- | @- name@
name :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSString)
name nsManagedObjectContext =
  sendMessage nsManagedObjectContext nameSelector

-- | @- setName:@
setName :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSString value) => nsManagedObjectContext -> value -> IO ()
setName nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setNameSelector (toNSString value)

-- | @- undoManager@
undoManager :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSUndoManager)
undoManager nsManagedObjectContext =
  sendMessage nsManagedObjectContext undoManagerSelector

-- | @- setUndoManager:@
setUndoManager :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSUndoManager value) => nsManagedObjectContext -> value -> IO ()
setUndoManager nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setUndoManagerSelector (toNSUndoManager value)

-- | @- hasChanges@
hasChanges :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
hasChanges nsManagedObjectContext =
  sendMessage nsManagedObjectContext hasChangesSelector

-- | @- userInfo@
userInfo :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSMutableDictionary)
userInfo nsManagedObjectContext =
  sendMessage nsManagedObjectContext userInfoSelector

-- | @- concurrencyType@
concurrencyType :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO NSManagedObjectContextConcurrencyType
concurrencyType nsManagedObjectContext =
  sendMessage nsManagedObjectContext concurrencyTypeSelector

-- | @- insertedObjects@
insertedObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
insertedObjects nsManagedObjectContext =
  sendMessage nsManagedObjectContext insertedObjectsSelector

-- | @- updatedObjects@
updatedObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
updatedObjects nsManagedObjectContext =
  sendMessage nsManagedObjectContext updatedObjectsSelector

-- | @- deletedObjects@
deletedObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
deletedObjects nsManagedObjectContext =
  sendMessage nsManagedObjectContext deletedObjectsSelector

-- | @- registeredObjects@
registeredObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSSet)
registeredObjects nsManagedObjectContext =
  sendMessage nsManagedObjectContext registeredObjectsSelector

-- | @- propagatesDeletesAtEndOfEvent@
propagatesDeletesAtEndOfEvent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
propagatesDeletesAtEndOfEvent nsManagedObjectContext =
  sendMessage nsManagedObjectContext propagatesDeletesAtEndOfEventSelector

-- | @- setPropagatesDeletesAtEndOfEvent:@
setPropagatesDeletesAtEndOfEvent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setPropagatesDeletesAtEndOfEvent nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setPropagatesDeletesAtEndOfEventSelector value

-- | @- retainsRegisteredObjects@
retainsRegisteredObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
retainsRegisteredObjects nsManagedObjectContext =
  sendMessage nsManagedObjectContext retainsRegisteredObjectsSelector

-- | @- setRetainsRegisteredObjects:@
setRetainsRegisteredObjects :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setRetainsRegisteredObjects nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setRetainsRegisteredObjectsSelector value

-- | @- shouldDeleteInaccessibleFaults@
shouldDeleteInaccessibleFaults :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
shouldDeleteInaccessibleFaults nsManagedObjectContext =
  sendMessage nsManagedObjectContext shouldDeleteInaccessibleFaultsSelector

-- | @- setShouldDeleteInaccessibleFaults:@
setShouldDeleteInaccessibleFaults :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setShouldDeleteInaccessibleFaults nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setShouldDeleteInaccessibleFaultsSelector value

-- | @- stalenessInterval@
stalenessInterval :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO CDouble
stalenessInterval nsManagedObjectContext =
  sendMessage nsManagedObjectContext stalenessIntervalSelector

-- | @- setStalenessInterval:@
setStalenessInterval :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> CDouble -> IO ()
setStalenessInterval nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setStalenessIntervalSelector value

-- | @- mergePolicy@
mergePolicy :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO RawId
mergePolicy nsManagedObjectContext =
  sendMessage nsManagedObjectContext mergePolicySelector

-- | @- setMergePolicy:@
setMergePolicy :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> RawId -> IO ()
setMergePolicy nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setMergePolicySelector value

-- | @- queryGenerationToken@
queryGenerationToken :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSQueryGenerationToken)
queryGenerationToken nsManagedObjectContext =
  sendMessage nsManagedObjectContext queryGenerationTokenSelector

-- | @- automaticallyMergesChangesFromParent@
automaticallyMergesChangesFromParent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO Bool
automaticallyMergesChangesFromParent nsManagedObjectContext =
  sendMessage nsManagedObjectContext automaticallyMergesChangesFromParentSelector

-- | @- setAutomaticallyMergesChangesFromParent:@
setAutomaticallyMergesChangesFromParent :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> Bool -> IO ()
setAutomaticallyMergesChangesFromParent nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setAutomaticallyMergesChangesFromParentSelector value

-- | @- transactionAuthor@
transactionAuthor :: IsNSManagedObjectContext nsManagedObjectContext => nsManagedObjectContext -> IO (Id NSString)
transactionAuthor nsManagedObjectContext =
  sendMessage nsManagedObjectContext transactionAuthorSelector

-- | @- setTransactionAuthor:@
setTransactionAuthor :: (IsNSManagedObjectContext nsManagedObjectContext, IsNSString value) => nsManagedObjectContext -> value -> IO ()
setTransactionAuthor nsManagedObjectContext value =
  sendMessage nsManagedObjectContext setTransactionAuthorSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id NSManagedObjectContext)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSManagedObjectContext)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithConcurrencyType:@
initWithConcurrencyTypeSelector :: Selector '[NSManagedObjectContextConcurrencyType] (Id NSManagedObjectContext)
initWithConcurrencyTypeSelector = mkSelector "initWithConcurrencyType:"

-- | @Selector@ for @performBlock:@
performBlockSelector :: Selector '[Ptr ()] ()
performBlockSelector = mkSelector "performBlock:"

-- | @Selector@ for @performBlockAndWait:@
performBlockAndWaitSelector :: Selector '[Ptr ()] ()
performBlockAndWaitSelector = mkSelector "performBlockAndWait:"

-- | @Selector@ for @objectRegisteredForID:@
objectRegisteredForIDSelector :: Selector '[Id NSManagedObjectID] (Id NSManagedObject)
objectRegisteredForIDSelector = mkSelector "objectRegisteredForID:"

-- | @Selector@ for @objectWithID:@
objectWithIDSelector :: Selector '[Id NSManagedObjectID] (Id NSManagedObject)
objectWithIDSelector = mkSelector "objectWithID:"

-- | @Selector@ for @existingObjectWithID:error:@
existingObjectWithID_errorSelector :: Selector '[Id NSManagedObjectID, Id NSError] (Id NSManagedObject)
existingObjectWithID_errorSelector = mkSelector "existingObjectWithID:error:"

-- | @Selector@ for @executeFetchRequest:error:@
executeFetchRequest_errorSelector :: Selector '[Id NSFetchRequest, Id NSError] (Id NSArray)
executeFetchRequest_errorSelector = mkSelector "executeFetchRequest:error:"

-- | @Selector@ for @countForFetchRequest:error:@
countForFetchRequest_errorSelector :: Selector '[Id NSFetchRequest, Id NSError] CULong
countForFetchRequest_errorSelector = mkSelector "countForFetchRequest:error:"

-- | @Selector@ for @executeRequest:error:@
executeRequest_errorSelector :: Selector '[Id NSPersistentStoreRequest, Id NSError] (Id NSPersistentStoreResult)
executeRequest_errorSelector = mkSelector "executeRequest:error:"

-- | @Selector@ for @insertObject:@
insertObjectSelector :: Selector '[Id NSManagedObject] ()
insertObjectSelector = mkSelector "insertObject:"

-- | @Selector@ for @deleteObject:@
deleteObjectSelector :: Selector '[Id NSManagedObject] ()
deleteObjectSelector = mkSelector "deleteObject:"

-- | @Selector@ for @refreshObject:mergeChanges:@
refreshObject_mergeChangesSelector :: Selector '[Id NSManagedObject, Bool] ()
refreshObject_mergeChangesSelector = mkSelector "refreshObject:mergeChanges:"

-- | @Selector@ for @detectConflictsForObject:@
detectConflictsForObjectSelector :: Selector '[Id NSManagedObject] ()
detectConflictsForObjectSelector = mkSelector "detectConflictsForObject:"

-- | @Selector@ for @observeValueForKeyPath:ofObject:change:context:@
observeValueForKeyPath_ofObject_change_contextSelector :: Selector '[Id NSString, RawId, Id NSDictionary, Ptr ()] ()
observeValueForKeyPath_ofObject_change_contextSelector = mkSelector "observeValueForKeyPath:ofObject:change:context:"

-- | @Selector@ for @processPendingChanges@
processPendingChangesSelector :: Selector '[] ()
processPendingChangesSelector = mkSelector "processPendingChanges"

-- | @Selector@ for @assignObject:toPersistentStore:@
assignObject_toPersistentStoreSelector :: Selector '[RawId, Id NSPersistentStore] ()
assignObject_toPersistentStoreSelector = mkSelector "assignObject:toPersistentStore:"

-- | @Selector@ for @undo@
undoSelector :: Selector '[] ()
undoSelector = mkSelector "undo"

-- | @Selector@ for @redo@
redoSelector :: Selector '[] ()
redoSelector = mkSelector "redo"

-- | @Selector@ for @reset@
resetSelector :: Selector '[] ()
resetSelector = mkSelector "reset"

-- | @Selector@ for @rollback@
rollbackSelector :: Selector '[] ()
rollbackSelector = mkSelector "rollback"

-- | @Selector@ for @save:@
saveSelector :: Selector '[Id NSError] Bool
saveSelector = mkSelector "save:"

-- | @Selector@ for @refreshAllObjects@
refreshAllObjectsSelector :: Selector '[] ()
refreshAllObjectsSelector = mkSelector "refreshAllObjects"

-- | @Selector@ for @lock@
lockSelector :: Selector '[] ()
lockSelector = mkSelector "lock"

-- | @Selector@ for @unlock@
unlockSelector :: Selector '[] ()
unlockSelector = mkSelector "unlock"

-- | @Selector@ for @tryLock@
tryLockSelector :: Selector '[] Bool
tryLockSelector = mkSelector "tryLock"

-- | @Selector@ for @shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:@
shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector :: Selector '[Id NSManagedObject, Id NSManagedObjectID, Id NSPropertyDescription] Bool
shouldHandleInaccessibleFault_forObjectID_triggeredByPropertySelector = mkSelector "shouldHandleInaccessibleFault:forObjectID:triggeredByProperty:"

-- | @Selector@ for @obtainPermanentIDsForObjects:error:@
obtainPermanentIDsForObjects_errorSelector :: Selector '[Id NSArray, Id NSError] Bool
obtainPermanentIDsForObjects_errorSelector = mkSelector "obtainPermanentIDsForObjects:error:"

-- | @Selector@ for @mergeChangesFromContextDidSaveNotification:@
mergeChangesFromContextDidSaveNotificationSelector :: Selector '[Id NSNotification] ()
mergeChangesFromContextDidSaveNotificationSelector = mkSelector "mergeChangesFromContextDidSaveNotification:"

-- | @Selector@ for @mergeChangesFromRemoteContextSave:intoContexts:@
mergeChangesFromRemoteContextSave_intoContextsSelector :: Selector '[Id NSDictionary, Id NSArray] ()
mergeChangesFromRemoteContextSave_intoContextsSelector = mkSelector "mergeChangesFromRemoteContextSave:intoContexts:"

-- | @Selector@ for @setQueryGenerationFromToken:error:@
setQueryGenerationFromToken_errorSelector :: Selector '[Id NSQueryGenerationToken, Id NSError] Bool
setQueryGenerationFromToken_errorSelector = mkSelector "setQueryGenerationFromToken:error:"

-- | @Selector@ for @persistentStoreCoordinator@
persistentStoreCoordinatorSelector :: Selector '[] (Id NSPersistentStoreCoordinator)
persistentStoreCoordinatorSelector = mkSelector "persistentStoreCoordinator"

-- | @Selector@ for @setPersistentStoreCoordinator:@
setPersistentStoreCoordinatorSelector :: Selector '[Id NSPersistentStoreCoordinator] ()
setPersistentStoreCoordinatorSelector = mkSelector "setPersistentStoreCoordinator:"

-- | @Selector@ for @parentContext@
parentContextSelector :: Selector '[] (Id NSManagedObjectContext)
parentContextSelector = mkSelector "parentContext"

-- | @Selector@ for @setParentContext:@
setParentContextSelector :: Selector '[Id NSManagedObjectContext] ()
setParentContextSelector = mkSelector "setParentContext:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @undoManager@
undoManagerSelector :: Selector '[] (Id NSUndoManager)
undoManagerSelector = mkSelector "undoManager"

-- | @Selector@ for @setUndoManager:@
setUndoManagerSelector :: Selector '[Id NSUndoManager] ()
setUndoManagerSelector = mkSelector "setUndoManager:"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector '[] Bool
hasChangesSelector = mkSelector "hasChanges"

-- | @Selector@ for @userInfo@
userInfoSelector :: Selector '[] (Id NSMutableDictionary)
userInfoSelector = mkSelector "userInfo"

-- | @Selector@ for @concurrencyType@
concurrencyTypeSelector :: Selector '[] NSManagedObjectContextConcurrencyType
concurrencyTypeSelector = mkSelector "concurrencyType"

-- | @Selector@ for @insertedObjects@
insertedObjectsSelector :: Selector '[] (Id NSSet)
insertedObjectsSelector = mkSelector "insertedObjects"

-- | @Selector@ for @updatedObjects@
updatedObjectsSelector :: Selector '[] (Id NSSet)
updatedObjectsSelector = mkSelector "updatedObjects"

-- | @Selector@ for @deletedObjects@
deletedObjectsSelector :: Selector '[] (Id NSSet)
deletedObjectsSelector = mkSelector "deletedObjects"

-- | @Selector@ for @registeredObjects@
registeredObjectsSelector :: Selector '[] (Id NSSet)
registeredObjectsSelector = mkSelector "registeredObjects"

-- | @Selector@ for @propagatesDeletesAtEndOfEvent@
propagatesDeletesAtEndOfEventSelector :: Selector '[] Bool
propagatesDeletesAtEndOfEventSelector = mkSelector "propagatesDeletesAtEndOfEvent"

-- | @Selector@ for @setPropagatesDeletesAtEndOfEvent:@
setPropagatesDeletesAtEndOfEventSelector :: Selector '[Bool] ()
setPropagatesDeletesAtEndOfEventSelector = mkSelector "setPropagatesDeletesAtEndOfEvent:"

-- | @Selector@ for @retainsRegisteredObjects@
retainsRegisteredObjectsSelector :: Selector '[] Bool
retainsRegisteredObjectsSelector = mkSelector "retainsRegisteredObjects"

-- | @Selector@ for @setRetainsRegisteredObjects:@
setRetainsRegisteredObjectsSelector :: Selector '[Bool] ()
setRetainsRegisteredObjectsSelector = mkSelector "setRetainsRegisteredObjects:"

-- | @Selector@ for @shouldDeleteInaccessibleFaults@
shouldDeleteInaccessibleFaultsSelector :: Selector '[] Bool
shouldDeleteInaccessibleFaultsSelector = mkSelector "shouldDeleteInaccessibleFaults"

-- | @Selector@ for @setShouldDeleteInaccessibleFaults:@
setShouldDeleteInaccessibleFaultsSelector :: Selector '[Bool] ()
setShouldDeleteInaccessibleFaultsSelector = mkSelector "setShouldDeleteInaccessibleFaults:"

-- | @Selector@ for @stalenessInterval@
stalenessIntervalSelector :: Selector '[] CDouble
stalenessIntervalSelector = mkSelector "stalenessInterval"

-- | @Selector@ for @setStalenessInterval:@
setStalenessIntervalSelector :: Selector '[CDouble] ()
setStalenessIntervalSelector = mkSelector "setStalenessInterval:"

-- | @Selector@ for @mergePolicy@
mergePolicySelector :: Selector '[] RawId
mergePolicySelector = mkSelector "mergePolicy"

-- | @Selector@ for @setMergePolicy:@
setMergePolicySelector :: Selector '[RawId] ()
setMergePolicySelector = mkSelector "setMergePolicy:"

-- | @Selector@ for @queryGenerationToken@
queryGenerationTokenSelector :: Selector '[] (Id NSQueryGenerationToken)
queryGenerationTokenSelector = mkSelector "queryGenerationToken"

-- | @Selector@ for @automaticallyMergesChangesFromParent@
automaticallyMergesChangesFromParentSelector :: Selector '[] Bool
automaticallyMergesChangesFromParentSelector = mkSelector "automaticallyMergesChangesFromParent"

-- | @Selector@ for @setAutomaticallyMergesChangesFromParent:@
setAutomaticallyMergesChangesFromParentSelector :: Selector '[Bool] ()
setAutomaticallyMergesChangesFromParentSelector = mkSelector "setAutomaticallyMergesChangesFromParent:"

-- | @Selector@ for @transactionAuthor@
transactionAuthorSelector :: Selector '[] (Id NSString)
transactionAuthorSelector = mkSelector "transactionAuthor"

-- | @Selector@ for @setTransactionAuthor:@
setTransactionAuthorSelector :: Selector '[Id NSString] ()
setTransactionAuthorSelector = mkSelector "setTransactionAuthor:"

