{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSManagedObject@.
module ObjC.CoreData.NSManagedObject
  ( NSManagedObject
  , IsNSManagedObject(..)
  , nsManagedObjectEntity
  , fetchRequest
  , initWithEntity_insertIntoManagedObjectContext
  , initWithContext
  , hasFaultForRelationshipNamed
  , objectIDsForRelationshipNamed
  , willAccessValueForKey
  , didAccessValueForKey
  , willChangeValueForKey
  , didChangeValueForKey
  , willChangeValueForKey_withSetMutation_usingObjects
  , didChangeValueForKey_withSetMutation_usingObjects
  , awakeFromFetch
  , awakeFromInsert
  , awakeFromSnapshotEvents
  , prepareForDeletion
  , willSave
  , didSave
  , willTurnIntoFault
  , didTurnIntoFault
  , valueForKey
  , setValue_forKey
  , primitiveValueForKey
  , setPrimitiveValue_forKey
  , committedValuesForKeys
  , changedValues
  , changedValuesForCurrentEvent
  , validateValue_forKey_error
  , validateForDelete
  , validateForInsert
  , validateForUpdate
  , setObservationInfo
  , observationInfo
  , contextShouldIgnoreUnmodeledPropertyChanges
  , managedObjectContext
  , entity
  , objectID
  , inserted
  , updated
  , deleted
  , hasChanges
  , hasPersistentChangedValues
  , fault
  , faultingState
  , awakeFromFetchSelector
  , awakeFromInsertSelector
  , awakeFromSnapshotEventsSelector
  , changedValuesForCurrentEventSelector
  , changedValuesSelector
  , committedValuesForKeysSelector
  , contextShouldIgnoreUnmodeledPropertyChangesSelector
  , deletedSelector
  , didAccessValueForKeySelector
  , didChangeValueForKeySelector
  , didChangeValueForKey_withSetMutation_usingObjectsSelector
  , didSaveSelector
  , didTurnIntoFaultSelector
  , entitySelector
  , faultSelector
  , faultingStateSelector
  , fetchRequestSelector
  , hasChangesSelector
  , hasFaultForRelationshipNamedSelector
  , hasPersistentChangedValuesSelector
  , initWithContextSelector
  , initWithEntity_insertIntoManagedObjectContextSelector
  , insertedSelector
  , managedObjectContextSelector
  , nsManagedObjectEntitySelector
  , objectIDSelector
  , objectIDsForRelationshipNamedSelector
  , observationInfoSelector
  , prepareForDeletionSelector
  , primitiveValueForKeySelector
  , setObservationInfoSelector
  , setPrimitiveValue_forKeySelector
  , setValue_forKeySelector
  , updatedSelector
  , validateForDeleteSelector
  , validateForInsertSelector
  , validateForUpdateSelector
  , validateValue_forKey_errorSelector
  , valueForKeySelector
  , willAccessValueForKeySelector
  , willChangeValueForKeySelector
  , willChangeValueForKey_withSetMutation_usingObjectsSelector
  , willSaveSelector
  , willTurnIntoFaultSelector

  -- * Enum types
  , NSKeyValueSetMutationKind(NSKeyValueSetMutationKind)
  , pattern NSKeyValueUnionSetMutation
  , pattern NSKeyValueMinusSetMutation
  , pattern NSKeyValueIntersectSetMutation
  , pattern NSKeyValueSetSetMutation
  , NSSnapshotEventType(NSSnapshotEventType)
  , pattern NSSnapshotEventUndoInsertion
  , pattern NSSnapshotEventUndoDeletion
  , pattern NSSnapshotEventUndoUpdate
  , pattern NSSnapshotEventRollback
  , pattern NSSnapshotEventRefresh
  , pattern NSSnapshotEventMergePolicy

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @+ entity@
nsManagedObjectEntity :: IO (Id NSEntityDescription)
nsManagedObjectEntity  =
  do
    cls' <- getRequiredClass "NSManagedObject"
    sendClassMessage cls' nsManagedObjectEntitySelector

-- | @+ fetchRequest@
fetchRequest :: IO (Id NSFetchRequest)
fetchRequest  =
  do
    cls' <- getRequiredClass "NSManagedObject"
    sendClassMessage cls' fetchRequestSelector

-- | @- initWithEntity:insertIntoManagedObjectContext:@
initWithEntity_insertIntoManagedObjectContext :: (IsNSManagedObject nsManagedObject, IsNSEntityDescription entity, IsNSManagedObjectContext context) => nsManagedObject -> entity -> context -> IO (Id NSManagedObject)
initWithEntity_insertIntoManagedObjectContext nsManagedObject entity context =
  sendOwnedMessage nsManagedObject initWithEntity_insertIntoManagedObjectContextSelector (toNSEntityDescription entity) (toNSManagedObjectContext context)

-- | @- initWithContext:@
initWithContext :: (IsNSManagedObject nsManagedObject, IsNSManagedObjectContext moc) => nsManagedObject -> moc -> IO (Id NSManagedObject)
initWithContext nsManagedObject moc =
  sendOwnedMessage nsManagedObject initWithContextSelector (toNSManagedObjectContext moc)

-- | @- hasFaultForRelationshipNamed:@
hasFaultForRelationshipNamed :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO Bool
hasFaultForRelationshipNamed nsManagedObject key =
  sendMessage nsManagedObject hasFaultForRelationshipNamedSelector (toNSString key)

-- | @- objectIDsForRelationshipNamed:@
objectIDsForRelationshipNamed :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO (Id NSArray)
objectIDsForRelationshipNamed nsManagedObject key =
  sendMessage nsManagedObject objectIDsForRelationshipNamedSelector (toNSString key)

-- | @- willAccessValueForKey:@
willAccessValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
willAccessValueForKey nsManagedObject key =
  sendMessage nsManagedObject willAccessValueForKeySelector (toNSString key)

-- | @- didAccessValueForKey:@
didAccessValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
didAccessValueForKey nsManagedObject key =
  sendMessage nsManagedObject didAccessValueForKeySelector (toNSString key)

-- | @- willChangeValueForKey:@
willChangeValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
willChangeValueForKey nsManagedObject key =
  sendMessage nsManagedObject willChangeValueForKeySelector (toNSString key)

-- | @- didChangeValueForKey:@
didChangeValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
didChangeValueForKey nsManagedObject key =
  sendMessage nsManagedObject didChangeValueForKeySelector (toNSString key)

-- | @- willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjects :: (IsNSManagedObject nsManagedObject, IsNSString inKey, IsNSSet inObjects) => nsManagedObject -> inKey -> NSKeyValueSetMutationKind -> inObjects -> IO ()
willChangeValueForKey_withSetMutation_usingObjects nsManagedObject inKey inMutationKind inObjects =
  sendMessage nsManagedObject willChangeValueForKey_withSetMutation_usingObjectsSelector (toNSString inKey) inMutationKind (toNSSet inObjects)

-- | @- didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjects :: (IsNSManagedObject nsManagedObject, IsNSString inKey, IsNSSet inObjects) => nsManagedObject -> inKey -> NSKeyValueSetMutationKind -> inObjects -> IO ()
didChangeValueForKey_withSetMutation_usingObjects nsManagedObject inKey inMutationKind inObjects =
  sendMessage nsManagedObject didChangeValueForKey_withSetMutation_usingObjectsSelector (toNSString inKey) inMutationKind (toNSSet inObjects)

-- | @- awakeFromFetch@
awakeFromFetch :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
awakeFromFetch nsManagedObject =
  sendMessage nsManagedObject awakeFromFetchSelector

-- | @- awakeFromInsert@
awakeFromInsert :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
awakeFromInsert nsManagedObject =
  sendMessage nsManagedObject awakeFromInsertSelector

-- | @- awakeFromSnapshotEvents:@
awakeFromSnapshotEvents :: IsNSManagedObject nsManagedObject => nsManagedObject -> NSSnapshotEventType -> IO ()
awakeFromSnapshotEvents nsManagedObject flags =
  sendMessage nsManagedObject awakeFromSnapshotEventsSelector flags

-- | @- prepareForDeletion@
prepareForDeletion :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
prepareForDeletion nsManagedObject =
  sendMessage nsManagedObject prepareForDeletionSelector

-- | @- willSave@
willSave :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
willSave nsManagedObject =
  sendMessage nsManagedObject willSaveSelector

-- | @- didSave@
didSave :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
didSave nsManagedObject =
  sendMessage nsManagedObject didSaveSelector

-- | @- willTurnIntoFault@
willTurnIntoFault :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
willTurnIntoFault nsManagedObject =
  sendMessage nsManagedObject willTurnIntoFaultSelector

-- | @- didTurnIntoFault@
didTurnIntoFault :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
didTurnIntoFault nsManagedObject =
  sendMessage nsManagedObject didTurnIntoFaultSelector

-- | @- valueForKey:@
valueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO RawId
valueForKey nsManagedObject key =
  sendMessage nsManagedObject valueForKeySelector (toNSString key)

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> RawId -> key -> IO ()
setValue_forKey nsManagedObject value key =
  sendMessage nsManagedObject setValue_forKeySelector value (toNSString key)

-- | @- primitiveValueForKey:@
primitiveValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO RawId
primitiveValueForKey nsManagedObject key =
  sendMessage nsManagedObject primitiveValueForKeySelector (toNSString key)

-- | @- setPrimitiveValue:forKey:@
setPrimitiveValue_forKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> RawId -> key -> IO ()
setPrimitiveValue_forKey nsManagedObject value key =
  sendMessage nsManagedObject setPrimitiveValue_forKeySelector value (toNSString key)

-- | @- committedValuesForKeys:@
committedValuesForKeys :: (IsNSManagedObject nsManagedObject, IsNSArray keys) => nsManagedObject -> keys -> IO (Id NSDictionary)
committedValuesForKeys nsManagedObject keys =
  sendMessage nsManagedObject committedValuesForKeysSelector (toNSArray keys)

-- | @- changedValues@
changedValues :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSDictionary)
changedValues nsManagedObject =
  sendMessage nsManagedObject changedValuesSelector

-- | @- changedValuesForCurrentEvent@
changedValuesForCurrentEvent :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSDictionary)
changedValuesForCurrentEvent nsManagedObject =
  sendMessage nsManagedObject changedValuesForCurrentEventSelector

-- | @- validateValue:forKey:error:@
validateValue_forKey_error :: (IsNSManagedObject nsManagedObject, IsNSString key, IsNSError error_) => nsManagedObject -> Ptr RawId -> key -> error_ -> IO Bool
validateValue_forKey_error nsManagedObject value key error_ =
  sendMessage nsManagedObject validateValue_forKey_errorSelector value (toNSString key) (toNSError error_)

-- | @- validateForDelete:@
validateForDelete :: (IsNSManagedObject nsManagedObject, IsNSError error_) => nsManagedObject -> error_ -> IO Bool
validateForDelete nsManagedObject error_ =
  sendMessage nsManagedObject validateForDeleteSelector (toNSError error_)

-- | @- validateForInsert:@
validateForInsert :: (IsNSManagedObject nsManagedObject, IsNSError error_) => nsManagedObject -> error_ -> IO Bool
validateForInsert nsManagedObject error_ =
  sendMessage nsManagedObject validateForInsertSelector (toNSError error_)

-- | @- validateForUpdate:@
validateForUpdate :: (IsNSManagedObject nsManagedObject, IsNSError error_) => nsManagedObject -> error_ -> IO Bool
validateForUpdate nsManagedObject error_ =
  sendMessage nsManagedObject validateForUpdateSelector (toNSError error_)

-- | @- setObservationInfo:@
setObservationInfo :: IsNSManagedObject nsManagedObject => nsManagedObject -> Ptr () -> IO ()
setObservationInfo nsManagedObject inObservationInfo =
  sendMessage nsManagedObject setObservationInfoSelector inObservationInfo

-- | @- observationInfo@
observationInfo :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Ptr ())
observationInfo nsManagedObject =
  sendMessage nsManagedObject observationInfoSelector

-- | @+ contextShouldIgnoreUnmodeledPropertyChanges@
contextShouldIgnoreUnmodeledPropertyChanges :: IO Bool
contextShouldIgnoreUnmodeledPropertyChanges  =
  do
    cls' <- getRequiredClass "NSManagedObject"
    sendClassMessage cls' contextShouldIgnoreUnmodeledPropertyChangesSelector

-- | @- managedObjectContext@
managedObjectContext :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSManagedObjectContext)
managedObjectContext nsManagedObject =
  sendMessage nsManagedObject managedObjectContextSelector

-- | @- entity@
entity :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSEntityDescription)
entity nsManagedObject =
  sendMessage nsManagedObject entitySelector

-- | @- objectID@
objectID :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSManagedObjectID)
objectID nsManagedObject =
  sendMessage nsManagedObject objectIDSelector

-- | @- inserted@
inserted :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
inserted nsManagedObject =
  sendMessage nsManagedObject insertedSelector

-- | @- updated@
updated :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
updated nsManagedObject =
  sendMessage nsManagedObject updatedSelector

-- | @- deleted@
deleted :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
deleted nsManagedObject =
  sendMessage nsManagedObject deletedSelector

-- | @- hasChanges@
hasChanges :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
hasChanges nsManagedObject =
  sendMessage nsManagedObject hasChangesSelector

-- | @- hasPersistentChangedValues@
hasPersistentChangedValues :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
hasPersistentChangedValues nsManagedObject =
  sendMessage nsManagedObject hasPersistentChangedValuesSelector

-- | @- fault@
fault :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
fault nsManagedObject =
  sendMessage nsManagedObject faultSelector

-- | @- faultingState@
faultingState :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO CULong
faultingState nsManagedObject =
  sendMessage nsManagedObject faultingStateSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entity@
nsManagedObjectEntitySelector :: Selector '[] (Id NSEntityDescription)
nsManagedObjectEntitySelector = mkSelector "entity"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector '[] (Id NSFetchRequest)
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @initWithEntity:insertIntoManagedObjectContext:@
initWithEntity_insertIntoManagedObjectContextSelector :: Selector '[Id NSEntityDescription, Id NSManagedObjectContext] (Id NSManagedObject)
initWithEntity_insertIntoManagedObjectContextSelector = mkSelector "initWithEntity:insertIntoManagedObjectContext:"

-- | @Selector@ for @initWithContext:@
initWithContextSelector :: Selector '[Id NSManagedObjectContext] (Id NSManagedObject)
initWithContextSelector = mkSelector "initWithContext:"

-- | @Selector@ for @hasFaultForRelationshipNamed:@
hasFaultForRelationshipNamedSelector :: Selector '[Id NSString] Bool
hasFaultForRelationshipNamedSelector = mkSelector "hasFaultForRelationshipNamed:"

-- | @Selector@ for @objectIDsForRelationshipNamed:@
objectIDsForRelationshipNamedSelector :: Selector '[Id NSString] (Id NSArray)
objectIDsForRelationshipNamedSelector = mkSelector "objectIDsForRelationshipNamed:"

-- | @Selector@ for @willAccessValueForKey:@
willAccessValueForKeySelector :: Selector '[Id NSString] ()
willAccessValueForKeySelector = mkSelector "willAccessValueForKey:"

-- | @Selector@ for @didAccessValueForKey:@
didAccessValueForKeySelector :: Selector '[Id NSString] ()
didAccessValueForKeySelector = mkSelector "didAccessValueForKey:"

-- | @Selector@ for @willChangeValueForKey:@
willChangeValueForKeySelector :: Selector '[Id NSString] ()
willChangeValueForKeySelector = mkSelector "willChangeValueForKey:"

-- | @Selector@ for @didChangeValueForKey:@
didChangeValueForKeySelector :: Selector '[Id NSString] ()
didChangeValueForKeySelector = mkSelector "didChangeValueForKey:"

-- | @Selector@ for @willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector '[Id NSString, NSKeyValueSetMutationKind, Id NSSet] ()
willChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "willChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector '[Id NSString, NSKeyValueSetMutationKind, Id NSSet] ()
didChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "didChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @awakeFromFetch@
awakeFromFetchSelector :: Selector '[] ()
awakeFromFetchSelector = mkSelector "awakeFromFetch"

-- | @Selector@ for @awakeFromInsert@
awakeFromInsertSelector :: Selector '[] ()
awakeFromInsertSelector = mkSelector "awakeFromInsert"

-- | @Selector@ for @awakeFromSnapshotEvents:@
awakeFromSnapshotEventsSelector :: Selector '[NSSnapshotEventType] ()
awakeFromSnapshotEventsSelector = mkSelector "awakeFromSnapshotEvents:"

-- | @Selector@ for @prepareForDeletion@
prepareForDeletionSelector :: Selector '[] ()
prepareForDeletionSelector = mkSelector "prepareForDeletion"

-- | @Selector@ for @willSave@
willSaveSelector :: Selector '[] ()
willSaveSelector = mkSelector "willSave"

-- | @Selector@ for @didSave@
didSaveSelector :: Selector '[] ()
didSaveSelector = mkSelector "didSave"

-- | @Selector@ for @willTurnIntoFault@
willTurnIntoFaultSelector :: Selector '[] ()
willTurnIntoFaultSelector = mkSelector "willTurnIntoFault"

-- | @Selector@ for @didTurnIntoFault@
didTurnIntoFaultSelector :: Selector '[] ()
didTurnIntoFaultSelector = mkSelector "didTurnIntoFault"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector '[Id NSString] RawId
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @primitiveValueForKey:@
primitiveValueForKeySelector :: Selector '[Id NSString] RawId
primitiveValueForKeySelector = mkSelector "primitiveValueForKey:"

-- | @Selector@ for @setPrimitiveValue:forKey:@
setPrimitiveValue_forKeySelector :: Selector '[RawId, Id NSString] ()
setPrimitiveValue_forKeySelector = mkSelector "setPrimitiveValue:forKey:"

-- | @Selector@ for @committedValuesForKeys:@
committedValuesForKeysSelector :: Selector '[Id NSArray] (Id NSDictionary)
committedValuesForKeysSelector = mkSelector "committedValuesForKeys:"

-- | @Selector@ for @changedValues@
changedValuesSelector :: Selector '[] (Id NSDictionary)
changedValuesSelector = mkSelector "changedValues"

-- | @Selector@ for @changedValuesForCurrentEvent@
changedValuesForCurrentEventSelector :: Selector '[] (Id NSDictionary)
changedValuesForCurrentEventSelector = mkSelector "changedValuesForCurrentEvent"

-- | @Selector@ for @validateValue:forKey:error:@
validateValue_forKey_errorSelector :: Selector '[Ptr RawId, Id NSString, Id NSError] Bool
validateValue_forKey_errorSelector = mkSelector "validateValue:forKey:error:"

-- | @Selector@ for @validateForDelete:@
validateForDeleteSelector :: Selector '[Id NSError] Bool
validateForDeleteSelector = mkSelector "validateForDelete:"

-- | @Selector@ for @validateForInsert:@
validateForInsertSelector :: Selector '[Id NSError] Bool
validateForInsertSelector = mkSelector "validateForInsert:"

-- | @Selector@ for @validateForUpdate:@
validateForUpdateSelector :: Selector '[Id NSError] Bool
validateForUpdateSelector = mkSelector "validateForUpdate:"

-- | @Selector@ for @setObservationInfo:@
setObservationInfoSelector :: Selector '[Ptr ()] ()
setObservationInfoSelector = mkSelector "setObservationInfo:"

-- | @Selector@ for @observationInfo@
observationInfoSelector :: Selector '[] (Ptr ())
observationInfoSelector = mkSelector "observationInfo"

-- | @Selector@ for @contextShouldIgnoreUnmodeledPropertyChanges@
contextShouldIgnoreUnmodeledPropertyChangesSelector :: Selector '[] Bool
contextShouldIgnoreUnmodeledPropertyChangesSelector = mkSelector "contextShouldIgnoreUnmodeledPropertyChanges"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector '[] (Id NSManagedObjectContext)
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector '[] (Id NSManagedObjectID)
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @inserted@
insertedSelector :: Selector '[] Bool
insertedSelector = mkSelector "inserted"

-- | @Selector@ for @updated@
updatedSelector :: Selector '[] Bool
updatedSelector = mkSelector "updated"

-- | @Selector@ for @deleted@
deletedSelector :: Selector '[] Bool
deletedSelector = mkSelector "deleted"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector '[] Bool
hasChangesSelector = mkSelector "hasChanges"

-- | @Selector@ for @hasPersistentChangedValues@
hasPersistentChangedValuesSelector :: Selector '[] Bool
hasPersistentChangedValuesSelector = mkSelector "hasPersistentChangedValues"

-- | @Selector@ for @fault@
faultSelector :: Selector '[] Bool
faultSelector = mkSelector "fault"

-- | @Selector@ for @faultingState@
faultingStateSelector :: Selector '[] CULong
faultingStateSelector = mkSelector "faultingState"

