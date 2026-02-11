{-# LANGUAGE PatternSynonyms #-}
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
  , entitySelector
  , fetchRequestSelector
  , initWithEntity_insertIntoManagedObjectContextSelector
  , initWithContextSelector
  , hasFaultForRelationshipNamedSelector
  , objectIDsForRelationshipNamedSelector
  , willAccessValueForKeySelector
  , didAccessValueForKeySelector
  , willChangeValueForKeySelector
  , didChangeValueForKeySelector
  , willChangeValueForKey_withSetMutation_usingObjectsSelector
  , didChangeValueForKey_withSetMutation_usingObjectsSelector
  , awakeFromFetchSelector
  , awakeFromInsertSelector
  , awakeFromSnapshotEventsSelector
  , prepareForDeletionSelector
  , willSaveSelector
  , didSaveSelector
  , willTurnIntoFaultSelector
  , didTurnIntoFaultSelector
  , valueForKeySelector
  , setValue_forKeySelector
  , primitiveValueForKeySelector
  , setPrimitiveValue_forKeySelector
  , committedValuesForKeysSelector
  , changedValuesSelector
  , changedValuesForCurrentEventSelector
  , validateValue_forKey_errorSelector
  , validateForDeleteSelector
  , validateForInsertSelector
  , validateForUpdateSelector
  , setObservationInfoSelector
  , observationInfoSelector
  , contextShouldIgnoreUnmodeledPropertyChangesSelector
  , managedObjectContextSelector
  , objectIDSelector
  , insertedSelector
  , updatedSelector
  , deletedSelector
  , hasChangesSelector
  , hasPersistentChangedValuesSelector
  , faultSelector
  , faultingStateSelector

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
import ObjC.CoreData.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ entity@
nsManagedObjectEntity :: IO (Id NSEntityDescription)
nsManagedObjectEntity  =
  do
    cls' <- getRequiredClass "NSManagedObject"
    sendClassMsg cls' (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fetchRequest@
fetchRequest :: IO (Id NSFetchRequest)
fetchRequest  =
  do
    cls' <- getRequiredClass "NSManagedObject"
    sendClassMsg cls' (mkSelector "fetchRequest") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- initWithEntity:insertIntoManagedObjectContext:@
initWithEntity_insertIntoManagedObjectContext :: (IsNSManagedObject nsManagedObject, IsNSEntityDescription entity, IsNSManagedObjectContext context) => nsManagedObject -> entity -> context -> IO (Id NSManagedObject)
initWithEntity_insertIntoManagedObjectContext nsManagedObject  entity context =
withObjCPtr entity $ \raw_entity ->
  withObjCPtr context $ \raw_context ->
      sendMsg nsManagedObject (mkSelector "initWithEntity:insertIntoManagedObjectContext:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithContext:@
initWithContext :: (IsNSManagedObject nsManagedObject, IsNSManagedObjectContext moc) => nsManagedObject -> moc -> IO (Id NSManagedObject)
initWithContext nsManagedObject  moc =
withObjCPtr moc $ \raw_moc ->
    sendMsg nsManagedObject (mkSelector "initWithContext:") (retPtr retVoid) [argPtr (castPtr raw_moc :: Ptr ())] >>= ownedObject . castPtr

-- | @- hasFaultForRelationshipNamed:@
hasFaultForRelationshipNamed :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO Bool
hasFaultForRelationshipNamed nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "hasFaultForRelationshipNamed:") retCULong [argPtr (castPtr raw_key :: Ptr ())]

-- | @- objectIDsForRelationshipNamed:@
objectIDsForRelationshipNamed :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO (Id NSArray)
objectIDsForRelationshipNamed nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "objectIDsForRelationshipNamed:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())] >>= retainedObject . castPtr

-- | @- willAccessValueForKey:@
willAccessValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
willAccessValueForKey nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "willAccessValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- didAccessValueForKey:@
didAccessValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
didAccessValueForKey nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "didAccessValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- willChangeValueForKey:@
willChangeValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
willChangeValueForKey nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "willChangeValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- didChangeValueForKey:@
didChangeValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO ()
didChangeValueForKey nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "didChangeValueForKey:") retVoid [argPtr (castPtr raw_key :: Ptr ())]

-- | @- willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjects :: (IsNSManagedObject nsManagedObject, IsNSString inKey, IsNSSet inObjects) => nsManagedObject -> inKey -> NSKeyValueSetMutationKind -> inObjects -> IO ()
willChangeValueForKey_withSetMutation_usingObjects nsManagedObject  inKey inMutationKind inObjects =
withObjCPtr inKey $ \raw_inKey ->
  withObjCPtr inObjects $ \raw_inObjects ->
      sendMsg nsManagedObject (mkSelector "willChangeValueForKey:withSetMutation:usingObjects:") retVoid [argPtr (castPtr raw_inKey :: Ptr ()), argCULong (coerce inMutationKind), argPtr (castPtr raw_inObjects :: Ptr ())]

-- | @- didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjects :: (IsNSManagedObject nsManagedObject, IsNSString inKey, IsNSSet inObjects) => nsManagedObject -> inKey -> NSKeyValueSetMutationKind -> inObjects -> IO ()
didChangeValueForKey_withSetMutation_usingObjects nsManagedObject  inKey inMutationKind inObjects =
withObjCPtr inKey $ \raw_inKey ->
  withObjCPtr inObjects $ \raw_inObjects ->
      sendMsg nsManagedObject (mkSelector "didChangeValueForKey:withSetMutation:usingObjects:") retVoid [argPtr (castPtr raw_inKey :: Ptr ()), argCULong (coerce inMutationKind), argPtr (castPtr raw_inObjects :: Ptr ())]

-- | @- awakeFromFetch@
awakeFromFetch :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
awakeFromFetch nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "awakeFromFetch") retVoid []

-- | @- awakeFromInsert@
awakeFromInsert :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
awakeFromInsert nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "awakeFromInsert") retVoid []

-- | @- awakeFromSnapshotEvents:@
awakeFromSnapshotEvents :: IsNSManagedObject nsManagedObject => nsManagedObject -> NSSnapshotEventType -> IO ()
awakeFromSnapshotEvents nsManagedObject  flags =
  sendMsg nsManagedObject (mkSelector "awakeFromSnapshotEvents:") retVoid [argCULong (coerce flags)]

-- | @- prepareForDeletion@
prepareForDeletion :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
prepareForDeletion nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "prepareForDeletion") retVoid []

-- | @- willSave@
willSave :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
willSave nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "willSave") retVoid []

-- | @- didSave@
didSave :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
didSave nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "didSave") retVoid []

-- | @- willTurnIntoFault@
willTurnIntoFault :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
willTurnIntoFault nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "willTurnIntoFault") retVoid []

-- | @- didTurnIntoFault@
didTurnIntoFault :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO ()
didTurnIntoFault nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "didTurnIntoFault") retVoid []

-- | @- valueForKey:@
valueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO RawId
valueForKey nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsManagedObject (mkSelector "valueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setValue:forKey:@
setValue_forKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> RawId -> key -> IO ()
setValue_forKey nsManagedObject  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "setValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- primitiveValueForKey:@
primitiveValueForKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> key -> IO RawId
primitiveValueForKey nsManagedObject  key =
withObjCPtr key $ \raw_key ->
    fmap (RawId . castPtr) $ sendMsg nsManagedObject (mkSelector "primitiveValueForKey:") (retPtr retVoid) [argPtr (castPtr raw_key :: Ptr ())]

-- | @- setPrimitiveValue:forKey:@
setPrimitiveValue_forKey :: (IsNSManagedObject nsManagedObject, IsNSString key) => nsManagedObject -> RawId -> key -> IO ()
setPrimitiveValue_forKey nsManagedObject  value key =
withObjCPtr key $ \raw_key ->
    sendMsg nsManagedObject (mkSelector "setPrimitiveValue:forKey:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ()), argPtr (castPtr raw_key :: Ptr ())]

-- | @- committedValuesForKeys:@
committedValuesForKeys :: (IsNSManagedObject nsManagedObject, IsNSArray keys) => nsManagedObject -> keys -> IO (Id NSDictionary)
committedValuesForKeys nsManagedObject  keys =
withObjCPtr keys $ \raw_keys ->
    sendMsg nsManagedObject (mkSelector "committedValuesForKeys:") (retPtr retVoid) [argPtr (castPtr raw_keys :: Ptr ())] >>= retainedObject . castPtr

-- | @- changedValues@
changedValues :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSDictionary)
changedValues nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "changedValues") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- changedValuesForCurrentEvent@
changedValuesForCurrentEvent :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSDictionary)
changedValuesForCurrentEvent nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "changedValuesForCurrentEvent") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- validateValue:forKey:error:@
validateValue_forKey_error :: (IsNSManagedObject nsManagedObject, IsNSString key, IsNSError error_) => nsManagedObject -> Ptr RawId -> key -> error_ -> IO Bool
validateValue_forKey_error nsManagedObject  value key error_ =
withObjCPtr key $ \raw_key ->
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "validateValue:forKey:error:") retCULong [argPtr value, argPtr (castPtr raw_key :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- validateForDelete:@
validateForDelete :: (IsNSManagedObject nsManagedObject, IsNSError error_) => nsManagedObject -> error_ -> IO Bool
validateForDelete nsManagedObject  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "validateForDelete:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- validateForInsert:@
validateForInsert :: (IsNSManagedObject nsManagedObject, IsNSError error_) => nsManagedObject -> error_ -> IO Bool
validateForInsert nsManagedObject  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "validateForInsert:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- validateForUpdate:@
validateForUpdate :: (IsNSManagedObject nsManagedObject, IsNSError error_) => nsManagedObject -> error_ -> IO Bool
validateForUpdate nsManagedObject  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "validateForUpdate:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- setObservationInfo:@
setObservationInfo :: IsNSManagedObject nsManagedObject => nsManagedObject -> Ptr () -> IO ()
setObservationInfo nsManagedObject  inObservationInfo =
  sendMsg nsManagedObject (mkSelector "setObservationInfo:") retVoid [argPtr inObservationInfo]

-- | @- observationInfo@
observationInfo :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Ptr ())
observationInfo nsManagedObject  =
  fmap castPtr $ sendMsg nsManagedObject (mkSelector "observationInfo") (retPtr retVoid) []

-- | @+ contextShouldIgnoreUnmodeledPropertyChanges@
contextShouldIgnoreUnmodeledPropertyChanges :: IO Bool
contextShouldIgnoreUnmodeledPropertyChanges  =
  do
    cls' <- getRequiredClass "NSManagedObject"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "contextShouldIgnoreUnmodeledPropertyChanges") retCULong []

-- | @- managedObjectContext@
managedObjectContext :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSManagedObjectContext)
managedObjectContext nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "managedObjectContext") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- entity@
entity :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSEntityDescription)
entity nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectID@
objectID :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO (Id NSManagedObjectID)
objectID nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "objectID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- inserted@
inserted :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
inserted nsManagedObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "inserted") retCULong []

-- | @- updated@
updated :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
updated nsManagedObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "updated") retCULong []

-- | @- deleted@
deleted :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
deleted nsManagedObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "deleted") retCULong []

-- | @- hasChanges@
hasChanges :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
hasChanges nsManagedObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "hasChanges") retCULong []

-- | @- hasPersistentChangedValues@
hasPersistentChangedValues :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
hasPersistentChangedValues nsManagedObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "hasPersistentChangedValues") retCULong []

-- | @- fault@
fault :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO Bool
fault nsManagedObject  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsManagedObject (mkSelector "fault") retCULong []

-- | @- faultingState@
faultingState :: IsNSManagedObject nsManagedObject => nsManagedObject -> IO CULong
faultingState nsManagedObject  =
  sendMsg nsManagedObject (mkSelector "faultingState") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @fetchRequest@
fetchRequestSelector :: Selector
fetchRequestSelector = mkSelector "fetchRequest"

-- | @Selector@ for @initWithEntity:insertIntoManagedObjectContext:@
initWithEntity_insertIntoManagedObjectContextSelector :: Selector
initWithEntity_insertIntoManagedObjectContextSelector = mkSelector "initWithEntity:insertIntoManagedObjectContext:"

-- | @Selector@ for @initWithContext:@
initWithContextSelector :: Selector
initWithContextSelector = mkSelector "initWithContext:"

-- | @Selector@ for @hasFaultForRelationshipNamed:@
hasFaultForRelationshipNamedSelector :: Selector
hasFaultForRelationshipNamedSelector = mkSelector "hasFaultForRelationshipNamed:"

-- | @Selector@ for @objectIDsForRelationshipNamed:@
objectIDsForRelationshipNamedSelector :: Selector
objectIDsForRelationshipNamedSelector = mkSelector "objectIDsForRelationshipNamed:"

-- | @Selector@ for @willAccessValueForKey:@
willAccessValueForKeySelector :: Selector
willAccessValueForKeySelector = mkSelector "willAccessValueForKey:"

-- | @Selector@ for @didAccessValueForKey:@
didAccessValueForKeySelector :: Selector
didAccessValueForKeySelector = mkSelector "didAccessValueForKey:"

-- | @Selector@ for @willChangeValueForKey:@
willChangeValueForKeySelector :: Selector
willChangeValueForKeySelector = mkSelector "willChangeValueForKey:"

-- | @Selector@ for @didChangeValueForKey:@
didChangeValueForKeySelector :: Selector
didChangeValueForKeySelector = mkSelector "didChangeValueForKey:"

-- | @Selector@ for @willChangeValueForKey:withSetMutation:usingObjects:@
willChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector
willChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "willChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @didChangeValueForKey:withSetMutation:usingObjects:@
didChangeValueForKey_withSetMutation_usingObjectsSelector :: Selector
didChangeValueForKey_withSetMutation_usingObjectsSelector = mkSelector "didChangeValueForKey:withSetMutation:usingObjects:"

-- | @Selector@ for @awakeFromFetch@
awakeFromFetchSelector :: Selector
awakeFromFetchSelector = mkSelector "awakeFromFetch"

-- | @Selector@ for @awakeFromInsert@
awakeFromInsertSelector :: Selector
awakeFromInsertSelector = mkSelector "awakeFromInsert"

-- | @Selector@ for @awakeFromSnapshotEvents:@
awakeFromSnapshotEventsSelector :: Selector
awakeFromSnapshotEventsSelector = mkSelector "awakeFromSnapshotEvents:"

-- | @Selector@ for @prepareForDeletion@
prepareForDeletionSelector :: Selector
prepareForDeletionSelector = mkSelector "prepareForDeletion"

-- | @Selector@ for @willSave@
willSaveSelector :: Selector
willSaveSelector = mkSelector "willSave"

-- | @Selector@ for @didSave@
didSaveSelector :: Selector
didSaveSelector = mkSelector "didSave"

-- | @Selector@ for @willTurnIntoFault@
willTurnIntoFaultSelector :: Selector
willTurnIntoFaultSelector = mkSelector "willTurnIntoFault"

-- | @Selector@ for @didTurnIntoFault@
didTurnIntoFaultSelector :: Selector
didTurnIntoFaultSelector = mkSelector "didTurnIntoFault"

-- | @Selector@ for @valueForKey:@
valueForKeySelector :: Selector
valueForKeySelector = mkSelector "valueForKey:"

-- | @Selector@ for @setValue:forKey:@
setValue_forKeySelector :: Selector
setValue_forKeySelector = mkSelector "setValue:forKey:"

-- | @Selector@ for @primitiveValueForKey:@
primitiveValueForKeySelector :: Selector
primitiveValueForKeySelector = mkSelector "primitiveValueForKey:"

-- | @Selector@ for @setPrimitiveValue:forKey:@
setPrimitiveValue_forKeySelector :: Selector
setPrimitiveValue_forKeySelector = mkSelector "setPrimitiveValue:forKey:"

-- | @Selector@ for @committedValuesForKeys:@
committedValuesForKeysSelector :: Selector
committedValuesForKeysSelector = mkSelector "committedValuesForKeys:"

-- | @Selector@ for @changedValues@
changedValuesSelector :: Selector
changedValuesSelector = mkSelector "changedValues"

-- | @Selector@ for @changedValuesForCurrentEvent@
changedValuesForCurrentEventSelector :: Selector
changedValuesForCurrentEventSelector = mkSelector "changedValuesForCurrentEvent"

-- | @Selector@ for @validateValue:forKey:error:@
validateValue_forKey_errorSelector :: Selector
validateValue_forKey_errorSelector = mkSelector "validateValue:forKey:error:"

-- | @Selector@ for @validateForDelete:@
validateForDeleteSelector :: Selector
validateForDeleteSelector = mkSelector "validateForDelete:"

-- | @Selector@ for @validateForInsert:@
validateForInsertSelector :: Selector
validateForInsertSelector = mkSelector "validateForInsert:"

-- | @Selector@ for @validateForUpdate:@
validateForUpdateSelector :: Selector
validateForUpdateSelector = mkSelector "validateForUpdate:"

-- | @Selector@ for @setObservationInfo:@
setObservationInfoSelector :: Selector
setObservationInfoSelector = mkSelector "setObservationInfo:"

-- | @Selector@ for @observationInfo@
observationInfoSelector :: Selector
observationInfoSelector = mkSelector "observationInfo"

-- | @Selector@ for @contextShouldIgnoreUnmodeledPropertyChanges@
contextShouldIgnoreUnmodeledPropertyChangesSelector :: Selector
contextShouldIgnoreUnmodeledPropertyChangesSelector = mkSelector "contextShouldIgnoreUnmodeledPropertyChanges"

-- | @Selector@ for @managedObjectContext@
managedObjectContextSelector :: Selector
managedObjectContextSelector = mkSelector "managedObjectContext"

-- | @Selector@ for @objectID@
objectIDSelector :: Selector
objectIDSelector = mkSelector "objectID"

-- | @Selector@ for @inserted@
insertedSelector :: Selector
insertedSelector = mkSelector "inserted"

-- | @Selector@ for @updated@
updatedSelector :: Selector
updatedSelector = mkSelector "updated"

-- | @Selector@ for @deleted@
deletedSelector :: Selector
deletedSelector = mkSelector "deleted"

-- | @Selector@ for @hasChanges@
hasChangesSelector :: Selector
hasChangesSelector = mkSelector "hasChanges"

-- | @Selector@ for @hasPersistentChangedValues@
hasPersistentChangedValuesSelector :: Selector
hasPersistentChangedValuesSelector = mkSelector "hasPersistentChangedValues"

-- | @Selector@ for @fault@
faultSelector :: Selector
faultSelector = mkSelector "fault"

-- | @Selector@ for @faultingState@
faultingStateSelector :: Selector
faultingStateSelector = mkSelector "faultingState"

