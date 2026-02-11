{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.CoreData.Internal.Classes (
    module ObjC.CoreData.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.CloudKit.Internal.Classes,
    module ObjC.CoreSpotlight.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
    module ObjC.SyncServices.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.CloudKit.Internal.Classes
import ObjC.CoreSpotlight.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.SyncServices.Internal.Classes

-- ---------- CKDatabase ----------

-- | Phantom type for @CKDatabase@.
data CKDatabase

instance IsObjCObject (Id CKDatabase) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "CKDatabase"

class IsNSObject a => IsCKDatabase a where
  toCKDatabase :: a -> Id CKDatabase

instance IsCKDatabase (Id CKDatabase) where
  toCKDatabase = unsafeCastId

instance IsNSObject (Id CKDatabase) where
  toNSObject = unsafeCastId

-- ---------- NSAtomicStoreCacheNode ----------

-- | Phantom type for @NSAtomicStoreCacheNode@.
data NSAtomicStoreCacheNode

instance IsObjCObject (Id NSAtomicStoreCacheNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAtomicStoreCacheNode"

class IsNSObject a => IsNSAtomicStoreCacheNode a where
  toNSAtomicStoreCacheNode :: a -> Id NSAtomicStoreCacheNode

instance IsNSAtomicStoreCacheNode (Id NSAtomicStoreCacheNode) where
  toNSAtomicStoreCacheNode = unsafeCastId

instance IsNSObject (Id NSAtomicStoreCacheNode) where
  toNSObject = unsafeCastId

-- ---------- NSConstraintConflict ----------

-- | Phantom type for @NSConstraintConflict@.
data NSConstraintConflict

instance IsObjCObject (Id NSConstraintConflict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSConstraintConflict"

class IsNSObject a => IsNSConstraintConflict a where
  toNSConstraintConflict :: a -> Id NSConstraintConflict

instance IsNSConstraintConflict (Id NSConstraintConflict) where
  toNSConstraintConflict = unsafeCastId

instance IsNSObject (Id NSConstraintConflict) where
  toNSObject = unsafeCastId

-- ---------- NSCoreDataCoreSpotlightDelegate ----------

-- | Phantom type for @NSCoreDataCoreSpotlightDelegate@.
data NSCoreDataCoreSpotlightDelegate

instance IsObjCObject (Id NSCoreDataCoreSpotlightDelegate) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCoreDataCoreSpotlightDelegate"

class IsNSObject a => IsNSCoreDataCoreSpotlightDelegate a where
  toNSCoreDataCoreSpotlightDelegate :: a -> Id NSCoreDataCoreSpotlightDelegate

instance IsNSCoreDataCoreSpotlightDelegate (Id NSCoreDataCoreSpotlightDelegate) where
  toNSCoreDataCoreSpotlightDelegate = unsafeCastId

instance IsNSObject (Id NSCoreDataCoreSpotlightDelegate) where
  toNSObject = unsafeCastId

-- ---------- NSEntityDescription ----------

-- | Phantom type for @NSEntityDescription@.
data NSEntityDescription

instance IsObjCObject (Id NSEntityDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEntityDescription"

class IsNSObject a => IsNSEntityDescription a where
  toNSEntityDescription :: a -> Id NSEntityDescription

instance IsNSEntityDescription (Id NSEntityDescription) where
  toNSEntityDescription = unsafeCastId

instance IsNSObject (Id NSEntityDescription) where
  toNSObject = unsafeCastId

-- ---------- NSEntityMapping ----------

-- | Phantom type for @NSEntityMapping@.
data NSEntityMapping

instance IsObjCObject (Id NSEntityMapping) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEntityMapping"

class IsNSObject a => IsNSEntityMapping a where
  toNSEntityMapping :: a -> Id NSEntityMapping

instance IsNSEntityMapping (Id NSEntityMapping) where
  toNSEntityMapping = unsafeCastId

instance IsNSObject (Id NSEntityMapping) where
  toNSObject = unsafeCastId

-- ---------- NSEntityMigrationPolicy ----------

-- | Phantom type for @NSEntityMigrationPolicy@.
data NSEntityMigrationPolicy

instance IsObjCObject (Id NSEntityMigrationPolicy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSEntityMigrationPolicy"

class IsNSObject a => IsNSEntityMigrationPolicy a where
  toNSEntityMigrationPolicy :: a -> Id NSEntityMigrationPolicy

instance IsNSEntityMigrationPolicy (Id NSEntityMigrationPolicy) where
  toNSEntityMigrationPolicy = unsafeCastId

instance IsNSObject (Id NSEntityMigrationPolicy) where
  toNSObject = unsafeCastId

-- ---------- NSFetchIndexDescription ----------

-- | Phantom type for @NSFetchIndexDescription@.
data NSFetchIndexDescription

instance IsObjCObject (Id NSFetchIndexDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFetchIndexDescription"

class IsNSObject a => IsNSFetchIndexDescription a where
  toNSFetchIndexDescription :: a -> Id NSFetchIndexDescription

instance IsNSFetchIndexDescription (Id NSFetchIndexDescription) where
  toNSFetchIndexDescription = unsafeCastId

instance IsNSObject (Id NSFetchIndexDescription) where
  toNSObject = unsafeCastId

-- ---------- NSFetchIndexElementDescription ----------

-- | Phantom type for @NSFetchIndexElementDescription@.
data NSFetchIndexElementDescription

instance IsObjCObject (Id NSFetchIndexElementDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFetchIndexElementDescription"

class IsNSObject a => IsNSFetchIndexElementDescription a where
  toNSFetchIndexElementDescription :: a -> Id NSFetchIndexElementDescription

instance IsNSFetchIndexElementDescription (Id NSFetchIndexElementDescription) where
  toNSFetchIndexElementDescription = unsafeCastId

instance IsNSObject (Id NSFetchIndexElementDescription) where
  toNSObject = unsafeCastId

-- ---------- NSFetchedResultsController ----------

-- | Phantom type for @NSFetchedResultsController@.
data NSFetchedResultsController

instance IsObjCObject (Id NSFetchedResultsController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFetchedResultsController"

class IsNSObject a => IsNSFetchedResultsController a where
  toNSFetchedResultsController :: a -> Id NSFetchedResultsController

instance IsNSFetchedResultsController (Id NSFetchedResultsController) where
  toNSFetchedResultsController = unsafeCastId

instance IsNSObject (Id NSFetchedResultsController) where
  toNSObject = unsafeCastId

-- ---------- NSIncrementalStoreNode ----------

-- | Phantom type for @NSIncrementalStoreNode@.
data NSIncrementalStoreNode

instance IsObjCObject (Id NSIncrementalStoreNode) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSIncrementalStoreNode"

class IsNSObject a => IsNSIncrementalStoreNode a where
  toNSIncrementalStoreNode :: a -> Id NSIncrementalStoreNode

instance IsNSIncrementalStoreNode (Id NSIncrementalStoreNode) where
  toNSIncrementalStoreNode = unsafeCastId

instance IsNSObject (Id NSIncrementalStoreNode) where
  toNSObject = unsafeCastId

-- ---------- NSManagedObject ----------

-- | Phantom type for @NSManagedObject@.
data NSManagedObject

instance IsObjCObject (Id NSManagedObject) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSManagedObject"

class IsNSObject a => IsNSManagedObject a where
  toNSManagedObject :: a -> Id NSManagedObject

instance IsNSManagedObject (Id NSManagedObject) where
  toNSManagedObject = unsafeCastId

instance IsNSObject (Id NSManagedObject) where
  toNSObject = unsafeCastId

-- ---------- NSManagedObjectID ----------

-- | Phantom type for @NSManagedObjectID@.
data NSManagedObjectID

instance IsObjCObject (Id NSManagedObjectID) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSManagedObjectID"

class IsNSObject a => IsNSManagedObjectID a where
  toNSManagedObjectID :: a -> Id NSManagedObjectID

instance IsNSManagedObjectID (Id NSManagedObjectID) where
  toNSManagedObjectID = unsafeCastId

instance IsNSObject (Id NSManagedObjectID) where
  toNSObject = unsafeCastId

-- ---------- NSManagedObjectModel ----------

-- | Phantom type for @NSManagedObjectModel@.
data NSManagedObjectModel

instance IsObjCObject (Id NSManagedObjectModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSManagedObjectModel"

class IsNSObject a => IsNSManagedObjectModel a where
  toNSManagedObjectModel :: a -> Id NSManagedObjectModel

instance IsNSManagedObjectModel (Id NSManagedObjectModel) where
  toNSManagedObjectModel = unsafeCastId

instance IsNSObject (Id NSManagedObjectModel) where
  toNSObject = unsafeCastId

-- ---------- NSManagedObjectModelReference ----------

-- | Phantom type for @NSManagedObjectModelReference@.
data NSManagedObjectModelReference

instance IsObjCObject (Id NSManagedObjectModelReference) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSManagedObjectModelReference"

class IsNSObject a => IsNSManagedObjectModelReference a where
  toNSManagedObjectModelReference :: a -> Id NSManagedObjectModelReference

instance IsNSManagedObjectModelReference (Id NSManagedObjectModelReference) where
  toNSManagedObjectModelReference = unsafeCastId

instance IsNSObject (Id NSManagedObjectModelReference) where
  toNSObject = unsafeCastId

-- ---------- NSMappingModel ----------

-- | Phantom type for @NSMappingModel@.
data NSMappingModel

instance IsObjCObject (Id NSMappingModel) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMappingModel"

class IsNSObject a => IsNSMappingModel a where
  toNSMappingModel :: a -> Id NSMappingModel

instance IsNSMappingModel (Id NSMappingModel) where
  toNSMappingModel = unsafeCastId

instance IsNSObject (Id NSMappingModel) where
  toNSObject = unsafeCastId

-- ---------- NSMergeConflict ----------

-- | Phantom type for @NSMergeConflict@.
data NSMergeConflict

instance IsObjCObject (Id NSMergeConflict) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMergeConflict"

class IsNSObject a => IsNSMergeConflict a where
  toNSMergeConflict :: a -> Id NSMergeConflict

instance IsNSMergeConflict (Id NSMergeConflict) where
  toNSMergeConflict = unsafeCastId

instance IsNSObject (Id NSMergeConflict) where
  toNSObject = unsafeCastId

-- ---------- NSMergePolicy ----------

-- | Phantom type for @NSMergePolicy@.
data NSMergePolicy

instance IsObjCObject (Id NSMergePolicy) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMergePolicy"

class IsNSObject a => IsNSMergePolicy a where
  toNSMergePolicy :: a -> Id NSMergePolicy

instance IsNSMergePolicy (Id NSMergePolicy) where
  toNSMergePolicy = unsafeCastId

instance IsNSObject (Id NSMergePolicy) where
  toNSObject = unsafeCastId

-- ---------- NSMigrationManager ----------

-- | Phantom type for @NSMigrationManager@.
data NSMigrationManager

instance IsObjCObject (Id NSMigrationManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMigrationManager"

class IsNSObject a => IsNSMigrationManager a where
  toNSMigrationManager :: a -> Id NSMigrationManager

instance IsNSMigrationManager (Id NSMigrationManager) where
  toNSMigrationManager = unsafeCastId

instance IsNSObject (Id NSMigrationManager) where
  toNSObject = unsafeCastId

-- ---------- NSMigrationStage ----------

-- | Phantom type for @NSMigrationStage@.
data NSMigrationStage

instance IsObjCObject (Id NSMigrationStage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSMigrationStage"

class IsNSObject a => IsNSMigrationStage a where
  toNSMigrationStage :: a -> Id NSMigrationStage

instance IsNSMigrationStage (Id NSMigrationStage) where
  toNSMigrationStage = unsafeCastId

instance IsNSObject (Id NSMigrationStage) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentCloudKitContainerEvent ----------

-- | Phantom type for @NSPersistentCloudKitContainerEvent@.
data NSPersistentCloudKitContainerEvent

instance IsObjCObject (Id NSPersistentCloudKitContainerEvent) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentCloudKitContainerEvent"

class IsNSObject a => IsNSPersistentCloudKitContainerEvent a where
  toNSPersistentCloudKitContainerEvent :: a -> Id NSPersistentCloudKitContainerEvent

instance IsNSPersistentCloudKitContainerEvent (Id NSPersistentCloudKitContainerEvent) where
  toNSPersistentCloudKitContainerEvent = unsafeCastId

instance IsNSObject (Id NSPersistentCloudKitContainerEvent) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentCloudKitContainerOptions ----------

-- | NSPersistentCloudKitContainerOptions provides customization of how NSPersistentCloudKitContainer aligns a given instance of NSPersistentStoreDescription with a CloudKit database.
-- 
-- Phantom type for @NSPersistentCloudKitContainerOptions@.
data NSPersistentCloudKitContainerOptions

instance IsObjCObject (Id NSPersistentCloudKitContainerOptions) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentCloudKitContainerOptions"

class IsNSObject a => IsNSPersistentCloudKitContainerOptions a where
  toNSPersistentCloudKitContainerOptions :: a -> Id NSPersistentCloudKitContainerOptions

instance IsNSPersistentCloudKitContainerOptions (Id NSPersistentCloudKitContainerOptions) where
  toNSPersistentCloudKitContainerOptions = unsafeCastId

instance IsNSObject (Id NSPersistentCloudKitContainerOptions) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentContainer ----------

-- | Phantom type for @NSPersistentContainer@.
data NSPersistentContainer

instance IsObjCObject (Id NSPersistentContainer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentContainer"

class IsNSObject a => IsNSPersistentContainer a where
  toNSPersistentContainer :: a -> Id NSPersistentContainer

instance IsNSPersistentContainer (Id NSPersistentContainer) where
  toNSPersistentContainer = unsafeCastId

instance IsNSObject (Id NSPersistentContainer) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentHistoryChange ----------

-- | Phantom type for @NSPersistentHistoryChange@.
data NSPersistentHistoryChange

instance IsObjCObject (Id NSPersistentHistoryChange) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentHistoryChange"

class IsNSObject a => IsNSPersistentHistoryChange a where
  toNSPersistentHistoryChange :: a -> Id NSPersistentHistoryChange

instance IsNSPersistentHistoryChange (Id NSPersistentHistoryChange) where
  toNSPersistentHistoryChange = unsafeCastId

instance IsNSObject (Id NSPersistentHistoryChange) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentHistoryToken ----------

-- | Phantom type for @NSPersistentHistoryToken@.
data NSPersistentHistoryToken

instance IsObjCObject (Id NSPersistentHistoryToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentHistoryToken"

class IsNSObject a => IsNSPersistentHistoryToken a where
  toNSPersistentHistoryToken :: a -> Id NSPersistentHistoryToken

instance IsNSPersistentHistoryToken (Id NSPersistentHistoryToken) where
  toNSPersistentHistoryToken = unsafeCastId

instance IsNSObject (Id NSPersistentHistoryToken) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentHistoryTransaction ----------

-- | Phantom type for @NSPersistentHistoryTransaction@.
data NSPersistentHistoryTransaction

instance IsObjCObject (Id NSPersistentHistoryTransaction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentHistoryTransaction"

class IsNSObject a => IsNSPersistentHistoryTransaction a where
  toNSPersistentHistoryTransaction :: a -> Id NSPersistentHistoryTransaction

instance IsNSPersistentHistoryTransaction (Id NSPersistentHistoryTransaction) where
  toNSPersistentHistoryTransaction = unsafeCastId

instance IsNSObject (Id NSPersistentHistoryTransaction) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentStore ----------

-- | Phantom type for @NSPersistentStore@.
data NSPersistentStore

instance IsObjCObject (Id NSPersistentStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentStore"

class IsNSObject a => IsNSPersistentStore a where
  toNSPersistentStore :: a -> Id NSPersistentStore

instance IsNSPersistentStore (Id NSPersistentStore) where
  toNSPersistentStore = unsafeCastId

instance IsNSObject (Id NSPersistentStore) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentStoreCoordinator ----------

-- | Phantom type for @NSPersistentStoreCoordinator@.
data NSPersistentStoreCoordinator

instance IsObjCObject (Id NSPersistentStoreCoordinator) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentStoreCoordinator"

class IsNSObject a => IsNSPersistentStoreCoordinator a where
  toNSPersistentStoreCoordinator :: a -> Id NSPersistentStoreCoordinator

instance IsNSPersistentStoreCoordinator (Id NSPersistentStoreCoordinator) where
  toNSPersistentStoreCoordinator = unsafeCastId

instance IsNSObject (Id NSPersistentStoreCoordinator) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentStoreDescription ----------

-- | Phantom type for @NSPersistentStoreDescription@.
data NSPersistentStoreDescription

instance IsObjCObject (Id NSPersistentStoreDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentStoreDescription"

class IsNSObject a => IsNSPersistentStoreDescription a where
  toNSPersistentStoreDescription :: a -> Id NSPersistentStoreDescription

instance IsNSPersistentStoreDescription (Id NSPersistentStoreDescription) where
  toNSPersistentStoreDescription = unsafeCastId

instance IsNSObject (Id NSPersistentStoreDescription) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentStoreRequest ----------

-- | Phantom type for @NSPersistentStoreRequest@.
data NSPersistentStoreRequest

instance IsObjCObject (Id NSPersistentStoreRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentStoreRequest"

class IsNSObject a => IsNSPersistentStoreRequest a where
  toNSPersistentStoreRequest :: a -> Id NSPersistentStoreRequest

instance IsNSPersistentStoreRequest (Id NSPersistentStoreRequest) where
  toNSPersistentStoreRequest = unsafeCastId

instance IsNSObject (Id NSPersistentStoreRequest) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentStoreResult ----------

-- | Phantom type for @NSPersistentStoreResult@.
data NSPersistentStoreResult

instance IsObjCObject (Id NSPersistentStoreResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentStoreResult"

class IsNSObject a => IsNSPersistentStoreResult a where
  toNSPersistentStoreResult :: a -> Id NSPersistentStoreResult

instance IsNSPersistentStoreResult (Id NSPersistentStoreResult) where
  toNSPersistentStoreResult = unsafeCastId

instance IsNSObject (Id NSPersistentStoreResult) where
  toNSObject = unsafeCastId

-- ---------- NSPropertyDescription ----------

-- | Phantom type for @NSPropertyDescription@.
data NSPropertyDescription

instance IsObjCObject (Id NSPropertyDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPropertyDescription"

class IsNSObject a => IsNSPropertyDescription a where
  toNSPropertyDescription :: a -> Id NSPropertyDescription

instance IsNSPropertyDescription (Id NSPropertyDescription) where
  toNSPropertyDescription = unsafeCastId

instance IsNSObject (Id NSPropertyDescription) where
  toNSObject = unsafeCastId

-- ---------- NSPropertyMapping ----------

-- | Phantom type for @NSPropertyMapping@.
data NSPropertyMapping

instance IsObjCObject (Id NSPropertyMapping) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPropertyMapping"

class IsNSObject a => IsNSPropertyMapping a where
  toNSPropertyMapping :: a -> Id NSPropertyMapping

instance IsNSPropertyMapping (Id NSPropertyMapping) where
  toNSPropertyMapping = unsafeCastId

instance IsNSObject (Id NSPropertyMapping) where
  toNSObject = unsafeCastId

-- ---------- NSQueryGenerationToken ----------

-- | Phantom type for @NSQueryGenerationToken@.
data NSQueryGenerationToken

instance IsObjCObject (Id NSQueryGenerationToken) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSQueryGenerationToken"

class IsNSObject a => IsNSQueryGenerationToken a where
  toNSQueryGenerationToken :: a -> Id NSQueryGenerationToken

instance IsNSQueryGenerationToken (Id NSQueryGenerationToken) where
  toNSQueryGenerationToken = unsafeCastId

instance IsNSObject (Id NSQueryGenerationToken) where
  toNSObject = unsafeCastId

-- ---------- NSStagedMigrationManager ----------

-- | Phantom type for @NSStagedMigrationManager@.
data NSStagedMigrationManager

instance IsObjCObject (Id NSStagedMigrationManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSStagedMigrationManager"

class IsNSObject a => IsNSStagedMigrationManager a where
  toNSStagedMigrationManager :: a -> Id NSStagedMigrationManager

instance IsNSStagedMigrationManager (Id NSStagedMigrationManager) where
  toNSStagedMigrationManager = unsafeCastId

instance IsNSObject (Id NSStagedMigrationManager) where
  toNSObject = unsafeCastId

-- ---------- NSFetchRequestExpression ----------

-- | Phantom type for @NSFetchRequestExpression@.
data NSFetchRequestExpression

instance IsObjCObject (Id NSFetchRequestExpression) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFetchRequestExpression"

class IsNSExpression a => IsNSFetchRequestExpression a where
  toNSFetchRequestExpression :: a -> Id NSFetchRequestExpression

instance IsNSFetchRequestExpression (Id NSFetchRequestExpression) where
  toNSFetchRequestExpression = unsafeCastId

instance IsNSExpression (Id NSFetchRequestExpression) where
  toNSExpression = unsafeCastId

instance IsNSObject (Id NSFetchRequestExpression) where
  toNSObject = unsafeCastId

-- ---------- NSCustomMigrationStage ----------

-- | Phantom type for @NSCustomMigrationStage@.
data NSCustomMigrationStage

instance IsObjCObject (Id NSCustomMigrationStage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCustomMigrationStage"

class IsNSMigrationStage a => IsNSCustomMigrationStage a where
  toNSCustomMigrationStage :: a -> Id NSCustomMigrationStage

instance IsNSCustomMigrationStage (Id NSCustomMigrationStage) where
  toNSCustomMigrationStage = unsafeCastId

instance IsNSMigrationStage (Id NSCustomMigrationStage) where
  toNSMigrationStage = unsafeCastId

instance IsNSObject (Id NSCustomMigrationStage) where
  toNSObject = unsafeCastId

-- ---------- NSLightweightMigrationStage ----------

-- | Phantom type for @NSLightweightMigrationStage@.
data NSLightweightMigrationStage

instance IsObjCObject (Id NSLightweightMigrationStage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSLightweightMigrationStage"

class IsNSMigrationStage a => IsNSLightweightMigrationStage a where
  toNSLightweightMigrationStage :: a -> Id NSLightweightMigrationStage

instance IsNSLightweightMigrationStage (Id NSLightweightMigrationStage) where
  toNSLightweightMigrationStage = unsafeCastId

instance IsNSMigrationStage (Id NSLightweightMigrationStage) where
  toNSMigrationStage = unsafeCastId

instance IsNSObject (Id NSLightweightMigrationStage) where
  toNSObject = unsafeCastId

-- ---------- NSPersistentCloudKitContainer ----------

-- | Phantom type for @NSPersistentCloudKitContainer@.
data NSPersistentCloudKitContainer

instance IsObjCObject (Id NSPersistentCloudKitContainer) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentCloudKitContainer"

class IsNSPersistentContainer a => IsNSPersistentCloudKitContainer a where
  toNSPersistentCloudKitContainer :: a -> Id NSPersistentCloudKitContainer

instance IsNSPersistentCloudKitContainer (Id NSPersistentCloudKitContainer) where
  toNSPersistentCloudKitContainer = unsafeCastId

instance IsNSObject (Id NSPersistentCloudKitContainer) where
  toNSObject = unsafeCastId

instance IsNSPersistentContainer (Id NSPersistentCloudKitContainer) where
  toNSPersistentContainer = unsafeCastId

-- ---------- NSAtomicStore ----------

-- | Phantom type for @NSAtomicStore@.
data NSAtomicStore

instance IsObjCObject (Id NSAtomicStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAtomicStore"

class IsNSPersistentStore a => IsNSAtomicStore a where
  toNSAtomicStore :: a -> Id NSAtomicStore

instance IsNSAtomicStore (Id NSAtomicStore) where
  toNSAtomicStore = unsafeCastId

instance IsNSObject (Id NSAtomicStore) where
  toNSObject = unsafeCastId

instance IsNSPersistentStore (Id NSAtomicStore) where
  toNSPersistentStore = unsafeCastId

-- ---------- NSIncrementalStore ----------

-- | Phantom type for @NSIncrementalStore@.
data NSIncrementalStore

instance IsObjCObject (Id NSIncrementalStore) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSIncrementalStore"

class IsNSPersistentStore a => IsNSIncrementalStore a where
  toNSIncrementalStore :: a -> Id NSIncrementalStore

instance IsNSIncrementalStore (Id NSIncrementalStore) where
  toNSIncrementalStore = unsafeCastId

instance IsNSObject (Id NSIncrementalStore) where
  toNSObject = unsafeCastId

instance IsNSPersistentStore (Id NSIncrementalStore) where
  toNSPersistentStore = unsafeCastId

-- ---------- NSAsynchronousFetchRequest ----------

-- | Phantom type for @NSAsynchronousFetchRequest@.
data NSAsynchronousFetchRequest

instance IsObjCObject (Id NSAsynchronousFetchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAsynchronousFetchRequest"

class IsNSPersistentStoreRequest a => IsNSAsynchronousFetchRequest a where
  toNSAsynchronousFetchRequest :: a -> Id NSAsynchronousFetchRequest

instance IsNSAsynchronousFetchRequest (Id NSAsynchronousFetchRequest) where
  toNSAsynchronousFetchRequest = unsafeCastId

instance IsNSObject (Id NSAsynchronousFetchRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSAsynchronousFetchRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSBatchDeleteRequest ----------

-- | Phantom type for @NSBatchDeleteRequest@.
data NSBatchDeleteRequest

instance IsObjCObject (Id NSBatchDeleteRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBatchDeleteRequest"

class IsNSPersistentStoreRequest a => IsNSBatchDeleteRequest a where
  toNSBatchDeleteRequest :: a -> Id NSBatchDeleteRequest

instance IsNSBatchDeleteRequest (Id NSBatchDeleteRequest) where
  toNSBatchDeleteRequest = unsafeCastId

instance IsNSObject (Id NSBatchDeleteRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSBatchDeleteRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSBatchInsertRequest ----------

-- | Phantom type for @NSBatchInsertRequest@.
data NSBatchInsertRequest

instance IsObjCObject (Id NSBatchInsertRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBatchInsertRequest"

class IsNSPersistentStoreRequest a => IsNSBatchInsertRequest a where
  toNSBatchInsertRequest :: a -> Id NSBatchInsertRequest

instance IsNSBatchInsertRequest (Id NSBatchInsertRequest) where
  toNSBatchInsertRequest = unsafeCastId

instance IsNSObject (Id NSBatchInsertRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSBatchInsertRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSBatchUpdateRequest ----------

-- | Phantom type for @NSBatchUpdateRequest@.
data NSBatchUpdateRequest

instance IsObjCObject (Id NSBatchUpdateRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBatchUpdateRequest"

class IsNSPersistentStoreRequest a => IsNSBatchUpdateRequest a where
  toNSBatchUpdateRequest :: a -> Id NSBatchUpdateRequest

instance IsNSBatchUpdateRequest (Id NSBatchUpdateRequest) where
  toNSBatchUpdateRequest = unsafeCastId

instance IsNSObject (Id NSBatchUpdateRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSBatchUpdateRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSFetchRequest ----------

-- | Phantom type for @NSFetchRequest@.
data NSFetchRequest

instance IsObjCObject (Id NSFetchRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFetchRequest"

class IsNSPersistentStoreRequest a => IsNSFetchRequest a where
  toNSFetchRequest :: a -> Id NSFetchRequest

instance IsNSFetchRequest (Id NSFetchRequest) where
  toNSFetchRequest = unsafeCastId

instance IsNSObject (Id NSFetchRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSFetchRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSPersistentCloudKitContainerEventRequest ----------

-- | Phantom type for @NSPersistentCloudKitContainerEventRequest@.
data NSPersistentCloudKitContainerEventRequest

instance IsObjCObject (Id NSPersistentCloudKitContainerEventRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentCloudKitContainerEventRequest"

class IsNSPersistentStoreRequest a => IsNSPersistentCloudKitContainerEventRequest a where
  toNSPersistentCloudKitContainerEventRequest :: a -> Id NSPersistentCloudKitContainerEventRequest

instance IsNSPersistentCloudKitContainerEventRequest (Id NSPersistentCloudKitContainerEventRequest) where
  toNSPersistentCloudKitContainerEventRequest = unsafeCastId

instance IsNSObject (Id NSPersistentCloudKitContainerEventRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSPersistentCloudKitContainerEventRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSPersistentHistoryChangeRequest ----------

-- | Phantom type for @NSPersistentHistoryChangeRequest@.
data NSPersistentHistoryChangeRequest

instance IsObjCObject (Id NSPersistentHistoryChangeRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentHistoryChangeRequest"

class IsNSPersistentStoreRequest a => IsNSPersistentHistoryChangeRequest a where
  toNSPersistentHistoryChangeRequest :: a -> Id NSPersistentHistoryChangeRequest

instance IsNSPersistentHistoryChangeRequest (Id NSPersistentHistoryChangeRequest) where
  toNSPersistentHistoryChangeRequest = unsafeCastId

instance IsNSObject (Id NSPersistentHistoryChangeRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSPersistentHistoryChangeRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSSaveChangesRequest ----------

-- | Phantom type for @NSSaveChangesRequest@.
data NSSaveChangesRequest

instance IsObjCObject (Id NSSaveChangesRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSSaveChangesRequest"

class IsNSPersistentStoreRequest a => IsNSSaveChangesRequest a where
  toNSSaveChangesRequest :: a -> Id NSSaveChangesRequest

instance IsNSSaveChangesRequest (Id NSSaveChangesRequest) where
  toNSSaveChangesRequest = unsafeCastId

instance IsNSObject (Id NSSaveChangesRequest) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreRequest (Id NSSaveChangesRequest) where
  toNSPersistentStoreRequest = unsafeCastId

-- ---------- NSBatchDeleteResult ----------

-- | Phantom type for @NSBatchDeleteResult@.
data NSBatchDeleteResult

instance IsObjCObject (Id NSBatchDeleteResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBatchDeleteResult"

class IsNSPersistentStoreResult a => IsNSBatchDeleteResult a where
  toNSBatchDeleteResult :: a -> Id NSBatchDeleteResult

instance IsNSBatchDeleteResult (Id NSBatchDeleteResult) where
  toNSBatchDeleteResult = unsafeCastId

instance IsNSObject (Id NSBatchDeleteResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreResult (Id NSBatchDeleteResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSBatchInsertResult ----------

-- | Phantom type for @NSBatchInsertResult@.
data NSBatchInsertResult

instance IsObjCObject (Id NSBatchInsertResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBatchInsertResult"

class IsNSPersistentStoreResult a => IsNSBatchInsertResult a where
  toNSBatchInsertResult :: a -> Id NSBatchInsertResult

instance IsNSBatchInsertResult (Id NSBatchInsertResult) where
  toNSBatchInsertResult = unsafeCastId

instance IsNSObject (Id NSBatchInsertResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreResult (Id NSBatchInsertResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSBatchUpdateResult ----------

-- | Phantom type for @NSBatchUpdateResult@.
data NSBatchUpdateResult

instance IsObjCObject (Id NSBatchUpdateResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSBatchUpdateResult"

class IsNSPersistentStoreResult a => IsNSBatchUpdateResult a where
  toNSBatchUpdateResult :: a -> Id NSBatchUpdateResult

instance IsNSBatchUpdateResult (Id NSBatchUpdateResult) where
  toNSBatchUpdateResult = unsafeCastId

instance IsNSObject (Id NSBatchUpdateResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreResult (Id NSBatchUpdateResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSPersistentCloudKitContainerEventResult ----------

-- | Phantom type for @NSPersistentCloudKitContainerEventResult@.
data NSPersistentCloudKitContainerEventResult

instance IsObjCObject (Id NSPersistentCloudKitContainerEventResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentCloudKitContainerEventResult"

class IsNSPersistentStoreResult a => IsNSPersistentCloudKitContainerEventResult a where
  toNSPersistentCloudKitContainerEventResult :: a -> Id NSPersistentCloudKitContainerEventResult

instance IsNSPersistentCloudKitContainerEventResult (Id NSPersistentCloudKitContainerEventResult) where
  toNSPersistentCloudKitContainerEventResult = unsafeCastId

instance IsNSObject (Id NSPersistentCloudKitContainerEventResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreResult (Id NSPersistentCloudKitContainerEventResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSPersistentHistoryResult ----------

-- | Phantom type for @NSPersistentHistoryResult@.
data NSPersistentHistoryResult

instance IsObjCObject (Id NSPersistentHistoryResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentHistoryResult"

class IsNSPersistentStoreResult a => IsNSPersistentHistoryResult a where
  toNSPersistentHistoryResult :: a -> Id NSPersistentHistoryResult

instance IsNSPersistentHistoryResult (Id NSPersistentHistoryResult) where
  toNSPersistentHistoryResult = unsafeCastId

instance IsNSObject (Id NSPersistentHistoryResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreResult (Id NSPersistentHistoryResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSPersistentStoreAsynchronousResult ----------

-- | Phantom type for @NSPersistentStoreAsynchronousResult@.
data NSPersistentStoreAsynchronousResult

instance IsObjCObject (Id NSPersistentStoreAsynchronousResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSPersistentStoreAsynchronousResult"

class IsNSPersistentStoreResult a => IsNSPersistentStoreAsynchronousResult a where
  toNSPersistentStoreAsynchronousResult :: a -> Id NSPersistentStoreAsynchronousResult

instance IsNSPersistentStoreAsynchronousResult (Id NSPersistentStoreAsynchronousResult) where
  toNSPersistentStoreAsynchronousResult = unsafeCastId

instance IsNSObject (Id NSPersistentStoreAsynchronousResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreResult (Id NSPersistentStoreAsynchronousResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSExpressionDescription ----------

-- | Phantom type for @NSExpressionDescription@.
data NSExpressionDescription

instance IsObjCObject (Id NSExpressionDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSExpressionDescription"

class IsNSPropertyDescription a => IsNSExpressionDescription a where
  toNSExpressionDescription :: a -> Id NSExpressionDescription

instance IsNSExpressionDescription (Id NSExpressionDescription) where
  toNSExpressionDescription = unsafeCastId

instance IsNSObject (Id NSExpressionDescription) where
  toNSObject = unsafeCastId

instance IsNSPropertyDescription (Id NSExpressionDescription) where
  toNSPropertyDescription = unsafeCastId

-- ---------- NSFetchedPropertyDescription ----------

-- | Phantom type for @NSFetchedPropertyDescription@.
data NSFetchedPropertyDescription

instance IsObjCObject (Id NSFetchedPropertyDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSFetchedPropertyDescription"

class IsNSPropertyDescription a => IsNSFetchedPropertyDescription a where
  toNSFetchedPropertyDescription :: a -> Id NSFetchedPropertyDescription

instance IsNSFetchedPropertyDescription (Id NSFetchedPropertyDescription) where
  toNSFetchedPropertyDescription = unsafeCastId

instance IsNSObject (Id NSFetchedPropertyDescription) where
  toNSObject = unsafeCastId

instance IsNSPropertyDescription (Id NSFetchedPropertyDescription) where
  toNSPropertyDescription = unsafeCastId

-- ---------- NSRelationshipDescription ----------

-- | Phantom type for @NSRelationshipDescription@.
data NSRelationshipDescription

instance IsObjCObject (Id NSRelationshipDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSRelationshipDescription"

class IsNSPropertyDescription a => IsNSRelationshipDescription a where
  toNSRelationshipDescription :: a -> Id NSRelationshipDescription

instance IsNSRelationshipDescription (Id NSRelationshipDescription) where
  toNSRelationshipDescription = unsafeCastId

instance IsNSObject (Id NSRelationshipDescription) where
  toNSObject = unsafeCastId

instance IsNSPropertyDescription (Id NSRelationshipDescription) where
  toNSPropertyDescription = unsafeCastId

-- ---------- NSAsynchronousFetchResult ----------

-- | Phantom type for @NSAsynchronousFetchResult@.
data NSAsynchronousFetchResult

instance IsObjCObject (Id NSAsynchronousFetchResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSAsynchronousFetchResult"

class IsNSPersistentStoreAsynchronousResult a => IsNSAsynchronousFetchResult a where
  toNSAsynchronousFetchResult :: a -> Id NSAsynchronousFetchResult

instance IsNSAsynchronousFetchResult (Id NSAsynchronousFetchResult) where
  toNSAsynchronousFetchResult = unsafeCastId

instance IsNSObject (Id NSAsynchronousFetchResult) where
  toNSObject = unsafeCastId

instance IsNSPersistentStoreAsynchronousResult (Id NSAsynchronousFetchResult) where
  toNSPersistentStoreAsynchronousResult = unsafeCastId

instance IsNSPersistentStoreResult (Id NSAsynchronousFetchResult) where
  toNSPersistentStoreResult = unsafeCastId

-- ---------- NSCompositeAttributeDescription ----------

-- | Phantom type for @NSCompositeAttributeDescription@.
data NSCompositeAttributeDescription

instance IsObjCObject (Id NSCompositeAttributeDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSCompositeAttributeDescription"

class IsNSAttributeDescription a => IsNSCompositeAttributeDescription a where
  toNSCompositeAttributeDescription :: a -> Id NSCompositeAttributeDescription

instance IsNSCompositeAttributeDescription (Id NSCompositeAttributeDescription) where
  toNSCompositeAttributeDescription = unsafeCastId

instance IsNSAttributeDescription (Id NSCompositeAttributeDescription) where
  toNSAttributeDescription = unsafeCastId

instance IsNSObject (Id NSCompositeAttributeDescription) where
  toNSObject = unsafeCastId

instance IsNSPropertyDescription (Id NSCompositeAttributeDescription) where
  toNSPropertyDescription = unsafeCastId

-- ---------- NSDerivedAttributeDescription ----------

-- | Phantom type for @NSDerivedAttributeDescription@.
data NSDerivedAttributeDescription

instance IsObjCObject (Id NSDerivedAttributeDescription) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "NSDerivedAttributeDescription"

class IsNSAttributeDescription a => IsNSDerivedAttributeDescription a where
  toNSDerivedAttributeDescription :: a -> Id NSDerivedAttributeDescription

instance IsNSDerivedAttributeDescription (Id NSDerivedAttributeDescription) where
  toNSDerivedAttributeDescription = unsafeCastId

instance IsNSAttributeDescription (Id NSDerivedAttributeDescription) where
  toNSAttributeDescription = unsafeCastId

instance IsNSObject (Id NSDerivedAttributeDescription) where
  toNSObject = unsafeCastId

instance IsNSPropertyDescription (Id NSDerivedAttributeDescription) where
  toNSPropertyDescription = unsafeCastId
