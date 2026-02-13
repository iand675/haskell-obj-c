{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSAtomicStore@.
module ObjC.CoreData.NSAtomicStore
  ( NSAtomicStore
  , IsNSAtomicStore(..)
  , initWithPersistentStoreCoordinator_configurationName_URL_options
  , load
  , save
  , newCacheNodeForManagedObject
  , updateCacheNode_fromManagedObject
  , cacheNodes
  , addCacheNodes
  , willRemoveCacheNodes
  , cacheNodeForObjectID
  , objectIDForEntity_referenceObject
  , newReferenceObjectForManagedObject
  , referenceObjectForObjectID
  , addCacheNodesSelector
  , cacheNodeForObjectIDSelector
  , cacheNodesSelector
  , initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector
  , loadSelector
  , newCacheNodeForManagedObjectSelector
  , newReferenceObjectForManagedObjectSelector
  , objectIDForEntity_referenceObjectSelector
  , referenceObjectForObjectIDSelector
  , saveSelector
  , updateCacheNode_fromManagedObjectSelector
  , willRemoveCacheNodesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreData.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_options :: (IsNSAtomicStore nsAtomicStore, IsNSPersistentStoreCoordinator coordinator, IsNSString configurationName, IsNSURL url, IsNSDictionary options) => nsAtomicStore -> coordinator -> configurationName -> url -> options -> IO (Id NSAtomicStore)
initWithPersistentStoreCoordinator_configurationName_URL_options nsAtomicStore coordinator configurationName url options =
  sendOwnedMessage nsAtomicStore initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector (toNSPersistentStoreCoordinator coordinator) (toNSString configurationName) (toNSURL url) (toNSDictionary options)

-- | @- load:@
load :: (IsNSAtomicStore nsAtomicStore, IsNSError error_) => nsAtomicStore -> error_ -> IO Bool
load nsAtomicStore error_ =
  sendMessage nsAtomicStore loadSelector (toNSError error_)

-- | @- save:@
save :: (IsNSAtomicStore nsAtomicStore, IsNSError error_) => nsAtomicStore -> error_ -> IO Bool
save nsAtomicStore error_ =
  sendMessage nsAtomicStore saveSelector (toNSError error_)

-- | @- newCacheNodeForManagedObject:@
newCacheNodeForManagedObject :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObject managedObject) => nsAtomicStore -> managedObject -> IO (Id NSAtomicStoreCacheNode)
newCacheNodeForManagedObject nsAtomicStore managedObject =
  sendOwnedMessage nsAtomicStore newCacheNodeForManagedObjectSelector (toNSManagedObject managedObject)

-- | @- updateCacheNode:fromManagedObject:@
updateCacheNode_fromManagedObject :: (IsNSAtomicStore nsAtomicStore, IsNSAtomicStoreCacheNode node, IsNSManagedObject managedObject) => nsAtomicStore -> node -> managedObject -> IO ()
updateCacheNode_fromManagedObject nsAtomicStore node managedObject =
  sendMessage nsAtomicStore updateCacheNode_fromManagedObjectSelector (toNSAtomicStoreCacheNode node) (toNSManagedObject managedObject)

-- | @- cacheNodes@
cacheNodes :: IsNSAtomicStore nsAtomicStore => nsAtomicStore -> IO (Id NSSet)
cacheNodes nsAtomicStore =
  sendMessage nsAtomicStore cacheNodesSelector

-- | @- addCacheNodes:@
addCacheNodes :: (IsNSAtomicStore nsAtomicStore, IsNSSet cacheNodes) => nsAtomicStore -> cacheNodes -> IO ()
addCacheNodes nsAtomicStore cacheNodes =
  sendMessage nsAtomicStore addCacheNodesSelector (toNSSet cacheNodes)

-- | @- willRemoveCacheNodes:@
willRemoveCacheNodes :: (IsNSAtomicStore nsAtomicStore, IsNSSet cacheNodes) => nsAtomicStore -> cacheNodes -> IO ()
willRemoveCacheNodes nsAtomicStore cacheNodes =
  sendMessage nsAtomicStore willRemoveCacheNodesSelector (toNSSet cacheNodes)

-- | @- cacheNodeForObjectID:@
cacheNodeForObjectID :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObjectID objectID) => nsAtomicStore -> objectID -> IO (Id NSAtomicStoreCacheNode)
cacheNodeForObjectID nsAtomicStore objectID =
  sendMessage nsAtomicStore cacheNodeForObjectIDSelector (toNSManagedObjectID objectID)

-- | @- objectIDForEntity:referenceObject:@
objectIDForEntity_referenceObject :: (IsNSAtomicStore nsAtomicStore, IsNSEntityDescription entity) => nsAtomicStore -> entity -> RawId -> IO (Id NSManagedObjectID)
objectIDForEntity_referenceObject nsAtomicStore entity data_ =
  sendMessage nsAtomicStore objectIDForEntity_referenceObjectSelector (toNSEntityDescription entity) data_

-- | @- newReferenceObjectForManagedObject:@
newReferenceObjectForManagedObject :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObject managedObject) => nsAtomicStore -> managedObject -> IO RawId
newReferenceObjectForManagedObject nsAtomicStore managedObject =
  sendOwnedMessage nsAtomicStore newReferenceObjectForManagedObjectSelector (toNSManagedObject managedObject)

-- | @- referenceObjectForObjectID:@
referenceObjectForObjectID :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObjectID objectID) => nsAtomicStore -> objectID -> IO RawId
referenceObjectForObjectID nsAtomicStore objectID =
  sendMessage nsAtomicStore referenceObjectForObjectIDSelector (toNSManagedObjectID objectID)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector :: Selector '[Id NSPersistentStoreCoordinator, Id NSString, Id NSURL, Id NSDictionary] (Id NSAtomicStore)
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector = mkSelector "initWithPersistentStoreCoordinator:configurationName:URL:options:"

-- | @Selector@ for @load:@
loadSelector :: Selector '[Id NSError] Bool
loadSelector = mkSelector "load:"

-- | @Selector@ for @save:@
saveSelector :: Selector '[Id NSError] Bool
saveSelector = mkSelector "save:"

-- | @Selector@ for @newCacheNodeForManagedObject:@
newCacheNodeForManagedObjectSelector :: Selector '[Id NSManagedObject] (Id NSAtomicStoreCacheNode)
newCacheNodeForManagedObjectSelector = mkSelector "newCacheNodeForManagedObject:"

-- | @Selector@ for @updateCacheNode:fromManagedObject:@
updateCacheNode_fromManagedObjectSelector :: Selector '[Id NSAtomicStoreCacheNode, Id NSManagedObject] ()
updateCacheNode_fromManagedObjectSelector = mkSelector "updateCacheNode:fromManagedObject:"

-- | @Selector@ for @cacheNodes@
cacheNodesSelector :: Selector '[] (Id NSSet)
cacheNodesSelector = mkSelector "cacheNodes"

-- | @Selector@ for @addCacheNodes:@
addCacheNodesSelector :: Selector '[Id NSSet] ()
addCacheNodesSelector = mkSelector "addCacheNodes:"

-- | @Selector@ for @willRemoveCacheNodes:@
willRemoveCacheNodesSelector :: Selector '[Id NSSet] ()
willRemoveCacheNodesSelector = mkSelector "willRemoveCacheNodes:"

-- | @Selector@ for @cacheNodeForObjectID:@
cacheNodeForObjectIDSelector :: Selector '[Id NSManagedObjectID] (Id NSAtomicStoreCacheNode)
cacheNodeForObjectIDSelector = mkSelector "cacheNodeForObjectID:"

-- | @Selector@ for @objectIDForEntity:referenceObject:@
objectIDForEntity_referenceObjectSelector :: Selector '[Id NSEntityDescription, RawId] (Id NSManagedObjectID)
objectIDForEntity_referenceObjectSelector = mkSelector "objectIDForEntity:referenceObject:"

-- | @Selector@ for @newReferenceObjectForManagedObject:@
newReferenceObjectForManagedObjectSelector :: Selector '[Id NSManagedObject] RawId
newReferenceObjectForManagedObjectSelector = mkSelector "newReferenceObjectForManagedObject:"

-- | @Selector@ for @referenceObjectForObjectID:@
referenceObjectForObjectIDSelector :: Selector '[Id NSManagedObjectID] RawId
referenceObjectForObjectIDSelector = mkSelector "referenceObjectForObjectID:"

