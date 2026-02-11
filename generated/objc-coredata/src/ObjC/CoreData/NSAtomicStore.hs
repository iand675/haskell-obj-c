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
  , initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector
  , loadSelector
  , saveSelector
  , newCacheNodeForManagedObjectSelector
  , updateCacheNode_fromManagedObjectSelector
  , cacheNodesSelector
  , addCacheNodesSelector
  , willRemoveCacheNodesSelector
  , cacheNodeForObjectIDSelector
  , objectIDForEntity_referenceObjectSelector
  , newReferenceObjectForManagedObjectSelector
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
import ObjC.Foundation.Internal.Classes

-- | @- initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_options :: (IsNSAtomicStore nsAtomicStore, IsNSPersistentStoreCoordinator coordinator, IsNSString configurationName, IsNSURL url, IsNSDictionary options) => nsAtomicStore -> coordinator -> configurationName -> url -> options -> IO (Id NSAtomicStore)
initWithPersistentStoreCoordinator_configurationName_URL_options nsAtomicStore  coordinator configurationName url options =
withObjCPtr coordinator $ \raw_coordinator ->
  withObjCPtr configurationName $ \raw_configurationName ->
    withObjCPtr url $ \raw_url ->
      withObjCPtr options $ \raw_options ->
          sendMsg nsAtomicStore (mkSelector "initWithPersistentStoreCoordinator:configurationName:URL:options:") (retPtr retVoid) [argPtr (castPtr raw_coordinator :: Ptr ()), argPtr (castPtr raw_configurationName :: Ptr ()), argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- load:@
load :: (IsNSAtomicStore nsAtomicStore, IsNSError error_) => nsAtomicStore -> error_ -> IO Bool
load nsAtomicStore  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAtomicStore (mkSelector "load:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- save:@
save :: (IsNSAtomicStore nsAtomicStore, IsNSError error_) => nsAtomicStore -> error_ -> IO Bool
save nsAtomicStore  error_ =
withObjCPtr error_ $ \raw_error_ ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsAtomicStore (mkSelector "save:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- newCacheNodeForManagedObject:@
newCacheNodeForManagedObject :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObject managedObject) => nsAtomicStore -> managedObject -> IO (Id NSAtomicStoreCacheNode)
newCacheNodeForManagedObject nsAtomicStore  managedObject =
withObjCPtr managedObject $ \raw_managedObject ->
    sendMsg nsAtomicStore (mkSelector "newCacheNodeForManagedObject:") (retPtr retVoid) [argPtr (castPtr raw_managedObject :: Ptr ())] >>= ownedObject . castPtr

-- | @- updateCacheNode:fromManagedObject:@
updateCacheNode_fromManagedObject :: (IsNSAtomicStore nsAtomicStore, IsNSAtomicStoreCacheNode node, IsNSManagedObject managedObject) => nsAtomicStore -> node -> managedObject -> IO ()
updateCacheNode_fromManagedObject nsAtomicStore  node managedObject =
withObjCPtr node $ \raw_node ->
  withObjCPtr managedObject $ \raw_managedObject ->
      sendMsg nsAtomicStore (mkSelector "updateCacheNode:fromManagedObject:") retVoid [argPtr (castPtr raw_node :: Ptr ()), argPtr (castPtr raw_managedObject :: Ptr ())]

-- | @- cacheNodes@
cacheNodes :: IsNSAtomicStore nsAtomicStore => nsAtomicStore -> IO (Id NSSet)
cacheNodes nsAtomicStore  =
  sendMsg nsAtomicStore (mkSelector "cacheNodes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- addCacheNodes:@
addCacheNodes :: (IsNSAtomicStore nsAtomicStore, IsNSSet cacheNodes) => nsAtomicStore -> cacheNodes -> IO ()
addCacheNodes nsAtomicStore  cacheNodes =
withObjCPtr cacheNodes $ \raw_cacheNodes ->
    sendMsg nsAtomicStore (mkSelector "addCacheNodes:") retVoid [argPtr (castPtr raw_cacheNodes :: Ptr ())]

-- | @- willRemoveCacheNodes:@
willRemoveCacheNodes :: (IsNSAtomicStore nsAtomicStore, IsNSSet cacheNodes) => nsAtomicStore -> cacheNodes -> IO ()
willRemoveCacheNodes nsAtomicStore  cacheNodes =
withObjCPtr cacheNodes $ \raw_cacheNodes ->
    sendMsg nsAtomicStore (mkSelector "willRemoveCacheNodes:") retVoid [argPtr (castPtr raw_cacheNodes :: Ptr ())]

-- | @- cacheNodeForObjectID:@
cacheNodeForObjectID :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObjectID objectID) => nsAtomicStore -> objectID -> IO (Id NSAtomicStoreCacheNode)
cacheNodeForObjectID nsAtomicStore  objectID =
withObjCPtr objectID $ \raw_objectID ->
    sendMsg nsAtomicStore (mkSelector "cacheNodeForObjectID:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ())] >>= retainedObject . castPtr

-- | @- objectIDForEntity:referenceObject:@
objectIDForEntity_referenceObject :: (IsNSAtomicStore nsAtomicStore, IsNSEntityDescription entity) => nsAtomicStore -> entity -> RawId -> IO (Id NSManagedObjectID)
objectIDForEntity_referenceObject nsAtomicStore  entity data_ =
withObjCPtr entity $ \raw_entity ->
    sendMsg nsAtomicStore (mkSelector "objectIDForEntity:referenceObject:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ()), argPtr (castPtr (unRawId data_) :: Ptr ())] >>= retainedObject . castPtr

-- | @- newReferenceObjectForManagedObject:@
newReferenceObjectForManagedObject :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObject managedObject) => nsAtomicStore -> managedObject -> IO RawId
newReferenceObjectForManagedObject nsAtomicStore  managedObject =
withObjCPtr managedObject $ \raw_managedObject ->
    fmap (RawId . castPtr) $ sendMsg nsAtomicStore (mkSelector "newReferenceObjectForManagedObject:") (retPtr retVoid) [argPtr (castPtr raw_managedObject :: Ptr ())]

-- | @- referenceObjectForObjectID:@
referenceObjectForObjectID :: (IsNSAtomicStore nsAtomicStore, IsNSManagedObjectID objectID) => nsAtomicStore -> objectID -> IO RawId
referenceObjectForObjectID nsAtomicStore  objectID =
withObjCPtr objectID $ \raw_objectID ->
    fmap (RawId . castPtr) $ sendMsg nsAtomicStore (mkSelector "referenceObjectForObjectID:") (retPtr retVoid) [argPtr (castPtr raw_objectID :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPersistentStoreCoordinator:configurationName:URL:options:@
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector :: Selector
initWithPersistentStoreCoordinator_configurationName_URL_optionsSelector = mkSelector "initWithPersistentStoreCoordinator:configurationName:URL:options:"

-- | @Selector@ for @load:@
loadSelector :: Selector
loadSelector = mkSelector "load:"

-- | @Selector@ for @save:@
saveSelector :: Selector
saveSelector = mkSelector "save:"

-- | @Selector@ for @newCacheNodeForManagedObject:@
newCacheNodeForManagedObjectSelector :: Selector
newCacheNodeForManagedObjectSelector = mkSelector "newCacheNodeForManagedObject:"

-- | @Selector@ for @updateCacheNode:fromManagedObject:@
updateCacheNode_fromManagedObjectSelector :: Selector
updateCacheNode_fromManagedObjectSelector = mkSelector "updateCacheNode:fromManagedObject:"

-- | @Selector@ for @cacheNodes@
cacheNodesSelector :: Selector
cacheNodesSelector = mkSelector "cacheNodes"

-- | @Selector@ for @addCacheNodes:@
addCacheNodesSelector :: Selector
addCacheNodesSelector = mkSelector "addCacheNodes:"

-- | @Selector@ for @willRemoveCacheNodes:@
willRemoveCacheNodesSelector :: Selector
willRemoveCacheNodesSelector = mkSelector "willRemoveCacheNodes:"

-- | @Selector@ for @cacheNodeForObjectID:@
cacheNodeForObjectIDSelector :: Selector
cacheNodeForObjectIDSelector = mkSelector "cacheNodeForObjectID:"

-- | @Selector@ for @objectIDForEntity:referenceObject:@
objectIDForEntity_referenceObjectSelector :: Selector
objectIDForEntity_referenceObjectSelector = mkSelector "objectIDForEntity:referenceObject:"

-- | @Selector@ for @newReferenceObjectForManagedObject:@
newReferenceObjectForManagedObjectSelector :: Selector
newReferenceObjectForManagedObjectSelector = mkSelector "newReferenceObjectForManagedObject:"

-- | @Selector@ for @referenceObjectForObjectID:@
referenceObjectForObjectIDSelector :: Selector
referenceObjectForObjectIDSelector = mkSelector "referenceObjectForObjectID:"

