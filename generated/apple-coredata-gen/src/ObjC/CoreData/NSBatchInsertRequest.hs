{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchInsertRequest@.
module ObjC.CoreData.NSBatchInsertRequest
  ( NSBatchInsertRequest
  , IsNSBatchInsertRequest(..)
  , batchInsertRequestWithEntityName_objects
  , batchInsertRequestWithEntityName_managedObjectHandler
  , init_
  , initWithEntityName_objects
  , initWithEntity_objects
  , initWithEntity_managedObjectHandler
  , initWithEntityName_managedObjectHandler
  , entityName
  , entity
  , objectsToInsert
  , setObjectsToInsert
  , managedObjectHandler
  , setManagedObjectHandler
  , resultType
  , setResultType
  , batchInsertRequestWithEntityName_managedObjectHandlerSelector
  , batchInsertRequestWithEntityName_objectsSelector
  , entityNameSelector
  , entitySelector
  , initSelector
  , initWithEntityName_managedObjectHandlerSelector
  , initWithEntityName_objectsSelector
  , initWithEntity_managedObjectHandlerSelector
  , initWithEntity_objectsSelector
  , managedObjectHandlerSelector
  , objectsToInsertSelector
  , resultTypeSelector
  , setManagedObjectHandlerSelector
  , setObjectsToInsertSelector
  , setResultTypeSelector

  -- * Enum types
  , NSBatchInsertRequestResultType(NSBatchInsertRequestResultType)
  , pattern NSBatchInsertRequestResultTypeStatusOnly
  , pattern NSBatchInsertRequestResultTypeObjectIDs
  , pattern NSBatchInsertRequestResultTypeCount

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

-- | @+ batchInsertRequestWithEntityName:objects:@
batchInsertRequestWithEntityName_objects :: (IsNSString entityName, IsNSArray dictionaries) => entityName -> dictionaries -> IO (Id NSBatchInsertRequest)
batchInsertRequestWithEntityName_objects entityName dictionaries =
  do
    cls' <- getRequiredClass "NSBatchInsertRequest"
    sendClassMessage cls' batchInsertRequestWithEntityName_objectsSelector (toNSString entityName) (toNSArray dictionaries)

-- | @+ batchInsertRequestWithEntityName:managedObjectHandler:@
batchInsertRequestWithEntityName_managedObjectHandler :: IsNSString entityName => entityName -> Ptr () -> IO (Id NSBatchInsertRequest)
batchInsertRequestWithEntityName_managedObjectHandler entityName handler =
  do
    cls' <- getRequiredClass "NSBatchInsertRequest"
    sendClassMessage cls' batchInsertRequestWithEntityName_managedObjectHandlerSelector (toNSString entityName) handler

-- | @- init@
init_ :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSBatchInsertRequest)
init_ nsBatchInsertRequest =
  sendOwnedMessage nsBatchInsertRequest initSelector

-- | @- initWithEntityName:objects:@
initWithEntityName_objects :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSString entityName, IsNSArray dictionaries) => nsBatchInsertRequest -> entityName -> dictionaries -> IO (Id NSBatchInsertRequest)
initWithEntityName_objects nsBatchInsertRequest entityName dictionaries =
  sendOwnedMessage nsBatchInsertRequest initWithEntityName_objectsSelector (toNSString entityName) (toNSArray dictionaries)

-- | @- initWithEntity:objects:@
initWithEntity_objects :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSEntityDescription entity, IsNSArray dictionaries) => nsBatchInsertRequest -> entity -> dictionaries -> IO (Id NSBatchInsertRequest)
initWithEntity_objects nsBatchInsertRequest entity dictionaries =
  sendOwnedMessage nsBatchInsertRequest initWithEntity_objectsSelector (toNSEntityDescription entity) (toNSArray dictionaries)

-- | @- initWithEntity:managedObjectHandler:@
initWithEntity_managedObjectHandler :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSEntityDescription entity) => nsBatchInsertRequest -> entity -> Ptr () -> IO (Id NSBatchInsertRequest)
initWithEntity_managedObjectHandler nsBatchInsertRequest entity handler =
  sendOwnedMessage nsBatchInsertRequest initWithEntity_managedObjectHandlerSelector (toNSEntityDescription entity) handler

-- | @- initWithEntityName:managedObjectHandler:@
initWithEntityName_managedObjectHandler :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSString entityName) => nsBatchInsertRequest -> entityName -> Ptr () -> IO (Id NSBatchInsertRequest)
initWithEntityName_managedObjectHandler nsBatchInsertRequest entityName handler =
  sendOwnedMessage nsBatchInsertRequest initWithEntityName_managedObjectHandlerSelector (toNSString entityName) handler

-- | @- entityName@
entityName :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSString)
entityName nsBatchInsertRequest =
  sendMessage nsBatchInsertRequest entityNameSelector

-- | @- entity@
entity :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSEntityDescription)
entity nsBatchInsertRequest =
  sendMessage nsBatchInsertRequest entitySelector

-- | @- objectsToInsert@
objectsToInsert :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSArray)
objectsToInsert nsBatchInsertRequest =
  sendMessage nsBatchInsertRequest objectsToInsertSelector

-- | @- setObjectsToInsert:@
setObjectsToInsert :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSArray value) => nsBatchInsertRequest -> value -> IO ()
setObjectsToInsert nsBatchInsertRequest value =
  sendMessage nsBatchInsertRequest setObjectsToInsertSelector (toNSArray value)

-- | @- managedObjectHandler@
managedObjectHandler :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Ptr ())
managedObjectHandler nsBatchInsertRequest =
  sendMessage nsBatchInsertRequest managedObjectHandlerSelector

-- | @- setManagedObjectHandler:@
setManagedObjectHandler :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> Ptr () -> IO ()
setManagedObjectHandler nsBatchInsertRequest value =
  sendMessage nsBatchInsertRequest setManagedObjectHandlerSelector value

-- | @- resultType@
resultType :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO NSBatchInsertRequestResultType
resultType nsBatchInsertRequest =
  sendMessage nsBatchInsertRequest resultTypeSelector

-- | @- setResultType:@
setResultType :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> NSBatchInsertRequestResultType -> IO ()
setResultType nsBatchInsertRequest value =
  sendMessage nsBatchInsertRequest setResultTypeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @batchInsertRequestWithEntityName:objects:@
batchInsertRequestWithEntityName_objectsSelector :: Selector '[Id NSString, Id NSArray] (Id NSBatchInsertRequest)
batchInsertRequestWithEntityName_objectsSelector = mkSelector "batchInsertRequestWithEntityName:objects:"

-- | @Selector@ for @batchInsertRequestWithEntityName:managedObjectHandler:@
batchInsertRequestWithEntityName_managedObjectHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSBatchInsertRequest)
batchInsertRequestWithEntityName_managedObjectHandlerSelector = mkSelector "batchInsertRequestWithEntityName:managedObjectHandler:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSBatchInsertRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEntityName:objects:@
initWithEntityName_objectsSelector :: Selector '[Id NSString, Id NSArray] (Id NSBatchInsertRequest)
initWithEntityName_objectsSelector = mkSelector "initWithEntityName:objects:"

-- | @Selector@ for @initWithEntity:objects:@
initWithEntity_objectsSelector :: Selector '[Id NSEntityDescription, Id NSArray] (Id NSBatchInsertRequest)
initWithEntity_objectsSelector = mkSelector "initWithEntity:objects:"

-- | @Selector@ for @initWithEntity:managedObjectHandler:@
initWithEntity_managedObjectHandlerSelector :: Selector '[Id NSEntityDescription, Ptr ()] (Id NSBatchInsertRequest)
initWithEntity_managedObjectHandlerSelector = mkSelector "initWithEntity:managedObjectHandler:"

-- | @Selector@ for @initWithEntityName:managedObjectHandler:@
initWithEntityName_managedObjectHandlerSelector :: Selector '[Id NSString, Ptr ()] (Id NSBatchInsertRequest)
initWithEntityName_managedObjectHandlerSelector = mkSelector "initWithEntityName:managedObjectHandler:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector '[] (Id NSString)
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @objectsToInsert@
objectsToInsertSelector :: Selector '[] (Id NSArray)
objectsToInsertSelector = mkSelector "objectsToInsert"

-- | @Selector@ for @setObjectsToInsert:@
setObjectsToInsertSelector :: Selector '[Id NSArray] ()
setObjectsToInsertSelector = mkSelector "setObjectsToInsert:"

-- | @Selector@ for @managedObjectHandler@
managedObjectHandlerSelector :: Selector '[] (Ptr ())
managedObjectHandlerSelector = mkSelector "managedObjectHandler"

-- | @Selector@ for @setManagedObjectHandler:@
setManagedObjectHandlerSelector :: Selector '[Ptr ()] ()
setManagedObjectHandlerSelector = mkSelector "setManagedObjectHandler:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSBatchInsertRequestResultType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector '[NSBatchInsertRequestResultType] ()
setResultTypeSelector = mkSelector "setResultType:"

