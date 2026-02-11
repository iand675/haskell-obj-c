{-# LANGUAGE PatternSynonyms #-}
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
  , batchInsertRequestWithEntityName_objectsSelector
  , batchInsertRequestWithEntityName_managedObjectHandlerSelector
  , initSelector
  , initWithEntityName_objectsSelector
  , initWithEntity_objectsSelector
  , initWithEntity_managedObjectHandlerSelector
  , initWithEntityName_managedObjectHandlerSelector
  , entityNameSelector
  , entitySelector
  , objectsToInsertSelector
  , setObjectsToInsertSelector
  , managedObjectHandlerSelector
  , setManagedObjectHandlerSelector
  , resultTypeSelector
  , setResultTypeSelector

  -- * Enum types
  , NSBatchInsertRequestResultType(NSBatchInsertRequestResultType)
  , pattern NSBatchInsertRequestResultTypeStatusOnly
  , pattern NSBatchInsertRequestResultTypeObjectIDs
  , pattern NSBatchInsertRequestResultTypeCount

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
import ObjC.Foundation.Internal.Classes

-- | @+ batchInsertRequestWithEntityName:objects:@
batchInsertRequestWithEntityName_objects :: (IsNSString entityName, IsNSArray dictionaries) => entityName -> dictionaries -> IO (Id NSBatchInsertRequest)
batchInsertRequestWithEntityName_objects entityName dictionaries =
  do
    cls' <- getRequiredClass "NSBatchInsertRequest"
    withObjCPtr entityName $ \raw_entityName ->
      withObjCPtr dictionaries $ \raw_dictionaries ->
        sendClassMsg cls' (mkSelector "batchInsertRequestWithEntityName:objects:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ()), argPtr (castPtr raw_dictionaries :: Ptr ())] >>= retainedObject . castPtr

-- | @+ batchInsertRequestWithEntityName:managedObjectHandler:@
batchInsertRequestWithEntityName_managedObjectHandler :: IsNSString entityName => entityName -> Ptr () -> IO (Id NSBatchInsertRequest)
batchInsertRequestWithEntityName_managedObjectHandler entityName handler =
  do
    cls' <- getRequiredClass "NSBatchInsertRequest"
    withObjCPtr entityName $ \raw_entityName ->
      sendClassMsg cls' (mkSelector "batchInsertRequestWithEntityName:managedObjectHandler:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSBatchInsertRequest)
init_ nsBatchInsertRequest  =
  sendMsg nsBatchInsertRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithEntityName:objects:@
initWithEntityName_objects :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSString entityName, IsNSArray dictionaries) => nsBatchInsertRequest -> entityName -> dictionaries -> IO (Id NSBatchInsertRequest)
initWithEntityName_objects nsBatchInsertRequest  entityName dictionaries =
withObjCPtr entityName $ \raw_entityName ->
  withObjCPtr dictionaries $ \raw_dictionaries ->
      sendMsg nsBatchInsertRequest (mkSelector "initWithEntityName:objects:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ()), argPtr (castPtr raw_dictionaries :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEntity:objects:@
initWithEntity_objects :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSEntityDescription entity, IsNSArray dictionaries) => nsBatchInsertRequest -> entity -> dictionaries -> IO (Id NSBatchInsertRequest)
initWithEntity_objects nsBatchInsertRequest  entity dictionaries =
withObjCPtr entity $ \raw_entity ->
  withObjCPtr dictionaries $ \raw_dictionaries ->
      sendMsg nsBatchInsertRequest (mkSelector "initWithEntity:objects:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ()), argPtr (castPtr raw_dictionaries :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEntity:managedObjectHandler:@
initWithEntity_managedObjectHandler :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSEntityDescription entity) => nsBatchInsertRequest -> entity -> Ptr () -> IO (Id NSBatchInsertRequest)
initWithEntity_managedObjectHandler nsBatchInsertRequest  entity handler =
withObjCPtr entity $ \raw_entity ->
    sendMsg nsBatchInsertRequest (mkSelector "initWithEntity:managedObjectHandler:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEntityName:managedObjectHandler:@
initWithEntityName_managedObjectHandler :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSString entityName) => nsBatchInsertRequest -> entityName -> Ptr () -> IO (Id NSBatchInsertRequest)
initWithEntityName_managedObjectHandler nsBatchInsertRequest  entityName handler =
withObjCPtr entityName $ \raw_entityName ->
    sendMsg nsBatchInsertRequest (mkSelector "initWithEntityName:managedObjectHandler:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ()), argPtr (castPtr handler :: Ptr ())] >>= ownedObject . castPtr

-- | @- entityName@
entityName :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSString)
entityName nsBatchInsertRequest  =
  sendMsg nsBatchInsertRequest (mkSelector "entityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- entity@
entity :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSEntityDescription)
entity nsBatchInsertRequest  =
  sendMsg nsBatchInsertRequest (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- objectsToInsert@
objectsToInsert :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Id NSArray)
objectsToInsert nsBatchInsertRequest  =
  sendMsg nsBatchInsertRequest (mkSelector "objectsToInsert") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setObjectsToInsert:@
setObjectsToInsert :: (IsNSBatchInsertRequest nsBatchInsertRequest, IsNSArray value) => nsBatchInsertRequest -> value -> IO ()
setObjectsToInsert nsBatchInsertRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBatchInsertRequest (mkSelector "setObjectsToInsert:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- managedObjectHandler@
managedObjectHandler :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO (Ptr ())
managedObjectHandler nsBatchInsertRequest  =
  fmap castPtr $ sendMsg nsBatchInsertRequest (mkSelector "managedObjectHandler") (retPtr retVoid) []

-- | @- setManagedObjectHandler:@
setManagedObjectHandler :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> Ptr () -> IO ()
setManagedObjectHandler nsBatchInsertRequest  value =
  sendMsg nsBatchInsertRequest (mkSelector "setManagedObjectHandler:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | @- resultType@
resultType :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> IO NSBatchInsertRequestResultType
resultType nsBatchInsertRequest  =
  fmap (coerce :: CULong -> NSBatchInsertRequestResultType) $ sendMsg nsBatchInsertRequest (mkSelector "resultType") retCULong []

-- | @- setResultType:@
setResultType :: IsNSBatchInsertRequest nsBatchInsertRequest => nsBatchInsertRequest -> NSBatchInsertRequestResultType -> IO ()
setResultType nsBatchInsertRequest  value =
  sendMsg nsBatchInsertRequest (mkSelector "setResultType:") retVoid [argCULong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @batchInsertRequestWithEntityName:objects:@
batchInsertRequestWithEntityName_objectsSelector :: Selector
batchInsertRequestWithEntityName_objectsSelector = mkSelector "batchInsertRequestWithEntityName:objects:"

-- | @Selector@ for @batchInsertRequestWithEntityName:managedObjectHandler:@
batchInsertRequestWithEntityName_managedObjectHandlerSelector :: Selector
batchInsertRequestWithEntityName_managedObjectHandlerSelector = mkSelector "batchInsertRequestWithEntityName:managedObjectHandler:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEntityName:objects:@
initWithEntityName_objectsSelector :: Selector
initWithEntityName_objectsSelector = mkSelector "initWithEntityName:objects:"

-- | @Selector@ for @initWithEntity:objects:@
initWithEntity_objectsSelector :: Selector
initWithEntity_objectsSelector = mkSelector "initWithEntity:objects:"

-- | @Selector@ for @initWithEntity:managedObjectHandler:@
initWithEntity_managedObjectHandlerSelector :: Selector
initWithEntity_managedObjectHandlerSelector = mkSelector "initWithEntity:managedObjectHandler:"

-- | @Selector@ for @initWithEntityName:managedObjectHandler:@
initWithEntityName_managedObjectHandlerSelector :: Selector
initWithEntityName_managedObjectHandlerSelector = mkSelector "initWithEntityName:managedObjectHandler:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @objectsToInsert@
objectsToInsertSelector :: Selector
objectsToInsertSelector = mkSelector "objectsToInsert"

-- | @Selector@ for @setObjectsToInsert:@
setObjectsToInsertSelector :: Selector
setObjectsToInsertSelector = mkSelector "setObjectsToInsert:"

-- | @Selector@ for @managedObjectHandler@
managedObjectHandlerSelector :: Selector
managedObjectHandlerSelector = mkSelector "managedObjectHandler"

-- | @Selector@ for @setManagedObjectHandler:@
setManagedObjectHandlerSelector :: Selector
setManagedObjectHandlerSelector = mkSelector "setManagedObjectHandler:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector
setResultTypeSelector = mkSelector "setResultType:"

