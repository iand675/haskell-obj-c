{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSBatchUpdateRequest@.
module ObjC.CoreData.NSBatchUpdateRequest
  ( NSBatchUpdateRequest
  , IsNSBatchUpdateRequest(..)
  , batchUpdateRequestWithEntityName
  , initWithEntityName
  , initWithEntity
  , entityName
  , entity
  , predicate
  , setPredicate
  , includesSubentities
  , setIncludesSubentities
  , resultType
  , setResultType
  , propertiesToUpdate
  , setPropertiesToUpdate
  , batchUpdateRequestWithEntityNameSelector
  , initWithEntityNameSelector
  , initWithEntitySelector
  , entityNameSelector
  , entitySelector
  , predicateSelector
  , setPredicateSelector
  , includesSubentitiesSelector
  , setIncludesSubentitiesSelector
  , resultTypeSelector
  , setResultTypeSelector
  , propertiesToUpdateSelector
  , setPropertiesToUpdateSelector

  -- * Enum types
  , NSBatchUpdateRequestResultType(NSBatchUpdateRequestResultType)
  , pattern NSStatusOnlyResultType
  , pattern NSUpdatedObjectIDsResultType
  , pattern NSUpdatedObjectsCountResultType

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

-- | @+ batchUpdateRequestWithEntityName:@
batchUpdateRequestWithEntityName :: IsNSString entityName => entityName -> IO (Id NSBatchUpdateRequest)
batchUpdateRequestWithEntityName entityName =
  do
    cls' <- getRequiredClass "NSBatchUpdateRequest"
    withObjCPtr entityName $ \raw_entityName ->
      sendClassMsg cls' (mkSelector "batchUpdateRequestWithEntityName:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithEntityName:@
initWithEntityName :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSString entityName) => nsBatchUpdateRequest -> entityName -> IO (Id NSBatchUpdateRequest)
initWithEntityName nsBatchUpdateRequest  entityName =
withObjCPtr entityName $ \raw_entityName ->
    sendMsg nsBatchUpdateRequest (mkSelector "initWithEntityName:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithEntity:@
initWithEntity :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSEntityDescription entity) => nsBatchUpdateRequest -> entity -> IO (Id NSBatchUpdateRequest)
initWithEntity nsBatchUpdateRequest  entity =
withObjCPtr entity $ \raw_entity ->
    sendMsg nsBatchUpdateRequest (mkSelector "initWithEntity:") (retPtr retVoid) [argPtr (castPtr raw_entity :: Ptr ())] >>= ownedObject . castPtr

-- | @- entityName@
entityName :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSString)
entityName nsBatchUpdateRequest  =
  sendMsg nsBatchUpdateRequest (mkSelector "entityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- entity@
entity :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSEntityDescription)
entity nsBatchUpdateRequest  =
  sendMsg nsBatchUpdateRequest (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- predicate@
predicate :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSPredicate)
predicate nsBatchUpdateRequest  =
  sendMsg nsBatchUpdateRequest (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPredicate:@
setPredicate :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSPredicate value) => nsBatchUpdateRequest -> value -> IO ()
setPredicate nsBatchUpdateRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBatchUpdateRequest (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- includesSubentities@
includesSubentities :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO Bool
includesSubentities nsBatchUpdateRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsBatchUpdateRequest (mkSelector "includesSubentities") retCULong []

-- | @- setIncludesSubentities:@
setIncludesSubentities :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> Bool -> IO ()
setIncludesSubentities nsBatchUpdateRequest  value =
  sendMsg nsBatchUpdateRequest (mkSelector "setIncludesSubentities:") retVoid [argCULong (if value then 1 else 0)]

-- | @- resultType@
resultType :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO NSBatchUpdateRequestResultType
resultType nsBatchUpdateRequest  =
  fmap (coerce :: CULong -> NSBatchUpdateRequestResultType) $ sendMsg nsBatchUpdateRequest (mkSelector "resultType") retCULong []

-- | @- setResultType:@
setResultType :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> NSBatchUpdateRequestResultType -> IO ()
setResultType nsBatchUpdateRequest  value =
  sendMsg nsBatchUpdateRequest (mkSelector "setResultType:") retVoid [argCULong (coerce value)]

-- | @- propertiesToUpdate@
propertiesToUpdate :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSDictionary)
propertiesToUpdate nsBatchUpdateRequest  =
  sendMsg nsBatchUpdateRequest (mkSelector "propertiesToUpdate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPropertiesToUpdate:@
setPropertiesToUpdate :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSDictionary value) => nsBatchUpdateRequest -> value -> IO ()
setPropertiesToUpdate nsBatchUpdateRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsBatchUpdateRequest (mkSelector "setPropertiesToUpdate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @batchUpdateRequestWithEntityName:@
batchUpdateRequestWithEntityNameSelector :: Selector
batchUpdateRequestWithEntityNameSelector = mkSelector "batchUpdateRequestWithEntityName:"

-- | @Selector@ for @initWithEntityName:@
initWithEntityNameSelector :: Selector
initWithEntityNameSelector = mkSelector "initWithEntityName:"

-- | @Selector@ for @initWithEntity:@
initWithEntitySelector :: Selector
initWithEntitySelector = mkSelector "initWithEntity:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @includesSubentities@
includesSubentitiesSelector :: Selector
includesSubentitiesSelector = mkSelector "includesSubentities"

-- | @Selector@ for @setIncludesSubentities:@
setIncludesSubentitiesSelector :: Selector
setIncludesSubentitiesSelector = mkSelector "setIncludesSubentities:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @propertiesToUpdate@
propertiesToUpdateSelector :: Selector
propertiesToUpdateSelector = mkSelector "propertiesToUpdate"

-- | @Selector@ for @setPropertiesToUpdate:@
setPropertiesToUpdateSelector :: Selector
setPropertiesToUpdateSelector = mkSelector "setPropertiesToUpdate:"

