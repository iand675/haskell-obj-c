{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , entityNameSelector
  , entitySelector
  , includesSubentitiesSelector
  , initWithEntityNameSelector
  , initWithEntitySelector
  , predicateSelector
  , propertiesToUpdateSelector
  , resultTypeSelector
  , setIncludesSubentitiesSelector
  , setPredicateSelector
  , setPropertiesToUpdateSelector
  , setResultTypeSelector

  -- * Enum types
  , NSBatchUpdateRequestResultType(NSBatchUpdateRequestResultType)
  , pattern NSStatusOnlyResultType
  , pattern NSUpdatedObjectIDsResultType
  , pattern NSUpdatedObjectsCountResultType

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

-- | @+ batchUpdateRequestWithEntityName:@
batchUpdateRequestWithEntityName :: IsNSString entityName => entityName -> IO (Id NSBatchUpdateRequest)
batchUpdateRequestWithEntityName entityName =
  do
    cls' <- getRequiredClass "NSBatchUpdateRequest"
    sendClassMessage cls' batchUpdateRequestWithEntityNameSelector (toNSString entityName)

-- | @- initWithEntityName:@
initWithEntityName :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSString entityName) => nsBatchUpdateRequest -> entityName -> IO (Id NSBatchUpdateRequest)
initWithEntityName nsBatchUpdateRequest entityName =
  sendOwnedMessage nsBatchUpdateRequest initWithEntityNameSelector (toNSString entityName)

-- | @- initWithEntity:@
initWithEntity :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSEntityDescription entity) => nsBatchUpdateRequest -> entity -> IO (Id NSBatchUpdateRequest)
initWithEntity nsBatchUpdateRequest entity =
  sendOwnedMessage nsBatchUpdateRequest initWithEntitySelector (toNSEntityDescription entity)

-- | @- entityName@
entityName :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSString)
entityName nsBatchUpdateRequest =
  sendMessage nsBatchUpdateRequest entityNameSelector

-- | @- entity@
entity :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSEntityDescription)
entity nsBatchUpdateRequest =
  sendMessage nsBatchUpdateRequest entitySelector

-- | @- predicate@
predicate :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSPredicate)
predicate nsBatchUpdateRequest =
  sendMessage nsBatchUpdateRequest predicateSelector

-- | @- setPredicate:@
setPredicate :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSPredicate value) => nsBatchUpdateRequest -> value -> IO ()
setPredicate nsBatchUpdateRequest value =
  sendMessage nsBatchUpdateRequest setPredicateSelector (toNSPredicate value)

-- | @- includesSubentities@
includesSubentities :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO Bool
includesSubentities nsBatchUpdateRequest =
  sendMessage nsBatchUpdateRequest includesSubentitiesSelector

-- | @- setIncludesSubentities:@
setIncludesSubentities :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> Bool -> IO ()
setIncludesSubentities nsBatchUpdateRequest value =
  sendMessage nsBatchUpdateRequest setIncludesSubentitiesSelector value

-- | @- resultType@
resultType :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO NSBatchUpdateRequestResultType
resultType nsBatchUpdateRequest =
  sendMessage nsBatchUpdateRequest resultTypeSelector

-- | @- setResultType:@
setResultType :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> NSBatchUpdateRequestResultType -> IO ()
setResultType nsBatchUpdateRequest value =
  sendMessage nsBatchUpdateRequest setResultTypeSelector value

-- | @- propertiesToUpdate@
propertiesToUpdate :: IsNSBatchUpdateRequest nsBatchUpdateRequest => nsBatchUpdateRequest -> IO (Id NSDictionary)
propertiesToUpdate nsBatchUpdateRequest =
  sendMessage nsBatchUpdateRequest propertiesToUpdateSelector

-- | @- setPropertiesToUpdate:@
setPropertiesToUpdate :: (IsNSBatchUpdateRequest nsBatchUpdateRequest, IsNSDictionary value) => nsBatchUpdateRequest -> value -> IO ()
setPropertiesToUpdate nsBatchUpdateRequest value =
  sendMessage nsBatchUpdateRequest setPropertiesToUpdateSelector (toNSDictionary value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @batchUpdateRequestWithEntityName:@
batchUpdateRequestWithEntityNameSelector :: Selector '[Id NSString] (Id NSBatchUpdateRequest)
batchUpdateRequestWithEntityNameSelector = mkSelector "batchUpdateRequestWithEntityName:"

-- | @Selector@ for @initWithEntityName:@
initWithEntityNameSelector :: Selector '[Id NSString] (Id NSBatchUpdateRequest)
initWithEntityNameSelector = mkSelector "initWithEntityName:"

-- | @Selector@ for @initWithEntity:@
initWithEntitySelector :: Selector '[Id NSEntityDescription] (Id NSBatchUpdateRequest)
initWithEntitySelector = mkSelector "initWithEntity:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector '[] (Id NSString)
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector '[Id NSPredicate] ()
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @includesSubentities@
includesSubentitiesSelector :: Selector '[] Bool
includesSubentitiesSelector = mkSelector "includesSubentities"

-- | @Selector@ for @setIncludesSubentities:@
setIncludesSubentitiesSelector :: Selector '[Bool] ()
setIncludesSubentitiesSelector = mkSelector "setIncludesSubentities:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSBatchUpdateRequestResultType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector '[NSBatchUpdateRequestResultType] ()
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @propertiesToUpdate@
propertiesToUpdateSelector :: Selector '[] (Id NSDictionary)
propertiesToUpdateSelector = mkSelector "propertiesToUpdate"

-- | @Selector@ for @setPropertiesToUpdate:@
setPropertiesToUpdateSelector :: Selector '[Id NSDictionary] ()
setPropertiesToUpdateSelector = mkSelector "setPropertiesToUpdate:"

