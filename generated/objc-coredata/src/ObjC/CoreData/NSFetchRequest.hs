{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSFetchRequest@.
module ObjC.CoreData.NSFetchRequest
  ( NSFetchRequest
  , IsNSFetchRequest(..)
  , fetchRequestWithEntityName
  , init_
  , initWithEntityName
  , execute
  , entity
  , setEntity
  , entityName
  , predicate
  , setPredicate
  , sortDescriptors
  , setSortDescriptors
  , fetchLimit
  , setFetchLimit
  , affectedStores
  , setAffectedStores
  , resultType
  , setResultType
  , includesSubentities
  , setIncludesSubentities
  , includesPropertyValues
  , setIncludesPropertyValues
  , returnsObjectsAsFaults
  , setReturnsObjectsAsFaults
  , relationshipKeyPathsForPrefetching
  , setRelationshipKeyPathsForPrefetching
  , includesPendingChanges
  , setIncludesPendingChanges
  , returnsDistinctResults
  , setReturnsDistinctResults
  , propertiesToFetch
  , setPropertiesToFetch
  , fetchOffset
  , setFetchOffset
  , fetchBatchSize
  , setFetchBatchSize
  , shouldRefreshRefetchedObjects
  , setShouldRefreshRefetchedObjects
  , propertiesToGroupBy
  , setPropertiesToGroupBy
  , havingPredicate
  , setHavingPredicate
  , fetchRequestWithEntityNameSelector
  , initSelector
  , initWithEntityNameSelector
  , executeSelector
  , entitySelector
  , setEntitySelector
  , entityNameSelector
  , predicateSelector
  , setPredicateSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector
  , fetchLimitSelector
  , setFetchLimitSelector
  , affectedStoresSelector
  , setAffectedStoresSelector
  , resultTypeSelector
  , setResultTypeSelector
  , includesSubentitiesSelector
  , setIncludesSubentitiesSelector
  , includesPropertyValuesSelector
  , setIncludesPropertyValuesSelector
  , returnsObjectsAsFaultsSelector
  , setReturnsObjectsAsFaultsSelector
  , relationshipKeyPathsForPrefetchingSelector
  , setRelationshipKeyPathsForPrefetchingSelector
  , includesPendingChangesSelector
  , setIncludesPendingChangesSelector
  , returnsDistinctResultsSelector
  , setReturnsDistinctResultsSelector
  , propertiesToFetchSelector
  , setPropertiesToFetchSelector
  , fetchOffsetSelector
  , setFetchOffsetSelector
  , fetchBatchSizeSelector
  , setFetchBatchSizeSelector
  , shouldRefreshRefetchedObjectsSelector
  , setShouldRefreshRefetchedObjectsSelector
  , propertiesToGroupBySelector
  , setPropertiesToGroupBySelector
  , havingPredicateSelector
  , setHavingPredicateSelector

  -- * Enum types
  , NSFetchRequestResultType(NSFetchRequestResultType)
  , pattern NSManagedObjectResultType
  , pattern NSManagedObjectIDResultType
  , pattern NSDictionaryResultType
  , pattern NSCountResultType

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

-- | @+ fetchRequestWithEntityName:@
fetchRequestWithEntityName :: IsNSString entityName => entityName -> IO (Id NSFetchRequest)
fetchRequestWithEntityName entityName =
  do
    cls' <- getRequiredClass "NSFetchRequest"
    withObjCPtr entityName $ \raw_entityName ->
      sendClassMsg cls' (mkSelector "fetchRequestWithEntityName:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ())] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSFetchRequest)
init_ nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithEntityName:@
initWithEntityName :: (IsNSFetchRequest nsFetchRequest, IsNSString entityName) => nsFetchRequest -> entityName -> IO (Id NSFetchRequest)
initWithEntityName nsFetchRequest  entityName =
withObjCPtr entityName $ \raw_entityName ->
    sendMsg nsFetchRequest (mkSelector "initWithEntityName:") (retPtr retVoid) [argPtr (castPtr raw_entityName :: Ptr ())] >>= ownedObject . castPtr

-- | @- execute:@
execute :: (IsNSFetchRequest nsFetchRequest, IsNSError error_) => nsFetchRequest -> error_ -> IO (Id NSArray)
execute nsFetchRequest  error_ =
withObjCPtr error_ $ \raw_error_ ->
    sendMsg nsFetchRequest (mkSelector "execute:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- entity@
entity :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSEntityDescription)
entity nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "entity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEntity:@
setEntity :: (IsNSFetchRequest nsFetchRequest, IsNSEntityDescription value) => nsFetchRequest -> value -> IO ()
setEntity nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setEntity:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- entityName@
entityName :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSString)
entityName nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "entityName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- predicate@
predicate :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSPredicate)
predicate nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPredicate:@
setPredicate :: (IsNSFetchRequest nsFetchRequest, IsNSPredicate value) => nsFetchRequest -> value -> IO ()
setPredicate nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sortDescriptors@
sortDescriptors :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
sortDescriptors nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setSortDescriptors nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fetchLimit@
fetchLimit :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO CULong
fetchLimit nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "fetchLimit") retCULong []

-- | @- setFetchLimit:@
setFetchLimit :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> CULong -> IO ()
setFetchLimit nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setFetchLimit:") retVoid [argCULong (fromIntegral value)]

-- | @- affectedStores@
affectedStores :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
affectedStores nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "affectedStores") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAffectedStores:@
setAffectedStores :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setAffectedStores nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setAffectedStores:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resultType@
resultType :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO NSFetchRequestResultType
resultType nsFetchRequest  =
  fmap (coerce :: CULong -> NSFetchRequestResultType) $ sendMsg nsFetchRequest (mkSelector "resultType") retCULong []

-- | @- setResultType:@
setResultType :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> NSFetchRequestResultType -> IO ()
setResultType nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setResultType:") retVoid [argCULong (coerce value)]

-- | @- includesSubentities@
includesSubentities :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
includesSubentities nsFetchRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequest (mkSelector "includesSubentities") retCULong []

-- | @- setIncludesSubentities:@
setIncludesSubentities :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setIncludesSubentities nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setIncludesSubentities:") retVoid [argCULong (if value then 1 else 0)]

-- | @- includesPropertyValues@
includesPropertyValues :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
includesPropertyValues nsFetchRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequest (mkSelector "includesPropertyValues") retCULong []

-- | @- setIncludesPropertyValues:@
setIncludesPropertyValues :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setIncludesPropertyValues nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setIncludesPropertyValues:") retVoid [argCULong (if value then 1 else 0)]

-- | @- returnsObjectsAsFaults@
returnsObjectsAsFaults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
returnsObjectsAsFaults nsFetchRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequest (mkSelector "returnsObjectsAsFaults") retCULong []

-- | @- setReturnsObjectsAsFaults:@
setReturnsObjectsAsFaults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setReturnsObjectsAsFaults nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setReturnsObjectsAsFaults:") retVoid [argCULong (if value then 1 else 0)]

-- | @- relationshipKeyPathsForPrefetching@
relationshipKeyPathsForPrefetching :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
relationshipKeyPathsForPrefetching nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "relationshipKeyPathsForPrefetching") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRelationshipKeyPathsForPrefetching:@
setRelationshipKeyPathsForPrefetching :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setRelationshipKeyPathsForPrefetching nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setRelationshipKeyPathsForPrefetching:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- includesPendingChanges@
includesPendingChanges :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
includesPendingChanges nsFetchRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequest (mkSelector "includesPendingChanges") retCULong []

-- | @- setIncludesPendingChanges:@
setIncludesPendingChanges :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setIncludesPendingChanges nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setIncludesPendingChanges:") retVoid [argCULong (if value then 1 else 0)]

-- | @- returnsDistinctResults@
returnsDistinctResults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
returnsDistinctResults nsFetchRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequest (mkSelector "returnsDistinctResults") retCULong []

-- | @- setReturnsDistinctResults:@
setReturnsDistinctResults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setReturnsDistinctResults nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setReturnsDistinctResults:") retVoid [argCULong (if value then 1 else 0)]

-- | @- propertiesToFetch@
propertiesToFetch :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
propertiesToFetch nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "propertiesToFetch") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPropertiesToFetch:@
setPropertiesToFetch :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setPropertiesToFetch nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setPropertiesToFetch:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fetchOffset@
fetchOffset :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO CULong
fetchOffset nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "fetchOffset") retCULong []

-- | @- setFetchOffset:@
setFetchOffset :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> CULong -> IO ()
setFetchOffset nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setFetchOffset:") retVoid [argCULong (fromIntegral value)]

-- | @- fetchBatchSize@
fetchBatchSize :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO CULong
fetchBatchSize nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "fetchBatchSize") retCULong []

-- | @- setFetchBatchSize:@
setFetchBatchSize :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> CULong -> IO ()
setFetchBatchSize nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setFetchBatchSize:") retVoid [argCULong (fromIntegral value)]

-- | @- shouldRefreshRefetchedObjects@
shouldRefreshRefetchedObjects :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
shouldRefreshRefetchedObjects nsFetchRequest  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsFetchRequest (mkSelector "shouldRefreshRefetchedObjects") retCULong []

-- | @- setShouldRefreshRefetchedObjects:@
setShouldRefreshRefetchedObjects :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setShouldRefreshRefetchedObjects nsFetchRequest  value =
  sendMsg nsFetchRequest (mkSelector "setShouldRefreshRefetchedObjects:") retVoid [argCULong (if value then 1 else 0)]

-- | @- propertiesToGroupBy@
propertiesToGroupBy :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
propertiesToGroupBy nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "propertiesToGroupBy") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPropertiesToGroupBy:@
setPropertiesToGroupBy :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setPropertiesToGroupBy nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setPropertiesToGroupBy:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- havingPredicate@
havingPredicate :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSPredicate)
havingPredicate nsFetchRequest  =
  sendMsg nsFetchRequest (mkSelector "havingPredicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setHavingPredicate:@
setHavingPredicate :: (IsNSFetchRequest nsFetchRequest, IsNSPredicate value) => nsFetchRequest -> value -> IO ()
setHavingPredicate nsFetchRequest  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsFetchRequest (mkSelector "setHavingPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchRequestWithEntityName:@
fetchRequestWithEntityNameSelector :: Selector
fetchRequestWithEntityNameSelector = mkSelector "fetchRequestWithEntityName:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEntityName:@
initWithEntityNameSelector :: Selector
initWithEntityNameSelector = mkSelector "initWithEntityName:"

-- | @Selector@ for @execute:@
executeSelector :: Selector
executeSelector = mkSelector "execute:"

-- | @Selector@ for @entity@
entitySelector :: Selector
entitySelector = mkSelector "entity"

-- | @Selector@ for @setEntity:@
setEntitySelector :: Selector
setEntitySelector = mkSelector "setEntity:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @predicate@
predicateSelector :: Selector
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @fetchLimit@
fetchLimitSelector :: Selector
fetchLimitSelector = mkSelector "fetchLimit"

-- | @Selector@ for @setFetchLimit:@
setFetchLimitSelector :: Selector
setFetchLimitSelector = mkSelector "setFetchLimit:"

-- | @Selector@ for @affectedStores@
affectedStoresSelector :: Selector
affectedStoresSelector = mkSelector "affectedStores"

-- | @Selector@ for @setAffectedStores:@
setAffectedStoresSelector :: Selector
setAffectedStoresSelector = mkSelector "setAffectedStores:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @includesSubentities@
includesSubentitiesSelector :: Selector
includesSubentitiesSelector = mkSelector "includesSubentities"

-- | @Selector@ for @setIncludesSubentities:@
setIncludesSubentitiesSelector :: Selector
setIncludesSubentitiesSelector = mkSelector "setIncludesSubentities:"

-- | @Selector@ for @includesPropertyValues@
includesPropertyValuesSelector :: Selector
includesPropertyValuesSelector = mkSelector "includesPropertyValues"

-- | @Selector@ for @setIncludesPropertyValues:@
setIncludesPropertyValuesSelector :: Selector
setIncludesPropertyValuesSelector = mkSelector "setIncludesPropertyValues:"

-- | @Selector@ for @returnsObjectsAsFaults@
returnsObjectsAsFaultsSelector :: Selector
returnsObjectsAsFaultsSelector = mkSelector "returnsObjectsAsFaults"

-- | @Selector@ for @setReturnsObjectsAsFaults:@
setReturnsObjectsAsFaultsSelector :: Selector
setReturnsObjectsAsFaultsSelector = mkSelector "setReturnsObjectsAsFaults:"

-- | @Selector@ for @relationshipKeyPathsForPrefetching@
relationshipKeyPathsForPrefetchingSelector :: Selector
relationshipKeyPathsForPrefetchingSelector = mkSelector "relationshipKeyPathsForPrefetching"

-- | @Selector@ for @setRelationshipKeyPathsForPrefetching:@
setRelationshipKeyPathsForPrefetchingSelector :: Selector
setRelationshipKeyPathsForPrefetchingSelector = mkSelector "setRelationshipKeyPathsForPrefetching:"

-- | @Selector@ for @includesPendingChanges@
includesPendingChangesSelector :: Selector
includesPendingChangesSelector = mkSelector "includesPendingChanges"

-- | @Selector@ for @setIncludesPendingChanges:@
setIncludesPendingChangesSelector :: Selector
setIncludesPendingChangesSelector = mkSelector "setIncludesPendingChanges:"

-- | @Selector@ for @returnsDistinctResults@
returnsDistinctResultsSelector :: Selector
returnsDistinctResultsSelector = mkSelector "returnsDistinctResults"

-- | @Selector@ for @setReturnsDistinctResults:@
setReturnsDistinctResultsSelector :: Selector
setReturnsDistinctResultsSelector = mkSelector "setReturnsDistinctResults:"

-- | @Selector@ for @propertiesToFetch@
propertiesToFetchSelector :: Selector
propertiesToFetchSelector = mkSelector "propertiesToFetch"

-- | @Selector@ for @setPropertiesToFetch:@
setPropertiesToFetchSelector :: Selector
setPropertiesToFetchSelector = mkSelector "setPropertiesToFetch:"

-- | @Selector@ for @fetchOffset@
fetchOffsetSelector :: Selector
fetchOffsetSelector = mkSelector "fetchOffset"

-- | @Selector@ for @setFetchOffset:@
setFetchOffsetSelector :: Selector
setFetchOffsetSelector = mkSelector "setFetchOffset:"

-- | @Selector@ for @fetchBatchSize@
fetchBatchSizeSelector :: Selector
fetchBatchSizeSelector = mkSelector "fetchBatchSize"

-- | @Selector@ for @setFetchBatchSize:@
setFetchBatchSizeSelector :: Selector
setFetchBatchSizeSelector = mkSelector "setFetchBatchSize:"

-- | @Selector@ for @shouldRefreshRefetchedObjects@
shouldRefreshRefetchedObjectsSelector :: Selector
shouldRefreshRefetchedObjectsSelector = mkSelector "shouldRefreshRefetchedObjects"

-- | @Selector@ for @setShouldRefreshRefetchedObjects:@
setShouldRefreshRefetchedObjectsSelector :: Selector
setShouldRefreshRefetchedObjectsSelector = mkSelector "setShouldRefreshRefetchedObjects:"

-- | @Selector@ for @propertiesToGroupBy@
propertiesToGroupBySelector :: Selector
propertiesToGroupBySelector = mkSelector "propertiesToGroupBy"

-- | @Selector@ for @setPropertiesToGroupBy:@
setPropertiesToGroupBySelector :: Selector
setPropertiesToGroupBySelector = mkSelector "setPropertiesToGroupBy:"

-- | @Selector@ for @havingPredicate@
havingPredicateSelector :: Selector
havingPredicateSelector = mkSelector "havingPredicate"

-- | @Selector@ for @setHavingPredicate:@
setHavingPredicateSelector :: Selector
setHavingPredicateSelector = mkSelector "setHavingPredicate:"

