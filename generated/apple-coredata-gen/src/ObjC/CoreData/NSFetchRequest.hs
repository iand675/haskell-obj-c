{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
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
  , affectedStoresSelector
  , entityNameSelector
  , entitySelector
  , executeSelector
  , fetchBatchSizeSelector
  , fetchLimitSelector
  , fetchOffsetSelector
  , fetchRequestWithEntityNameSelector
  , havingPredicateSelector
  , includesPendingChangesSelector
  , includesPropertyValuesSelector
  , includesSubentitiesSelector
  , initSelector
  , initWithEntityNameSelector
  , predicateSelector
  , propertiesToFetchSelector
  , propertiesToGroupBySelector
  , relationshipKeyPathsForPrefetchingSelector
  , resultTypeSelector
  , returnsDistinctResultsSelector
  , returnsObjectsAsFaultsSelector
  , setAffectedStoresSelector
  , setEntitySelector
  , setFetchBatchSizeSelector
  , setFetchLimitSelector
  , setFetchOffsetSelector
  , setHavingPredicateSelector
  , setIncludesPendingChangesSelector
  , setIncludesPropertyValuesSelector
  , setIncludesSubentitiesSelector
  , setPredicateSelector
  , setPropertiesToFetchSelector
  , setPropertiesToGroupBySelector
  , setRelationshipKeyPathsForPrefetchingSelector
  , setResultTypeSelector
  , setReturnsDistinctResultsSelector
  , setReturnsObjectsAsFaultsSelector
  , setShouldRefreshRefetchedObjectsSelector
  , setSortDescriptorsSelector
  , shouldRefreshRefetchedObjectsSelector
  , sortDescriptorsSelector

  -- * Enum types
  , NSFetchRequestResultType(NSFetchRequestResultType)
  , pattern NSManagedObjectResultType
  , pattern NSManagedObjectIDResultType
  , pattern NSDictionaryResultType
  , pattern NSCountResultType

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

-- | @+ fetchRequestWithEntityName:@
fetchRequestWithEntityName :: IsNSString entityName => entityName -> IO (Id NSFetchRequest)
fetchRequestWithEntityName entityName =
  do
    cls' <- getRequiredClass "NSFetchRequest"
    sendClassMessage cls' fetchRequestWithEntityNameSelector (toNSString entityName)

-- | @- init@
init_ :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSFetchRequest)
init_ nsFetchRequest =
  sendOwnedMessage nsFetchRequest initSelector

-- | @- initWithEntityName:@
initWithEntityName :: (IsNSFetchRequest nsFetchRequest, IsNSString entityName) => nsFetchRequest -> entityName -> IO (Id NSFetchRequest)
initWithEntityName nsFetchRequest entityName =
  sendOwnedMessage nsFetchRequest initWithEntityNameSelector (toNSString entityName)

-- | @- execute:@
execute :: (IsNSFetchRequest nsFetchRequest, IsNSError error_) => nsFetchRequest -> error_ -> IO (Id NSArray)
execute nsFetchRequest error_ =
  sendMessage nsFetchRequest executeSelector (toNSError error_)

-- | @- entity@
entity :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSEntityDescription)
entity nsFetchRequest =
  sendMessage nsFetchRequest entitySelector

-- | @- setEntity:@
setEntity :: (IsNSFetchRequest nsFetchRequest, IsNSEntityDescription value) => nsFetchRequest -> value -> IO ()
setEntity nsFetchRequest value =
  sendMessage nsFetchRequest setEntitySelector (toNSEntityDescription value)

-- | @- entityName@
entityName :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSString)
entityName nsFetchRequest =
  sendMessage nsFetchRequest entityNameSelector

-- | @- predicate@
predicate :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSPredicate)
predicate nsFetchRequest =
  sendMessage nsFetchRequest predicateSelector

-- | @- setPredicate:@
setPredicate :: (IsNSFetchRequest nsFetchRequest, IsNSPredicate value) => nsFetchRequest -> value -> IO ()
setPredicate nsFetchRequest value =
  sendMessage nsFetchRequest setPredicateSelector (toNSPredicate value)

-- | @- sortDescriptors@
sortDescriptors :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
sortDescriptors nsFetchRequest =
  sendMessage nsFetchRequest sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setSortDescriptors nsFetchRequest value =
  sendMessage nsFetchRequest setSortDescriptorsSelector (toNSArray value)

-- | @- fetchLimit@
fetchLimit :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO CULong
fetchLimit nsFetchRequest =
  sendMessage nsFetchRequest fetchLimitSelector

-- | @- setFetchLimit:@
setFetchLimit :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> CULong -> IO ()
setFetchLimit nsFetchRequest value =
  sendMessage nsFetchRequest setFetchLimitSelector value

-- | @- affectedStores@
affectedStores :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
affectedStores nsFetchRequest =
  sendMessage nsFetchRequest affectedStoresSelector

-- | @- setAffectedStores:@
setAffectedStores :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setAffectedStores nsFetchRequest value =
  sendMessage nsFetchRequest setAffectedStoresSelector (toNSArray value)

-- | @- resultType@
resultType :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO NSFetchRequestResultType
resultType nsFetchRequest =
  sendMessage nsFetchRequest resultTypeSelector

-- | @- setResultType:@
setResultType :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> NSFetchRequestResultType -> IO ()
setResultType nsFetchRequest value =
  sendMessage nsFetchRequest setResultTypeSelector value

-- | @- includesSubentities@
includesSubentities :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
includesSubentities nsFetchRequest =
  sendMessage nsFetchRequest includesSubentitiesSelector

-- | @- setIncludesSubentities:@
setIncludesSubentities :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setIncludesSubentities nsFetchRequest value =
  sendMessage nsFetchRequest setIncludesSubentitiesSelector value

-- | @- includesPropertyValues@
includesPropertyValues :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
includesPropertyValues nsFetchRequest =
  sendMessage nsFetchRequest includesPropertyValuesSelector

-- | @- setIncludesPropertyValues:@
setIncludesPropertyValues :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setIncludesPropertyValues nsFetchRequest value =
  sendMessage nsFetchRequest setIncludesPropertyValuesSelector value

-- | @- returnsObjectsAsFaults@
returnsObjectsAsFaults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
returnsObjectsAsFaults nsFetchRequest =
  sendMessage nsFetchRequest returnsObjectsAsFaultsSelector

-- | @- setReturnsObjectsAsFaults:@
setReturnsObjectsAsFaults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setReturnsObjectsAsFaults nsFetchRequest value =
  sendMessage nsFetchRequest setReturnsObjectsAsFaultsSelector value

-- | @- relationshipKeyPathsForPrefetching@
relationshipKeyPathsForPrefetching :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
relationshipKeyPathsForPrefetching nsFetchRequest =
  sendMessage nsFetchRequest relationshipKeyPathsForPrefetchingSelector

-- | @- setRelationshipKeyPathsForPrefetching:@
setRelationshipKeyPathsForPrefetching :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setRelationshipKeyPathsForPrefetching nsFetchRequest value =
  sendMessage nsFetchRequest setRelationshipKeyPathsForPrefetchingSelector (toNSArray value)

-- | @- includesPendingChanges@
includesPendingChanges :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
includesPendingChanges nsFetchRequest =
  sendMessage nsFetchRequest includesPendingChangesSelector

-- | @- setIncludesPendingChanges:@
setIncludesPendingChanges :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setIncludesPendingChanges nsFetchRequest value =
  sendMessage nsFetchRequest setIncludesPendingChangesSelector value

-- | @- returnsDistinctResults@
returnsDistinctResults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
returnsDistinctResults nsFetchRequest =
  sendMessage nsFetchRequest returnsDistinctResultsSelector

-- | @- setReturnsDistinctResults:@
setReturnsDistinctResults :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setReturnsDistinctResults nsFetchRequest value =
  sendMessage nsFetchRequest setReturnsDistinctResultsSelector value

-- | @- propertiesToFetch@
propertiesToFetch :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
propertiesToFetch nsFetchRequest =
  sendMessage nsFetchRequest propertiesToFetchSelector

-- | @- setPropertiesToFetch:@
setPropertiesToFetch :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setPropertiesToFetch nsFetchRequest value =
  sendMessage nsFetchRequest setPropertiesToFetchSelector (toNSArray value)

-- | @- fetchOffset@
fetchOffset :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO CULong
fetchOffset nsFetchRequest =
  sendMessage nsFetchRequest fetchOffsetSelector

-- | @- setFetchOffset:@
setFetchOffset :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> CULong -> IO ()
setFetchOffset nsFetchRequest value =
  sendMessage nsFetchRequest setFetchOffsetSelector value

-- | @- fetchBatchSize@
fetchBatchSize :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO CULong
fetchBatchSize nsFetchRequest =
  sendMessage nsFetchRequest fetchBatchSizeSelector

-- | @- setFetchBatchSize:@
setFetchBatchSize :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> CULong -> IO ()
setFetchBatchSize nsFetchRequest value =
  sendMessage nsFetchRequest setFetchBatchSizeSelector value

-- | @- shouldRefreshRefetchedObjects@
shouldRefreshRefetchedObjects :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO Bool
shouldRefreshRefetchedObjects nsFetchRequest =
  sendMessage nsFetchRequest shouldRefreshRefetchedObjectsSelector

-- | @- setShouldRefreshRefetchedObjects:@
setShouldRefreshRefetchedObjects :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> Bool -> IO ()
setShouldRefreshRefetchedObjects nsFetchRequest value =
  sendMessage nsFetchRequest setShouldRefreshRefetchedObjectsSelector value

-- | @- propertiesToGroupBy@
propertiesToGroupBy :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSArray)
propertiesToGroupBy nsFetchRequest =
  sendMessage nsFetchRequest propertiesToGroupBySelector

-- | @- setPropertiesToGroupBy:@
setPropertiesToGroupBy :: (IsNSFetchRequest nsFetchRequest, IsNSArray value) => nsFetchRequest -> value -> IO ()
setPropertiesToGroupBy nsFetchRequest value =
  sendMessage nsFetchRequest setPropertiesToGroupBySelector (toNSArray value)

-- | @- havingPredicate@
havingPredicate :: IsNSFetchRequest nsFetchRequest => nsFetchRequest -> IO (Id NSPredicate)
havingPredicate nsFetchRequest =
  sendMessage nsFetchRequest havingPredicateSelector

-- | @- setHavingPredicate:@
setHavingPredicate :: (IsNSFetchRequest nsFetchRequest, IsNSPredicate value) => nsFetchRequest -> value -> IO ()
setHavingPredicate nsFetchRequest value =
  sendMessage nsFetchRequest setHavingPredicateSelector (toNSPredicate value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @fetchRequestWithEntityName:@
fetchRequestWithEntityNameSelector :: Selector '[Id NSString] (Id NSFetchRequest)
fetchRequestWithEntityNameSelector = mkSelector "fetchRequestWithEntityName:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSFetchRequest)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEntityName:@
initWithEntityNameSelector :: Selector '[Id NSString] (Id NSFetchRequest)
initWithEntityNameSelector = mkSelector "initWithEntityName:"

-- | @Selector@ for @execute:@
executeSelector :: Selector '[Id NSError] (Id NSArray)
executeSelector = mkSelector "execute:"

-- | @Selector@ for @entity@
entitySelector :: Selector '[] (Id NSEntityDescription)
entitySelector = mkSelector "entity"

-- | @Selector@ for @setEntity:@
setEntitySelector :: Selector '[Id NSEntityDescription] ()
setEntitySelector = mkSelector "setEntity:"

-- | @Selector@ for @entityName@
entityNameSelector :: Selector '[] (Id NSString)
entityNameSelector = mkSelector "entityName"

-- | @Selector@ for @predicate@
predicateSelector :: Selector '[] (Id NSPredicate)
predicateSelector = mkSelector "predicate"

-- | @Selector@ for @setPredicate:@
setPredicateSelector :: Selector '[Id NSPredicate] ()
setPredicateSelector = mkSelector "setPredicate:"

-- | @Selector@ for @sortDescriptors@
sortDescriptorsSelector :: Selector '[] (Id NSArray)
sortDescriptorsSelector = mkSelector "sortDescriptors"

-- | @Selector@ for @setSortDescriptors:@
setSortDescriptorsSelector :: Selector '[Id NSArray] ()
setSortDescriptorsSelector = mkSelector "setSortDescriptors:"

-- | @Selector@ for @fetchLimit@
fetchLimitSelector :: Selector '[] CULong
fetchLimitSelector = mkSelector "fetchLimit"

-- | @Selector@ for @setFetchLimit:@
setFetchLimitSelector :: Selector '[CULong] ()
setFetchLimitSelector = mkSelector "setFetchLimit:"

-- | @Selector@ for @affectedStores@
affectedStoresSelector :: Selector '[] (Id NSArray)
affectedStoresSelector = mkSelector "affectedStores"

-- | @Selector@ for @setAffectedStores:@
setAffectedStoresSelector :: Selector '[Id NSArray] ()
setAffectedStoresSelector = mkSelector "setAffectedStores:"

-- | @Selector@ for @resultType@
resultTypeSelector :: Selector '[] NSFetchRequestResultType
resultTypeSelector = mkSelector "resultType"

-- | @Selector@ for @setResultType:@
setResultTypeSelector :: Selector '[NSFetchRequestResultType] ()
setResultTypeSelector = mkSelector "setResultType:"

-- | @Selector@ for @includesSubentities@
includesSubentitiesSelector :: Selector '[] Bool
includesSubentitiesSelector = mkSelector "includesSubentities"

-- | @Selector@ for @setIncludesSubentities:@
setIncludesSubentitiesSelector :: Selector '[Bool] ()
setIncludesSubentitiesSelector = mkSelector "setIncludesSubentities:"

-- | @Selector@ for @includesPropertyValues@
includesPropertyValuesSelector :: Selector '[] Bool
includesPropertyValuesSelector = mkSelector "includesPropertyValues"

-- | @Selector@ for @setIncludesPropertyValues:@
setIncludesPropertyValuesSelector :: Selector '[Bool] ()
setIncludesPropertyValuesSelector = mkSelector "setIncludesPropertyValues:"

-- | @Selector@ for @returnsObjectsAsFaults@
returnsObjectsAsFaultsSelector :: Selector '[] Bool
returnsObjectsAsFaultsSelector = mkSelector "returnsObjectsAsFaults"

-- | @Selector@ for @setReturnsObjectsAsFaults:@
setReturnsObjectsAsFaultsSelector :: Selector '[Bool] ()
setReturnsObjectsAsFaultsSelector = mkSelector "setReturnsObjectsAsFaults:"

-- | @Selector@ for @relationshipKeyPathsForPrefetching@
relationshipKeyPathsForPrefetchingSelector :: Selector '[] (Id NSArray)
relationshipKeyPathsForPrefetchingSelector = mkSelector "relationshipKeyPathsForPrefetching"

-- | @Selector@ for @setRelationshipKeyPathsForPrefetching:@
setRelationshipKeyPathsForPrefetchingSelector :: Selector '[Id NSArray] ()
setRelationshipKeyPathsForPrefetchingSelector = mkSelector "setRelationshipKeyPathsForPrefetching:"

-- | @Selector@ for @includesPendingChanges@
includesPendingChangesSelector :: Selector '[] Bool
includesPendingChangesSelector = mkSelector "includesPendingChanges"

-- | @Selector@ for @setIncludesPendingChanges:@
setIncludesPendingChangesSelector :: Selector '[Bool] ()
setIncludesPendingChangesSelector = mkSelector "setIncludesPendingChanges:"

-- | @Selector@ for @returnsDistinctResults@
returnsDistinctResultsSelector :: Selector '[] Bool
returnsDistinctResultsSelector = mkSelector "returnsDistinctResults"

-- | @Selector@ for @setReturnsDistinctResults:@
setReturnsDistinctResultsSelector :: Selector '[Bool] ()
setReturnsDistinctResultsSelector = mkSelector "setReturnsDistinctResults:"

-- | @Selector@ for @propertiesToFetch@
propertiesToFetchSelector :: Selector '[] (Id NSArray)
propertiesToFetchSelector = mkSelector "propertiesToFetch"

-- | @Selector@ for @setPropertiesToFetch:@
setPropertiesToFetchSelector :: Selector '[Id NSArray] ()
setPropertiesToFetchSelector = mkSelector "setPropertiesToFetch:"

-- | @Selector@ for @fetchOffset@
fetchOffsetSelector :: Selector '[] CULong
fetchOffsetSelector = mkSelector "fetchOffset"

-- | @Selector@ for @setFetchOffset:@
setFetchOffsetSelector :: Selector '[CULong] ()
setFetchOffsetSelector = mkSelector "setFetchOffset:"

-- | @Selector@ for @fetchBatchSize@
fetchBatchSizeSelector :: Selector '[] CULong
fetchBatchSizeSelector = mkSelector "fetchBatchSize"

-- | @Selector@ for @setFetchBatchSize:@
setFetchBatchSizeSelector :: Selector '[CULong] ()
setFetchBatchSizeSelector = mkSelector "setFetchBatchSize:"

-- | @Selector@ for @shouldRefreshRefetchedObjects@
shouldRefreshRefetchedObjectsSelector :: Selector '[] Bool
shouldRefreshRefetchedObjectsSelector = mkSelector "shouldRefreshRefetchedObjects"

-- | @Selector@ for @setShouldRefreshRefetchedObjects:@
setShouldRefreshRefetchedObjectsSelector :: Selector '[Bool] ()
setShouldRefreshRefetchedObjectsSelector = mkSelector "setShouldRefreshRefetchedObjects:"

-- | @Selector@ for @propertiesToGroupBy@
propertiesToGroupBySelector :: Selector '[] (Id NSArray)
propertiesToGroupBySelector = mkSelector "propertiesToGroupBy"

-- | @Selector@ for @setPropertiesToGroupBy:@
setPropertiesToGroupBySelector :: Selector '[Id NSArray] ()
setPropertiesToGroupBySelector = mkSelector "setPropertiesToGroupBy:"

-- | @Selector@ for @havingPredicate@
havingPredicateSelector :: Selector '[] (Id NSPredicate)
havingPredicateSelector = mkSelector "havingPredicate"

-- | @Selector@ for @setHavingPredicate:@
setHavingPredicateSelector :: Selector '[Id NSPredicate] ()
setHavingPredicateSelector = mkSelector "setHavingPredicate:"

