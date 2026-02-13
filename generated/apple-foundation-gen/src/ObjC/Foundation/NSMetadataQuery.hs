{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSMetadataQuery@.
module ObjC.Foundation.NSMetadataQuery
  ( NSMetadataQuery
  , IsNSMetadataQuery(..)
  , startQuery
  , stopQuery
  , disableUpdates
  , enableUpdates
  , resultAtIndex
  , enumerateResultsUsingBlock
  , enumerateResultsWithOptions_usingBlock
  , indexOfResult
  , valueOfAttribute_forResultAtIndex
  , delegate
  , setDelegate
  , predicate
  , setPredicate
  , sortDescriptors
  , setSortDescriptors
  , valueListAttributes
  , setValueListAttributes
  , groupingAttributes
  , setGroupingAttributes
  , notificationBatchingInterval
  , setNotificationBatchingInterval
  , searchScopes
  , setSearchScopes
  , searchItems
  , setSearchItems
  , operationQueue
  , setOperationQueue
  , started
  , gathering
  , stopped
  , resultCount
  , results
  , valueLists
  , groupedResults
  , delegateSelector
  , disableUpdatesSelector
  , enableUpdatesSelector
  , enumerateResultsUsingBlockSelector
  , enumerateResultsWithOptions_usingBlockSelector
  , gatheringSelector
  , groupedResultsSelector
  , groupingAttributesSelector
  , indexOfResultSelector
  , notificationBatchingIntervalSelector
  , operationQueueSelector
  , predicateSelector
  , resultAtIndexSelector
  , resultCountSelector
  , resultsSelector
  , searchItemsSelector
  , searchScopesSelector
  , setDelegateSelector
  , setGroupingAttributesSelector
  , setNotificationBatchingIntervalSelector
  , setOperationQueueSelector
  , setPredicateSelector
  , setSearchItemsSelector
  , setSearchScopesSelector
  , setSortDescriptorsSelector
  , setValueListAttributesSelector
  , sortDescriptorsSelector
  , startQuerySelector
  , startedSelector
  , stopQuerySelector
  , stoppedSelector
  , valueListAttributesSelector
  , valueListsSelector
  , valueOfAttribute_forResultAtIndexSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse

  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- startQuery@
startQuery :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
startQuery nsMetadataQuery =
  sendMessage nsMetadataQuery startQuerySelector

-- | @- stopQuery@
stopQuery :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO ()
stopQuery nsMetadataQuery =
  sendMessage nsMetadataQuery stopQuerySelector

-- | @- disableUpdates@
disableUpdates :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO ()
disableUpdates nsMetadataQuery =
  sendMessage nsMetadataQuery disableUpdatesSelector

-- | @- enableUpdates@
enableUpdates :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO ()
enableUpdates nsMetadataQuery =
  sendMessage nsMetadataQuery enableUpdatesSelector

-- | @- resultAtIndex:@
resultAtIndex :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> CULong -> IO RawId
resultAtIndex nsMetadataQuery idx =
  sendMessage nsMetadataQuery resultAtIndexSelector idx

-- | @- enumerateResultsUsingBlock:@
enumerateResultsUsingBlock :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> Ptr () -> IO ()
enumerateResultsUsingBlock nsMetadataQuery block =
  sendMessage nsMetadataQuery enumerateResultsUsingBlockSelector block

-- | @- enumerateResultsWithOptions:usingBlock:@
enumerateResultsWithOptions_usingBlock :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateResultsWithOptions_usingBlock nsMetadataQuery opts block =
  sendMessage nsMetadataQuery enumerateResultsWithOptions_usingBlockSelector opts block

-- | @- indexOfResult:@
indexOfResult :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> RawId -> IO CULong
indexOfResult nsMetadataQuery result =
  sendMessage nsMetadataQuery indexOfResultSelector result

-- | @- valueOfAttribute:forResultAtIndex:@
valueOfAttribute_forResultAtIndex :: (IsNSMetadataQuery nsMetadataQuery, IsNSString attrName) => nsMetadataQuery -> attrName -> CULong -> IO RawId
valueOfAttribute_forResultAtIndex nsMetadataQuery attrName idx =
  sendMessage nsMetadataQuery valueOfAttribute_forResultAtIndexSelector (toNSString attrName) idx

-- | @- delegate@
delegate :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO RawId
delegate nsMetadataQuery =
  sendMessage nsMetadataQuery delegateSelector

-- | @- setDelegate:@
setDelegate :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> RawId -> IO ()
setDelegate nsMetadataQuery value =
  sendMessage nsMetadataQuery setDelegateSelector value

-- | @- predicate@
predicate :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSPredicate)
predicate nsMetadataQuery =
  sendMessage nsMetadataQuery predicateSelector

-- | @- setPredicate:@
setPredicate :: (IsNSMetadataQuery nsMetadataQuery, IsNSPredicate value) => nsMetadataQuery -> value -> IO ()
setPredicate nsMetadataQuery value =
  sendMessage nsMetadataQuery setPredicateSelector (toNSPredicate value)

-- | @- sortDescriptors@
sortDescriptors :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
sortDescriptors nsMetadataQuery =
  sendMessage nsMetadataQuery sortDescriptorsSelector

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setSortDescriptors nsMetadataQuery value =
  sendMessage nsMetadataQuery setSortDescriptorsSelector (toNSArray value)

-- | @- valueListAttributes@
valueListAttributes :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
valueListAttributes nsMetadataQuery =
  sendMessage nsMetadataQuery valueListAttributesSelector

-- | @- setValueListAttributes:@
setValueListAttributes :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setValueListAttributes nsMetadataQuery value =
  sendMessage nsMetadataQuery setValueListAttributesSelector (toNSArray value)

-- | @- groupingAttributes@
groupingAttributes :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
groupingAttributes nsMetadataQuery =
  sendMessage nsMetadataQuery groupingAttributesSelector

-- | @- setGroupingAttributes:@
setGroupingAttributes :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setGroupingAttributes nsMetadataQuery value =
  sendMessage nsMetadataQuery setGroupingAttributesSelector (toNSArray value)

-- | @- notificationBatchingInterval@
notificationBatchingInterval :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO CDouble
notificationBatchingInterval nsMetadataQuery =
  sendMessage nsMetadataQuery notificationBatchingIntervalSelector

-- | @- setNotificationBatchingInterval:@
setNotificationBatchingInterval :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> CDouble -> IO ()
setNotificationBatchingInterval nsMetadataQuery value =
  sendMessage nsMetadataQuery setNotificationBatchingIntervalSelector value

-- | @- searchScopes@
searchScopes :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
searchScopes nsMetadataQuery =
  sendMessage nsMetadataQuery searchScopesSelector

-- | @- setSearchScopes:@
setSearchScopes :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setSearchScopes nsMetadataQuery value =
  sendMessage nsMetadataQuery setSearchScopesSelector (toNSArray value)

-- | @- searchItems@
searchItems :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
searchItems nsMetadataQuery =
  sendMessage nsMetadataQuery searchItemsSelector

-- | @- setSearchItems:@
setSearchItems :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setSearchItems nsMetadataQuery value =
  sendMessage nsMetadataQuery setSearchItemsSelector (toNSArray value)

-- | @- operationQueue@
operationQueue :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSOperationQueue)
operationQueue nsMetadataQuery =
  sendMessage nsMetadataQuery operationQueueSelector

-- | @- setOperationQueue:@
setOperationQueue :: (IsNSMetadataQuery nsMetadataQuery, IsNSOperationQueue value) => nsMetadataQuery -> value -> IO ()
setOperationQueue nsMetadataQuery value =
  sendMessage nsMetadataQuery setOperationQueueSelector (toNSOperationQueue value)

-- | @- started@
started :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
started nsMetadataQuery =
  sendMessage nsMetadataQuery startedSelector

-- | @- gathering@
gathering :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
gathering nsMetadataQuery =
  sendMessage nsMetadataQuery gatheringSelector

-- | @- stopped@
stopped :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
stopped nsMetadataQuery =
  sendMessage nsMetadataQuery stoppedSelector

-- | @- resultCount@
resultCount :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO CULong
resultCount nsMetadataQuery =
  sendMessage nsMetadataQuery resultCountSelector

-- | @- results@
results :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
results nsMetadataQuery =
  sendMessage nsMetadataQuery resultsSelector

-- | @- valueLists@
valueLists :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSDictionary)
valueLists nsMetadataQuery =
  sendMessage nsMetadataQuery valueListsSelector

-- | @- groupedResults@
groupedResults :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
groupedResults nsMetadataQuery =
  sendMessage nsMetadataQuery groupedResultsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startQuery@
startQuerySelector :: Selector '[] Bool
startQuerySelector = mkSelector "startQuery"

-- | @Selector@ for @stopQuery@
stopQuerySelector :: Selector '[] ()
stopQuerySelector = mkSelector "stopQuery"

-- | @Selector@ for @disableUpdates@
disableUpdatesSelector :: Selector '[] ()
disableUpdatesSelector = mkSelector "disableUpdates"

-- | @Selector@ for @enableUpdates@
enableUpdatesSelector :: Selector '[] ()
enableUpdatesSelector = mkSelector "enableUpdates"

-- | @Selector@ for @resultAtIndex:@
resultAtIndexSelector :: Selector '[CULong] RawId
resultAtIndexSelector = mkSelector "resultAtIndex:"

-- | @Selector@ for @enumerateResultsUsingBlock:@
enumerateResultsUsingBlockSelector :: Selector '[Ptr ()] ()
enumerateResultsUsingBlockSelector = mkSelector "enumerateResultsUsingBlock:"

-- | @Selector@ for @enumerateResultsWithOptions:usingBlock:@
enumerateResultsWithOptions_usingBlockSelector :: Selector '[NSEnumerationOptions, Ptr ()] ()
enumerateResultsWithOptions_usingBlockSelector = mkSelector "enumerateResultsWithOptions:usingBlock:"

-- | @Selector@ for @indexOfResult:@
indexOfResultSelector :: Selector '[RawId] CULong
indexOfResultSelector = mkSelector "indexOfResult:"

-- | @Selector@ for @valueOfAttribute:forResultAtIndex:@
valueOfAttribute_forResultAtIndexSelector :: Selector '[Id NSString, CULong] RawId
valueOfAttribute_forResultAtIndexSelector = mkSelector "valueOfAttribute:forResultAtIndex:"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

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

-- | @Selector@ for @valueListAttributes@
valueListAttributesSelector :: Selector '[] (Id NSArray)
valueListAttributesSelector = mkSelector "valueListAttributes"

-- | @Selector@ for @setValueListAttributes:@
setValueListAttributesSelector :: Selector '[Id NSArray] ()
setValueListAttributesSelector = mkSelector "setValueListAttributes:"

-- | @Selector@ for @groupingAttributes@
groupingAttributesSelector :: Selector '[] (Id NSArray)
groupingAttributesSelector = mkSelector "groupingAttributes"

-- | @Selector@ for @setGroupingAttributes:@
setGroupingAttributesSelector :: Selector '[Id NSArray] ()
setGroupingAttributesSelector = mkSelector "setGroupingAttributes:"

-- | @Selector@ for @notificationBatchingInterval@
notificationBatchingIntervalSelector :: Selector '[] CDouble
notificationBatchingIntervalSelector = mkSelector "notificationBatchingInterval"

-- | @Selector@ for @setNotificationBatchingInterval:@
setNotificationBatchingIntervalSelector :: Selector '[CDouble] ()
setNotificationBatchingIntervalSelector = mkSelector "setNotificationBatchingInterval:"

-- | @Selector@ for @searchScopes@
searchScopesSelector :: Selector '[] (Id NSArray)
searchScopesSelector = mkSelector "searchScopes"

-- | @Selector@ for @setSearchScopes:@
setSearchScopesSelector :: Selector '[Id NSArray] ()
setSearchScopesSelector = mkSelector "setSearchScopes:"

-- | @Selector@ for @searchItems@
searchItemsSelector :: Selector '[] (Id NSArray)
searchItemsSelector = mkSelector "searchItems"

-- | @Selector@ for @setSearchItems:@
setSearchItemsSelector :: Selector '[Id NSArray] ()
setSearchItemsSelector = mkSelector "setSearchItems:"

-- | @Selector@ for @operationQueue@
operationQueueSelector :: Selector '[] (Id NSOperationQueue)
operationQueueSelector = mkSelector "operationQueue"

-- | @Selector@ for @setOperationQueue:@
setOperationQueueSelector :: Selector '[Id NSOperationQueue] ()
setOperationQueueSelector = mkSelector "setOperationQueue:"

-- | @Selector@ for @started@
startedSelector :: Selector '[] Bool
startedSelector = mkSelector "started"

-- | @Selector@ for @gathering@
gatheringSelector :: Selector '[] Bool
gatheringSelector = mkSelector "gathering"

-- | @Selector@ for @stopped@
stoppedSelector :: Selector '[] Bool
stoppedSelector = mkSelector "stopped"

-- | @Selector@ for @resultCount@
resultCountSelector :: Selector '[] CULong
resultCountSelector = mkSelector "resultCount"

-- | @Selector@ for @results@
resultsSelector :: Selector '[] (Id NSArray)
resultsSelector = mkSelector "results"

-- | @Selector@ for @valueLists@
valueListsSelector :: Selector '[] (Id NSDictionary)
valueListsSelector = mkSelector "valueLists"

-- | @Selector@ for @groupedResults@
groupedResultsSelector :: Selector '[] (Id NSArray)
groupedResultsSelector = mkSelector "groupedResults"

