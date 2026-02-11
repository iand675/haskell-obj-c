{-# LANGUAGE PatternSynonyms #-}
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
  , startQuerySelector
  , stopQuerySelector
  , disableUpdatesSelector
  , enableUpdatesSelector
  , resultAtIndexSelector
  , enumerateResultsUsingBlockSelector
  , enumerateResultsWithOptions_usingBlockSelector
  , indexOfResultSelector
  , valueOfAttribute_forResultAtIndexSelector
  , predicateSelector
  , setPredicateSelector
  , sortDescriptorsSelector
  , setSortDescriptorsSelector
  , valueListAttributesSelector
  , setValueListAttributesSelector
  , groupingAttributesSelector
  , setGroupingAttributesSelector
  , notificationBatchingIntervalSelector
  , setNotificationBatchingIntervalSelector
  , searchScopesSelector
  , setSearchScopesSelector
  , searchItemsSelector
  , setSearchItemsSelector
  , operationQueueSelector
  , setOperationQueueSelector
  , startedSelector
  , gatheringSelector
  , stoppedSelector
  , resultCountSelector
  , resultsSelector
  , valueListsSelector
  , groupedResultsSelector

  -- * Enum types
  , NSEnumerationOptions(NSEnumerationOptions)
  , pattern NSEnumerationConcurrent
  , pattern NSEnumerationReverse

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

import ObjC.Foundation.Internal.Classes
import ObjC.Foundation.Internal.Enums

-- | @- startQuery@
startQuery :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
startQuery nsMetadataQuery  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMetadataQuery (mkSelector "startQuery") retCULong []

-- | @- stopQuery@
stopQuery :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO ()
stopQuery nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "stopQuery") retVoid []

-- | @- disableUpdates@
disableUpdates :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO ()
disableUpdates nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "disableUpdates") retVoid []

-- | @- enableUpdates@
enableUpdates :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO ()
enableUpdates nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "enableUpdates") retVoid []

-- | @- resultAtIndex:@
resultAtIndex :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> CULong -> IO RawId
resultAtIndex nsMetadataQuery  idx =
  fmap (RawId . castPtr) $ sendMsg nsMetadataQuery (mkSelector "resultAtIndex:") (retPtr retVoid) [argCULong (fromIntegral idx)]

-- | @- enumerateResultsUsingBlock:@
enumerateResultsUsingBlock :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> Ptr () -> IO ()
enumerateResultsUsingBlock nsMetadataQuery  block =
  sendMsg nsMetadataQuery (mkSelector "enumerateResultsUsingBlock:") retVoid [argPtr (castPtr block :: Ptr ())]

-- | @- enumerateResultsWithOptions:usingBlock:@
enumerateResultsWithOptions_usingBlock :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> NSEnumerationOptions -> Ptr () -> IO ()
enumerateResultsWithOptions_usingBlock nsMetadataQuery  opts block =
  sendMsg nsMetadataQuery (mkSelector "enumerateResultsWithOptions:usingBlock:") retVoid [argCULong (coerce opts), argPtr (castPtr block :: Ptr ())]

-- | @- indexOfResult:@
indexOfResult :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> RawId -> IO CULong
indexOfResult nsMetadataQuery  result =
  sendMsg nsMetadataQuery (mkSelector "indexOfResult:") retCULong [argPtr (castPtr (unRawId result) :: Ptr ())]

-- | @- valueOfAttribute:forResultAtIndex:@
valueOfAttribute_forResultAtIndex :: (IsNSMetadataQuery nsMetadataQuery, IsNSString attrName) => nsMetadataQuery -> attrName -> CULong -> IO RawId
valueOfAttribute_forResultAtIndex nsMetadataQuery  attrName idx =
withObjCPtr attrName $ \raw_attrName ->
    fmap (RawId . castPtr) $ sendMsg nsMetadataQuery (mkSelector "valueOfAttribute:forResultAtIndex:") (retPtr retVoid) [argPtr (castPtr raw_attrName :: Ptr ()), argCULong (fromIntegral idx)]

-- | @- predicate@
predicate :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSPredicate)
predicate nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "predicate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPredicate:@
setPredicate :: (IsNSMetadataQuery nsMetadataQuery, IsNSPredicate value) => nsMetadataQuery -> value -> IO ()
setPredicate nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setPredicate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sortDescriptors@
sortDescriptors :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
sortDescriptors nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "sortDescriptors") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSortDescriptors:@
setSortDescriptors :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setSortDescriptors nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setSortDescriptors:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- valueListAttributes@
valueListAttributes :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
valueListAttributes nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "valueListAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValueListAttributes:@
setValueListAttributes :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setValueListAttributes nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setValueListAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- groupingAttributes@
groupingAttributes :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
groupingAttributes nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "groupingAttributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setGroupingAttributes:@
setGroupingAttributes :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setGroupingAttributes nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setGroupingAttributes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- notificationBatchingInterval@
notificationBatchingInterval :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO CDouble
notificationBatchingInterval nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "notificationBatchingInterval") retCDouble []

-- | @- setNotificationBatchingInterval:@
setNotificationBatchingInterval :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> CDouble -> IO ()
setNotificationBatchingInterval nsMetadataQuery  value =
  sendMsg nsMetadataQuery (mkSelector "setNotificationBatchingInterval:") retVoid [argCDouble (fromIntegral value)]

-- | @- searchScopes@
searchScopes :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
searchScopes nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "searchScopes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchScopes:@
setSearchScopes :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setSearchScopes nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setSearchScopes:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- searchItems@
searchItems :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
searchItems nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "searchItems") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSearchItems:@
setSearchItems :: (IsNSMetadataQuery nsMetadataQuery, IsNSArray value) => nsMetadataQuery -> value -> IO ()
setSearchItems nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setSearchItems:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationQueue@
operationQueue :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSOperationQueue)
operationQueue nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "operationQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationQueue:@
setOperationQueue :: (IsNSMetadataQuery nsMetadataQuery, IsNSOperationQueue value) => nsMetadataQuery -> value -> IO ()
setOperationQueue nsMetadataQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsMetadataQuery (mkSelector "setOperationQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- started@
started :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
started nsMetadataQuery  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMetadataQuery (mkSelector "started") retCULong []

-- | @- gathering@
gathering :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
gathering nsMetadataQuery  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMetadataQuery (mkSelector "gathering") retCULong []

-- | @- stopped@
stopped :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO Bool
stopped nsMetadataQuery  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsMetadataQuery (mkSelector "stopped") retCULong []

-- | @- resultCount@
resultCount :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO CULong
resultCount nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "resultCount") retCULong []

-- | @- results@
results :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
results nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "results") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- valueLists@
valueLists :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSDictionary)
valueLists nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "valueLists") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- groupedResults@
groupedResults :: IsNSMetadataQuery nsMetadataQuery => nsMetadataQuery -> IO (Id NSArray)
groupedResults nsMetadataQuery  =
  sendMsg nsMetadataQuery (mkSelector "groupedResults") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startQuery@
startQuerySelector :: Selector
startQuerySelector = mkSelector "startQuery"

-- | @Selector@ for @stopQuery@
stopQuerySelector :: Selector
stopQuerySelector = mkSelector "stopQuery"

-- | @Selector@ for @disableUpdates@
disableUpdatesSelector :: Selector
disableUpdatesSelector = mkSelector "disableUpdates"

-- | @Selector@ for @enableUpdates@
enableUpdatesSelector :: Selector
enableUpdatesSelector = mkSelector "enableUpdates"

-- | @Selector@ for @resultAtIndex:@
resultAtIndexSelector :: Selector
resultAtIndexSelector = mkSelector "resultAtIndex:"

-- | @Selector@ for @enumerateResultsUsingBlock:@
enumerateResultsUsingBlockSelector :: Selector
enumerateResultsUsingBlockSelector = mkSelector "enumerateResultsUsingBlock:"

-- | @Selector@ for @enumerateResultsWithOptions:usingBlock:@
enumerateResultsWithOptions_usingBlockSelector :: Selector
enumerateResultsWithOptions_usingBlockSelector = mkSelector "enumerateResultsWithOptions:usingBlock:"

-- | @Selector@ for @indexOfResult:@
indexOfResultSelector :: Selector
indexOfResultSelector = mkSelector "indexOfResult:"

-- | @Selector@ for @valueOfAttribute:forResultAtIndex:@
valueOfAttribute_forResultAtIndexSelector :: Selector
valueOfAttribute_forResultAtIndexSelector = mkSelector "valueOfAttribute:forResultAtIndex:"

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

-- | @Selector@ for @valueListAttributes@
valueListAttributesSelector :: Selector
valueListAttributesSelector = mkSelector "valueListAttributes"

-- | @Selector@ for @setValueListAttributes:@
setValueListAttributesSelector :: Selector
setValueListAttributesSelector = mkSelector "setValueListAttributes:"

-- | @Selector@ for @groupingAttributes@
groupingAttributesSelector :: Selector
groupingAttributesSelector = mkSelector "groupingAttributes"

-- | @Selector@ for @setGroupingAttributes:@
setGroupingAttributesSelector :: Selector
setGroupingAttributesSelector = mkSelector "setGroupingAttributes:"

-- | @Selector@ for @notificationBatchingInterval@
notificationBatchingIntervalSelector :: Selector
notificationBatchingIntervalSelector = mkSelector "notificationBatchingInterval"

-- | @Selector@ for @setNotificationBatchingInterval:@
setNotificationBatchingIntervalSelector :: Selector
setNotificationBatchingIntervalSelector = mkSelector "setNotificationBatchingInterval:"

-- | @Selector@ for @searchScopes@
searchScopesSelector :: Selector
searchScopesSelector = mkSelector "searchScopes"

-- | @Selector@ for @setSearchScopes:@
setSearchScopesSelector :: Selector
setSearchScopesSelector = mkSelector "setSearchScopes:"

-- | @Selector@ for @searchItems@
searchItemsSelector :: Selector
searchItemsSelector = mkSelector "searchItems"

-- | @Selector@ for @setSearchItems:@
setSearchItemsSelector :: Selector
setSearchItemsSelector = mkSelector "setSearchItems:"

-- | @Selector@ for @operationQueue@
operationQueueSelector :: Selector
operationQueueSelector = mkSelector "operationQueue"

-- | @Selector@ for @setOperationQueue:@
setOperationQueueSelector :: Selector
setOperationQueueSelector = mkSelector "setOperationQueue:"

-- | @Selector@ for @started@
startedSelector :: Selector
startedSelector = mkSelector "started"

-- | @Selector@ for @gathering@
gatheringSelector :: Selector
gatheringSelector = mkSelector "gathering"

-- | @Selector@ for @stopped@
stoppedSelector :: Selector
stoppedSelector = mkSelector "stopped"

-- | @Selector@ for @resultCount@
resultCountSelector :: Selector
resultCountSelector = mkSelector "resultCount"

-- | @Selector@ for @results@
resultsSelector :: Selector
resultsSelector = mkSelector "results"

-- | @Selector@ for @valueLists@
valueListsSelector :: Selector
valueListsSelector = mkSelector "valueLists"

-- | @Selector@ for @groupedResults@
groupedResultsSelector :: Selector
groupedResultsSelector = mkSelector "groupedResults"

