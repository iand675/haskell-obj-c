{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | ODQuery
--
-- Class used for querying OpenDirectory.
--
-- OpenDirectory queries may be used to search for different types of records, e.g. users, groups.
--
-- Generated bindings for @ODQuery@.
module ObjC.OpenDirectory.ODQuery
  ( ODQuery
  , IsODQuery(..)
  , queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error
  , initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error
  , resultsAllowingPartial_error
  , scheduleInRunLoop_forMode
  , removeFromRunLoop_forMode
  , synchronize
  , delegate
  , setDelegate
  , operationQueue
  , setOperationQueue
  , delegateSelector
  , initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector
  , operationQueueSelector
  , queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector
  , removeFromRunLoop_forModeSelector
  , resultsAllowingPartial_errorSelector
  , scheduleInRunLoop_forModeSelector
  , setDelegateSelector
  , setOperationQueueSelector
  , synchronizeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.OpenDirectory.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:
--
-- Creates an autoreleased query with the node using the parameters provided
--
-- Creates an autoreleased query with the node using the supplied query parameters.  Some parameters                can either be NSString or NSData or an NSArray of either NSString or NSData.  Passing nil for                 returnAttributes is equivalent to passing kODAttributeTypeStandardOnly.  outError is optional parameter,                nil can be passed if error details are not needed.
--
-- ObjC selector: @+ queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error :: (IsODNode inNode, IsNSString inAttribute, IsNSError outError) => inNode -> RawId -> inAttribute -> CUInt -> RawId -> RawId -> CLong -> outError -> IO (Id ODQuery)
queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error inNode inRecordTypeOrList inAttribute inMatchType inQueryValueOrList inReturnAttributeOrList inMaximumResults outError =
  do
    cls' <- getRequiredClass "ODQuery"
    sendClassMessage cls' queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector (toODNode inNode) inRecordTypeOrList (toNSString inAttribute) inMatchType inQueryValueOrList inReturnAttributeOrList inMaximumResults (toNSError outError)

-- | initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:
--
-- Creates a query with the node using the parameters provided
--
-- Creates a query with the node using the supplied query parameters.  Some parameters                can either be NSString or NSData or an NSArray of either NSString or NSData.  Passing nil for                 returnAttributes is equivalent to passing kODAttributeTypeStandardOnly. outError is optional parameter,                nil can be passed if error details are not needed.
--
-- ObjC selector: @- initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error :: (IsODQuery odQuery, IsODNode inNode, IsNSString inAttribute, IsNSError outError) => odQuery -> inNode -> RawId -> inAttribute -> CUInt -> RawId -> RawId -> CLong -> outError -> IO (Id ODQuery)
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error odQuery inNode inRecordTypeOrList inAttribute inMatchType inQueryValueOrList inReturnAttributeOrList inMaximumResults outError =
  sendOwnedMessage odQuery initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector (toODNode inNode) inRecordTypeOrList (toNSString inAttribute) inMatchType inQueryValueOrList inReturnAttributeOrList inMaximumResults (toNSError outError)

-- | resultsAllowingPartial:error:
--
-- Returns results from a provided ODQuery synchronously
--
-- Returns results from a provided ODQuery synchronously.  Passing NO to inAllowPartialResults                will block the call until all results are returned or an error occurs.  YES can be passed at any time                even if previous calls were made with NO.  outError is optional parameter, nil can be passed if error                 details are not needed.
--
-- ObjC selector: @- resultsAllowingPartial:error:@
resultsAllowingPartial_error :: (IsODQuery odQuery, IsNSError outError) => odQuery -> Bool -> outError -> IO (Id NSArray)
resultsAllowingPartial_error odQuery inAllowPartialResults outError =
  sendMessage odQuery resultsAllowingPartial_errorSelector inAllowPartialResults (toNSError outError)

-- | scheduleInRunLoop:forMode:
--
-- Adds the query object to the specified NSRunLoop to receive asynchronous results
--
-- Adds the query object to the specified NSRunLoop to receive asynchronous results.  A delegate must be set                in advance otherwise results may be lost due to the lack of a receiver.
--
-- ObjC selector: @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsODQuery odQuery, IsNSRunLoop inRunLoop, IsNSString inMode) => odQuery -> inRunLoop -> inMode -> IO ()
scheduleInRunLoop_forMode odQuery inRunLoop inMode =
  sendMessage odQuery scheduleInRunLoop_forModeSelector (toNSRunLoop inRunLoop) (toNSString inMode)

-- | removeFromRunLoop:forMode:
--
-- Removes the query object from the specified NSRunLoop
--
-- Removes the query object from the specified NSRunLoop.
--
-- ObjC selector: @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsODQuery odQuery, IsNSRunLoop inRunLoop, IsNSString inMode) => odQuery -> inRunLoop -> inMode -> IO ()
removeFromRunLoop_forMode odQuery inRunLoop inMode =
  sendMessage odQuery removeFromRunLoop_forModeSelector (toNSRunLoop inRunLoop) (toNSString inMode)

-- | synchronize
--
-- Will dispose of any results and restart the query.
--
-- Will dispose of any results and restart the query for subsequent resultsAllowingPartial: calls.  If the query                is currently scheduled on a RunLoop, then the delegate will be called with inResults == nil and                [inError code] == kODErrorQuerySynchronize and [inError domain] == ODFrameworkErrorDomain, signifying that                all existing results should be thrown away in preparation for new results.
--
-- ObjC selector: @- synchronize@
synchronize :: IsODQuery odQuery => odQuery -> IO ()
synchronize odQuery =
  sendMessage odQuery synchronizeSelector

-- | delegate
--
-- The currently set delegate
--
-- The query delegate which will receive asynchronous query results.
--
-- ObjC selector: @- delegate@
delegate :: IsODQuery odQuery => odQuery -> IO RawId
delegate odQuery =
  sendMessage odQuery delegateSelector

-- | delegate
--
-- The currently set delegate
--
-- The query delegate which will receive asynchronous query results.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsODQuery odQuery => odQuery -> RawId -> IO ()
setDelegate odQuery value =
  sendMessage odQuery setDelegateSelector value

-- | operationQueue
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- ObjC selector: @- operationQueue@
operationQueue :: IsODQuery odQuery => odQuery -> IO (Id NSOperationQueue)
operationQueue odQuery =
  sendMessage odQuery operationQueueSelector

-- | operationQueue
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- ObjC selector: @- setOperationQueue:@
setOperationQueue :: (IsODQuery odQuery, IsNSOperationQueue value) => odQuery -> value -> IO ()
setOperationQueue odQuery value =
  sendMessage odQuery setOperationQueueSelector (toNSOperationQueue value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector :: Selector '[Id ODNode, RawId, Id NSString, CUInt, RawId, RawId, CLong, Id NSError] (Id ODQuery)
queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector = mkSelector "queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:"

-- | @Selector@ for @initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector :: Selector '[Id ODNode, RawId, Id NSString, CUInt, RawId, RawId, CLong, Id NSError] (Id ODQuery)
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector = mkSelector "initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:"

-- | @Selector@ for @resultsAllowingPartial:error:@
resultsAllowingPartial_errorSelector :: Selector '[Bool, Id NSError] (Id NSArray)
resultsAllowingPartial_errorSelector = mkSelector "resultsAllowingPartial:error:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector '[Id NSRunLoop, Id NSString] ()
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @synchronize@
synchronizeSelector :: Selector '[] ()
synchronizeSelector = mkSelector "synchronize"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

-- | @Selector@ for @operationQueue@
operationQueueSelector :: Selector '[] (Id NSOperationQueue)
operationQueueSelector = mkSelector "operationQueue"

-- | @Selector@ for @setOperationQueue:@
setOperationQueueSelector :: Selector '[Id NSOperationQueue] ()
setOperationQueueSelector = mkSelector "setOperationQueue:"

