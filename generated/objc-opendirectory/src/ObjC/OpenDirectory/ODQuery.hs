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
  , operationQueue
  , setOperationQueue
  , queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector
  , initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector
  , resultsAllowingPartial_errorSelector
  , scheduleInRunLoop_forModeSelector
  , removeFromRunLoop_forModeSelector
  , synchronizeSelector
  , operationQueueSelector
  , setOperationQueueSelector


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
    withObjCPtr inNode $ \raw_inNode ->
      withObjCPtr inAttribute $ \raw_inAttribute ->
        withObjCPtr outError $ \raw_outError ->
          sendClassMsg cls' (mkSelector "queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:") (retPtr retVoid) [argPtr (castPtr raw_inNode :: Ptr ()), argPtr (castPtr (unRawId inRecordTypeOrList) :: Ptr ()), argPtr (castPtr raw_inAttribute :: Ptr ()), argCUInt (fromIntegral inMatchType), argPtr (castPtr (unRawId inQueryValueOrList) :: Ptr ()), argPtr (castPtr (unRawId inReturnAttributeOrList) :: Ptr ()), argCLong (fromIntegral inMaximumResults), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:
--
-- Creates a query with the node using the parameters provided
--
-- Creates a query with the node using the supplied query parameters.  Some parameters                can either be NSString or NSData or an NSArray of either NSString or NSData.  Passing nil for                 returnAttributes is equivalent to passing kODAttributeTypeStandardOnly. outError is optional parameter,                nil can be passed if error details are not needed.
--
-- ObjC selector: @- initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error :: (IsODQuery odQuery, IsODNode inNode, IsNSString inAttribute, IsNSError outError) => odQuery -> inNode -> RawId -> inAttribute -> CUInt -> RawId -> RawId -> CLong -> outError -> IO (Id ODQuery)
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_error odQuery  inNode inRecordTypeOrList inAttribute inMatchType inQueryValueOrList inReturnAttributeOrList inMaximumResults outError =
withObjCPtr inNode $ \raw_inNode ->
  withObjCPtr inAttribute $ \raw_inAttribute ->
    withObjCPtr outError $ \raw_outError ->
        sendMsg odQuery (mkSelector "initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:") (retPtr retVoid) [argPtr (castPtr raw_inNode :: Ptr ()), argPtr (castPtr (unRawId inRecordTypeOrList) :: Ptr ()), argPtr (castPtr raw_inAttribute :: Ptr ()), argCUInt (fromIntegral inMatchType), argPtr (castPtr (unRawId inQueryValueOrList) :: Ptr ()), argPtr (castPtr (unRawId inReturnAttributeOrList) :: Ptr ()), argCLong (fromIntegral inMaximumResults), argPtr (castPtr raw_outError :: Ptr ())] >>= ownedObject . castPtr

-- | resultsAllowingPartial:error:
--
-- Returns results from a provided ODQuery synchronously
--
-- Returns results from a provided ODQuery synchronously.  Passing NO to inAllowPartialResults                will block the call until all results are returned or an error occurs.  YES can be passed at any time                even if previous calls were made with NO.  outError is optional parameter, nil can be passed if error                 details are not needed.
--
-- ObjC selector: @- resultsAllowingPartial:error:@
resultsAllowingPartial_error :: (IsODQuery odQuery, IsNSError outError) => odQuery -> Bool -> outError -> IO (Id NSArray)
resultsAllowingPartial_error odQuery  inAllowPartialResults outError =
withObjCPtr outError $ \raw_outError ->
    sendMsg odQuery (mkSelector "resultsAllowingPartial:error:") (retPtr retVoid) [argCULong (if inAllowPartialResults then 1 else 0), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | scheduleInRunLoop:forMode:
--
-- Adds the query object to the specified NSRunLoop to receive asynchronous results
--
-- Adds the query object to the specified NSRunLoop to receive asynchronous results.  A delegate must be set                in advance otherwise results may be lost due to the lack of a receiver.
--
-- ObjC selector: @- scheduleInRunLoop:forMode:@
scheduleInRunLoop_forMode :: (IsODQuery odQuery, IsNSRunLoop inRunLoop, IsNSString inMode) => odQuery -> inRunLoop -> inMode -> IO ()
scheduleInRunLoop_forMode odQuery  inRunLoop inMode =
withObjCPtr inRunLoop $ \raw_inRunLoop ->
  withObjCPtr inMode $ \raw_inMode ->
      sendMsg odQuery (mkSelector "scheduleInRunLoop:forMode:") retVoid [argPtr (castPtr raw_inRunLoop :: Ptr ()), argPtr (castPtr raw_inMode :: Ptr ())]

-- | removeFromRunLoop:forMode:
--
-- Removes the query object from the specified NSRunLoop
--
-- Removes the query object from the specified NSRunLoop.
--
-- ObjC selector: @- removeFromRunLoop:forMode:@
removeFromRunLoop_forMode :: (IsODQuery odQuery, IsNSRunLoop inRunLoop, IsNSString inMode) => odQuery -> inRunLoop -> inMode -> IO ()
removeFromRunLoop_forMode odQuery  inRunLoop inMode =
withObjCPtr inRunLoop $ \raw_inRunLoop ->
  withObjCPtr inMode $ \raw_inMode ->
      sendMsg odQuery (mkSelector "removeFromRunLoop:forMode:") retVoid [argPtr (castPtr raw_inRunLoop :: Ptr ()), argPtr (castPtr raw_inMode :: Ptr ())]

-- | synchronize
--
-- Will dispose of any results and restart the query.
--
-- Will dispose of any results and restart the query for subsequent resultsAllowingPartial: calls.  If the query                is currently scheduled on a RunLoop, then the delegate will be called with inResults == nil and                [inError code] == kODErrorQuerySynchronize and [inError domain] == ODFrameworkErrorDomain, signifying that                all existing results should be thrown away in preparation for new results.
--
-- ObjC selector: @- synchronize@
synchronize :: IsODQuery odQuery => odQuery -> IO ()
synchronize odQuery  =
  sendMsg odQuery (mkSelector "synchronize") retVoid []

-- | operationQueue
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- ObjC selector: @- operationQueue@
operationQueue :: IsODQuery odQuery => odQuery -> IO (Id NSOperationQueue)
operationQueue odQuery  =
  sendMsg odQuery (mkSelector "operationQueue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | operationQueue
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- The NSOperationQueue on which asynchronous results are delivered to the delegate.
--
-- ObjC selector: @- setOperationQueue:@
setOperationQueue :: (IsODQuery odQuery, IsNSOperationQueue value) => odQuery -> value -> IO ()
setOperationQueue odQuery  value =
withObjCPtr value $ \raw_value ->
    sendMsg odQuery (mkSelector "setOperationQueue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector :: Selector
queryWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector = mkSelector "queryWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:"

-- | @Selector@ for @initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:@
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector :: Selector
initWithNode_forRecordTypes_attribute_matchType_queryValues_returnAttributes_maximumResults_errorSelector = mkSelector "initWithNode:forRecordTypes:attribute:matchType:queryValues:returnAttributes:maximumResults:error:"

-- | @Selector@ for @resultsAllowingPartial:error:@
resultsAllowingPartial_errorSelector :: Selector
resultsAllowingPartial_errorSelector = mkSelector "resultsAllowingPartial:error:"

-- | @Selector@ for @scheduleInRunLoop:forMode:@
scheduleInRunLoop_forModeSelector :: Selector
scheduleInRunLoop_forModeSelector = mkSelector "scheduleInRunLoop:forMode:"

-- | @Selector@ for @removeFromRunLoop:forMode:@
removeFromRunLoop_forModeSelector :: Selector
removeFromRunLoop_forModeSelector = mkSelector "removeFromRunLoop:forMode:"

-- | @Selector@ for @synchronize@
synchronizeSelector :: Selector
synchronizeSelector = mkSelector "synchronize"

-- | @Selector@ for @operationQueue@
operationQueueSelector :: Selector
operationQueueSelector = mkSelector "operationQueue"

-- | @Selector@ for @setOperationQueue:@
setOperationQueueSelector :: Selector
setOperationQueueSelector = mkSelector "setOperationQueue:"

