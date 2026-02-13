{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CKQueryOperation@.
module ObjC.CloudKit.CKQueryOperation
  ( CKQueryOperation
  , IsCKQueryOperation(..)
  , init_
  , initWithQuery
  , initWithCursor
  , query
  , setQuery
  , cursor
  , setCursor
  , zoneID
  , setZoneID
  , resultsLimit
  , setResultsLimit
  , desiredKeys
  , setDesiredKeys
  , recordFetchedBlock
  , setRecordFetchedBlock
  , recordMatchedBlock
  , setRecordMatchedBlock
  , queryCompletionBlock
  , setQueryCompletionBlock
  , cursorSelector
  , desiredKeysSelector
  , initSelector
  , initWithCursorSelector
  , initWithQuerySelector
  , queryCompletionBlockSelector
  , querySelector
  , recordFetchedBlockSelector
  , recordMatchedBlockSelector
  , resultsLimitSelector
  , setCursorSelector
  , setDesiredKeysSelector
  , setQueryCompletionBlockSelector
  , setQuerySelector
  , setRecordFetchedBlockSelector
  , setRecordMatchedBlockSelector
  , setResultsLimitSelector
  , setZoneIDSelector
  , zoneIDSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Queries invoked within a sharedCloudDatabase must specify a zoneID.  Cross-zone queries are not supported in a sharedCloudDatabase
--
-- ObjC selector: @- init@
init_ :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKQueryOperation)
init_ ckQueryOperation =
  sendOwnedMessage ckQueryOperation initSelector

-- | @- initWithQuery:@
initWithQuery :: (IsCKQueryOperation ckQueryOperation, IsCKQuery query) => ckQueryOperation -> query -> IO (Id CKQueryOperation)
initWithQuery ckQueryOperation query =
  sendOwnedMessage ckQueryOperation initWithQuerySelector (toCKQuery query)

-- | @- initWithCursor:@
initWithCursor :: (IsCKQueryOperation ckQueryOperation, IsCKQueryCursor cursor) => ckQueryOperation -> cursor -> IO (Id CKQueryOperation)
initWithCursor ckQueryOperation cursor =
  sendOwnedMessage ckQueryOperation initWithCursorSelector (toCKQueryCursor cursor)

-- | @- query@
query :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKQuery)
query ckQueryOperation =
  sendMessage ckQueryOperation querySelector

-- | @- setQuery:@
setQuery :: (IsCKQueryOperation ckQueryOperation, IsCKQuery value) => ckQueryOperation -> value -> IO ()
setQuery ckQueryOperation value =
  sendMessage ckQueryOperation setQuerySelector (toCKQuery value)

-- | @- cursor@
cursor :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKQueryCursor)
cursor ckQueryOperation =
  sendMessage ckQueryOperation cursorSelector

-- | @- setCursor:@
setCursor :: (IsCKQueryOperation ckQueryOperation, IsCKQueryCursor value) => ckQueryOperation -> value -> IO ()
setCursor ckQueryOperation value =
  sendMessage ckQueryOperation setCursorSelector (toCKQueryCursor value)

-- | Indicates which record zone to query.
--
-- For query operations constructed using a cursor, this property is ignored and instead will be evaluated in the record zone in which the cursor was originally created.  Queries that do not specify a @zoneID@ will perform a query across all zones in the database.
--
-- ObjC selector: @- zoneID@
zoneID :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKRecordZoneID)
zoneID ckQueryOperation =
  sendMessage ckQueryOperation zoneIDSelector

-- | Indicates which record zone to query.
--
-- For query operations constructed using a cursor, this property is ignored and instead will be evaluated in the record zone in which the cursor was originally created.  Queries that do not specify a @zoneID@ will perform a query across all zones in the database.
--
-- ObjC selector: @- setZoneID:@
setZoneID :: (IsCKQueryOperation ckQueryOperation, IsCKRecordZoneID value) => ckQueryOperation -> value -> IO ()
setZoneID ckQueryOperation value =
  sendMessage ckQueryOperation setZoneIDSelector (toCKRecordZoneID value)

-- | Defaults to @CKQueryOperationMaximumResults.@  Queries may return fewer than @resultsLimit@ in some scenarios:  - There are legitimately fewer than @resultsLimit@ number of records matching the query (and visible to the current user).  - During the process of querying and fetching the results, some records were deleted, or became un-readable by the current user.  When determining if there are more records to fetch, always check for the presence of a cursor in @queryCompletionBlock.@
--
-- ObjC selector: @- resultsLimit@
resultsLimit :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO CULong
resultsLimit ckQueryOperation =
  sendMessage ckQueryOperation resultsLimitSelector

-- | Defaults to @CKQueryOperationMaximumResults.@  Queries may return fewer than @resultsLimit@ in some scenarios:  - There are legitimately fewer than @resultsLimit@ number of records matching the query (and visible to the current user).  - During the process of querying and fetching the results, some records were deleted, or became un-readable by the current user.  When determining if there are more records to fetch, always check for the presence of a cursor in @queryCompletionBlock.@
--
-- ObjC selector: @- setResultsLimit:@
setResultsLimit :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> CULong -> IO ()
setResultsLimit ckQueryOperation value =
  sendMessage ckQueryOperation setResultsLimitSelector value

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id NSArray)
desiredKeys ckQueryOperation =
  sendMessage ckQueryOperation desiredKeysSelector

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKQueryOperation ckQueryOperation, IsNSArray value) => ckQueryOperation -> value -> IO ()
setDesiredKeys ckQueryOperation value =
  sendMessage ckQueryOperation setDesiredKeysSelector (toNSArray value)

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If the replacement callback @recordMatchedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordFetchedBlock@
recordFetchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Ptr ())
recordFetchedBlock ckQueryOperation =
  sendMessage ckQueryOperation recordFetchedBlockSelector

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If the replacement callback @recordMatchedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordFetchedBlock:@
setRecordFetchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> Ptr () -> IO ()
setRecordFetchedBlock ckQueryOperation value =
  sendMessage ckQueryOperation setRecordFetchedBlockSelector value

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordMatchedBlock@
recordMatchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Ptr ())
recordMatchedBlock ckQueryOperation =
  sendMessage ckQueryOperation recordMatchedBlockSelector

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordMatchedBlock:@
setRecordMatchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> Ptr () -> IO ()
setRecordMatchedBlock ckQueryOperation value =
  sendMessage ckQueryOperation setRecordMatchedBlockSelector value

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of recordIDs to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @recordMatchedBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- queryCompletionBlock@
queryCompletionBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Ptr ())
queryCompletionBlock ckQueryOperation =
  sendMessage ckQueryOperation queryCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- The
--
-- -[NSOperation completionBlock]
--
-- will also be called if both are set.  If the error is @CKErrorPartialFailure,@ the error's userInfo dictionary contains a dictionary of recordIDs to errors keyed off of @CKPartialErrorsByItemIDKey.@  These errors are repeats of those sent back in previous @recordMatchedBlock@ invocations  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setQueryCompletionBlock:@
setQueryCompletionBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> Ptr () -> IO ()
setQueryCompletionBlock ckQueryOperation value =
  sendMessage ckQueryOperation setQueryCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKQueryOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithQuery:@
initWithQuerySelector :: Selector '[Id CKQuery] (Id CKQueryOperation)
initWithQuerySelector = mkSelector "initWithQuery:"

-- | @Selector@ for @initWithCursor:@
initWithCursorSelector :: Selector '[Id CKQueryCursor] (Id CKQueryOperation)
initWithCursorSelector = mkSelector "initWithCursor:"

-- | @Selector@ for @query@
querySelector :: Selector '[] (Id CKQuery)
querySelector = mkSelector "query"

-- | @Selector@ for @setQuery:@
setQuerySelector :: Selector '[Id CKQuery] ()
setQuerySelector = mkSelector "setQuery:"

-- | @Selector@ for @cursor@
cursorSelector :: Selector '[] (Id CKQueryCursor)
cursorSelector = mkSelector "cursor"

-- | @Selector@ for @setCursor:@
setCursorSelector :: Selector '[Id CKQueryCursor] ()
setCursorSelector = mkSelector "setCursor:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector '[] (Id CKRecordZoneID)
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector '[Id CKRecordZoneID] ()
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @resultsLimit@
resultsLimitSelector :: Selector '[] CULong
resultsLimitSelector = mkSelector "resultsLimit"

-- | @Selector@ for @setResultsLimit:@
setResultsLimitSelector :: Selector '[CULong] ()
setResultsLimitSelector = mkSelector "setResultsLimit:"

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector '[] (Id NSArray)
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector '[Id NSArray] ()
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

-- | @Selector@ for @recordFetchedBlock@
recordFetchedBlockSelector :: Selector '[] (Ptr ())
recordFetchedBlockSelector = mkSelector "recordFetchedBlock"

-- | @Selector@ for @setRecordFetchedBlock:@
setRecordFetchedBlockSelector :: Selector '[Ptr ()] ()
setRecordFetchedBlockSelector = mkSelector "setRecordFetchedBlock:"

-- | @Selector@ for @recordMatchedBlock@
recordMatchedBlockSelector :: Selector '[] (Ptr ())
recordMatchedBlockSelector = mkSelector "recordMatchedBlock"

-- | @Selector@ for @setRecordMatchedBlock:@
setRecordMatchedBlockSelector :: Selector '[Ptr ()] ()
setRecordMatchedBlockSelector = mkSelector "setRecordMatchedBlock:"

-- | @Selector@ for @queryCompletionBlock@
queryCompletionBlockSelector :: Selector '[] (Ptr ())
queryCompletionBlockSelector = mkSelector "queryCompletionBlock"

-- | @Selector@ for @setQueryCompletionBlock:@
setQueryCompletionBlockSelector :: Selector '[Ptr ()] ()
setQueryCompletionBlockSelector = mkSelector "setQueryCompletionBlock:"

