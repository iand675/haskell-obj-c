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
  , initSelector
  , initWithQuerySelector
  , initWithCursorSelector
  , querySelector
  , setQuerySelector
  , cursorSelector
  , setCursorSelector
  , zoneIDSelector
  , setZoneIDSelector
  , resultsLimitSelector
  , setResultsLimitSelector
  , desiredKeysSelector
  , setDesiredKeysSelector
  , recordFetchedBlockSelector
  , setRecordFetchedBlockSelector
  , recordMatchedBlockSelector
  , setRecordMatchedBlockSelector
  , queryCompletionBlockSelector
  , setQueryCompletionBlockSelector


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

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Queries invoked within a sharedCloudDatabase must specify a zoneID.  Cross-zone queries are not supported in a sharedCloudDatabase
--
-- ObjC selector: @- init@
init_ :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKQueryOperation)
init_ ckQueryOperation  =
  sendMsg ckQueryOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithQuery:@
initWithQuery :: (IsCKQueryOperation ckQueryOperation, IsCKQuery query) => ckQueryOperation -> query -> IO (Id CKQueryOperation)
initWithQuery ckQueryOperation  query =
withObjCPtr query $ \raw_query ->
    sendMsg ckQueryOperation (mkSelector "initWithQuery:") (retPtr retVoid) [argPtr (castPtr raw_query :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCursor:@
initWithCursor :: (IsCKQueryOperation ckQueryOperation, IsCKQueryCursor cursor) => ckQueryOperation -> cursor -> IO (Id CKQueryOperation)
initWithCursor ckQueryOperation  cursor =
withObjCPtr cursor $ \raw_cursor ->
    sendMsg ckQueryOperation (mkSelector "initWithCursor:") (retPtr retVoid) [argPtr (castPtr raw_cursor :: Ptr ())] >>= ownedObject . castPtr

-- | @- query@
query :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKQuery)
query ckQueryOperation  =
  sendMsg ckQueryOperation (mkSelector "query") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setQuery:@
setQuery :: (IsCKQueryOperation ckQueryOperation, IsCKQuery value) => ckQueryOperation -> value -> IO ()
setQuery ckQueryOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckQueryOperation (mkSelector "setQuery:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cursor@
cursor :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKQueryCursor)
cursor ckQueryOperation  =
  sendMsg ckQueryOperation (mkSelector "cursor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCursor:@
setCursor :: (IsCKQueryOperation ckQueryOperation, IsCKQueryCursor value) => ckQueryOperation -> value -> IO ()
setCursor ckQueryOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckQueryOperation (mkSelector "setCursor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Indicates which record zone to query.
--
-- For query operations constructed using a cursor, this property is ignored and instead will be evaluated in the record zone in which the cursor was originally created.  Queries that do not specify a @zoneID@ will perform a query across all zones in the database.
--
-- ObjC selector: @- zoneID@
zoneID :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id CKRecordZoneID)
zoneID ckQueryOperation  =
  sendMsg ckQueryOperation (mkSelector "zoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Indicates which record zone to query.
--
-- For query operations constructed using a cursor, this property is ignored and instead will be evaluated in the record zone in which the cursor was originally created.  Queries that do not specify a @zoneID@ will perform a query across all zones in the database.
--
-- ObjC selector: @- setZoneID:@
setZoneID :: (IsCKQueryOperation ckQueryOperation, IsCKRecordZoneID value) => ckQueryOperation -> value -> IO ()
setZoneID ckQueryOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckQueryOperation (mkSelector "setZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Defaults to @CKQueryOperationMaximumResults.@  Queries may return fewer than @resultsLimit@ in some scenarios:  - There are legitimately fewer than @resultsLimit@ number of records matching the query (and visible to the current user).  - During the process of querying and fetching the results, some records were deleted, or became un-readable by the current user.  When determining if there are more records to fetch, always check for the presence of a cursor in @queryCompletionBlock.@
--
-- ObjC selector: @- resultsLimit@
resultsLimit :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO CULong
resultsLimit ckQueryOperation  =
  sendMsg ckQueryOperation (mkSelector "resultsLimit") retCULong []

-- | Defaults to @CKQueryOperationMaximumResults.@  Queries may return fewer than @resultsLimit@ in some scenarios:  - There are legitimately fewer than @resultsLimit@ number of records matching the query (and visible to the current user).  - During the process of querying and fetching the results, some records were deleted, or became un-readable by the current user.  When determining if there are more records to fetch, always check for the presence of a cursor in @queryCompletionBlock.@
--
-- ObjC selector: @- setResultsLimit:@
setResultsLimit :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> CULong -> IO ()
setResultsLimit ckQueryOperation  value =
  sendMsg ckQueryOperation (mkSelector "setResultsLimit:") retVoid [argCULong (fromIntegral value)]

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Id NSArray)
desiredKeys ckQueryOperation  =
  sendMsg ckQueryOperation (mkSelector "desiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKQueryOperation ckQueryOperation, IsNSArray value) => ckQueryOperation -> value -> IO ()
setDesiredKeys ckQueryOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckQueryOperation (mkSelector "setDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If the replacement callback @recordMatchedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordFetchedBlock@
recordFetchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Ptr ())
recordFetchedBlock ckQueryOperation  =
  fmap castPtr $ sendMsg ckQueryOperation (mkSelector "recordFetchedBlock") (retPtr retVoid) []

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If the replacement callback @recordMatchedBlock@ is set, this callback block is ignored.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordFetchedBlock:@
setRecordFetchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> Ptr () -> IO ()
setRecordFetchedBlock ckQueryOperation  value =
  sendMsg ckQueryOperation (mkSelector "setRecordFetchedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordMatchedBlock@
recordMatchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> IO (Ptr ())
recordMatchedBlock ckQueryOperation  =
  fmap castPtr $ sendMsg ckQueryOperation (mkSelector "recordMatchedBlock") (retPtr retVoid) []

-- | This block will be called once for every record that is returned as a result of the query.
--
-- The callbacks will happen in the order that the results were sorted in.  If a record fails in post-processing (say, a network failure materializing a @CKAsset@ record field), the per-record error will be passed here.  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordMatchedBlock:@
setRecordMatchedBlock :: IsCKQueryOperation ckQueryOperation => ckQueryOperation -> Ptr () -> IO ()
setRecordMatchedBlock ckQueryOperation  value =
  sendMsg ckQueryOperation (mkSelector "setRecordMatchedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

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
queryCompletionBlock ckQueryOperation  =
  fmap castPtr $ sendMsg ckQueryOperation (mkSelector "queryCompletionBlock") (retPtr retVoid) []

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
setQueryCompletionBlock ckQueryOperation  value =
  sendMsg ckQueryOperation (mkSelector "setQueryCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithQuery:@
initWithQuerySelector :: Selector
initWithQuerySelector = mkSelector "initWithQuery:"

-- | @Selector@ for @initWithCursor:@
initWithCursorSelector :: Selector
initWithCursorSelector = mkSelector "initWithCursor:"

-- | @Selector@ for @query@
querySelector :: Selector
querySelector = mkSelector "query"

-- | @Selector@ for @setQuery:@
setQuerySelector :: Selector
setQuerySelector = mkSelector "setQuery:"

-- | @Selector@ for @cursor@
cursorSelector :: Selector
cursorSelector = mkSelector "cursor"

-- | @Selector@ for @setCursor:@
setCursorSelector :: Selector
setCursorSelector = mkSelector "setCursor:"

-- | @Selector@ for @zoneID@
zoneIDSelector :: Selector
zoneIDSelector = mkSelector "zoneID"

-- | @Selector@ for @setZoneID:@
setZoneIDSelector :: Selector
setZoneIDSelector = mkSelector "setZoneID:"

-- | @Selector@ for @resultsLimit@
resultsLimitSelector :: Selector
resultsLimitSelector = mkSelector "resultsLimit"

-- | @Selector@ for @setResultsLimit:@
setResultsLimitSelector :: Selector
setResultsLimitSelector = mkSelector "setResultsLimit:"

-- | @Selector@ for @desiredKeys@
desiredKeysSelector :: Selector
desiredKeysSelector = mkSelector "desiredKeys"

-- | @Selector@ for @setDesiredKeys:@
setDesiredKeysSelector :: Selector
setDesiredKeysSelector = mkSelector "setDesiredKeys:"

-- | @Selector@ for @recordFetchedBlock@
recordFetchedBlockSelector :: Selector
recordFetchedBlockSelector = mkSelector "recordFetchedBlock"

-- | @Selector@ for @setRecordFetchedBlock:@
setRecordFetchedBlockSelector :: Selector
setRecordFetchedBlockSelector = mkSelector "setRecordFetchedBlock:"

-- | @Selector@ for @recordMatchedBlock@
recordMatchedBlockSelector :: Selector
recordMatchedBlockSelector = mkSelector "recordMatchedBlock"

-- | @Selector@ for @setRecordMatchedBlock:@
setRecordMatchedBlockSelector :: Selector
setRecordMatchedBlockSelector = mkSelector "setRecordMatchedBlock:"

-- | @Selector@ for @queryCompletionBlock@
queryCompletionBlockSelector :: Selector
queryCompletionBlockSelector = mkSelector "queryCompletionBlock"

-- | @Selector@ for @setQueryCompletionBlock:@
setQueryCompletionBlockSelector :: Selector
setQueryCompletionBlockSelector = mkSelector "setQueryCompletionBlock:"

