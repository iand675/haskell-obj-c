{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | CKFetchRecordChangesOperation
--
-- Use CKFetchRecordZoneChangesOperation instead of this class.
--
-- Any serverChangeTokens saved from a CKFetchRecordChangesOperation are usable as a serverRecordZoneChangeToken in CKFetchRecordZoneChangesOperation
--
-- This operation will fetch records changes in the given record zone.
--
-- If a change token from a previous @CKFetchRecordChangesOperation@ is passed in, only the records that have changed since that token will be fetched.  If this is your first fetch or if you wish to re-fetch all records, pass nil for the change token.  Change tokens are opaque tokens and clients should not infer any behavior based on their content
--
-- Generated bindings for @CKFetchRecordChangesOperation@.
module ObjC.CloudKit.CKFetchRecordChangesOperation
  ( CKFetchRecordChangesOperation
  , IsCKFetchRecordChangesOperation(..)
  , init_
  , initWithRecordZoneID_previousServerChangeToken
  , recordZoneID
  , setRecordZoneID
  , previousServerChangeToken
  , setPreviousServerChangeToken
  , resultsLimit
  , setResultsLimit
  , desiredKeys
  , setDesiredKeys
  , recordChangedBlock
  , setRecordChangedBlock
  , recordWithIDWasDeletedBlock
  , setRecordWithIDWasDeletedBlock
  , moreComing
  , fetchRecordChangesCompletionBlock
  , setFetchRecordChangesCompletionBlock
  , desiredKeysSelector
  , fetchRecordChangesCompletionBlockSelector
  , initSelector
  , initWithRecordZoneID_previousServerChangeTokenSelector
  , moreComingSelector
  , previousServerChangeTokenSelector
  , recordChangedBlockSelector
  , recordWithIDWasDeletedBlockSelector
  , recordZoneIDSelector
  , resultsLimitSelector
  , setDesiredKeysSelector
  , setFetchRecordChangesCompletionBlockSelector
  , setPreviousServerChangeTokenSelector
  , setRecordChangedBlockSelector
  , setRecordWithIDWasDeletedBlockSelector
  , setRecordZoneIDSelector
  , setResultsLimitSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CloudKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id CKFetchRecordChangesOperation)
init_ ckFetchRecordChangesOperation =
  sendOwnedMessage ckFetchRecordChangesOperation initSelector

-- | @- initWithRecordZoneID:previousServerChangeToken:@
initWithRecordZoneID_previousServerChangeToken :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsCKRecordZoneID recordZoneID, IsCKServerChangeToken previousServerChangeToken) => ckFetchRecordChangesOperation -> recordZoneID -> previousServerChangeToken -> IO (Id CKFetchRecordChangesOperation)
initWithRecordZoneID_previousServerChangeToken ckFetchRecordChangesOperation recordZoneID previousServerChangeToken =
  sendOwnedMessage ckFetchRecordChangesOperation initWithRecordZoneID_previousServerChangeTokenSelector (toCKRecordZoneID recordZoneID) (toCKServerChangeToken previousServerChangeToken)

-- | @- recordZoneID@
recordZoneID :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id CKRecordZoneID)
recordZoneID ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation recordZoneIDSelector

-- | @- setRecordZoneID:@
setRecordZoneID :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsCKRecordZoneID value) => ckFetchRecordChangesOperation -> value -> IO ()
setRecordZoneID ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setRecordZoneIDSelector (toCKRecordZoneID value)

-- | @- previousServerChangeToken@
previousServerChangeToken :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation previousServerChangeTokenSelector

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsCKServerChangeToken value) => ckFetchRecordChangesOperation -> value -> IO ()
setPreviousServerChangeToken ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setPreviousServerChangeTokenSelector (toCKServerChangeToken value)

-- | @- resultsLimit@
resultsLimit :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO CULong
resultsLimit ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation resultsLimitSelector

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> CULong -> IO ()
setResultsLimit ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setResultsLimitSelector value

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id NSArray)
desiredKeys ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation desiredKeysSelector

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsNSArray value) => ckFetchRecordChangesOperation -> value -> IO ()
setDesiredKeys ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setDesiredKeysSelector (toNSArray value)

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordChangedBlock@
recordChangedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Ptr ())
recordChangedBlock ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation recordChangedBlockSelector

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordChangedBlock:@
setRecordChangedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> Ptr () -> IO ()
setRecordChangedBlock ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setRecordChangedBlockSelector value

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Ptr ())
recordWithIDWasDeletedBlock ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation recordWithIDWasDeletedBlockSelector

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> Ptr () -> IO ()
setRecordWithIDWasDeletedBlock ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setRecordWithIDWasDeletedBlockSelector value

-- | If true, then the server wasn't able to return all the changes in this response.
--
-- Will be set before fetchRecordChangesCompletionBlock is called.  Another CKFetchRecordChangesOperation operation should be run with the updated serverChangeToken token from this operation.
--
-- ObjC selector: @- moreComing@
moreComing :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO Bool
moreComing ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation moreComingSelector

-- | This block is called when the operation completes.
--
-- Clients are responsible for saving the change token at the end of the operation and passing it in to the next call to @CKFetchRecordChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in the completion block so that already fetched records don't need to be re-downloaded on a subsequent operation.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @previousServerChangeToken@ value was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @previousServerChangeToken.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchRecordChangesCompletionBlock@
fetchRecordChangesCompletionBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Ptr ())
fetchRecordChangesCompletionBlock ckFetchRecordChangesOperation =
  sendMessage ckFetchRecordChangesOperation fetchRecordChangesCompletionBlockSelector

-- | This block is called when the operation completes.
--
-- Clients are responsible for saving the change token at the end of the operation and passing it in to the next call to @CKFetchRecordChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in the completion block so that already fetched records don't need to be re-downloaded on a subsequent operation.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @previousServerChangeToken@ value was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @previousServerChangeToken.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchRecordChangesCompletionBlock:@
setFetchRecordChangesCompletionBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> Ptr () -> IO ()
setFetchRecordChangesCompletionBlock ckFetchRecordChangesOperation value =
  sendMessage ckFetchRecordChangesOperation setFetchRecordChangesCompletionBlockSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id CKFetchRecordChangesOperation)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZoneID:previousServerChangeToken:@
initWithRecordZoneID_previousServerChangeTokenSelector :: Selector '[Id CKRecordZoneID, Id CKServerChangeToken] (Id CKFetchRecordChangesOperation)
initWithRecordZoneID_previousServerChangeTokenSelector = mkSelector "initWithRecordZoneID:previousServerChangeToken:"

-- | @Selector@ for @recordZoneID@
recordZoneIDSelector :: Selector '[] (Id CKRecordZoneID)
recordZoneIDSelector = mkSelector "recordZoneID"

-- | @Selector@ for @setRecordZoneID:@
setRecordZoneIDSelector :: Selector '[Id CKRecordZoneID] ()
setRecordZoneIDSelector = mkSelector "setRecordZoneID:"

-- | @Selector@ for @previousServerChangeToken@
previousServerChangeTokenSelector :: Selector '[] (Id CKServerChangeToken)
previousServerChangeTokenSelector = mkSelector "previousServerChangeToken"

-- | @Selector@ for @setPreviousServerChangeToken:@
setPreviousServerChangeTokenSelector :: Selector '[Id CKServerChangeToken] ()
setPreviousServerChangeTokenSelector = mkSelector "setPreviousServerChangeToken:"

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

-- | @Selector@ for @recordChangedBlock@
recordChangedBlockSelector :: Selector '[] (Ptr ())
recordChangedBlockSelector = mkSelector "recordChangedBlock"

-- | @Selector@ for @setRecordChangedBlock:@
setRecordChangedBlockSelector :: Selector '[Ptr ()] ()
setRecordChangedBlockSelector = mkSelector "setRecordChangedBlock:"

-- | @Selector@ for @recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlockSelector :: Selector '[] (Ptr ())
recordWithIDWasDeletedBlockSelector = mkSelector "recordWithIDWasDeletedBlock"

-- | @Selector@ for @setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlockSelector :: Selector '[Ptr ()] ()
setRecordWithIDWasDeletedBlockSelector = mkSelector "setRecordWithIDWasDeletedBlock:"

-- | @Selector@ for @moreComing@
moreComingSelector :: Selector '[] Bool
moreComingSelector = mkSelector "moreComing"

-- | @Selector@ for @fetchRecordChangesCompletionBlock@
fetchRecordChangesCompletionBlockSelector :: Selector '[] (Ptr ())
fetchRecordChangesCompletionBlockSelector = mkSelector "fetchRecordChangesCompletionBlock"

-- | @Selector@ for @setFetchRecordChangesCompletionBlock:@
setFetchRecordChangesCompletionBlockSelector :: Selector '[Ptr ()] ()
setFetchRecordChangesCompletionBlockSelector = mkSelector "setFetchRecordChangesCompletionBlock:"

