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
  , initSelector
  , initWithRecordZoneID_previousServerChangeTokenSelector
  , recordZoneIDSelector
  , setRecordZoneIDSelector
  , previousServerChangeTokenSelector
  , setPreviousServerChangeTokenSelector
  , resultsLimitSelector
  , setResultsLimitSelector
  , desiredKeysSelector
  , setDesiredKeysSelector
  , recordChangedBlockSelector
  , setRecordChangedBlockSelector
  , recordWithIDWasDeletedBlockSelector
  , setRecordWithIDWasDeletedBlockSelector
  , moreComingSelector
  , fetchRecordChangesCompletionBlockSelector
  , setFetchRecordChangesCompletionBlockSelector


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

-- | @- init@
init_ :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id CKFetchRecordChangesOperation)
init_ ckFetchRecordChangesOperation  =
  sendMsg ckFetchRecordChangesOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRecordZoneID:previousServerChangeToken:@
initWithRecordZoneID_previousServerChangeToken :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsCKRecordZoneID recordZoneID, IsCKServerChangeToken previousServerChangeToken) => ckFetchRecordChangesOperation -> recordZoneID -> previousServerChangeToken -> IO (Id CKFetchRecordChangesOperation)
initWithRecordZoneID_previousServerChangeToken ckFetchRecordChangesOperation  recordZoneID previousServerChangeToken =
withObjCPtr recordZoneID $ \raw_recordZoneID ->
  withObjCPtr previousServerChangeToken $ \raw_previousServerChangeToken ->
      sendMsg ckFetchRecordChangesOperation (mkSelector "initWithRecordZoneID:previousServerChangeToken:") (retPtr retVoid) [argPtr (castPtr raw_recordZoneID :: Ptr ()), argPtr (castPtr raw_previousServerChangeToken :: Ptr ())] >>= ownedObject . castPtr

-- | @- recordZoneID@
recordZoneID :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id CKRecordZoneID)
recordZoneID ckFetchRecordChangesOperation  =
  sendMsg ckFetchRecordChangesOperation (mkSelector "recordZoneID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRecordZoneID:@
setRecordZoneID :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsCKRecordZoneID value) => ckFetchRecordChangesOperation -> value -> IO ()
setRecordZoneID ckFetchRecordChangesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordChangesOperation (mkSelector "setRecordZoneID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- previousServerChangeToken@
previousServerChangeToken :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id CKServerChangeToken)
previousServerChangeToken ckFetchRecordChangesOperation  =
  sendMsg ckFetchRecordChangesOperation (mkSelector "previousServerChangeToken") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPreviousServerChangeToken:@
setPreviousServerChangeToken :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsCKServerChangeToken value) => ckFetchRecordChangesOperation -> value -> IO ()
setPreviousServerChangeToken ckFetchRecordChangesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordChangesOperation (mkSelector "setPreviousServerChangeToken:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- resultsLimit@
resultsLimit :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO CULong
resultsLimit ckFetchRecordChangesOperation  =
  sendMsg ckFetchRecordChangesOperation (mkSelector "resultsLimit") retCULong []

-- | @- setResultsLimit:@
setResultsLimit :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> CULong -> IO ()
setResultsLimit ckFetchRecordChangesOperation  value =
  sendMsg ckFetchRecordChangesOperation (mkSelector "setResultsLimit:") retVoid [argCULong (fromIntegral value)]

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- desiredKeys@
desiredKeys :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Id NSArray)
desiredKeys ckFetchRecordChangesOperation  =
  sendMsg ckFetchRecordChangesOperation (mkSelector "desiredKeys") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Declares which user-defined keys should be fetched and added to the resulting CKRecords.
--
-- If nil, declares the entire record should be downloaded. If set to an empty array, declares that no user fields should be downloaded.  Defaults to @nil.@
--
-- ObjC selector: @- setDesiredKeys:@
setDesiredKeys :: (IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation, IsNSArray value) => ckFetchRecordChangesOperation -> value -> IO ()
setDesiredKeys ckFetchRecordChangesOperation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ckFetchRecordChangesOperation (mkSelector "setDesiredKeys:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordChangedBlock@
recordChangedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Ptr ())
recordChangedBlock ckFetchRecordChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordChangesOperation (mkSelector "recordChangedBlock") (retPtr retVoid) []

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordChangedBlock:@
setRecordChangedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> Ptr () -> IO ()
setRecordChangedBlock ckFetchRecordChangesOperation  value =
  sendMsg ckFetchRecordChangesOperation (mkSelector "setRecordChangedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Ptr ())
recordWithIDWasDeletedBlock ckFetchRecordChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordChangesOperation (mkSelector "recordWithIDWasDeletedBlock") (retPtr retVoid) []

-- | Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> Ptr () -> IO ()
setRecordWithIDWasDeletedBlock ckFetchRecordChangesOperation  value =
  sendMsg ckFetchRecordChangesOperation (mkSelector "setRecordWithIDWasDeletedBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- | If true, then the server wasn't able to return all the changes in this response.
--
-- Will be set before fetchRecordChangesCompletionBlock is called.  Another CKFetchRecordChangesOperation operation should be run with the updated serverChangeToken token from this operation.
--
-- ObjC selector: @- moreComing@
moreComing :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO Bool
moreComing ckFetchRecordChangesOperation  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg ckFetchRecordChangesOperation (mkSelector "moreComing") retCULong []

-- | This block is called when the operation completes.
--
-- Clients are responsible for saving the change token at the end of the operation and passing it in to the next call to @CKFetchRecordChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in the completion block so that already fetched records don't need to be re-downloaded on a subsequent operation.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @previousServerChangeToken@ value was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @previousServerChangeToken.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- fetchRecordChangesCompletionBlock@
fetchRecordChangesCompletionBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> IO (Ptr ())
fetchRecordChangesCompletionBlock ckFetchRecordChangesOperation  =
  fmap castPtr $ sendMsg ckFetchRecordChangesOperation (mkSelector "fetchRecordChangesCompletionBlock") (retPtr retVoid) []

-- | This block is called when the operation completes.
--
-- Clients are responsible for saving the change token at the end of the operation and passing it in to the next call to @CKFetchRecordChangesOperation.@  Note that a fetch can fail partway. If that happens, an updated change token may be returned in the completion block so that already fetched records don't need to be re-downloaded on a subsequent operation.  The @clientChangeTokenData@ from the most recent @CKModifyRecordsOperation@ is also returned, or nil if none was provided.  If the server returns a @CKErrorChangeTokenExpired@ error, the @previousServerChangeToken@ value was too old and the client should toss its local cache and re-fetch the changes in this record zone starting with a nil @previousServerChangeToken.@  Each @CKOperation@ instance has a private serial queue. This queue is used for all callback block invocations.  This block may share mutable state with other blocks assigned to this operation, but any such mutable state  should not be concurrently used outside of blocks assigned to this operation.
--
-- ObjC selector: @- setFetchRecordChangesCompletionBlock:@
setFetchRecordChangesCompletionBlock :: IsCKFetchRecordChangesOperation ckFetchRecordChangesOperation => ckFetchRecordChangesOperation -> Ptr () -> IO ()
setFetchRecordChangesCompletionBlock ckFetchRecordChangesOperation  value =
  sendMsg ckFetchRecordChangesOperation (mkSelector "setFetchRecordChangesCompletionBlock:") retVoid [argPtr (castPtr value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRecordZoneID:previousServerChangeToken:@
initWithRecordZoneID_previousServerChangeTokenSelector :: Selector
initWithRecordZoneID_previousServerChangeTokenSelector = mkSelector "initWithRecordZoneID:previousServerChangeToken:"

-- | @Selector@ for @recordZoneID@
recordZoneIDSelector :: Selector
recordZoneIDSelector = mkSelector "recordZoneID"

-- | @Selector@ for @setRecordZoneID:@
setRecordZoneIDSelector :: Selector
setRecordZoneIDSelector = mkSelector "setRecordZoneID:"

-- | @Selector@ for @previousServerChangeToken@
previousServerChangeTokenSelector :: Selector
previousServerChangeTokenSelector = mkSelector "previousServerChangeToken"

-- | @Selector@ for @setPreviousServerChangeToken:@
setPreviousServerChangeTokenSelector :: Selector
setPreviousServerChangeTokenSelector = mkSelector "setPreviousServerChangeToken:"

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

-- | @Selector@ for @recordChangedBlock@
recordChangedBlockSelector :: Selector
recordChangedBlockSelector = mkSelector "recordChangedBlock"

-- | @Selector@ for @setRecordChangedBlock:@
setRecordChangedBlockSelector :: Selector
setRecordChangedBlockSelector = mkSelector "setRecordChangedBlock:"

-- | @Selector@ for @recordWithIDWasDeletedBlock@
recordWithIDWasDeletedBlockSelector :: Selector
recordWithIDWasDeletedBlockSelector = mkSelector "recordWithIDWasDeletedBlock"

-- | @Selector@ for @setRecordWithIDWasDeletedBlock:@
setRecordWithIDWasDeletedBlockSelector :: Selector
setRecordWithIDWasDeletedBlockSelector = mkSelector "setRecordWithIDWasDeletedBlock:"

-- | @Selector@ for @moreComing@
moreComingSelector :: Selector
moreComingSelector = mkSelector "moreComing"

-- | @Selector@ for @fetchRecordChangesCompletionBlock@
fetchRecordChangesCompletionBlockSelector :: Selector
fetchRecordChangesCompletionBlockSelector = mkSelector "fetchRecordChangesCompletionBlock"

-- | @Selector@ for @setFetchRecordChangesCompletionBlock:@
setFetchRecordChangesCompletionBlockSelector :: Selector
setFetchRecordChangesCompletionBlockSelector = mkSelector "setFetchRecordChangesCompletionBlock:"

